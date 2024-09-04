from gpt.gptcore import BaseChatClass
import re
from gpt.prompt import get_incontext_learning_contents
from gpt.prompt_all import *
from gpt.utils import fix_missing_closing_brackets, deconstruct_expr
import logging
from lark import Token, Tree
from gpt.analysizer import RoleParser, TopParser


def parse_with_fallback(parser, spec):
    try:
        return parser.parse(spec)
    except:
        return None


def collect_subtrees(tree_node, name: str):
    subtrees = []

    if isinstance(tree_node, Tree):
        if tree_node.data == name:
            subtrees.append(tree_node)
        for child in tree_node.children:
            subtrees.extend(collect_subtrees(child, name))
    return subtrees


def _vars(tree_node):
    if isinstance(tree_node, Token):
        return {tree_node.value}
    elif isinstance(tree_node, Tree):
        vars_set = set()
        start_index = 0
        if tree_node.data in ["func", "role"]:  # reduce the funcname
            start_index = 1
        for child in tree_node.children[start_index:]:
            vars_set.update(_vars(child))
        return vars_set
    else:
        return set()


def fresh_nonces(tree_node):
    if isinstance(tree_node, Token):
        return {tree_node.value}
    elif isinstance(tree_node, Tree):
        vars_set = set()
        if tree_node.data == "new":
            for child in tree_node.children:
                if isinstance(child, Token):
                    vars_set.add(child.value)
            return vars_set
        else:
            for child in tree_node.children:
                vars_set.update(fresh_nonces(child))
            return vars_set
    else:
        return set()


def build_role_dict(node: Tree):
    role_dict = {}
    if node.data == "role":
        role_name = node.children[0]
        init_knowledge = node.children[1]
        if isinstance(role_name, Token):
            role_name = role_name.value
        vars = _vars(node)
        role_dict[role_name] = vars
    else:
        for child in node.children:
            if isinstance(child, Tree):
                child_dict = build_role_dict(child)
                role_dict.update(child_dict)
    return role_dict


def extract_fresh(node: Tree):
    new_stmts = collect_subtrees(node, "new")
    fresh = set([_vars(t) for t in new_stmts])
    return set().union(*fresh)


def extract_intermediate_vars(root: Tree):
    binding_stms = collect_subtrees(root, "binding")
    intermediate_vars = [_vars(equation.children[0]) for equation in binding_stms]
    return set().union(*intermediate_vars)


def extract_from_comments(comment_string):
    matches = re.findall(r"/\*(.*?)\*/", comment_string, re.DOTALL)
    return matches[0].strip()


def record_reading_process(chunks: list, Lambda: list, output_folder):
    log = ""
    for i, _ in enumerate(chunks):
        log += f"\n\U0001f4d6:{chunks[i]}" + f"\n\U0001f916:/*\n{Lambda[i]}\n*/\n"
    try:
        with open(output_folder, "w") as f:
            f.write(log)
    except:
        pass


def hire_llm_read_doc(
    File: str,
    llm_model,
    temperature,
    n_choices,
    maxTokens,
    recording_folder,
    prompt_list=[],
    useOpenKey=True,
):
    if prompt_list:
        chatveri = BaseChatClass(
            prompt_list, useOpenKey=useOpenKey, continuous_talking=True
        )
    else:
        chatveri = BaseChatClass(
            get_incontext_learning_contents("SeqReader"),
            useOpenKey=useOpenKey,
            continuous_talking=True,
        )
    # chatveri.show_conversation(chatveri.conversation_list)
    chunks = File.split("\n\n")
    Lambda = []
    logging.info(f"\U0001f914: Ok, I'm reading the document you give me...\n")
    for i, chunk in enumerate(chunks):
        chunks[i] += "\n/* >>The lambda calculus << */"

        if i > 0:
            chunks[i - 1] = chunks[i - 1].replace(
                "/* >>The lambda calculus << */", f"/*\n{Lambda[-1]}\n*/"
            )
        if i == 0:
            content = chunks[i]
        else:
            content = chunks[i - 1] + "\n" + chunks[i]
        question = content
        logging.info(f"\U0001f4d6:{question}")

        full_reply_content_list, tokens_usage = chatveri.get_respone(
            question,
            model=llm_model,
            maxTokens=maxTokens,
            temperature_arg=temperature,
            n_choices=n_choices,
        )
        output = full_reply_content_list[0]

        lam_expr = extract_from_comments(output)
        Lambda += [lam_expr]

    texts = File.split("\n\n")
    Lambda_spec = "\n".join(Lambda)

    try:
        with open("./lambda.txt", "w") as f:
            f.write(Lambda_spec)
    except:
        pass

    record_reading_process(texts, Lambda, recording_folder)

    return Lambda_spec


def T_transform(Lambda_spec: str) -> str:
    try:
        ##== Filter out all the deconstrction expressions,  ==##
        ##== only allowing construction and I/O expressions ==##
        
        corr_Lambda_spec = "\n".join(
            fix_missing_closing_brackets(line)
            for line in Lambda_spec.split("\n")
            if not deconstruct_expr(line)
        )

        with open("./filter_expr.txt", "w") as f:
            f.write(corr_Lambda_spec)

        from gpt.translator import lambda_to_processes, format_parse_output

        spec, _ = lambda_to_processes(corr_Lambda_spec)
        Role_spec, _ = format_parse_output(spec)
    except Exception as e:
        logging.error(str(e))
        Role_spec = ""
    return Role_spec


def repair_role_spec(
    repair_question: str, llm_model, temperature, n_choices, maxTokens, useOpenKey
) -> str:
    chatveri = BaseChatClass(
        get_incontext_learning_contents("repair"), useOpenKey=useOpenKey
    )
    repaired, tokens_usage = chatveri.get_respone(
        repair_question,
        model=llm_model,
        maxTokens=maxTokens,
        temperature_arg=temperature,
        n_choices=n_choices,
    )
    return repaired, tokens_usage


def eliminate_comments_from_top_spec(spec: str) -> str:
    """elimiate comments from top specs,
    ensures that it starts with the KEYWORD: process
    """
    spec = re.sub(r"//.*?$", "", spec, flags=re.MULTILINE)
    spec = re.sub(r"\n\s*\n", "\n", spec)
    spec = spec[0].lower() + spec[1:]

    # ensures that it starts with the KEYWORD: process
    KEYWORD = "process:"
    assert spec.startswith(KEYWORD)
    return spec


def determin_initial_role_vars(
    doc: str,
    spec: str,
    llm_model,
    temperature,
    n_choices,
    maxTokens,
    useOpenKey=True,
    hint="",
) -> str:

    detemine_template = """\
Description: <The protocol text I give you>           
Incomplete spec:
<The spec I give you>

<The hints I give you>
"""
    chatveri = BaseChatClass(
        get_incontext_learning_contents("determine_init_var"), useOpenKey=useOpenKey
    )
    determine_question = (
        detemine_template.replace("<The protocol text I give you>", doc)
        .replace("<The spec I give you>", spec)
        .replace("<The hints I give you>", hint)
    )
    determine_answer, tokens_usage = chatveri.get_respone(
        determine_question,
        model=llm_model,
        maxTokens=maxTokens,
        temperature_arg=temperature,
        n_choices=n_choices,
    )
    conclude_in_dict = """\
Based on above analysis, conclude your results in dictionary. If you think there are some implicit knowledge for some role, \
update the dictionary with them. 
If you think there are some mistakes in the above result, correct them.
Do not include any other explaination in your result.
<The spec I give you>"""
    determine_answer, tokens_usage = chatveri.get_respone(
        conclude_in_dict.replace("<The spec I give you>", spec),
        model=llm_model,
        maxTokens=maxTokens,
        temperature_arg=temperature,
        n_choices=n_choices,
    )
    from ast import literal_eval

    dicts: dict = literal_eval(determine_answer[0])

    spec_list = spec.split("\n")
    new_spec = []
    for line in spec_list:
        for name, args in dicts.items():
            if f"let {name}" in line:
                line = f"let {name}({', '.join(list(args))}) = "
        new_spec += [line]

    new_spec = "\n".join(new_spec)
    return new_spec


def LLM_parser(
    doc: str,
) -> str:
    pass


if __name__ == "__main__":
    code = """
Op(A, assign(shared_secret, dh_exchange(g, p, a, b)))
Op(A, assign(signed_secret, sign(shared_secret, host_key)))
Knows(role(C), V_C, I_C)
Knows(role(S), V_S, I_S, K_S)
Op(C, assign(shared_secret_C, dh_exchange(g, p, q)))
Op(S, assign(shared_secret_S, dh_exchange(g, p, q)))
Op(S, assign(signed_secret_S, sign(shared_secret_S, K_S)))
Send(S, C, signed_secret_S)
Recv(C, S, signed_secret_S)
Gen(C, x)
Op(C, assign(e, exp(g, x, p)))
Send(C, S, e)
Recv(S, C, e)
Gen(S, y)
Op(S, assign(f, exp(g, y, p)))
Op(S, assign(K, exp(e, y, p)))
Op(S, assign(H, hash(concat(V_C, V_S, I_C, I_S, K_S, e, f, K))))
Op(S, assign(s, sign(H, private_host_key)))
Op(S, assign(message, concat(K_S, f, s)))
Send(S, C, message)
Recv(C, S, message)
Op(C, assign(K, exp(f, x, p)))
Op(C, assign(H, hash(concat(V_C, V_S, I_C, I_S, K_S, e, f, K))))
Op(C, verify_signature(s, H, K_S))
Op(C, assign(HASH, define_hash_algorithm(method_name)))
Op(S, assign(HASH, define_hash_algorithm(method_name)))
Op(C, assign(sign_alg, negotiate_signing_algorithm(I_C, I_S)))
Op(S, assign(sign_alg, negotiate_signing_algorithm(I_C, I_S)))
Op(C, assign(session_id, H))
Op(S, assign(session_id, H))
Op(C, derive_keys(K, H))
Op(S, derive_keys(K, H))
Op(C, assign(HASH, define_hash_algorithm(key_exchange_method)))
Op(S, assign(HASH, define_hash_algorithm(key_exchange_method)))
Op(C, derive_keys_using_hash(K, HASH))
Op(S, derive_keys_using_hash(K, HASH))
Op(C, assign(encryption_key, hash(concat(known_value, K), HASH)))
Op(S, assign(encryption_key, hash(concat(known_value, K), HASH)))
Op(C, assign(initial_iv_ctos, hash(concat(K, H, "A", session_id), HASH)))
Op(S, assign(initial_iv_ctos, hash(concat(K, H, "A", session_id), HASH)))
Op(C, assign(initial_iv_stoc, hash(concat(K, H, "B", session_id), HASH)))
Op(S, assign(initial_iv_stoc, hash(concat(K, H, "B", session_id), HASH)))
"""
    print(T_transform(code))
