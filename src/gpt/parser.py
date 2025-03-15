import re
from gpt.prompt_all import *
from gpt.utils import (
    fix_brackets,
    deconstruct_expr,
    is_loop_expression,
    remove_comments_from_sapic,
    extract_line_commment_from_spec
)
import logging
from lark import Token, Tree
from gpt.bnf import pretty_stmts
from lark import Lark

from gpt.bnf import ROLE_BNF, TOP_BNF
RoleParser = Lark(ROLE_BNF, parser='lalr', start='start', propagate_positions=True)
TopParser = Lark(TOP_BNF, parser='lalr', start='start', propagate_positions=True)


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


def collect_funcs(local_processes:str, top_process:str) -> list:
    """collect function declaration from top process and local processes.

    Returns:
        functions:list of function name with arity
    """
    role_root = parse_with_fallback(
        RoleParser, remove_comments_from_sapic(local_processes)
    )
    top_spec_without_comments = remove_comments_from_sapic(top_process)
    config_root = parse_with_fallback(TopParser, top_spec_without_comments)

    func_nodes = []
    for root in [role_root, config_root]:
        try:
            func_nodes += collect_subtrees(root, "func")
        except Exception as e:
            pass

    functions = set()
    for node in func_nodes:
        node: Tree
        func_name = node.children[0].value
        arity = len(node.children[1].children)
        functions.add(f"{func_name}/{arity}")
    return functions


        
def indent_local_processes(local_processes:str) -> str:
    
    comments = extract_line_commment_from_spec(local_processes)
            
    role_root = parse_with_fallback(
        RoleParser, remove_comments_from_sapic(local_processes)
    )

    # try:
    new_stmt = []
    pretty_stmts(role_root, new_stmt)
    new_spec = "\n".join(new_stmt)

    i, j = 0, 0
    stmt_l = []
    while j < len(comments) + len(new_stmt):
        if j in comments:
            stmt_l += [comments[j]]
        else:
            stmt_l += [new_stmt[i]]
            i += 1
        j += 1
    new_spec = "\n".join(stmt_l)
    return new_spec
    
    
    

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


def extract_from_comments(comment_string:str) -> str:
    matches = re.findall(r"/\*(.*?)\*/", comment_string, re.DOTALL)
    fcalls = []
    lines = matches[0].strip().split("\n")
    for line in lines:
        fcall_pattern = r"^\w+\((.*)\)\s*(?:\/\/.*)?$"
        match = re.match(fcall_pattern, line)
        if match:
            expr = line.split("//")[0].strip()
            fcalls.append(expr)
    return "\n".join(fcalls)



def T_transform(Lambda_spec: str) -> str:
    """
    Take pure expressions as input, then output the particial process.
    """
    try:
        ##== Filter out all the deconstrction expressions,  ==##
        ##== only allowing construction and I/O expressions ==##

        corr_Lambda_spec = "\n".join(
            fix_brackets(line)
            for line in Lambda_spec.split("\n")
            if (not deconstruct_expr(line)) and (not is_loop_expression(line))
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




if __name__ == "__main__":
    code = """
Op(A, assign(shared_secret, dh_exchange(g, p, a, b)))
Op(A, assign(signed_secret, sign(shared_secret, host_key)))
Knows(role(C), V_C, I_C)
Knows(role(C), V_C, I_E)
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
Send(S, C, message)
"""
#     code = """
#     /*
# Gen(S, y)
# Op(S, assign(f, exp(g, y)))
# Recv(S, C, e)
# Op(S, assign(K, exp(e, y)))
# Op(S, assign(H, hash(concat(V_C, V_S, I_C, I_S, K_S, e, f, K))))
# Op(S, assign(s, sign(H, sk_S)))
# Op(S, assign(message, concat(K_S, f, s)))
# Send(S, C, message)
# Recv(C, S, message)
# // Extending key material if the required key length exceeds the hash output:
# Op(C, assign(extended_key_CtoS, extend_key(enc_key_CtoS, K, H)))  // Client extends encryption key for client-to-server
# Op(S, assign(extended_key_StoC, extend_key(enc_key_StoC, K, H)))  // Server extends encryption key for server-to-client
# Op(C, assign(extended_int_key_CtoS, extend_key(int_key_CtoS, K, H)))  // Client extends integrity key for client-to-server
# Op(S, assign(extended_int_key_StoC, extend_key(int_key_StoC, K, H)))  // Server extends integrity key for server-to-client

# // Assigning extended keys to respective agents:
# Knows(C, extended_key_CtoS, extended_int_key_CtoS)  // Client knows its extended keys
# Knows(S, extended_key_StoC, extended_int_key_StoC)  // Server knows its extended keys
# */"""
    code = """
Knows(A, Kas, idA)
Knows(B, Kbs, idB)
Knows(S, Kas, Kbs)
Send(A, B, idA)
Recv(B, A, idA)
Gen(B, Nb)
Send(B, A, Nb)
Recv(A, B, Nb)
Op(A, assign(encryptedNb, senc(Nb, Kas)))
Send(A, B, encryptedNb)
Recv(B, A, encryptedNb)
Op(B, assign(encryptedIdA, senc(idA, Kbs)))
Send(B, S, concat(encryptedNb, encryptedIdA))
Recv(S, B, concat(encryptedNb, encryptedIdA))
Op(S, assign(decryptedNb, dec(encryptedNb, Kas)))
Op(S, assign(decryptedIdA, dec(encryptedIdA, Kbs)))
Op(S, assign(encryptedNbForB, senc(decryptedNb, Kbs)))
Send(S, B, encryptedNbForB)
Recv(B, S, encryptedNbForB)
Op(B, assign(decryptedNbForB, dec(encryptedNbForB, Kbs)))# """
    print(T_transform(code))
    # print(extract_from_comments(code))
