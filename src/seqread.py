from gpt.gptcore import BaseChatClass
import re
from gpt.prompt import get_incontext_learning_contents
from gpt.prompt_all import *
from gpt.utils import fix_missing_closing_brackets, parse_diff, setup_logger
import logging
from lark import Token, Tree
from gpt.analysizer import RoleParser, TopParser

def collect_subtrees(tree_node, name:str):
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
        if tree_node.data in ['func','role']:  # reduce the funcname
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
        if tree_node.data == 'new':
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

def build_role_dict(node:Tree):
    role_dict = {}
    if node.data == 'role':  
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

def extract_fresh(node:Tree):
    new_stmts =  collect_subtrees(node, "new")
    fresh = set([_vars(t) for t in new_stmts])
    return set().union(*fresh)


def extract_intermediate_vars(root:Tree):
    binding_stms = collect_subtrees(root, "binding")
    intermediate_vars = [_vars(equation.children[0]) for equation in binding_stms]
    return set().union(*intermediate_vars)



def extract_from_comments(comment_string):
    matches = re.findall(r'/\*(.*?)\*/', comment_string, re.DOTALL)
    return matches[0].strip()

def record_reading_process(chunks:list, Lambda:list, output_folder):
    log = ""
    for i, _ in enumerate(chunks):
        log += f"\n\U0001f4d6:{chunks[i]}" + f"\n\U0001f916:/*\n{Lambda[i]}\n*/\n"
    try:
        with open(output_folder, "w") as f:
            f.write(log)
    except:
        pass
        
def hire_llm_read_doc(File:str, llm_model, temperature, n_choices, maxTokens, recording_folder, useOpenKey=True):
    chatveri = BaseChatClass(get_incontext_learning_contents('SeqReader'), useOpenKey=useOpenKey, continuous_talking=True)
    # chatveri.show_conversation(chatveri.conversation_list)
    chunks = File.split("\n\n")
    Lambda = []
    logging.info(f"\U0001f914: Ok, I'm reading the document you give me...")
    for i, chunk in enumerate(chunks):
        chunks[i] += "\n/* >>The lambda calculus << */"

        if i > 0:
            chunks[i-1] = chunks[i-1].replace("/* >>The lambda calculus << */", f"/*\n{Lambda[-1]}\n*/")
        if i == 0:
            content = chunks[i]
        else:
            content = chunks[i-1] + "\n" + chunks[i]  
        question = content
        logging.info(f"\U0001f4d6:{question}")
        
        full_reply_content_list, tokens_usage = chatveri.get_respone(question, model = llm_model, maxTokens=maxTokens, 
                                                                    temperature_arg=temperature, n_choices = n_choices)  
        output = full_reply_content_list[0]
        
        lam_expr = extract_from_comments(output)
        Lambda += [lam_expr]
    
    texts = File.split("\n\n")
    Lambda_spec = "\n".join(Lambda)
    record_reading_process(texts, Lambda, recording_folder)
    
    return Lambda_spec
    
    
def T_transform(Lambda_spec:str) -> str:
    try:    
        corr_Lambda_spec = "\n".join(fix_missing_closing_brackets(line) for line in Lambda_spec.split("\n"))
        from gpt.translator import lambda_to_processes, format_parse_output
        spec, _ = lambda_to_processes(corr_Lambda_spec)
        Role_spec, _ = format_parse_output(spec)
    except Exception as e:
        logging.error(str(e))
        Role_spec = ""
    return Role_spec


def repair_role_spec(repair_question:str, llm_model, temperature, n_choices, maxTokens, useOpenKey) -> str:
    chatveri = BaseChatClass(get_incontext_learning_contents('repair'), useOpenKey=useOpenKey)
    repaired, tokens_usage = chatveri.get_respone(repair_question, model = llm_model, maxTokens=maxTokens, temperature_arg=temperature, n_choices = n_choices)
    return repaired, tokens_usage         
    
def eliminate_comments_from_top_spec(spec:str) -> str:
    spec = re.sub(r'//.*?$', '', spec, flags=re.MULTILINE)
    spec = re.sub(r'\n\s*\n', '\n', spec)
    return spec

if __name__ == "__main__":
    from gpt.benchmark import DB
    setup_logger()
    nssk = DB["X509_1"]
    nssk = DB["kca"]  # ✅
    nssk = DB["nssk"] # ✅ 
    nssk = DB["Yahalom"]  #✅
    nssk = DB["Woo_Lam_Pi_f"] #✅
    nssk = DB["or"] 
    nssk = DB["Denning_Sacco"] #✅
    llm_model = "gpt-4"
    n_choices = 1
    useOpenKey = True
    temperature = 0.4
    maxTokens = 1024
    # Step1: hire llm to read the given documents
    Lambda_spec = hire_llm_read_doc(File=nssk, llm_model="gpt-4", temperature=0.4, n_choices=1, maxTokens=1024, recording_folder="./parsing_log.log")

    # Step2: Tranform the lambda expressions to partial Sapic+ specification
    Role_spec = T_transform(Lambda_spec)
        
        
    if Role_spec:
        repair_prompt_template = """<The spec I give you>\n"""
        repair_question = (repair_prompt_template
                            # .replace("<The protocol text I give you>", nssk)
                            .replace("<The spec I give you>", Role_spec))
        
        # Step3: Repair the spec
        repaired, tokens_usage = repair_role_spec(repair_question, llm_model, temperature, n_choices, maxTokens, useOpenKey)            
        _, new_file = parse_diff(repaired[0])


        with open("./lambda.spthy", "w") as f:
            f.write(new_file)
        root = RoleParser.parse(new_file)
        role_dicts = build_role_dict(root)
        
        Hints = []
        for role, variables in role_dicts.items():
            values = [f"'{v}'" for v in variables if v != '0']
            hint = f"Variables in role {role}: {{ {','.join(values)}}}"
            Hints += [hint]

        # Hints += ["This is Kao_Chow_Authentication_V1 protocol, the identites are known publicly, the symmetric key are only by the corresponding roles."]
        # Hints += ["This is Needham Schroeder Symmetric Key protocol, the identites are known publicly, the symmetric key are only by the corresponding roles."]
        # Hints += ["This is Woo and Lam Pi protocol, the identites are known publicly, the symmetric key are only by the corresponding roles."]
        Hints += ["This is Denning_Sacco protocol, the identites are known publicly, the symmetric key are only by the corresponding roles."]
        detemine_template = """\
Description: <The protocol text I give you>           
Incomplete spec:
<The spec I give you>

<The hints I give you>
"""     
        chatveri = BaseChatClass(get_incontext_learning_contents('determine_init_var'), useOpenKey=useOpenKey)
        determine_question = (detemine_template
                            .replace("<The protocol text I give you>", nssk)
                            .replace("<The spec I give you>", new_file)
                            .replace("<The hints I give you>", "\n".join(Hints)))
        determine_answer, tokens_usage = chatveri.get_respone(repair_question, model = llm_model, maxTokens=maxTokens, temperature_arg=temperature, n_choices = n_choices) 
        determine_answer, tokens_usage = chatveri.get_respone("Output the analysis result, do not explain it.", model = llm_model, maxTokens=maxTokens, temperature_arg=temperature, n_choices = n_choices)
        from ast import literal_eval
        dicts:dict = literal_eval(determine_answer[0])

        spec = new_file.split("\n")
        new_spec = []
        for line in spec:
            for name, args in dicts.items():
                if f"let {name}" in line:
                    line = f"let {name}({', '.join(list(args))}) = "
            new_spec += [line]

        # print("\n".join(new_spec))
        
        new_spec = "\n".join(new_spec)
        from auto_complete import modify_variables_based_on_comments
        from gpt.analysizer import analysis
        spec_with_err_reports = analysis(new_spec)
        new_spec = modify_variables_based_on_comments(spec_with_err_reports)
        
        
        pattern = re.compile(r"^let\s+\w+\s*\(.*?\)\s*=.*$", re.MULTILINE)
        matches = pattern.findall(new_spec)
        role_signature = "\n".join(matches)
        chatveri = BaseChatClass(get_incontext_learning_contents('init_signature'), useOpenKey=useOpenKey)
        question = (init_prompt_template
                                .replace("<The protocol text I give you>", nssk)
                                .replace("<The signature I give you>", role_signature))

        maxTokens = 2048
        config_spec, tokens_usage = chatveri.get_respone(question, model = llm_model, maxTokens=maxTokens, temperature_arg=temperature, n_choices = n_choices)
        config_spec = config_spec[0]
        config_spec = eliminate_comments_from_top_spec(config_spec)
        
        functions = set()
        role_root = RoleParser.parse(new_spec)
        from gpt.utils import remove_comments_from_sapic
        config_root = TopParser.parse(remove_comments_from_sapic(config_spec))
        
        func_nodes = collect_subtrees(role_root, "func") + collect_subtrees(config_root, "func")
        for node in func_nodes:
            node:Tree
            func_name = node.children[0].value
            arity = len(node.children[1].children)        
            functions.add(f"{func_name}/{arity}")

        
        
        header = "theory nssk\nbegin\n"    
        
        spec_template = f"""\
theory temp
begin
functions: {', '.join(list(functions))}
{new_spec}
{config_spec}
end
"""
        print(spec_template)
\
    
    
    
    