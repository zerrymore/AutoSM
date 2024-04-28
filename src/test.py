import re
import json
import pickle
from pathlib import Path
import logging
from gpt.prompt_all import *
from gpt.gptcore import BaseChatClass
from gpt.prompt import get_incontext_learning_contents
from gpt.utils import parse_diff,remove_comments_from_sapic, append_blank_line, compile_verify
from flask import Flask, render_template, jsonify, request, session, Response
from utils.gen_html import loadText, sapic_hl_gen
from gpt.gen_msc import calculus_to_msc
from gpt.parser import *
from gpt.benchmark import DB
from lambda_prompt import repair_lambda_ret_list, seq_ret_list

def repair_lambda_spec(repair_question:str, llm_model, temperature, n_choices, maxTokens, useOpenKey) -> str:
    chatveri = BaseChatClass(repair_lambda_ret_list, useOpenKey=useOpenKey)
    repaired, tokens_usage = chatveri.get_respone(repair_question, model = llm_model, maxTokens=maxTokens, temperature_arg=temperature, n_choices = n_choices)
    return repaired, tokens_usage         

REPAIR_LAMBDA_TEMPLATE = """\
<The spec I give you>
"""

setup_logger()
if __name__ == "__main__":
    prot_sel = "Woo_Lam_Pi_f"
    prot_sel = "kca"
    prot_sel = "or"
    doc = DB[prot_sel]
    n_choices = 1
    useOpenKey = True
    temperature = 0.4
    maxTokens = 1024
    llm_model = "gpt-4"
    # Step1: hire llm to read the given documents

    Lambda_spec_with_init = hire_llm_read_doc(File=doc, llm_model=llm_model, temperature=0.4, n_choices=1, maxTokens=1024, prompt_list=seq_ret_list, recording_folder="")
    
    
    
#     Lambda_spec_with_init = """\
# Knows(role(A), Kas, idA, idB)
# Knows(role(B), Kbs, idA, idB)
# Knows(role(S), Kas, Kbs, idA, idB)
# Gen(A, I)
# Gen(A, Na)
# Op(A, assign(message1, concat(I, idA, idB, senc(concat(Na, I, idA, idB), Kas))))
# Send(A, B, message1)
# Gen(A, I)
# Gen(A, Na)
# Op(A, assign(message1, concat(I, idA, idB, senc(concat(Na, I, idA, idB), Kas))))
# Send(A, B, message1)
# Recv(B, A, message1)
# Gen(B, Nb)
# Op(B, assign(message2, concat(message1, senc(concat(Nb, I, idA, idB), Kbs))))
# Send(B, S, message2)
# Recv(S, B, message2)
# Op(S, assign(Na, adec(message2, Kas)))
# Op(S, assign(Nb, adec(message2, Kbs)))
# Gen(S, kab)
# Op(S, assign(message3, concat(I, senc(concat(Na, kab), Kas), senc(concat(Nb, kab), Kbs))))
# Send(S, B, message3)
# Recv(B, S, message3)
# Op(B, bind(message4, adec(message3, Kbs)))
# Op(B, assign(kab, adec(message4, Kbs)))
# Send(B, A, message4)
# Recv(A, B, message4)
# Op(A, assign(kab, adec(message4, Kas)))"""


    # Step: Repair the Lambda Spec
    repair_question = (repair_prompt_template
                                    .replace("<The spec I give you>", Lambda_spec_with_init))
    repair_Lambda_spec, token_used = repair_lambda_spec(repair_question, llm_model, temperature, n_choices, maxTokens, useOpenKey)
    _, new_file = parse_diff(repair_Lambda_spec[0])
    
    # Step2: Tranform the lambda expressions to partial Sapic+ specification
    
    
    
    repaired_result = """\
"""
    repaired_result = new_file
    remove_comments_spec = remove_comments_from_sapic(repaired_result)
    print(remove_comments_spec)
    _, after_diff = parse_diff(remove_comments_spec)
    cleaned_text = re.sub(r'^\s+', '', after_diff, flags=re.M)
    print()
    print(cleaned_text)
    cleaned_text = cleaned_text.replace("<", "concat(").replace(">", ")")
    Role_spec = T_transform(cleaned_text)
    print(Role_spec)

    from gpt.utils import remove_comments_from_sapic
    from auto_complete import modify_variables_based_on_comments
    from gpt.analysizer import analysis
    spec_with_err_reports = analysis(Role_spec)
    new_spec = modify_variables_based_on_comments(spec_with_err_reports)
    
    
    SPEC_TEMPLATE = """\
theory temp
begin
functions: {functions}
{new_spec}
{config_spec}
end
"""
    sig_pattern = re.compile(r"^let\s+\w+\s*\(.*?\)\s*=.*$", re.MULTILINE)
    matches = sig_pattern.findall(new_spec)
    role_signature = "\n".join(matches)
    chatveri = BaseChatClass(get_incontext_learning_contents('init_signature'), useOpenKey=useOpenKey)
    question = (init_prompt_template
                            .replace("<The protocol text I give you>", doc)
                            .replace("<The signature I give you>", role_signature))

    maxTokens = 1024
    config_spec, tokens_usage = chatveri.get_respone(question, model=llm_model, maxTokens=maxTokens, temperature_arg=temperature, n_choices=n_choices)
    config_spec = config_spec[0]
    config_spec = eliminate_comments_from_top_spec(config_spec)
    config_spec = config_spec[0].lower() + config_spec[1:]
    
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

    
    spec = SPEC_TEMPLATE.format(functions=', '.join(list(functions)), new_spec=new_spec, config_spec=config_spec)
    print(spec)