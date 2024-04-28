import re
import json
import pickle
from pathlib import Path
import logging
from gpt.prompt_all import *
from gpt.gptcore import BaseChatClass
from gpt.prompt import get_incontext_learning_contents
from gpt.utils import parse_diff, fix_missing_closing_brackets,remove_comments_from_sapic, append_blank_line, compile_verify
from flask import Flask, render_template, jsonify, request, session, Response
from utils.gen_html import loadText, sapic_hl_gen
from gpt.gen_msc import calculus_to_msc
from gpt.parser import *
import datetime

DETERMINE_TEMPLATE = """\
Description: <The protocol text I give you>           
Incomplete spec:
<The spec I give you>

<The hints I give you>
"""     

SPEC_TEMPLATE = """\
theory temp
begin
functions: {functions}
{new_spec}
{config_spec}
end
"""
 
HINT = '''\
# Here display a sequence of lambda
# calculus, describing the beheavior
# of the roles in this protocol.'''

from gpt.utils import setup_logger
setup_logger()

app = Flask(__name__, template_folder="../templates", static_folder="../static")
app.secret_key = 'your_secret_key'
prots_dataset = Path(__file__).parent/ "dataset" / "DATA.json" 

from gpt.benchmark import DB



@app.route('/get-loadtext', methods=['POST'])
def get_content():
    selected_option = request.form.get('option')
    content = DB.get(selected_option,
                f"// Can not find text description for {selected_option} protocol.")
    text_html = loadText(content)
    return jsonify(html=text_html, calculus="", sapic=sapic_hl_gen(""))


@app.route('/download', methods=['GET', 'POST'])
def download_file():
    # content = request.json['editorContent']
    json_data:json = request.get_json()  # 解析 JSON 数据
    editor_content = json_data.get('data', '')  # 从 JSON 对象中获取 'data' 键的值
    date, time= str(datetime.datetime.now()).split(" ")
    timestamp = date.replace("-", "_") + "_" +time.replace(":", "_").split(".")[0]
    response = Response(editor_content)
    response.headers['Content-Type'] = 'text/plain'
    response.headers['Content-Disposition'] = f'attachment; filename="example_{timestamp}.spthy"'
    return response

@app.route("/", methods=["POST", "GET"])
def few_shot():
    if request.method == "POST":
        req = request.json       
        if req["type"] == "parse":
            prot_sel, llm_model = req["protocol_sel"], req["model_sel"]
            prompt_sel = req["prompt_sel"]
            doc = DB[prot_sel]
            n_choices = 1
            useOpenKey = True
            temperature = 0.4
            maxTokens = 1024
            
            if prompt_sel in ["lambda"]:
            # Step1: hire llm to read the given documents
                Lambda_spec = hire_llm_read_doc(File=doc, llm_model=llm_model, temperature=0.4, n_choices=1, maxTokens=1024, recording_folder="./parsing_log.log")

                # Step2: Tranform the lambda expressions to partial Sapic+ specification
                Role_spec = T_transform(Lambda_spec)
                    
                    
                if Role_spec:
                    # Step3: Determine the initial knowledge
                    role_spec_with_init = determin_initial_role_vars(doc, Role_spec, llm_model, temperature, n_choices, maxTokens, hint="")
                    
                    
                    repair_prompt_template = """<The spec I give you>\n"""
                    repair_question = (repair_prompt_template
                                        # .replace("<The protocol text I give you>", doc)
                                        .replace("<The spec I give you>", role_spec_with_init))
                    
                    # Step4: Repair the spec
                    repaired, tokens_usage = repair_role_spec(repair_question, llm_model, temperature, n_choices, maxTokens, useOpenKey)            
                    _, new_file = parse_diff(repaired[0])

                    from gpt.utils import remove_comments_from_sapic
                    from auto_complete import modify_variables_based_on_comments
                    from gpt.analysizer import analysis
                    # Step5: static analysis
                    new_spec = remove_comments_from_sapic(new_file)
                    spec_with_err_reports = analysis(new_spec)
                    new_spec = modify_variables_based_on_comments(spec_with_err_reports)
                    
                    
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
                    
                    func_nodes = []
                    # func_nodes = collect_subtrees(role_root, "func") + collect_subtrees(config_root, "func")
                    try:
                        func_nodes += collect_subtrees(role_root, "func")
                    except:
                        pass
                    try:
                        func_nodes += collect_subtrees(config_root, "func")
                    except:
                        pass
                    for node in func_nodes:
                        node:Tree
                        func_name = node.children[0].value
                        arity = len(node.children[1].children)        
                        functions.add(f"{func_name}/{arity}")

                    
                    spec = SPEC_TEMPLATE.format(functions=', '.join(list(functions)), new_spec=new_spec, config_spec=config_spec)
                    
                data = {
                    "parseResp":append_blank_line(spec, 25), 
                    "loadtext":loadText(doc),
                    # "parseHTML": sapic_hl_gen(role_spec)
                }
            elif prompt_sel in ["simple"]:
                chatveri = BaseChatClass(get_incontext_learning_contents('sapic'), useOpenKey=useOpenKey)
                question = sapic_prompt_template.replace("<The protocol text I give you>", doc)
                answer, tokens_usage = chatveri.get_respone(question, model=llm_model, maxTokens=maxTokens, temperature_arg=temperature, n_choices=n_choices)
                spec = answer[0]
                data = {
                    "parseResp":append_blank_line(spec, 25), 
                    "loadtext":loadText(doc),
                }
            return json.dumps(data)
        
        if req["type"] == "analysis":
            editorContent = req['editorContent']
            # codes = remove_comments_from_sapic(editorContent)
            # from gpt.analysizer import analysis
            # return jsonify(analysis(codes))
            response = ""
            return response
        
        if req["type"] == "repair":
            editorContent = req['editorContent']
            spec_with_repts = editorContent
            spec_with_repts = "\n".join(line for line in editorContent.split('\n') if line.strip())
            from gpt.utils import extract_multiline_comment_from_sapic
            hints = extract_multiline_comment_from_sapic(spec_with_repts)
            with open('chatveri.pkl', 'rb') as input_file:
                chatveri:BaseChatClass = pickle.load(input_file)
            chatveri.show_conversation(chatveri.conversation_list)
            prot_sel, model_sel = req["protocol_sel"], req["model_sel"]
            llm_model = model_sel
            n_choices = 1
            useOpenKey = True
            temperature = 0.4
            pattern = re.compile(r"^let\s+\w+\s*\(.*?\)\s*=.*$", re.MULTILINE)
            matches = pattern.findall(spec_with_repts)
            role_signature = "\n".join(matches)
            chatveri = BaseChatClass(get_incontext_learning_contents('init_signature'), useOpenKey=True)
            repair_question = (init_prompt_template
                                .replace("<The hints I give you>", hints)
                                .replace("<The signature I give you>", role_signature))
            maxTokens = 2048
            config_spec, tokens_usage = chatveri.get_respone(repair_question, model = llm_model, maxTokens=maxTokens, temperature_arg=temperature, n_choices = n_choices)
            config_spec = config_spec[0]
            header = f"theory {prot_sel}\nbegin\n\n"
            functions = f"functions: id/1\n"
            builtins = f"builtins: diffie-hellman, symmetric-encryption, signing, xor, hashing, asymmetric-encryption\n"
            complete_spec = header + functions + builtins + spec_with_repts + config_spec + "\nend"
            return json.dumps(sapic_hl_gen(complete_spec))
        
        if req["type"] == "synthesis":
            editorContent = req['editorContent']
            prot_sel = req["protocol_sel"]
            # codes = remove_comments_from_sapic(editorContent)
            codes = editorContent
            sapic_spec = codes
            output_file = "synthesis.spthy" 
            with open(output_file, "w") as f:
                f.write(sapic_spec) 
    
            stdout, stderr = compile_verify()
            hlsapic = sapic_hl_gen(sapic_spec  
                                   + f"\n/*\n {stdout + stderr} + */")
            
            with open("lambda.txt", "r") as f:
                lambda_spec = f.read()
            msc = calculus_to_msc(lambda_spec)
            data = {"sapic":hlsapic, "msc": msc}
            return json.dumps(data)
        
    return render_template(
        "home_temp.html", 
        loadText=loadText(DB["running_ex"]), 
        num_of_tries=1, 
        initSapic=sapic_hl_gen("// This is Sapic+ specification"+"\n "*30)
    )