import json
import datetime
from pathlib import Path

from gpt.parser import *
from gpt.benchmark import DB
from gpt.rewriter import Rewriter
from gpt.utils import setup_logger
# from gpt.bnf import pretty_stmts
from gpt.gptcore import BaseChatClass
from gpt.prompt_all import *
from gpt.parser import collect_func_declarations, format_local_processes_with_indents
from gpt.gen_msc import calculus_to_msc
from gpt.prompt import get_incontext_learning_contents
from gpt.utils import append_blank_line, compile_verify
from gpt.top_spec import generate_top_spec
from gpt.utils import remove_comments_from_sapic, extract_line_commment_from_spec

from utils.gen_html import loadText, sapic_hl_gen
from conf.jsoninfo import load_json_config
from flask import Flask, render_template, jsonify, request, Response


setup_logger()
load_json_config()


SPEC_TEMPLATE = """\
theory temp
begin
builtins: diffie-hellman, symmetric-encryption, signing, xor, hashing, asymmetric-encryption
functions: {functions}
{new_spec}
{top_spec}
end
"""

app = Flask(__name__, template_folder="../templates", static_folder="../static")
app.secret_key = "your_secret_key"
prots_dataset = Path(__file__).parent / "dataset" / "DATA.json"


@app.route("/get-loadtext", methods=["POST"])
def get_content():
    selected_option = request.form.get("option")
    content = DB.get(
        selected_option,
        f"// Can not find text description for {selected_option} protocol.",
    )
    text_html = loadText(content)
    return jsonify(html=text_html, calculus="", sapic=sapic_hl_gen(""))


@app.route("/download", methods=["GET", "POST"])
def download_file():
    json_data: json = request.get_json()
    ##== Retrieve the value of the 'data' key from a JSON object. ==##
    editor_content = json_data.get("data", "")
    date, time = str(datetime.datetime.now()).split(" ")
    timestamp = date.replace("-", "_") + "_" + time.replace(":", "_").split(".")[0]
    response = Response(editor_content)
    response.headers["Content-Type"] = "text/plain"
    response.headers["Content-Disposition"] = (
        f'attachment; filename="example_{timestamp}.spthy"'
    )
    return response


@app.route("/", methods=["POST", "GET"])
def few_shot():
    if request.method == "POST":
        req = request.json
        if req["type"] == "parse":
            prot_sel, llm_model = req["protocol_sel"], req["model_sel"]
            if llm_model == "gpt-4":
                llm_model = "gpt-4-ca"
            if llm_model == "gpt-4-turbo":
                llm_model = "gpt-4-turbo-ca"
            prompt_sel = req["prompt_sel"]
            doc = DB[prot_sel]
            n_choices = 1
            useOpenKey = True
            temperature = 0.4
            maxTokens = 1024

            if prompt_sel in ["lambda"]:
                """Step1: hire llm to read the given documents"""
                Lambda_spec = hire_llm_read_doc(
                    File=doc,
                    llm_model=llm_model,
                    temperature=0.4,
                    n_choices=1,
                    maxTokens=1024,
                    recording_folder="./parsing_log.log",
                )

                """Step2: Tranform the lambda expressions to partial Sapic+ specification"""
                Role_spec = T_transform(Lambda_spec)
                with open("parsing_lambda_exprs.txt", "w") as file:
                        file.write(Lambda_spec)

                if Role_spec:
                    """Step3: Determine the initial knowledge"""
                    role_spec_with_init = determin_initial_role_vars(
                        doc=doc,
                        spec=Role_spec,
                        llm_model=llm_model,
                        temperature=temperature,
                        n_choices=n_choices,
                        maxTokens=maxTokens,
                    )

                    """Step4: Rewrite global expressions into local processes"""
                    with open("intermediate.txt", "w") as file:
                        file.write(role_spec_with_init)

                    ##==   lambda expresions ~~> local processes  ==##
                    local_processes = Rewriter(role_spec_with_init)

                    """Step5: Generate top specfications from local processes"""
                    top_spec = generate_top_spec(
                        doc=doc, 
                        local_processes=local_processes, 
                        llm_model=llm_model, 
                        temperature=temperature, 
                        n_choices=n_choices, 
                        maxTokens=maxTokens
                    )


                    local_processes = format_local_processes_with_indents(local_processes)
                    
                    """Function declaration"""                
                    functions = collect_func_declarations(local_processes, top_spec)
                    

                    spec = SPEC_TEMPLATE.format(
                        functions=", ".join(list(functions)),
                        new_spec=local_processes,
                        top_spec=top_spec,
                    )

                data = {
                    "parseResp": append_blank_line(spec, 25),
                    "loadtext": loadText(doc),
                    # "parseHTML": sapic_hl_gen(role_spec)
                }

            elif prompt_sel in ["simple"]:
                chatveri = BaseChatClass(
                    get_incontext_learning_contents("sapic"), useOpenKey=useOpenKey
                )
                question = sapic_prompt_template.replace(
                    "<The protocol text I give you>", doc
                )
                answer, tokens_usage = chatveri.get_respone(
                    question,
                    model=llm_model,
                    maxTokens=maxTokens,
                    temperature_arg=temperature,
                    n_choices=n_choices,
                )
                spec = answer[0]
                data = {
                    "parseResp": append_blank_line(spec, 25),
                    "loadtext": loadText(doc),
                }
            return json.dumps(data)

        if req["type"] == "analysis":
            editorContent = req["editorContent"]
            response = ""
            return response
            return json.dumps(sapic_hl_gen(complete_spec))

        if req["type"] == "synthesis":
            editorContent = req["editorContent"]
            prot_sel = req["protocol_sel"]

            # codes = remove_comments_from_sapic(editorContent)
            codes = editorContent
            sapic_spec = codes
            output_file = "synthesis.spthy"
            with open(output_file, "w") as f:
                f.write(sapic_spec)

            stdout, stderr = compile_verify()
            hlsapic = sapic_hl_gen(sapic_spec + f"\n/*\n {stdout + stderr} + */")

            with open("lambda.txt", "r") as f:
                lambda_spec = f.read()
            msc = calculus_to_msc(lambda_spec)
            data = {"sapic": hlsapic, "msc": msc}
            return json.dumps(data)

    return render_template(
        "home_temp.html",
        loadText=loadText(DB["running_ex"]),
        num_of_tries=1,
        initSapic=sapic_hl_gen("// This is Sapic+ specification" + "\n " * 30),
    )