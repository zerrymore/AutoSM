import json

from gpt.parser import *
from gpt.prompt_all import *
from gpt.benchmark import DB
from gpt.rewriter import Rewriter
from gpt.utils import setup_logger, extract_exprs, parse_json
from gpt.gptcore import BaseChatClass
from gpt.auto_model import AutoModel
from gpt.gen_msc import calculus_to_msc
from gpt.prompt import get_incontext_learning_contents
from gpt.prompt_all import REVIEW_TEMPLATE
from gpt.utils import compile_verify
from gpt.regression import errs_report


from utils.gen_html import loadText, sapic_hl_gen
from conf.jsoninfo import load_json_config
from flask import Flask, render_template, jsonify, request, Blueprint


from flask import Blueprint

setup_logger()
load_json_config("gpt")



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

ace_builds_bp = Blueprint(
    "ace_builds", __name__, static_folder="../ace-builds", static_url_path="/ace-builds"
)


app.register_blueprint(ace_builds_bp)


@app.route("/get-loadtext", methods=["POST"])
def get_content():
    selected_option = request.form.get("option")
    content = DB.get(
        selected_option,
        f"// Can not find text description for {selected_option} protocol.",
    )
    return jsonify(content=content)


@app.route("/", methods=["POST", "GET"])
def few_shot():
    if request.method == "POST":
        req = request.json
        if req["type"] == "parse":
            case, llm, prompt, doc = req["case"], req["model"], req["mode"], req["doc"]
            temperature = float(req["temperature"])

            load_json_config(llm)

            if llm.startswith("gpt"):
                llm = llm + "-ca"

            if prompt in ["lambda"]:
                """Step1: hire llm to read the given documents"""

                chatmodel = AutoModel(
                    # get_incontext_learning_contents("SeqReader"),
                    conversation_list=[],
                    useOpenKey=True,
                    continuous_talking=True,
                    model=llm,
                    case=case,
                    doc=doc,
                    temperature=0.4,
                    max_tokens=1024,
                    n_choices=1,
                )

                # 1. Parse the document
                parse_prompt = get_incontext_learning_contents("SeqReader")
                parse_prompt = get_incontext_learning_contents("ccg_parser")
                chatmodel.extend_conversation(parse_prompt)
                draft_exprs = chatmodel.parse_doc(recording_folder="./parsing_log.log")

                                
                while True:
                    report = errs_report(draft_exprs)
                    
                    logging.info(f'\U0001f4d6:{REVIEW_TEMPLATE.format(report)}')

                    response, _ = chatmodel.get_respone(
                        REVIEW_TEMPLATE.format(report),
                        model=chatmodel.model,
                        maxTokens=chatmodel.max_tokens,
                        temperature_arg=chatmodel.temperature,
                        n_choices=chatmodel.n_choices)
                    review_answer = response[0]
                
                    json_res = parse_json(review_answer)
                    
                    
                    if json_res["ret"] == "need_more_info":
                        logging.info(json_res["question"])
                        
                        user_input = input("Answer:")
                        
                        question = user_input + "Now revision the expressions again, use the same data format to return the answer."
                        
                        response, _ = chatmodel.get_respone(
                            question,
                            model=chatmodel.model,
                            maxTokens=chatmodel.max_tokens,
                            temperature_arg=chatmodel.temperature,
                            n_choices=chatmodel.n_choices)
                        review_answer = response[0]
                        
                        
                    elif json_res["ret"] == "ok":
                        draft_exprs = extract_exprs(json_res["revision"])
                        if errs_report(draft_exprs) == "":
                            final_exprs = draft_exprs
                            break
    
                
                # 3. Rewrite to sapic local process          
                partial_process = T_transform(final_exprs)
                local_processes = indent_local_processes(Rewriter(partial_process))


                # 4. Synthesize top specification
                top_prompt = get_incontext_learning_contents("init_signature")
                top_process = chatmodel.generate_top_spec(top_prompt, local_processes)

                # Some post operations
                functions = collect_funcs(local_processes, top_process)

                with open("./parsing_log.log", "r", encoding="utf-8") as file:
                    llm_parsing_log = file.read() + "\n" + final_exprs

                spec = SPEC_TEMPLATE.format(
                    functions=", ".join(list(functions)),
                    new_spec=local_processes,
                    top_spec=top_process,
                )

                data = {
                    "parseResp": spec,
                    "parseLogs": llm_parsing_log,
                }

            elif prompt in ["simple"]:
                
                doc = req["doc"]
                
                chatmodel = BaseChatClass(
                    get_incontext_learning_contents("sapic"), useOpenKey=True
                )
                question = sapic_prompt_template.replace(
                    "<The protocol text I give you>", doc
                )
                answer, tokens_usage = chatmodel.get_respone(
                    question,
                    model=llm,
                    maxTokens=2048,
                    temperature_arg=temperature,
                    n_choices=1,
                )
                spec = answer[0]
                data = {"parseLogs": doc, "parseResp": spec}
            return json.dumps(data)

        if req["type"] == "analysis":
            logs = req["logs"]
            exprs = extract_exprs(logs)
            report = errs_report(exprs)
            
            if report == "":
                hints = "\n# No unbounded variables found!"
            else:
                hints = "\n".join(["# "+ l for l in report.split("\n")])
            msc = calculus_to_msc(exprs)
            
            
            
            
            data = {"msc": msc, "report": f"\n{hints}"}
            return json.dumps(data)

        if req["type"] == "rewrite":
            logs = req["logs"]
            exprs = extract_exprs(logs)
            partial_process = T_transform(exprs)
            role_process = Rewriter(partial_process)

            with open("./top_spec.txt", "r") as f:
                top_spec = f.read()

            functions = collect_funcs(role_process, top_spec)

            spec = SPEC_TEMPLATE.format(
                functions=", ".join(list(functions)),
                new_spec=indent_local_processes(role_process),
                top_spec=top_spec,
            )

            data = {"rewriteResp": spec}
            return json.dumps(data)

        if req["type"] == "synthesis":
            editorContent = req["editorContent"]
            case = req["case"]

            pattern = r"(theory[\s\S]*?\nend)"
            matches = re.findall(pattern, editorContent)
            if matches:
                codes = matches[0]
            else:
                codes = f"/* Nothing can be compiled! */\n\n"

            sapic_spec = codes
            output_file = "synthesis.spthy"
            with open(output_file, "w") as f:
                f.write(sapic_spec)

            stdout, stderr = compile_verify()

            if stderr:
                print(f"error:{stderr}")

            compile_output = f"\n/*\n {stdout + stderr} + */"

            data = {
                "compile": sapic_spec + compile_output,
            }
            return json.dumps(data)

    return render_template("home.html")
