import json
from pathlib import Path
import logging
from argparse import Namespace
from flask import Flask, render_template, jsonify, request, session
from utils.load import load_prots
from utils.gen_html import loadText, genCheckIR, sapic_hl_gen
from module_parse.parse import parse_module, format_parse_output, append_blank_line
from module_synthsize.gen_msc import calculus_to_msc
# from module_synthsize.synthsis_ops import calculus_to_processes
from module_synthsize.synthsis_ops import remove_comments_from_calculus
from module_synthsize.synthsis_ops import remove_comments_from_sapic
from evaluator.bleu import _file_bleu
from evaluator.Bleu import corpus_bleu
from evaluator import weighted_ngram_match
from evaluator.dataflow_match import corpus_dataflow_match
from evaluator.syntax_match import corpus_syntax_match, corpus_terms_match
from gpt.prompt_all import *
from gpt.gptcore import BaseChatClass
from gpt.prompt import get_incontext_learning_contents
from gpt.benchmark import DB

prots_dataset = Path(__file__).parent/ "dataset" / "DATA.json" 
PROTS = load_prots(prots_dataset)

app = Flask(__name__, template_folder="../templates", static_folder="../static")
app.secret_key = 'your_secret_key'

# logging.basicConfig(
#         level=logging.INFO, format='%(asctime)s %(levelname)-s %(filename)s:%(lineno)s - %(funcName)20s() :: %(message)s')

EVAL_TEMPLATE = '''
// =========================
// BLEU SCORE: {bleu} / {ngram_match}
// W-Ngram: {w_ngram}
// TM (Term Match): {tm}
// SM (Syntax Match): {sm}
// DM (Dataflow Match): {dm}
// CodeBLEU: {codebleu}
// =========================
'''


HINT = '''\
# Here display a sequence of lambda
# calculus, describing the beheavior
# of the roles in this protocol.'''


@app.route("/", methods=["POST", "GET"])
@app.route("/home", methods=["POST", "GET"])
def home():
    if request.method == "POST":
        req = request.json       
        if req["type"] == "parse":
            prot_sel, model_sel = req["protocol_sel"], req["model_sel"]
            tokenized = PROTS[prot_sel]["description"]
            ns = Namespace(
                model=model_sel,
                prot_text=tokenized,
                prompt_template="CoT_parse_nsl"
            )

            parseResp, functions = parse_module(ns)
            session["functions"] = functions
            data = {
                "parseResp":append_blank_line(parseResp, 25), 
                "loadtext":loadText(tokenized)
            }
            return json.dumps(data)
        
        if req["type"] == "synthesis":
            editorContent = req['editorContent']
            prot_sel = req["protocol_sel"]
            codes = remove_comments_from_calculus(editorContent)

            msc = calculus_to_msc(editorContent)
            # spec, roles_init = calculus_to_processes(codes)
            from module_synthsize.simp_syn import lambda_to_processes
            spec, roles_init = lambda_to_processes(codes)
            sapic_spec = format_parse_output(spec, roles_init, prot_sel)
            output_file = "synthesis.spthy" 
            with open(output_file, "w") as f:
                f.write(sapic_spec) 
                
            err_flag = False
            try:   
                reference_file = f"dataset/sapic_{prot_sel}.spthy"
                candidate_file = output_file
                cand_code = ( Path(__file__).parent / output_file ).read_text()
                ref_code = ( Path(__file__).parent / reference_file ).read_text()
                ref_code = remove_comments_from_sapic(ref_code)

                bleu_score = round(_file_bleu(reference_file, candidate_file) * 100, 2)
                tokenized_hyps = [cand_code.split()]
                tokenized_refs = [[ref_code.split()]]
                ngram_match_score = round(corpus_bleu(tokenized_refs,tokenized_hyps) * 100, 2)
            except:
                bleu_score = -1
                ngram_match_score = -1
                err_flag = True
                
            try:  
                keywords = [x.strip() for x in open('evaluator/keywords.txt', 'r', encoding='utf-8').readlines()]
                def make_weights(reference_tokens, key_word_list):
                    return {token:1 if token in key_word_list else 0.2 for token in reference_tokens}
                tokenized_refs_with_weights = [[[reference_tokens, make_weights(reference_tokens, keywords)]\
                    for reference_tokens in reference] for reference in tokenized_refs]
                weighted_ngram_match_score = round(weighted_ngram_match.corpus_bleu( 
                    tokenized_refs_with_weights,tokenized_hyps) * 100, 2)
            except Exception as e:        
                weighted_ngram_match_score = -1
                err_flag = True
                
            try:
                tm_score = round(corpus_terms_match([[ref_code]], [cand_code]) * 100, 2)
            except Exception as e:
                print(e)
                tm_score = -1
                err_flag = True
                
            try:
                syntax_match_score = round(corpus_syntax_match([[ref_code]], [cand_code]) * 100, 2)
            except:
                syntax_match_score = -1
                err_flag = True
                
            try:
                dataflow_match_score = round(corpus_dataflow_match([[ref_code]], [cand_code]) * 100, 2)
            except:
                dataflow_match_score = -1
                err_flag = True
                
            if err_flag:
                code_bleu_score = -1
            else:
                alpha = 0.25; beta=0.25
                gamma = 0.25; theta=0.25
                code_bleu_score = round(alpha*ngram_match_score\
                    + beta*weighted_ngram_match_score\
                    + gamma*syntax_match_score\
                    + theta*dataflow_match_score, 2)
                print(weighted_ngram_match_score)
            
            Evaluation = EVAL_TEMPLATE.format(
                bleu=bleu_score,
                ngram_match=ngram_match_score,
                w_ngram=weighted_ngram_match_score,
                tm=tm_score,
                sm=syntax_match_score,
                dm=dataflow_match_score,
                codebleu=code_bleu_score
            )

            from module_synthsize.prover_cmd import compile_verify
            stdout, stderr = compile_verify()
            hlsapic = sapic_hl_gen(sapic_spec + Evaluation 
                                   + f"/*\n {stdout + stderr} + */")
            data = {"sapic":hlsapic, "msc":msc}
            return json.dumps(data)


        if req["type"] == "repair":
            editorContent = req['editorContent']
            codes = remove_comments_from_calculus(editorContent)
            from module_analyze.well_formedness import data_flow
            code_err = data_flow(codes) + "\nCorrect:"
            from module_repair.repair import sim_repair
            code_repair = sim_repair(code_err)
            # exp_repair = []
            # for line in code_repair.split("\n"):
            #     if "#" not in line:
            #         exp_repair.append(line)
            hlIR = genCheckIR(code_repair)
            repair_res = {"code": hlIR, "code_repair":code_repair}
            return json.dumps(repair_res)

    return render_template(
        "home_temp.html", 
        loadText=loadText(PROTS["running_ex"]["description"]), 
        num_of_tries=1, 
        initSapic=sapic_hl_gen("// This is Sapic+ specification"+"\n "*30)
    )


# @app.route('/get-loadtext', methods=['POST'])
# def get_content():
#     selected_option = request.form.get('option')
#     content = PROTS[selected_option].get("description",
#                 f"// Can not find text description for {selected_option} protocol.")
#     text_html = loadText(content)
#     calculus = PROTS[selected_option].get("calculus", HINT)

#     sapic = PROTS[selected_option].get("sapic", "// Here display the sapic+ specification")
#     return jsonify(html=text_html, calculus=calculus, sapic=sapic_hl_gen(sapic))


@app.route('/get-loadtext', methods=['POST'])
def get_content():
    selected_option = request.form.get('option')
    content = DB.get(selected_option,
                f"// Can not find text description for {selected_option} protocol.")
    text_html = loadText(content)
    # calculus = PROTS[selected_option].get("calculus", HINT)

    # sapic = PROTS[selected_option].get("sapic", "// Here display the sapic+ specification")
    return jsonify(html=text_html, calculus="", sapic=sapic_hl_gen(""))



from gpt.benchmark import DB

@app.route("/few-shot", methods=["POST", "GET"])
def few_shot():
    if request.method == "POST":
        req = request.json       
        if req["type"] == "parse":
            prot_sel, model_sel = req["protocol_sel"], req["model_sel"]
            # tokenized = PROTS[prot_sel]["description"]
            tokenized = DB[prot_sel]
            non_comments = [line for line in tokenized.split("\n") if not line.startswith("// ")]
            gpt_file_strings = "\n".join(non_comments)
            llm_model = model_sel
            n_choices = 1
            useOpenKey = True
            temperature = 0.4

            chatveri = BaseChatClass(get_incontext_learning_contents('sapic'), useOpenKey=useOpenKey)
            question = sapic_prompt_template.replace("<The protocol text I give you>", gpt_file_strings)
            chatveri.show_conversation(chatveri.conversation_list)
            logging.info(question)
            maxTokens = 1024
            full_reply_content_list, tokens_usage = chatveri.get_respone(question, model = llm_model, maxTokens=maxTokens, 
                                                                         temperature_arg=temperature, n_choices = n_choices)  
            if len(full_reply_content_list) == 1:
                output = full_reply_content_list[0]
            print(output)
            data = {
                "parseResp":append_blank_line(output, 25), 
                "loadtext":loadText(tokenized),
                # "parseHTML": sapic_hl_gen(role_spec)
            }
            return json.dumps(data)
        
        if req["type"] == "repair":
            editorContent = req['editorContent']
            codes = remove_comments_from_calculus(editorContent)
            from module_analyze.well_formedness import data_flow
            code_err = data_flow(codes) + "\nCorrect:"
            from module_repair.repair import sim_repair
            code_repair = sim_repair(code_err)
            hlIR = genCheckIR(code_repair)
            repair_res = {"code": hlIR, "code_repair":code_repair}
            return json.dumps(repair_res)
        
        if req["type"] == "synthesis":
            editorContent = req['editorContent']
            prot_sel = req["protocol_sel"]
            codes = remove_comments_from_sapic(editorContent)
            
            sapic_spec = codes
            output_file = "synthesis.spthy" 
            with open(output_file, "w") as f:
                f.write(sapic_spec) 
                
            err_flag = False
            try:   
                reference_file = f"dataset/sapic_{prot_sel}.spthy"
                candidate_file = output_file
                cand_code = ( Path(__file__).parent / output_file ).read_text()
                ref_code = ( Path(__file__).parent / reference_file ).read_text()
                ref_code = remove_comments_from_sapic(ref_code)

                bleu_score = round(_file_bleu(reference_file, candidate_file) * 100, 2)
                tokenized_hyps = [cand_code.split()]
                tokenized_refs = [[ref_code.split()]]
                ngram_match_score = round(corpus_bleu(tokenized_refs,tokenized_hyps) * 100, 2)
            except:
                bleu_score = 0
                ngram_match_score = 0
                err_flag = True
                
            try:  
                keywords = [x.strip() for x in open('evaluator/keywords.txt', 'r', encoding='utf-8').readlines()]
                def make_weights(reference_tokens, key_word_list):
                    return {token:1 if token in key_word_list else 0.2 for token in reference_tokens}
                tokenized_refs_with_weights = [[[reference_tokens, make_weights(reference_tokens, keywords)]\
                    for reference_tokens in reference] for reference in tokenized_refs]
                weighted_ngram_match_score = round(weighted_ngram_match.corpus_bleu( 
                    tokenized_refs_with_weights,tokenized_hyps) * 100, 2)
            except Exception as e:        
                weighted_ngram_match_score = 0
                err_flag = True
                
            try:
                tm_score = round(corpus_terms_match([[ref_code]], [cand_code]) * 100, 2)
            except Exception as e:
                print(e)
                tm_score = 0
                err_flag = True
                
            try:
                syntax_match_score = round(corpus_syntax_match([[ref_code]], [cand_code]) * 100, 2)
            except:
                syntax_match_score = 0
                err_flag = True
                
            try:
                dataflow_match_score = round(corpus_dataflow_match([[ref_code]], [cand_code]) * 100, 2)
            except:
                dataflow_match_score = 0
                err_flag = True
                
 
            alpha = 0.25; beta=0.25
            gamma = 0.25; theta=0.25
            code_bleu_score = round(alpha*ngram_match_score\
                + beta*weighted_ngram_match_score\
                + gamma*syntax_match_score\
                + theta*dataflow_match_score, 2)
            print(weighted_ngram_match_score)
            
            Evaluation = EVAL_TEMPLATE.format(
                bleu=bleu_score,
                ngram_match=ngram_match_score,
                w_ngram=weighted_ngram_match_score,
                tm=tm_score,
                sm=syntax_match_score,
                dm=dataflow_match_score,
                codebleu=code_bleu_score
            )

            from module_synthsize.prover_cmd import compile_verify
            stdout, stderr = compile_verify()
            hlsapic = sapic_hl_gen(sapic_spec + Evaluation 
                                   + f"/*\n {stdout + stderr} + */")
            data = {"sapic":hlsapic}
            return json.dumps(data)
    return render_template(
        "few_shot.html", 
        loadText=loadText(PROTS["running_ex"]["description"]), 
        num_of_tries=1, 
        initSapic=sapic_hl_gen("// This is Sapic+ specification"+"\n "*30)
    )