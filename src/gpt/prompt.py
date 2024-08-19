import gpt
from gpt.prompt_all import *

def get_incontext_learning_contents(content_type, shot_num=2):
    ret_list = []
    if content_type == "sapic":
        if shot_num == 0:
            ret_list = [{'role': 'system', 'content': sys_sapic_prompt}]
        elif shot_num == 1:
            ret_list = [{'role': 'system', 'content': sys_sapic_prompt},
                        {'role': 'user', 'content': first_shot_sapic_question},
                        {'role': 'assistant', 'content': first_shot_sapic_answer}]
        elif shot_num == 2:
            ret_list = [{'role': 'system', 'content': sys_sapic_prompt},
                        {'role': 'user', 'content': first_shot_sapic_question},
                        {'role': 'assistant', 'content': first_shot_sapic_answer},
                        {'role': 'user', 'content': second_shot_sapic_question},
                        {'role': 'assistant', 'content': second_shot_sapic_answer},
                        {'role': 'user', 'content': third_shot_sapic_question},
                        {'role': 'assistant', 'content': third_shot_sapic_answer}]
    elif content_type == 'ccg_parser':
        # set system prompt
        if shot_num == 0:
            ret_list = [{'role': 'system', 'content': sys_ccg_prompt}]
        elif shot_num == 1:
            ret_list = [{'role': 'system', 'content': sys_ccg_prompt},
                        {'role': 'user', 'content': first_shot_ccg_question},
                        {'role': 'assistant', 'content': first_shot_ccg_answer}]
        elif shot_num == 2:
            ret_list = [{'role': 'system', 'content': sys_ccg_prompt},
                        {'role': 'user', 'content': first_shot_ccg_question},
                        {'role': 'assistant', 'content': first_shot_ccg_answer},
                        {'role': 'user', 'content': second_shot_ccg_question},
                        {'role': 'assistant', 'content': second_shot_ccg_answer}]
        else:
            raise Exception("Error: shot_num is not correct: " + str(shot_num))
    if content_type == "init_signature":
        if shot_num == 0:
            ret_list = [{'role': 'system', 'content': sys_init_prompt}]
        elif shot_num == 1:
            ret_list = [{'role': 'system', 'content': sys_init_prompt},
                        {'role': 'user', 'content': first_shot_init_question},
                        {'role': 'assistant', 'content': first_shot_init_answer}]
        elif shot_num == 2:
            ret_list = [{'role': 'system', 'content': sys_init_prompt},
                        {'role': 'user', 'content': first_shot_init_question},
                        {'role': 'assistant', 'content': first_shot_init_answer},
                        {'role': 'user', 'content': second_shot_init_question},
                        {'role': 'assistant', 'content': second_shot_init_answer}]
        else:
            raise Exception("Error: shot_num is not correct: " + str(shot_num))
    if content_type == "repair":
        ret_list = [
            {'role': 'system', 'content': sys_repair_prompt},
            {'role': 'user', 'content': first_shot_repair_question},
            {'role': 'assistant', 'content': first_shot_repair_answer},
            # {'role': 'user', 'content': second_shot_repair_question},
            # {'role': 'assistant', 'content': second_shot_repair_answer},
            {'role': 'user', 'content': third_shot_repair_question},
            {'role': 'assistant', 'content': third_shot_repair_answer},
            ]
    else:
        pass
    if content_type == "repair_lambda":
        ret_list = [
            {'role': 'system', 'content': sys_repair_lambda_prompt},
            {'role': 'user', 'content': first_shot_lambda_repair_question},
            {'role': 'assistant', 'content': first_shot_lambda_repair_answer},
            {'role': 'user', 'content': second_shot_lambda_repair_question},
            {'role': 'assistant', 'content': second_shot_lambda_repair_answer},
            # {'role': 'user', 'content': second_shot_repair_question},
            # {'role': 'assistant', 'content': second_shot_repair_answer},
            # {'role': 'user', 'content': third_shot_repair_question},
            # {'role': 'assistant', 'content': third_shot_repair_answer},
            ]
    if content_type == "SeqReader":# Reading process as few-shots
        ret_list = [
            {'role': 'system', 'content': sys_SeqReader_prompt},
            {'role': 'user', 'content': first_SeqReader_question},
            {'role': 'assistant', 'content': first_SeqReader_answer},
            {'role': 'user', 'content': second_SeqReader_question},
            {'role': 'assistant', 'content': second_SeqReader_answer},
            {'role': 'user', 'content': third_SeqReader_question},
            {'role': 'assistant', 'content': third_SeqReader_answer},
            {'role': 'user', 'content': fourth_SeqReader_question},
            {'role': 'assistant', 'content': fourth_SeqReader_answer},
            {'role': 'user', 'content': fifth_SeqReader_question},
            {'role': 'assistant', 'content': fifth_SeqReader_answer},
            {'role': 'user', 'content': sixth_SeqReader_question},
            {'role': 'assistant', 'content': sixth_SeqReader_answer},
            ]
    if content_type == "determine_init_var":  # Reading process as few-shots
        ret_list = [
            {'role': 'system', 'content': sys_variable_prompt},
            {'role': 'user', 'content': first_determine_var_question},
            {'role': 'assistant', 'content': first_determine_var_answer},
            ]
    
    return ret_list
    
    
    