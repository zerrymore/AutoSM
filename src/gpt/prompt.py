import gpt
from gpt.prompt_all import *

def get_incontext_learning_contents(content_type, shot_num=2):
    ret_list = []
    if content_type == "sapic":
        """
        When attempting end-to-end few-shot learning, we should use simple examples 
        as few-shots to prompt the LLM in generating more complex outputs. 
        Conducting this experiment in reverse would be unfair, as, 
        in the parsing stage of our method, we only use very basic few-shots that 
        contain almost no technical information related to the target protocol. 
        These few-shots merely serve to prompt the LLM to parse the input.

        In practice, as discussed in the paper, using lengthy documents and corresponding 
        models as few-shots is impractical. Typically, we only have access to smaller, 
        more concise cases.
        """
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
                        # {'role': 'user', 'content': third_shot_sapic_question},
                        # {'role': 'assistant', 'content': third_shot_sapic_answer}
                        ]
    elif content_type == 'ccg_parser':
        # set system prompt
        ret_list = [{'role': 'system', 'content': sys_ccg_prompt},
                    {'role': 'user', 'content': first_ccg_question},
                    {'role': 'assistant', 'content': first_ccg_answer},
                    {'role': 'user', 'content': second_ccg_question},
                    {'role': 'assistant', 'content': second_ccg_answer},
                    {'role': 'user', 'content': third_ccg_question},
                    {'role': 'assistant', 'content': third_ccg_answer},
                    {'role': 'user', 'content': fourth_ccg_question},
                    {'role': 'assistant', 'content': fourth_ccg_answer},
                    # {'role': 'user', 'content': fifth_ccg_question},
                    # {'role': 'assistant', 'content': fifth_ccg_answer},
                    # {'role': 'user', 'content': sixth_ccg_question},
                    # {'role': 'assistant', 'content': sixth_ccg_answer},
                    # {'role': 'user', 'content': end_ccg_question},
                    # {'role': 'assistant', 'content': end_ccg_answer},
                    ]
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
    if content_type == "review_logs":
        ret_list = [
            {'role': 'system', 'content': sys_review_prompt},
            # {'role': 'user', 'content': first_review_question},
            # {'role': 'assistant', 'content': first_review_answer} 
            ]
    return ret_list
    
    
    