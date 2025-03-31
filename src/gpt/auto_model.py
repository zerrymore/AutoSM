#!/usr/bin/env python3
# -*- coding: UTF-8 -*-

import re
import logging

from gpt.utils import *
from gpt.gptcore import BaseChatClass
from gpt.prompt_all import CONCLUDE_IN_DICT

            
class AutoModel(BaseChatClass):
    def __init__(
        self, conversation_list=[], 
        continuous_talking=True, 
        useOpenKey=False,
        case="",
        doc="",
        model="",
        temperature=0.4,
        max_tokens=2048,
        n_choices=1
    ) -> None:
        
        super().__init__(conversation_list, continuous_talking, useOpenKey)

        self.model = model
        self.temperature = temperature
        self.max_tokens = max_tokens
        self.n_choices = n_choices
        self.case = case
        self.doc = doc
        
    def parse_doc(
        self,
        prompt_list=[],
        recording_folder = "",
    ) -> str:
        if prompt_list:
            self.conversation_list += prompt_list

        self.show_conversation(self.conversation_list)
        doc = self.doc
        chunks = doc.split("\n\n")
        Lambda = []
        logging.info(f"\U0001f914: Ok, I'm reading the document you give me...\n")
        for i, chunk in enumerate(chunks):
            chunks[i] += "\n/* >>The lambda calculus << */"
            question = chunks[i]
            logging.info(f"\U0001f4d6:{question}")

            full_reply_content_list, _ = self.get_respone(
                question,
                model=self.model,
                maxTokens=self.max_tokens,
                temperature_arg=self.temperature,
                n_choices=self.n_choices,
            )
            output:str = full_reply_content_list[0]

            
            lam_expr = extract_from_comments(output)
            
            logging.info(f'*********\n{lam_expr}\n******************')
            
            
            # handle unclosed '('
            lam_expr = "\n".join(
                fix_brackets(line)
                for line in lam_expr.split("\n")
                if not deconstruct_expr(line)
            )
            
            Lambda += [lam_expr]

        texts = doc.split("\n\n")
        Lambda_spec = "\n".join(Lambda)

        try:
            with open("./lambda.txt", "w") as f:
                f.write(Lambda_spec)
        except:
            pass

        record_reading_process(texts, Lambda, recording_folder)

        return Lambda_spec
    
    
    def review_logs(
        self,
        # prompt_list = [],
        draft_exprs = "",
        hint = "",
    ) -> str:

        case = self.case
        doc = self.doc
        
        # if prompt_list:
        #     self.conversation_list = prompt_list

        review_question = f"""Now review the output for {case} protocol, determine the initial knowledge for each role. 
    
    The protocol description: {doc}
    {draft_exprs}

    Do not introduce extra role variables, only use: {hint}.
    """
        logging.info(f"\U0001f4d6:{review_question}")
        
        review_answer, _ = self.get_respone(
            review_question,
            model=self.model,
            maxTokens=self.max_tokens,
            temperature_arg=self.temperature,
            n_choices=self.n_choices
        )
        
        analysis_text = review_answer[0]
        

        answer_list, _ = self.get_respone(
            CONCLUDE_IN_DICT,
            model=self.model,
            maxTokens=self.max_tokens,
            temperature_arg=self.temperature,
            n_choices=self.n_choices,
        )
        
        determine_answer = answer_list[0]
        
        if determine_answer.startswith("```"):
            match = re.search(r'```\n([\s\S]*?)\n```', determine_answer)
            if match:
                determine_answer = match.group(1)
            
        if determine_answer.startswith("/*"):
            match = re.search(r'/*\n([\s\S]*?)\n*/', determine_answer)
            if match:
                determine_answer = match.group(1)
        
        with open("./parsing_log.log", "a") as f:
            appending_content = f"{determine_answer}"
            f.write(appending_content)
        
        return determine_answer



    def generate_top_spec(self, prompt_list, local_procs) -> str:
        if prompt_list:
            self.conversation_list = prompt_list
            
        sig_pattern = re.compile(r"^let\s+\w+\s*\(.*?\)\s*=.*$", re.MULTILINE)
        matches = sig_pattern.findall(local_procs)
        role_signature = "\n".join(matches)
        
        init_prompt_template = """\
Hints: <The hints text I give you>
Role signatures:
```
<The signature I give you>
```"""
        
        question = init_prompt_template.replace(
            "<The protocol text I give you>", self.doc
        ).replace("<The signature I give you>", role_signature)

        maxTokens = 1024
        top_spec, tokens_usage = self.get_respone(
            question,
            model=self.model,
            maxTokens=maxTokens,
            temperature_arg=self.temperature,
            n_choices=self.n_choices,
        )
        top_spec = eliminate_comments_from_top_spec(top_spec[0])

        with open("./top_spec.txt", "w") as f:
            f.write(top_spec)
        return top_spec