from gpt.prompt import get_incontext_learning_contents
from gpt.gptcore import BaseChatClass
from gpt.prompt_all import *
import re

def eliminate_comments_from_top_spec(spec:str) -> str:
    """elimiate comments from top specs, 
    ensures that it starts with the KEYWORD: process
    """
    spec = re.sub(r'//.*?$', '', spec, flags=re.MULTILINE)
    spec = re.sub(r'\n\s*\n', '\n', spec)
    spec = spec[0].lower() + spec[1:]
    
    # ensures that it starts with the KEYWORD: process
    KEYWORD = 'process:'
    assert spec.startswith(KEYWORD)
    return spec


def generate_top_spec(doc:str, local_processes:str, llm_model, temperature, n_choices, maxTokens, useOpenKey=True):
    sig_pattern = re.compile(r"^let\s+\w+\s*\(.*?\)\s*=.*$", re.MULTILINE)
    matches = sig_pattern.findall(local_processes)
    role_signature = "\n".join(matches)
    chatveri = BaseChatClass(get_incontext_learning_contents('init_signature'), useOpenKey=useOpenKey)
    question = (init_prompt_template
                            .replace("<The protocol text I give you>", doc)
                            .replace("<The signature I give you>", role_signature))

    maxTokens = 1024
    top_spec, tokens_usage = chatveri.get_respone(
        question, 
        model=llm_model, 
        maxTokens=maxTokens, 
        temperature_arg=temperature, 
        n_choices=n_choices)                    
    top_spec = eliminate_comments_from_top_spec(top_spec[0])
    
    return top_spec
