from gpt.prompt_all import *
from gpt.gptcore import BaseChatClass
from gpt.prompt import get_incontext_learning_contents
from gpt.benchmark import DB
if __name__ == "__main__":
    llm_model = "gpt-4"
    n_choices = 1
    few_shot = 2
    useOpenKey = True
    temperature = 0.4
    maxTokens = 2048
    run_times = 5
    for name in DB.keys():
        for t in range(run_times):
            doc = DB[name]
            chatveri = BaseChatClass(get_incontext_learning_contents('sapic', shot_num=few_shot), useOpenKey=useOpenKey)
            question = sapic_prompt_template.replace("<The protocol text I give you>", doc)
            answer, tokens_usage = chatveri.get_respone(question, model=llm_model, maxTokens=maxTokens, temperature_arg=temperature, n_choices=n_choices)
            spec = answer[0]
            dir_name = "./output/gpt_4_few_2_shot"
            file_name = f"{name}_{t+1}"
            with open(f"{dir_name}/{file_name}.spthy", "w") as f:
                f.write(spec)
            
