#!/usr/bin/env python3
# -*- coding: UTF-8 -*-

import os, time
import openai
import logging
import tiktoken


def num_tokens_from_messages(messages, model="gpt-4o-mini-2024-07-18"):
    """Return the number of tokens used by a list of messages."""
    try:
        encoding = tiktoken.encoding_for_model(model)
    except KeyError:
        print("Warning: model not found. Using o200k_base encoding.")
        encoding = tiktoken.get_encoding("o200k_base")
    if model in {
        "gpt-3.5-turbo-0125",
        "gpt-4-0314",
        "gpt-4-32k-0314",
        "gpt-4-0613",
        "gpt-4-32k-0613",
        "gpt-4o-mini-2024-07-18",
        "gpt-4o-2024-08-06"
        }:
        tokens_per_message = 3
        tokens_per_name = 1
    elif "gpt-3.5-turbo" in model:
        print("Warning: gpt-3.5-turbo may update over time. Returning num tokens assuming gpt-3.5-turbo-0125.")
        return num_tokens_from_messages(messages, model="gpt-3.5-turbo-0125")
    elif "gpt-4o-mini" in model:
        print("Warning: gpt-4o-mini may update over time. Returning num tokens assuming gpt-4o-mini-2024-07-18.")
        return num_tokens_from_messages(messages, model="gpt-4o-mini-2024-07-18")
    elif "gpt-4o" in model:
        print("Warning: gpt-4o and gpt-4o-mini may update over time. Returning num tokens assuming gpt-4o-2024-08-06.")
        return num_tokens_from_messages(messages, model="gpt-4o-2024-08-06")
    elif "gpt-4" in model:
        print("Warning: gpt-4 may update over time. Returning num tokens assuming gpt-4-0613.")
        return num_tokens_from_messages(messages, model="gpt-4-0613")
    else:
        raise NotImplementedError(
            f"""num_tokens_from_messages() is not implemented for model {model}."""
        )
    num_tokens = 0
    for message in messages:
        num_tokens += tokens_per_message
        for key, value in message.items():
            num_tokens += len(encoding.encode(value))
            if key == "name":
                num_tokens += tokens_per_name
    num_tokens += 3  # every reply is primed with <|start|>assistant<|message|>
    return num_tokens


def find_best_reply_content(reply_list):
    best_reply = reply_list[0]
    return best_reply


class BaseChatClass:
    def __init__(
        self, 
        conversation_list=[], 
        continuous_talking=True, 
        useOpenKey=False,

    ) -> None:
        # set API KEY
        openai.api_base = os.environ.get("API_URL_BASE")
        openai.api_key = os.environ.get("OPENAI_API_KEY")

        self.useOpenKey = useOpenKey
        self.conversation_list = conversation_list
        self.continuous_talking = continuous_talking
        
    def reload_conversation(self, msg_list):
        self.conversation_list = msg_list

    def extend_conversation(self, msg_list):
        self.conversation_list.extend(msg_list)
    
    def show_conversation(self, msg_list):
        for msg in msg_list:
            if msg["role"] == "user":
                print(f"\U0001f47b: {msg['content']}\n")
            else:
                print(f"\U0001f47D: {msg['content']}\n")

    def get_respone(
        self,
        prompt,
        model="gpt-3.5-turbo",
        maxTokens=2048,
        temperature_arg=0.5, 
        stream_out=True,
        stop_str=None,
        n_choices=1,
    ):
        
        if self.continuous_talking:
            self.conversation_list.append({"role": "user", "content": prompt})

        for i in range(3):
            try:
                response = openai.ChatCompletion.create(
                    model=model,
                    messages=self.conversation_list,
                    max_tokens=maxTokens,
                    temperature=temperature_arg,
                    frequency_penalty=0,
                    presence_penalty=0,
                    top_p=1,
                    stream=stream_out,
                    stop=stop_str,
                    n=n_choices,
                )
                break
            except openai.error.RateLimitError as e:
                if i < 2:
                    # if the error is due to rate limit, wait 10 seconds and try again
                    logging.warning(e, f"\nwait {(i+1)*10} seconds and try again")
                    time.sleep((i + 1) * 10)
                else:
                    raise e
            except Exception as e:
                if i < 2:
                    logging.warning(e, f"\nwait {(i+1)*2} seconds and try again")
                    time.sleep((i + 1) * 2)
                else:
                    raise e

        logging.info(f"LLM {model} querying ...\n")
        full_reply_content_list = []
        # use stream of chunks
        if stream_out:
            # create variables to collect the stream of chunks
            collected_messages = []
            for i in range(n_choices):
                collected_messages.append([])

            # iterate through the stream of events
            print(f"\U0001f47D: ", end="")
            for chunk in response:
                # print(chunk)
                try:
                    chunk_msg = chunk.choices[0]["delta"]  # extract the message
                    collected_messages[chunk.choices[0]["index"]].append(
                        chunk_msg
                    )  # save the message
                    if chunk.choices[0]["index"] == 0:
                        print(chunk_msg.get("content", ""), end="")
                except Exception as e:
                    print(e)
            print("\n")

            # get the full reply content in list
            for each_collected_message in collected_messages:
                full_reply_content_list.append(
                    "".join([m.get("content", "") for m in each_collected_message])
                )

            # get the useage of the API
            # prompt_tokens = num_tokens_from_messages(self.conversation_list, model)
            tmp_cvlist = self.conversation_list.copy()
            for each_reply_content in full_reply_content_list:
                tmp_cvlist.append({"role": "assistant", "content": each_reply_content})
            print(model)
            try:
                tokens_usage = num_tokens_from_messages(tmp_cvlist, model) - n_choices * 5
            except:
                tokens_usage = -1
        # don't use stream of chunks
        else:
            # get the reply content
            for each_choice in response.choices:
                one_reply_content = each_choice.message["content"]
                full_reply_content_list.append(one_reply_content)
                logging.info(f"\U0001f47D: {one_reply_content}\n")
            # get the useage of the API
            tokens_usage = response.usage["total_tokens"]

        # find the best reply
        best_reply_content = find_best_reply_content(full_reply_content_list)

        if self.continuous_talking:
            self.conversation_list.append(
                {"role": "assistant", "content": best_reply_content}
            )

        return full_reply_content_list, tokens_usage