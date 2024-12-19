#!/usr/bin/env python3
# -*- coding: UTF-8 -*-
import json
import os, sys
from typing import Any, Dict, Text

def write_json_config_to_env(config: Dict[Text, Any]) -> None:
    # Write the JSON config to the environment.
    for key in config:
        # If the key is 'open_ai_api_key', set the environment variable
        # 'OPENAI_API_KEY' to the value of the key.
        if key == 'API_URL_BASE':
            os.environ['API_URL_BASE'] = str(config[key])
        # If the key is 'proxy', set the environment variable 'OPENAI_PROXY'
        # to the value of the key.
        elif key == 'proxy':
            # The config key is the name of the environment variable that contains the proxy
            os.environ['OPENAI_PROXY'] = str(config[key])
            os.environ["http_proxy"] = str(config[key])
            os.environ["https_proxy"] = str(config[key])
        elif key == 'open_ai_api_key':
            os.environ['OPENAI_API_KEY'] = str(config[key])
        # Otherwise, set the environment variable to the value of the key.
        else:
            os.environ[key] = str(config[key])


def load_json_config() -> Dict[Text, Any]:
    ##== Load the JSON config from the config.json file. ==##
    # returns the path to the current directory
    current_path = os.path.dirname(__file__)
    try:
        # opens config.json file in read mode
        with open(current_path+'/config.json', 'r') as f:
            # loads the config file into the variable config
            config = json.load(f)
            # calls the function write_json_config_to_env and passes the config variable as a parameter
            write_json_config_to_env(config)
            # returns the config variable
            return config
    except FileNotFoundError:
        print("Config file not found.")
        sys.exit(-1)
        return {}
    except json.decoder.JSONDecodeError:
        print("JSON file is not valid.")
        return {}