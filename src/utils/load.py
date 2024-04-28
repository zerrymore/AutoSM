import json
from pathlib import Path

def load_prots(prots_json_file: Path):
    with prots_json_file.open('r') as file:
        data = json.load(file)
    return data