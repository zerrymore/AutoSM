import re

from prettytable import PrettyTable
# from tabulate import tabulate
import pandas as pd
from tabulate_cell_merger.tabulate_cell_merger import tabulate

import os
import subprocess
import tempfile

def get_summary(file_name:str):

    
    with open(file_name, 'w') as f:
        pass

    for file in os.listdir('.'):
        if file.endswith('.spthy'):
            print(f"Processing file: {file}")

            with tempfile.NamedTemporaryFile(delete=False) as temp_output:
                temp_output_name = temp_output.name

            try:
                with open(temp_output_name, 'w') as temp_output_file:
                    subprocess.run(['tamarin-prover', '--derivcheck-timeout=0', '--prove', '+RTS', '-N24', '-RTS', '--auto-sources', file],
                                stdout=temp_output_file, stderr=subprocess.PIPE, check=True)

                with open(temp_output_name, 'r', encoding='utf-8', errors='ignore') as temp_output_file:
                    content = temp_output_file.read()
                    content = content.replace('\x00', '')

                # Extract "summary of summaries"
                start_idx = content.find("summary of summaries:")
                end_idx = content.find("==============================================================================", start_idx)

                if start_idx != -1 and end_idx != -1:
                    summary = content[start_idx:end_idx].strip()

                    with open(file_name, 'a') as output:
                        output.write(summary + "\n\n")
                else:
                    print(f"Warning: No summary found in file: {file}")

            except subprocess.CalledProcessError as e:
                print(f"Error running tamarin-prover for file {file}: {e}")
            finally:
                os.remove(temp_output_name)
    with open(file_name, 'r') as f:
        return f.read()
    print(f"Extraction complete. Summarized content is stored in '{output_file}'.")



# def data_to_table(data):
#   table = PrettyTable()
#   table.padding_width = 1
#   table.align = "c"

#   split_row = ['—' * x for x in [22, 15, 15, 15, 15]]
  
#   table.title = "Proof Results"
#   table.field_names = ["Lemma", "Model-P", "Steps-P", "Model-R", "Steps-R"]
#   for model, values in data.items():
#     for lemma, c in values.items():
#       if lemma not in ["time", "index"]:
        
#         print(c)
#         for i in range(5):
#           if c[i] == "":
#             c[i] = "-"
            
  
#         table.add_row([lemma, c[1], c[2], c[3], c[4]])
#     # table.add_divider()
#     table.add_row(split_row)
#     # print(model)
#     table.add_row([f'{values["index"]} Time', f"\033[1m{model}-P.spthy\033[0m", values["time"][2], f"\033[1m{model}-R.spthy\033[0m", values["time"][4]])
#     # table.add_divider()
#     table.add_row(split_row)
#     table.add_row(["", "", "", "", ""])
#     # table.set_style(TableStyle.MARKDOWN)
#   # print(table)
  
#   return table

def data_to_table(data, rowspan:dict, colspan:dict):
  table = []
  
  row = 0
  start = 0
  for gdx, (model, values) in enumerate(data.items()):
    
    len_merge = 0
    
    for idx, (lemma, c) in enumerate(values.items()):
      if lemma not in ["time", "index"]:
        # print(idx)
        if idx == 2:
          start = row
        for i in range(5):
          if c[i] == "":
            c[i] = "-"
      
        table.append([model, lemma, c[1], c[2], c[3], c[4]])
        len_merge += 1
        row += 1

    rowspan[(start, 0)] = len_merge
    

    table.append([f'Time', f"", values["time"][2], f"", values["time"][4], ""])
    colspan[(row, 0)] = 2
    colspan[(row, 2)] = 2
    colspan[(row, 4)] = 2
    row += 1
    
    if gdx != len(data.items()) -1:
      table.append([""])
      row += 1

  return table

def read_summuary(summary_file:str):
  

  with open(summary_file, "r") as f:
    log_data = f.read()

  groups = re.split(r'\nanalyzed:', log_data.strip())

  groups = [group.strip() for group in groups if group.strip()]
  groups = groups[1:]


  model_pat = r'(.*?-[A-Za-z])\.spthy'


  time_pattern = r"processing time:\s*([0-9\.]+s)"
  lemma_pattern = r"([A-Za-z0-9_]+)\s\(all-traces\):\s([a-z]+).*?(\d+)\ssteps|([A-Za-z0-9_]+)\s\(exists-trace\):\s([a-z]+).*?(\d+)\ssteps"

  # 解析每个组
  data = dict()
  i = 0
  for group in groups:

      model_match = re.search(model_pat, group)
      model_p = model_match.group(1)
      modelName = model_p[:-2]
      # print(modelName)
      
      if modelName not in data:
        i += 1
        data[modelName] = {f"time": ["", "", "", "", ""]}
        data[modelName]["index"] = i
  
      time_match = re.search(time_pattern, group)    
      time = time_match.group(1) if time_match else "N/A"

      if model_p.endswith("-P"):
        data[modelName]["time"][2] = time
      elif model_p.endswith("-R"):
        data[modelName]["time"][4] = time


      lemma_matches = re.findall(lemma_pattern, group)

      for match in lemma_matches:
          # Find `all-traces` lemmas
          if match[0] and match[1] and match[2]:
              lemma = match[0]
              status = match[1]
              step = match[2]
          # Find `exists-trace` lemmas
          elif match[3] and match[4] and match[5]:
              lemma = match[3]
              status = match[4]
              step = match[5]
          
          if lemma not in data[modelName]:
            data[modelName][lemma] = ["", "", "", "", ""]

          
          # if model_p.endswith("-P"):
          Tag = {"verified": "√", "falsified": "x"}
          
          if model_p.endswith("-P"):
            data[modelName][lemma][1] = Tag[status]
            data[modelName][lemma][2] = step
          elif model_p.endswith("-R"):
            data[modelName][lemma][3] = Tag[status]
            data[modelName][lemma][4] = step

  return data

  
if __name__ == "__main__":
  rowspan = dict()
  colspan = dict()
  summary = read_summuary("summarized_results.txt")
  Table = data_to_table(summary, rowspan, colspan)

  headers = ["Protocol", "Lemma", "Sapic Model", "", "Tamarin Model", ""]
  rowspan = {((k[0] + 1), k[1]): v for k, v in rowspan.items()}
  colspan = {((k[0] + 1), k[1]): v for k, v in colspan.items()}
  # table_fancy = tabulate(Table, headers=headers, tablefmt="HTML")
  # print(table_fancy)
  Table = [headers] + Table
  colspan[(0,2)] = 2
  colspan[(0,4)] = 2
  tabulate(Table, colspan=colspan, rowspan=rowspan)
