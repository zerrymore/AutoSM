import re

from tabulate_cell_merger.tabulate_cell_merger import tabulate

import argparse


def data_to_table(data, rowspan:dict, colspan:dict):
  """
  Converts the parsed model summary data into a table format. This function generates
  a list of rows for a table where each row corresponds to a lemma and its associated
  values (status, steps, etc.). It also handles merging cells using the `rowspan` and `colspan` 
  dictionaries, which define which cells should span multiple rows or columns.
  """
  
  table = []

  row = 0
  start = 0
  for gdx, (model, values) in enumerate(data.items()):
    
    len_merge = 0
    for idx, (lemma, c) in enumerate(values.items()):
      if lemma not in ["time", "index"]:
        if idx == 2:
          start = row
        for i in range(5):
          if c[i] == "":
            c[i] = "-"
      
        table.append([f'{values["index"]}-{model}', lemma, c[1], c[2], c[3], c[4]])
        len_merge += 1
        row += 1

    rowspan[(start, 0)] = len_merge
    
    table.append([f'Time', f"", values["time"][2], f"", values["time"][4], ""])
    
    # Set colspan for multiple columns (0, 2, 4)
    for col in [0, 2, 4]:
      colspan[(row, col)] = 2

    row += 1
    if gdx != len(data)-1:
      table.append([""])
      row += 1
  return table

  
def read_summuary(log_data:str):
  """
  Parses the provided log data to extract model information, processing times,
  and lemma status from the analysis results. This function processes log entries
  that are separated by the 'analyzed:' keyword and organizes the data into a 
  structured dictionary.
  
  Output:
    {
        'model1': {
            'index': 1,
            'time': ['', '', '1.23s', '', ''],
            'lemma1': ['', '√', '10', '', ''],
            'lemma2': ['', 'x', '20', '', '']
        },
        'model2': {
            'index': 2,
            'time': ['', '', '', '', '2.34s'],
            'lemma3': ['', '√', '30', '', ''],
            'lemma4': ['', 'x', '40', '', '']
        }
  """
  
  # Split the log data into groups based on the 'analyzed:' separator and clean up the data
  groups = [group.strip() for group in re.split(r'\nanalyzed:', log_data) if group.strip()]

  # Pattern for matching the model name in the format 'something-<letters>.spthy'
  model_pat = r'(.*?-[A-Za-z])\.spthy'

  # Pattern for extracting processing time, e.g., 'processing time: 1.23s'
  time_pattern = r"processing time:\s*([0-9\.]+s)"

  # Pattern for extracting lemma information for both 'all-traces' and 'exists-trace' types
  lemma_pattern = r"([A-Za-z0-9_]+)\s\(all-traces\):\s([a-z]+).*?(\d+)\ssteps|" \
                r"([A-Za-z0-9_]+)\s\(exists-trace\):\s([a-z]+).*?(\d+)\ssteps"

  data = dict()
  i = 0
  for group in groups:

      model_match = re.search(model_pat, group)
      if not model_match:
        continue  # Skip if no model match is found
      
      model_name = model_match.group(1)[:-2]  # Extract model name
      model_p = model_match.group(1)
      
      if model_name not in data:
        i += 1
        data[model_name] = {
            "index": i,
            "time": ["", "", "", "", ""]
        }
  
      time_match = re.search(time_pattern, group)    
      time = time_match.group(1) if time_match else "N/A"

      if model_p.endswith("-P"):
        data[model_name]["time"][2] = time
      elif model_p.endswith("-R"):
        data[model_name]["time"][4] = time


      lemma_matches = re.findall(lemma_pattern, group)

      for match in lemma_matches:
          # Find `all-traces` lemmas
          if match[0] and match[1] and match[2]:
              lemma, status, step = match[0], match[1], match[2]
              
          # Find `exists-trace` lemmas
          elif match[3] and match[4] and match[5]:
              lemma, status, step = match[3], match[4], match[5]
          
          if lemma not in data[model_name]:
            data[model_name][lemma] = ["", "", "", "", ""]

          
          Tag = {"verified": "√", "falsified": "x"}
                    
          if model_p.endswith("-P"):
            data[model_name][lemma][1] = Tag[status]
            data[model_name][lemma][2] = step
          elif model_p.endswith("-R"):
            data[model_name][lemma][3] = Tag[status]
            data[model_name][lemma][4] = step

  return data

    
def main():
  
  parser = argparse.ArgumentParser(description="Process log files and generate a summary table.")
  
  # Define required argument: input file
  parser.add_argument(
      'logfile', 
      type=str, 
      help="Path to the log file containing the summarized results."
  )
  
  # Define optional argument: output file (default is None)
  parser.add_argument(
      '-o', '--output', 
      type=str, 
      help="Path to the output file to save the generated table (optional)."
  )
  
  args = parser.parse_args()
  
  rowspan = dict()
  colspan = dict()

  with open(args.logfile, 'r') as f:
      log_data = f.read()
    
  summary = read_summuary(log_data)

  Table = data_to_table(summary, rowspan, colspan)

  headers = ["Protocol", "Lemma", "Sapic+ Model", "", "Tamarin Model", ""]
  rowspan = {((k[0] + 1), k[1]): v for k, v in rowspan.items()}
  colspan = {((k[0] + 1), k[1]): v for k, v in colspan.items()}
  
  Table = [headers] + Table
  
  # Set colspan for specific cells
  colspan.update({(0, 2): 2, (0, 4): 2})

  tabulate(Table, colspan=colspan, rowspan=rowspan)
        
if __name__ == "__main__":
  main()