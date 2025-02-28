import re

from prettytable import PrettyTable, TableStyle


import os
import subprocess
import tempfile

def get_summary():
    output_file = "results.txt"

    with open(output_file, 'w') as f:
        pass

    # 查找当前目录下所有以 .spthy 结尾的文件
    for file in os.listdir('.'):
        if file.endswith('.spthy'):
            print(f"Processing file: {file}")

            # 创建临时文件
            with tempfile.NamedTemporaryFile(delete=False) as temp_output:
                temp_output_name = temp_output.name

            try:
                # 运行 tamarin-prover 并将输出重定向到临时文件
                with open(temp_output_name, 'w') as temp_output_file:
                    subprocess.run(['tamarin-prover', '--derivcheck-timeout=0', '--prove', '+RTS', '-N24', '-RTS', '--auto-sources', file],
                                stdout=temp_output_file, stderr=subprocess.PIPE, check=True)

                # 读取临时文件的内容并去掉 null 字符
                with open(temp_output_name, 'r', encoding='utf-8', errors='ignore') as temp_output_file:
                    content = temp_output_file.read()
                    content = content.replace('\x00', '')  # 去掉 null 字符

                # 提取 "summary of summaries" 部分
                start_idx = content.find("summary of summaries:")
                end_idx = content.find("==============================================================================", start_idx)

                if start_idx != -1 and end_idx != -1:
                    summary = content[start_idx:end_idx].strip()

                    # 将提取的内容追加到输出文件
                    with open(output_file, 'a') as output:
                        output.write(summary + "\n\n")
                else:
                    print(f"Warning: No summary found in file: {file}")

            except subprocess.CalledProcessError as e:
                print(f"Error running tamarin-prover for file {file}: {e}")
            finally:
                # 删除临时文件
                os.remove(temp_output_name)
    with open(output_file, 'r') as f:
        return f.read()
    print(f"Extraction complete. Summarized content is stored in '{output_file}'.")


# log_data = get_summary()

with open("summarized_results.txt", "r") as f:
  log_data = f.read()
# with open("results.txt", "r") as f:
#   log_data = f.read()



groups = re.split(r'\nanalyzed:', log_data.strip())

groups = [group.strip() for group in groups if group.strip()]


# print(groups)
# # print(len(groups))  # 打印分组数量
groups = groups[1:]  # 去掉第一个空字符串
# for group in groups:
#   print("-" * 40)  # 分隔符
#   print(group)


# 正则表达式匹配每组中的各项内容
process_pat = r"([A-Za-z0-9_-]+-P)\.spthy"
msr_pat = r"([A-Za-z0-9_-]+-R)\.spthy"
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
    # 匹配 model 和 time
    time_match = re.search(time_pattern, group)    
    time = time_match.group(1) if time_match else "N/A"

    if model_p.endswith("-P"):
      data[modelName]["time"][2] = time
    elif model_p.endswith("-R"):
      data[modelName]["time"][4] = time


    lemma_matches = re.findall(lemma_pattern, group)

    for match in lemma_matches:
        # 对于 (all-traces) 类型的引理
        if match[0] and match[1] and match[2]:
            lemma = match[0]
            status = match[1]
            step = match[2]
        # 对于 (exists-trace) 类型的引理
        elif match[3] and match[4] and match[5]:
            lemma = match[3]
            status = match[4]
            step = match[5]
        
        if lemma not in data[modelName]:
          data[modelName][lemma] = ["", "", "", "", ""]
        # 打印匹配到的引理
        # print(f"  Lemma: {lemma}, Status: {status}, Steps: {step}")
        
        # if model_p.endswith("-P"):
        Tag = {"verified": "\033[32m✔\033[0m", "falsified": "\033[31m✘\033[0m"}
        
        if model_p.endswith("-P"):
          data[modelName][lemma][1] = Tag[status]
          data[modelName][lemma][2] = step
        elif model_p.endswith("-R"):
          data[modelName][lemma][3] = Tag[status]
          data[modelName][lemma][4] = step


def data_to_table(data):
  # table = ColorTable(theme=Themes.OCEAN)

  # print(data)
  table = PrettyTable()
  table.align = "l"

  split_row = ['—' * x for x in [22, 15, 15, 15, 15]]
  
  table.title = "Proof Results"
  table.field_names = ["Lemma", "Model-P", "Steps-P", "Model-R", "Steps-R"]
  for model, values in data.items():
    for lemma, c in values.items():
      if lemma not in ["time", "index"]:
        
        print(c)
        for i in range(5):
          if c[i] == "":
            c[i] = "-"
            
  
        table.add_row([lemma, c[1], c[2], c[3], c[4]])
    # table.add_divider()
    table.add_row(split_row)
    # print(model)
    table.add_row([f"{values["index"]} Time", f"\033[1m{model}-P.spthy\033[0m", values["time"][2], f"\033[1m{model}-R.spthy\033[0m", values["time"][4]])
    # table.add_divider()
    table.add_row(split_row)
    table.add_row(["", "", "", "", ""])
    table.set_style(TableStyle.MARKDOWN)
    
  return table


data_to_table(data)
print(data_to_table(data))