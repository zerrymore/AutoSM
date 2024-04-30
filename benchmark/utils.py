import os
import subprocess
import glob
# 获取当前目录下的所有文件名
# current_directory = '.'  # 当前目录
# files = os.listdir(current_directory)
# print(len(files))


files_to_delete = glob.glob('msr_*.spthy')

def remove_files(files_to_delete):
    for file in files_to_delete:
        try:
            os.remove(file)
            print(f"Deleted {file}")
        except OSError as e:
            print(f"Error: {file} : {e.strerror}")

spthy_files = glob.glob('*.spthy')
spthy_files = [file for file in spthy_files if os.path.isfile(file)]
print(len(spthy_files))
def output_msrs(spthy_files, format="msr"):
    postfix = {"msr": "spthy", "proverif": "pv"}
    for file in spthy_files:
        print(file)
        file_name = file.split(".")[0]
        try:
            cmd_command = f'tamarin-prover {file} -m={format} > ./output/{format}_{file_name}.{postfix[format]}'  # 示例命令：打印文件名
            subprocess.run(cmd_command, shell=True)  # 执行命令
        except:
            print("error")

output_msrs(spthy_files, "proverif")
if __name__ == "__main__":
    file_str = """\
example 
exercise 
NSPK       
NSSK     
SigFox    
LAKE       		
NAXOS     		 
X509.1     		
SSH              
EDHOC            
KEMTLS          
Yahalom         	
Kao Chow         
SPLICE/AS        	
Otway Rees       	
Woo and Lam     
Denning-Sacco   
Stubblebine 
"""
    file_id = """\
running_ex
exercise
nsl     
nssk     
sigfox   
LAK06       		
NAXOS     		 
X509_1     		
ssh             
EDHOC            
KEMTLS          
Yahalom         	
kao_chow         
SPLICE/AS        	
or       	
WooLamPi_f     
Denning_Sacco   
neu 
"""
    import re
    files = [f.strip() for f in file_id.split("\n")]
    print(files)
    for name in files:
        file = f"{name}_verified.spthy"
        if os.path.isfile(file):
            with open(file, "r") as f:
                content = f.read()
            lemma_pattern = r'lemma .*?:' 
            lemma_num = re.findall(lemma_pattern, content)
            ptime_attern = r'processing time: ([\d.]+)s'
            match = re.search(ptime_attern, content)
            if match:
                processing_time = match.group(1)  # group(1) 表示第一个括号内匹配的内容
            else:
                processing_time = "None"
            print(name, len(lemma_num), processing_time)