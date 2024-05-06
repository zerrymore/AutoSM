import os
import subprocess
import glob

files_to_delete = glob.glob('msr_*.spthy')

def remove_files(files_to_delete):
    for file in files_to_delete:
        try:
            os.remove(file)
            print(f"Deleted {file}")
        except OSError as e:
            print(f"Error: {file} : {e.strerror}")

def output_msrs(spthy_files, format="tamarin"):
    postfix = {"tamarin": "spthy", "proverif": "pv"}
    file_type = {"tamarin": "msr", "proverif": "pv"}
    for file in spthy_files:
        print(file)
        file_name = file.split(".")[0]
        try:
            cmd_command = f'tamarin-prover {file} -m={file_type[format]} > ./output/{format}/{format}_{file_name}.{postfix[format]}'  # 示例命令：打印文件名
            subprocess.run(cmd_command, shell=True)  # 执行命令
        except:
            print("error")
          
spthy_files = glob.glob('*.spthy')            
# The sapic+ files which can be compiled into Tamarin and ProVerif
source_files = [file for file in spthy_files if os.path.isfile(file)]


print(len(source_files))

output_msrs(source_files, "proverif")
output_msrs(source_files, "tamarin")
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