import logging
import subprocess
import ast
from colorama import Fore, Style
import re
import regex
import dirtyjson


class CustomFormatter(logging.Formatter):
    def format(self, record):
        original_format = self._style._fmt
        if record.levelno == logging.INFO:
            self._style._fmt = original_format.replace('%(funcName)10s', Fore.BLUE + '%(funcName)10s()' + Style.RESET_ALL)
        result = logging.Formatter.format(self, record)
        self._style._fmt = original_format
        return result
    

def setup_logger():        
    formatter = CustomFormatter('%(asctime)s %(levelname)-s %(filename)s:%(lineno)s - %(funcName)10s ::\n%(message)s')
    logger = logging.getLogger()
    logger.setLevel(logging.INFO)
    ch = logging.StreamHandler()
    ch.setLevel(logging.INFO)
    ch.setFormatter(formatter)
    logger.addHandler(ch)
    

def record_reading_process(chunks: list, Lambda: list, output_folder):
    log = ""
    for i, _ in enumerate(chunks):
        # log += f"```\n{chunks[i]}\n```" + f"\n{Lambda[i]}\n\n\n"
        log += f"```\U0001f4d6\n{chunks[i]}\n```\n"
        exprs = '\U0001f47D:\n' + "\n".join(["# "+ e for e in Lambda[i].split("\n")])
        log += exprs + "\n\n\n"
    
    try:
        with open(output_folder, "w") as f:
            f.write(log)
    except:
        pass
    
    
ERR_PARSE_JSON = {"error": "no json found!"}

def parse_json(json_str):
    
    ori_str = json_str
    
    filter_pattern = r'```json\n([\s\S]*?)\n```'
    match = re.search(filter_pattern, json_str)
    if match:
        json_str = match.group(1)
    try:
        pattern = regex.compile('\{(?:[^{}]|(?R))*\}')
        json_res = pattern.findall(json_str)
    except Exception as e:
        print(f"Error decoding JSON: {e}")


    with open("json_debug.txt", "w") as f:
        f.write(ori_str + "\n\n\n\n\n" +json_str)
    
    if len(json_res) == 0:
        return ERR_PARSE_JSON

    json_objs = []
    for json_str in json_res:
        json_str = json_str.replace('”','"')
        try:
            json_objs.append(dirtyjson.loads(json_str))
        except Exception as e:
            print(e)
            pass
    if len(json_objs) == 0:
        return ERR_PARSE_JSON
    
    return json_objs[-1]
    
    

def append_blank_line(parse_req:str, N:int):
    n = len(parse_req.split("\n"))
    parse_req += "\n"*(N-n) if n < N else ""
    return parse_req
    
     
def remove_comments_from_sapic(source:str):
    def replacer(match):
        s = match.group(0)
        if s.startswith('/'):
            return " " # note: a space and not an empty string
        else:
            return s
    import re
    pattern = re.compile(
            r'//.*?$|/\*.*?\*/|\'(?:\\.|[^\\\'])*\'|"(?:\\.|[^\\"])*"',
            re.DOTALL | re.MULTILINE
        )
    temp=[]
    for x in re.sub(pattern, replacer, source).split('\n'):
        if x.strip()!="":
            temp.append(x)
    return '\n'.join(temp)



def extract_line_commment_from_spec(spec:str) -> dict:
    comments = {}
    for i, line in enumerate(spec.split("\n")):
        if line.strip().startswith("//"):
            comments[i] = line
    return comments


def extract_exprs(logs:str) -> str:
    """
    Return only expressions, excluding anything else.
    """
    E = []
    for l in logs.split("\n"):
        # Sometimes this is a single line comment behind the expression
        l = l.split("//")[0].strip()
        l = l.split("#")[0].strip()
        if is_function_call(l):
            E.append(l)
        else:
            l = fix_brackets(l)
            if is_function_call(l):
                E.append(l)
    return "\n".join(E)


def is_function_call(code):
    try:
        parsed_code = ast.parse(code)
        class FunctionCallVisitor(ast.NodeVisitor):
            def __init__(self):
                self.found = False
            
            def visit_Call(self, node):
                self.found = True
                raise StopIteration

        visitor = FunctionCallVisitor()        
        try:
            visitor.visit(parsed_code)
        except StopIteration:
            pass
        return visitor.found
    except SyntaxError:
        return False
    
    
def extract_from_comments(comment_string:str) -> str:
    """
    Extract expressions from comments
    """
    try:
        matches = re.findall(r"/\*(.*?)\*/", comment_string, re.DOTALL)
        fcalls = []
        lines = matches[0].strip().split("\n")
        for line in lines:
            fcall_pattern = r"^\w+\((.*)\)\s*(?:\/\/.*)?$"
            match = re.match(fcall_pattern, line)
            if match:
                expr = line.split("//")[0].strip()
                fcalls.append(expr)
        return "\n".join(fcalls)
    except:
        pass
    
    return extract_exprs(comment_string)


def eliminate_comments_from_top_spec(spec: str) -> str:
    """ 
    Elimiate comments from top specs,
    ensures that it starts with the KEYWORD: process
    """
    spec = re.sub(r"//.*?$", "", spec, flags=re.MULTILINE)
    spec = re.sub(r"\n\s*\n", "\n", spec)
    spec = spec[0].lower() + spec[1:]

    # ensures that it starts with the KEYWORD: process
    KEYWORD = "process:"
    # assert spec.startswith(KEYWORD)
    return spec


def fix_brackets(input_str):
    """
    Fixes mismatched or missing brackets in a given string by ensuring every opening bracket has a corresponding
    closing bracket and removing unmatched closing brackets.
    """
    stack = []
    bracket_map = {')': '(', ']': '[', '}': '{'}
    inverse_bracket_map = {v: k for k, v in bracket_map.items()}
    opening_brackets = set(bracket_map.values())

    output_list = []
    unmatched_closing_indices = []

    for index, char in enumerate(input_str):
        if char in opening_brackets:
            stack.append((char, index))
            output_list.append(char)
        elif char in bracket_map:
            if stack and stack[-1][0] == bracket_map[char]:
                stack.pop()
                output_list.append(char)
            else:
                unmatched_closing_indices.append(index)
                logging.error(f"Unmatched closing bracket {char} at index {index}")
        else:
            output_list.append(char)

    while stack:
        output_list.append(inverse_bracket_map[stack.pop()[0]])

    fixed_str = ''.join(output_list)

    if unmatched_closing_indices:
        error_message = "Removed unmatched closing brackets at positions: "
        error_message += ", ".join(str(pos) for pos in unmatched_closing_indices)
        return fixed_str

    return fixed_str


def deconstruct_expr(input_str):
    function_names = ["sdec", "adec", "if_then_else", "If", "dec", "if"]
    pattern = r'\b(' + '|'.join(function_names) + r')\s*\((.*?)\)'
    matches = re.findall(pattern, input_str)
    if matches:
        return True
    else:
        return False


class LoopExpressionChecker(ast.NodeVisitor):
    def __init__(self, target_var):
        self.target_var = target_var
        self.found = False

    def visit_Name(self, node):
        if node.id == self.target_var:
            self.found = True

def is_loop_expression(code):
    """
    We do not consider loop feature in this work.
    Thus we exclude these operations that may lead to loop, e.g., update the counter.
    """
    try:
        tree = ast.parse(code, mode='exec')
    
        for node in ast.walk(tree):
            # Check: if this is a function call AND if this is an assignment statment
            if isinstance(node, ast.Call) and isinstance(node.func, ast.Name) and node.func.id == 'assign':

                if len(node.args) == 2:
                    first_assign_arg = node.args[0]
                    second_assign_arg = node.args[1]
                    if isinstance(first_assign_arg, ast.Name):
                        checker = LoopExpressionChecker(first_assign_arg.id)
                        checker.visit(second_assign_arg)
                        if checker.found:
                            return True
    except SyntaxError:
        return False
    return False



def compile_verify():
    cmd = "tamarin-prover synthesis.spthy -m=msr"
    try:
        result = subprocess.run(cmd, capture_output=True, text=True, shell=True)
        # print(result.stdout)
        return result.stdout, result.stderr
    except Exception as e:
        return str(e)



    
if __name__ == "__main__":   

    with open("json_debug.txt", "r") as f:
        json_str = f.read()
        
    json_str = """
To resolve the errors identified in the error reports, we need to ensure that the variables `idB`, `skA`, and `pkB` are correctly categorized and used in the lambda calculus expressions. Here's the categorization:

- `idB` (Identity of B): Initial Knowledge for A, as identities are commonly known in protocols.
- `skA` (Secret key of A): Initial Knowledge for A, as secret keys are known only to their respective owners.
- `pkB` (Public key of B): Common Knowledge, as public keys are publicly known.

Revised Lambda Calculus Expressions:
```json
{
  "ret": "ok",
  "revision": "Gen(A, rA) Op(A, assign(tA, timestamp())) Op(A, assign(encData, 'encData'))  // confidential parameters Op(A, assign(sgnData, 'sgnData'))  // non-confidential parameters Knows(A, idB, skA, pkB) Op(A, assign(hashData, hash(concat(tA, rA, idB, sgnData, encData)))) Op(A, assign(signedData, concat(tA, rA, idB, sgnData, encData, asenc(hashData, skA)))) Op(A, assign(encryptedData, aenc(signedData, pkB))) Send(A, B, encryptedData) Recv(B, A, encryptedData)"
}
```
"""

    res = parse_json(json_str)
    
    print(res["revision"])
