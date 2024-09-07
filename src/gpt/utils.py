import logging
import subprocess
from colorama import Fore, Style

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



def extract_multiline_comment_from_sapic(source:str):
    import re
    comments = re.findall(r'/\*.*?\*/', source, re.DOTALL)
    return comments[0]


def extract_line_commment_from_spec(spec:str) -> dict:
    """extract comments starting with '//'

    Args:
        spec (str): the input specification

    Returns:
        comments: a dictionary, where key is line number, value is comment
    """
    comments = {}
    for i, line in enumerate(spec.split("\n")):
        if line.strip().startswith("//"):
            comments[i] = line
    return comments

def fix_missing_closing_brackets(input_str):
    stack = []
    bracket_map = {')': '(', ']': '[', '}': '{'}
    inverse_bracket_map = {v: k for k, v in bracket_map.items()}
    opening_brackets = set(bracket_map.values())

    # Scan the input to find unmatched opening brackets and record them in a stack
    for char in input_str:
        if char in opening_brackets:
            stack.append(char)
        elif char in bracket_map:
            if stack and stack[-1] == bracket_map[char]:
                stack.pop()
            else:
                return "Error: Unmatched closing bracket found. Cannot fix."
    if len(stack) > 1:
        logging.error("unmatched '(' found")
    else:
        # logging.info("matched")
        pass
    # Append missing closing brackets in reverse order of unmatched opening brackets
    missing_closing_brackets = ''.join(inverse_bracket_map[bracket] for bracket in reversed(stack))
    fixed_str = input_str + missing_closing_brackets

    return fixed_str


def deconstruct_expr(expr:str) -> bool:
    deconstruct_ops = ["sdec", "adec"]
    for op in deconstruct_ops:
        if op in expr:
            return True
    return False

def parse_diff(diff_text):
    old_version = []
    new_version = []
    for line in diff_text.split('\n'):
        if line.startswith('-'):
            old_version.append(line.replace("-", " "))  
        elif line.startswith('+'):
            new_version.append(line.replace("+", " "))  
        else:
            old_version.append(line)
            new_version.append(line)
    return "\n".join(old_version), "\n".join(new_version)

def compile_verify():
    cmd = "tamarin-prover FirstExample.spthy --prove | sed -n '/^==*/,/^==*/p'"
    cmd = "tamarin-prover FirstExample.spthy --prove"
    cmd = "tamarin-prover synthesis.spthy -m=msr"
    try:
        result = subprocess.run(cmd, capture_output=True, text=True, shell=True)
        # print(result.stdout)
        return result.stdout, result.stderr
    except Exception as e:
        return str(e)

if __name__ == "__main__":
    code = """\
let A(Kas, idA, idB) =
  new Na;
  out(<idA,idB,Na>);
  in(cypher1);
  let message2 = sdec(cypher1, Kas) in
  // Extract Kab and Kbs from message2
  let <=Kab, =idA, Kbs> = message2 in
  out(senc(<Kab,idA>, Kbs));
  in(cypher2);
  let msg1 = sdec(cypher2, Kab) in
  // Extract Nb from msg1
  let =Nb = msg1 in
  out(senc(Nb, Kab)); 0

let B(Kbs, idA, idB) =
  in(cypher3);
  let msg1 = sdec(cypher3, Kbs) in
  // Extract Kab from msg1
  let <=Kab, =idA> = msg1 in
  new Nb;
  out(senc(Nb, Kab));
  in(cypher4);
  let msg2 = sdec(cypher4, Kab) in 0

let S(Kas, Kbs, idA, idB) =
  new Kab;
  // Introduce Na and cypher5 from received message
  in(cypher5);
  let =Na = cypher5 in
  let message1 = senc(<Na,idB,Kab,cypher5>, Kas) in
  out(message1); 0
"""
    print(remove_comments_from_sapic(code))