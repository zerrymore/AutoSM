from lark import Lark, Transformer, Tree, Token
from gpt.bnf import ROLE_BNF, TOP_BNF

RoleParser = Lark(ROLE_BNF, parser='lalr', start='start', propagate_positions=True)
TopParser = Lark(TOP_BNF, parser='lalr', start='start', propagate_positions=True)
class Role:
    def __init__(self):
        self.vars = []   
    
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

def filter_match_subtrees(tree, match="match"):
    matched_subtrees = []
    
    if tree.data == match:
        matched_subtrees.append(tree)
    
    if isinstance(tree, Tree):
        for child in tree.children:
            if isinstance(child, Tree):
                matched_subtrees.extend(filter_match_subtrees(child, match))
    
    return matched_subtrees
    
def _vars(tree_node):
    if isinstance(tree_node, Token):
        return {tree_node.value}
    elif isinstance(tree_node, Tree):
        vars_set = set()  
        start_index = 0  

        if tree_node.data in ['func','event']:
            start_index = 1
            
        for child in tree_node.children[start_index:]:
            vars_set.update(_vars(child)) 
        return vars_set
    else:
        return set()
    
def count_leading_spaces(s):
    return len(s) - len(s.lstrip())

def static_analysis(root:Tree, vars:set, Err:dict[int]):
    for node in root.children:
        if isinstance(node, Tree):
            if node.data in ["stmt"]:
                stmt = node.children[0]
                if stmt.data in ["new"]:
                    vars.update(_vars(stmt))
                elif stmt.data in ["_in"]:
                    matched_trees:list = [_vars(t) for t in filter_match_subtrees(stmt)]
                    if matched_trees:
                        matched:set = set.union(*matched_trees)
                    else: 
                        matched = set()
                    unmatched = _vars(stmt) - matched
                    
                    # Err 1: Extra matching
                    if len(matched - vars) != 0:
                        err_str = f"// {matched-vars} is not local variable, can not be matched."
                        Err.setdefault(node.meta.line, []).append(err_str)
                        
                    # Err 2: Lacking mathching
                    if len(unmatched & vars) != 0:
                        err_str = f"// {unmatched & vars} should use `=` to be matched."
                        Err.setdefault(node.meta.line, []).append(err_str)
                    vars.update(_vars(stmt))
                    
                elif stmt.data in ["out", "event"]:
                    out_vars = _vars(stmt)
                    if len(out_vars-vars) != 0:
                        err_str = f"// Variables {out_vars-vars} are unbounded."
                        Err.setdefault(stmt.meta.line, []).append(err_str)
                    # vars.update(_vars(stmt))
            elif node.data in ["binding"]:
                left = node.children[0]
                matched_trees:list = [_vars(t) for t in filter_match_subtrees(left)]
                
                # There exists some matched variables
                if matched_trees:
                    matched:set = set.union(*matched_trees)
                    # print(f"matched:{matched}")
                else: 
                    matched = set()
                unmatched = _vars(left) - matched
                
                # Err 1: Extra matching
                if len(matched - vars) != 0:
                    # print(f"// Extra matching:{matched-vars}")
                    err_str = f"// {matched-vars} is not local variable, can not be matched."
                    Err.setdefault(node.meta.line, []).append(err_str)
                    
                # Err 2: Lacking mathching
                if len(unmatched & vars) != 0:
                    # print(f"lack matching:{unmatched & vars}")
                    err_str = f"// {unmatched & vars} should use `=` to be matched."
                    Err.setdefault(node.meta.line, []).append(err_str)
                    
                right = node.children[1]
                # Err 3: Unbounded variable used
                if len(_vars(right)-vars) != 0:
                    err_str = f"// Variables {_vars(right)-vars} are unbounded"
                    Err.setdefault(node.meta.line, []).append(err_str)
                    # print(f"unbounded varaible used:{_vars(right)-vars}")
                vars.update(_vars(left))
            elif node.data in ["guard"]:
                pass
            else:
                static_analysis(node, vars, Err)

import logging
def analysis(code):
    logging.info(f'\U0001f605:{code}')
    R = []
    root = RoleParser.parse(code)
    for node in root.children:
        if node.data == "role":
            R += [node]
    
    Err = {}
    for role in R:
        vars = set()
        for node in role.children:
            if isinstance(node, Tree) and node.data in ["parameters"]:
                vars.update(_vars(node))
            if isinstance(node, Tree) and node.data in ["stms"]:
                static_analysis(node, vars, Err)

    start = 0
    spec = []
    codes = code.split("\n")
    for line_num, errs in Err.items():
        end = line_num - 1
        spec += [line for line in codes[start:end]]
        space_num = count_leading_spaces(codes[line_num-1])
        spec += [" "*space_num + line for line in errs]
        start = end
    spec += codes[start:]
    return "\n".join(spec)

if __name__ == "__main__":
    code2 ="""\
let A(Kas, idA, idB) =
  new Na;
  out(<idA, idB, Na>);
  in(cipher1);
  let message2 = sdec(cipher1, Kas) in
  let <=Na, =idB, Kab, cipher2> = message2 in
  out(cipher2);
  in(cipher3);
  let message3 = sdec(cipher3, Kab) in
  out(senc(message3, Kab)); 0

let B(Kbs, idA, idB) =
  in(cipher);
  let message = sdec(cipher, Kbs) in
  let <Kab, =idA> = message in
  new Nb;
  out(senc(Nb, Kab));
  in(cipher3);
  let message3 = sdec(cipher3, Kab) in 0

let S(Kas, Kbs, idA, idB) =
  new Kab;
  in(<=idA, =idB, Na>);
  let message1 = senc(<Na, idB, Kab, senc(<Kab, idA>, Kbs)>, Kas) in
  out(message1); 0"""
    new_version = []
    for line in code2.split('\n'):
        if line.startswith('-'):
            pass  
        elif line.startswith('+'):
            new_version.append(line.replace("+", " "))  
        else:
            new_version.append(line)
    file = "\n".join(new_version)

    print(analysis(file) == file)
    
    
