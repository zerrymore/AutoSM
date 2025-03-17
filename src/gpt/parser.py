import re
from gpt.prompt_all import *
from gpt.utils import (
    fix_brackets,
    deconstruct_expr,
    is_loop_expression,
    remove_comments_from_sapic,
    extract_line_commment_from_spec
)
import logging
from lark import Token, Tree
from gpt.bnf import pretty_stmts
from lark import Lark

from gpt.bnf import ROLE_BNF, TOP_BNF
RoleParser = Lark(ROLE_BNF, parser='lalr', start='start', propagate_positions=True)
TopParser = Lark(TOP_BNF, parser='lalr', start='start', propagate_positions=True)


def parse_with_fallback(parser, spec):
    try:
        return parser.parse(spec)
    except:
        return None


def collect_subtrees(tree_node, name: str):
    subtrees = []

    if isinstance(tree_node, Tree):
        if tree_node.data == name:
            subtrees.append(tree_node)
        for child in tree_node.children:
            subtrees.extend(collect_subtrees(child, name))
    return subtrees


def collect_funcs(local_processes:str, top_process:str) -> list:
    """collect function declaration from top process and local processes.

    Returns:
        functions:list of function name with arity
    """
    role_root = parse_with_fallback(
        RoleParser, remove_comments_from_sapic(local_processes)
    )
    top_spec_without_comments = remove_comments_from_sapic(top_process)
    config_root = parse_with_fallback(TopParser, top_spec_without_comments)

    func_nodes = []
    for root in [role_root, config_root]:
        try:
            func_nodes += collect_subtrees(root, "func")
        except Exception as e:
            pass

    functions = set()
    for node in func_nodes:
        node: Tree
        func_name = node.children[0].value
        arity = len(node.children[1].children)
        functions.add(f"{func_name}/{arity}")
    return functions


        
def indent_local_processes(local_processes:str) -> str:
    
    comments = extract_line_commment_from_spec(local_processes)
            
    role_root = parse_with_fallback(
        RoleParser, remove_comments_from_sapic(local_processes)
    )

    # try:
    new_stmt = []
    pretty_stmts(role_root, new_stmt)
    new_spec = "\n".join(new_stmt)

    i, j = 0, 0
    stmt_l = []
    while j < len(comments) + len(new_stmt):
        if j in comments:
            stmt_l += [comments[j]]
        else:
            stmt_l += [new_stmt[i]]
            i += 1
        j += 1
    new_spec = "\n".join(stmt_l)
    return new_spec
    
    
    
def extract_from_comments(comment_string:str) -> str:
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



# def T_transform(Lambda_spec: str) -> str:
#     """
#     Take pure expressions as input, then output the particial process.
#     """
#     try:
#         ##== Filter out all the deconstrction expressions,  ==##
#         ##== only allowing construction and I/O expressions ==##

#         corr_Lambda_spec = "\n".join(
#             fix_brackets(line)
#             for line in Lambda_spec.split("\n")
#             if (not deconstruct_expr(line)) and (not is_loop_expression(line))
#         )

#         with open("./filter_expr.txt", "w") as f:
#             f.write(corr_Lambda_spec)

#         from gpt.translator import lambda_to_processes, format_parse_output

#         spec, _ = lambda_to_processes(corr_Lambda_spec)
#         Role_spec, _ = format_parse_output(spec)
#     except Exception as e:
#         logging.error(str(e))
#         Role_spec = ""
#     return Role_spec




if __name__ == "__main__":
    code = """
Knows(A, Kas, idA)
Knows(B, Kbs, idB)
Knows(S, Kas, Kbs)
Send(A, B, idA)
Recv(B, A, idA)
Gen(B, Nb)
Send(B, A, Nb)
Recv(A, B, Nb)
Op(A, assign(encryptedNb, senc(Nb, Kas)))
Send(A, B, encryptedNb)
Recv(B, A, encryptedNb)
Op(B, assign(encryptedIdA, senc(idA, Kbs)))
Send(B, S, concat(encryptedNb, encryptedIdA))
Recv(S, B, concat(encryptedNb, encryptedIdA))
Op(S, assign(decryptedNb, dec(encryptedNb, Kas)))
Op(S, assign(decryptedIdA, dec(encryptedIdA, Kbs)))
Op(S, assign(encryptedNbForB, senc(decryptedNb, Kbs)))
Send(S, B, encryptedNbForB)
Recv(B, S, encryptedNbForB)
Op(B, assign(decryptedNbForB, dec(encryptedNbForB, Kbs)))# """
    print(T_transform(code))
    # print(extract_from_comments(code))
