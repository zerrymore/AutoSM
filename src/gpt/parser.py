import re
from gpt.prompt_all import *
from gpt.utils import (
    remove_comments_from_sapic,
    extract_line_commment_from_spec
)
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
    functions.add("placeholder/0")
    for node in func_nodes:
        node: Tree
        func_name = node.children[0].value
        arity = len(node.children[1].children)
        #Todo: should be a more elegant way to get arity
        declarations = f"{func_name}/{arity}"
        built_ins = ["pk/1", "sign/2", "verify/3", "aenc/2", "adec/2", "senc/2", "sdec/2"]   
        if func_name != "exp":
            if declarations not in built_ins:
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
