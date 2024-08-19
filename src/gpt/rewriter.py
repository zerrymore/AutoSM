from lark import Lark, Token, Tree, Transformer
BNF = """\
	?start: stms
	stms            : bind_stms
									| END
	bind_stms       : "let" binding "in" stms
	END             : "0"
	binding         : expression "=" expression
	expression      : term
                    | match
                    | xor
                    | exp
                    | func
                    | concat
	match           : "=" NAME
	xor             : expression "XOR" expression
	exp             : expression"^"expression
	concat          : "<" arguments ">"
	func            : NAME "(" arguments ")"
	arguments       : expression ("," expression)* |
	term            : nonce | NUMBER | STRING
	nonce           : "~" NAME | NAME
	STRING          : /'[^']*'/
	NAME            : /[a-zA-Z_][a-zA-Z0-9_-]*/
	NUMBER          : /\d+/
	%import common.WS
	%ignore WS
"""


Parser = Lark(BNF, parser='lalr', start='start', propagate_positions=True)


class TreeToString(Transformer):
    def concat(self, items):
      return f"<{', '.join(items)}>"
    
    def arguments(self, items):
      return ", ".join(items)

    def param(self, items):
      assert isinstance(items[0], Token)
      # print(f'this is param: {items[0]}')
      return f'{items[0].value}'
        
    def exp(self, items):
      return f"{items[0]} - {items[1]}"
    
    def xor(self, items):
      return f"{items[0]} * {items[1]}"
    
    def func(self, items):
      return f"{items[0]}({items[1]})"
    
    def match(self, items):
      return f'={items[0]}'

    def exp(self, items):
        return f"{items[0]}^{items[1]}"   

    def nonce(self, items):
        return f'{items[0]}'
        
    def term(self, items):
      # [Token('NAME', 'gamma_0')]
      # [Token('NAME', 'gamma_0')]
      # print(items)
      return f'{items[0]}'


    def __default__(self, data, children, meta):
      return children[0] if children else data
      
      
def _vars(tree_node):
  if isinstance(tree_node, Token):
    return {tree_node.value}
  elif isinstance(tree_node, Tree):
    vars_set = set()  
    start_index = 0  
    if tree_node.data in ['func','role']:  # reduce the funcname
            start_index = 1
    for child in tree_node.children[start_index:]:
            vars_set.update(_vars(child)) 
    return vars_set
  else:
    return set()


def binding_map(node:Tree, vdg, pointer):
    if node.data == 'binding':  
        left_vars = _vars(node.children[0])
        right_values = _vars(node.children[1])
        if len(left_vars) == 1:
            vdg[list(left_vars)[0]] = right_values
            pointer[list(left_vars)[0]] = node.children[1]
    else:
        for child in node.children:
            if isinstance(child, Tree): 
                binding_map(child, vdg, pointer)



def deconstruct_binding_map(node:Tree, vdg, pointers):
    if node.data == 'binding':  
        left_values = _vars(node.children[0])
        right_vars = _vars(node.children[1])
        if len(right_vars) == 1:
            vdg[list(right_vars)[0]] = left_values
            pointers[list(right_vars)[0]] = node.children[0]
    else:
        for child in node.children:
            if isinstance(child, Tree): 
                deconstruct_binding_map(child, vdg, pointers)

def tree2str(node) -> str:
    reconstructed_string = ""
    if isinstance(node, Token):
        reconstructed_string = node.value
        # print(reconstructed_string, type(reconstructed_string))
    elif isinstance(node, Tree):
        transformer = TreeToString()
        reconstructed_string = transformer.transform(node)
    return reconstructed_string


# ghost variables
gamma = []

def rewrite(message:str, vdg:dict, pointers:dict, j:int):
    init_re = []
    stack = [message]

    # deconstructed_vars used to include all variavles that 
    # have been unbinded.
    deconstructed_vars = set()
    deconstructed_vars |= {message}
    while stack:
        v = stack.pop()
        deconstructed_vars |= {v}
        if v in pointers.keys():
            # Suppose that v |-> p, i.e., let v = p in
            p:Tree = pointers[v]  

            p_type = p.children[0].data


            if p_type in ["func", "concat", "exp"]:
                p_tree = p.children[0]
                if p_type == "func":
                    func_name = p_tree.children[0].value
                    args_tree = p_tree.children[1]
                elif p_type == "concat":
                    args_tree = p_tree.children[0]
                elif p_type == "exp":
                    func_name = p_tree.data
                    assert func_name == "exp"
                    base = p_tree.children[0]
                    exponent = p_tree.children[1]
                    
                
                for i, arg in enumerate(args_tree.children):
                    assert isinstance(arg, Tree)
                    if arg.children[0].data == "term":
                        # print(arg)
                        pass
                    elif arg.children[0].data in ["func", "concat", "exp"]:
                        inter_vars = f'gamma_{j}'

                        if arg.children[0].data == "func":
                            # wrap it into an expression
                            pointers[inter_vars] = Tree('expression', [arg.children[0]])
                        else:
                            pointers[inter_vars] = arg.children[0]

                        
                        vdg[inter_vars] = _vars(arg.children[0])

                        vdg[v] |= {inter_vars}              # update variable dependency
                        vdg[v] -= _vars(arg.children[0])    # remove variables dependencies
                        j += 1
                        
                        arg.children[0] = Tree('expression', 
                                               [Tree('term', [Tree('nonce', [Token('NAME', inter_vars)])])])
                        
                        pass
                
            elif p_type == "concat":
                pass
            
            reconstructed_string = tree2str(p)
            # print(f'let {reconstructed_string} = {v} in')
            init_re += [f'let {reconstructed_string} = {v} in']
            # print(init_re)

            for vi in list(vdg[v]):
                if vi in pointers.keys():
                    if vi not in stack:
                        if vi not in list(deconstructed_vars):
                            stack += [vi]
                            deconstructed_vars |= {vi}

        else:
            pass
    return init_re, j




def var_mapping(node:Tree, mapping:dict):
    if node.data == 'binding':  
        right_vars = node.children[1]
        left_values = node.children[0]
        mapping[right_vars] = left_values
    else:
        for child in node.children:
            if isinstance(child, Tree): 
                var_mapping(child, mapping)


def inverse_key(key:str) -> str:
    if key.startswith('sk'):
        return f'pk{key[-1]}'
    elif key.startswith('pk'):
        return f'sk{key[-1]}'


def rewrite_local(mapping, K):
    res = []
    unseen = []
    for v in mapping.keys():
        p = mapping[v]
        assert p.data == "expression"
        if v not in unseen:
            p_tree = p.children[0]
            if p_tree.data == "func":
                func_name = p_tree.children[0].value
                args = p_tree.children[1]
                if func_name == "senc":
                    plain = tree2str(args.children[0])
                    key = tree2str(args.children[1])
                    if key in K:
                        stmt = f'let {plain} = sdec({tree2str(v)}, {key}) in'
                        res += [stmt]
                    else:
                        vds = _vars(p)
                        for vd in vds:
                            vtree = Tree('expression', [Tree('term', [Tree('nonce', [Token('NAME', f'{vd}')])])])
                            unseen += [vtree]
                            # print(unseen)
    
    
                        
                elif func_name == "aenc":
                    plain = tree2str(args.children[0])
                    key = tree2str(args.children[1])
                    if inverse_key(key) in K:
                        if plain in K:
                            stmt = f'let ={plain} = adec({tree2str(v)}, {inverse_key(key)}) in'
                        else:
                            stmt = f'let {plain} = adec({tree2str(v)}, {inverse_key(key)}) in'
                        res += [stmt]
                        # print(stmt)
                    else:
                        pass
                else:
                    res += [f'let {tree2str(p)} = {tree2str(v)} in']
                     
            elif p_tree.data == "concat":
                concat_args = p_tree.children[0]
                for i, c_arg in enumerate(concat_args.children):
                    c = tree2str(c_arg)
                    if not c.startswith("'"):
                        if c in K:
                            concat_args.children[i] = Tree('expression', [Tree('match', [Token('NAME', f'{c}')])])
                    K |= { tree2str(c_arg) }
                K |= { tree2str(v) }
                # print(K)
                res += [f'let {tree2str(p)} = {tree2str(v)} in']
            elif p_tree.data == "term":
                res += [f'let {tree2str(p)} = {tree2str(v)} in']
            elif p_tree.data == "exp":
                res += [f'let {tree2str(p)} = {tree2str(v)} in']
    return res


ROLE_BNF = """\
   ?start: role_spec
    role_spec       : role+
    role            : "let" NAME "(" parameters ")" "=" stms 
    parameters      : param ("," param)* 
    param           : NAME | "~" NAME |
    role_insts      : NAME "(" arguments ")"
    stms            : rep
                    | "(" stms ")"
                    | bind_stms
                    | cond_stms
                    | stmt ";" stms
                    | stmt
                    | END
    bind_stms       : "let" binding "in" stms
    cond_stms       : "if" guard "then" stms
    rep             : "!" stms
    END             : "0"
    stmt            : event | out | in | new | role_insts
    event           : "event" NAME "(" arguments ")"
    out             : "out" "(" arguments ")"
    in              : "in" "(" arguments ")" -> _in
    new             : "new" nonce
    guard           : expression "=" expression
    binding         : expression "=" expression
    expression      : term
                    | match
                    | xor
                    | exp
                    | func
                    | concat
    match           : "=" NAME
    xor             : expression "XOR" expression
    exp             : expression"^"expression
    concat          : "<" arguments ">"
    func            : NAME "(" arguments ")"
    arguments       : expression ("," expression)* |
    term            : nonce | NUMBER | STRING
    nonce           : "~" NAME | NAME 
    STRING          : /'[^']*'/
    NAME            : /[a-zA-Z_][a-zA-Z0-9_-]*/
    NUMBER          : /\d+/
    
    %import common.WS
    %ignore WS
"""

def get_role_root(node:Tree, role_roots):
    if node.data == 'role':  
        role_roots += [node]
    else:
        for child in node.children:
            if isinstance(child, Tree): 
                get_role_root(child, role_roots)


def rewrite_local_process(K:set, node:Tree, local_p:list, vdg, pointers, unconstructed, j):
    if node.data == 'new':  
        nonce = tree2str(node.children[0])
        K |= { nonce }

        stmt = f'new {nonce};'
        local_p += [stmt]
        
    elif node.data == 'binding':
        right_values = _vars(node.children[1])
        vars_without_str = set([v for v in right_values if not (v.startswith("'") and v.endswith("'"))])
        # if vars_without_str 
        if not vars_without_str.issubset(K):
            # print(f"Can not construct {tree2str(node.children[0])} from {vars_without_str}")
            stmt = f'// let {tree2str(node.children[0])} = {tree2str(node.children[1])} in \n// Can not construct {tree2str(node.children[0])} from {vars_without_str}'
            unconstructed += [tree2str(node.children[0])]
        else:
            stmt = f'let {tree2str(node.children[0])} = {tree2str(node.children[1])} in '
            K |= { tree2str(node.children[0]) }
        local_p += [stmt]
        
    elif node.data == 'out':
        sending_message = tree2str(node)
        if sending_message not in unconstructed:
            stmt = f'out({sending_message});'
        else:
            stmt = f'// out({sending_message})'
        local_p += [stmt]
        
    elif node.data == '_in':
        incoming_message = tree2str(node)

        local_p += [f'in({incoming_message});']
        # print(incoming_message)

        K |= {incoming_message}

        # j = 0
        rewrite_init, j = rewrite(incoming_message, vdg, pointers, j)
        rewrite_init += ["0"]
        init_str = "\n".join(rewrite_init)
        last_str = init_str
        
        while True:
            init_root = Parser.parse(last_str)
            in_vdg, in_pointers = {}, {}

            deconstruct_binding_map(init_root, in_vdg, in_pointers)

            inter_res, j = rewrite(incoming_message, in_vdg, in_pointers, j)
            # print(j)
            inter_res += ["0"]
            inter_str = "\n".join(inter_res)
            # print(inter_str)
            if inter_str == last_str:
                break
            last_str = inter_str
            # print(last_str)
        
        root = Parser.parse(last_str)
        mapping = {}
        var_mapping(root, mapping)
        
        res = rewrite_local(mapping, K) # Todo: K should be updated when the process is rewriten
        local_p += ["\n".join(res)]

    

    else:
        for child in node.children:
            if isinstance(child, Tree): 
                j = rewrite_local_process(K, child, local_p, vdg, pointers, unconstructed, j)

    return j



import copy

def Rewriter(code:str) -> str:
    role_Parser = Lark(ROLE_BNF, parser='lalr', start='start', propagate_positions=True)
    root = role_Parser.parse(code)

    vdg = {}
    pointers = {}

    binding_map(root, vdg, pointers)

    role_roots = []
    get_role_root(root, role_roots)

    res = ""
    for role in role_roots:

        role_vdg = copy.deepcopy(vdg)
        role_pointers = copy.deepcopy(pointers)
        
        assert isinstance(role, Tree)    
        role_name = role.children[0].value

        K = set()
        parameters:Tree = role.children[1]
        for k in parameters.children:            
            # Initialize the knowledge set K
            K |= { tree2str(k) }
        
        stms:Tree = role.children[2]
        local_p = []
        local_p += [f'let {role_name}({", ".join(list(K))})=']
        
        rewrite_local_process(K, stms, local_p, role_vdg, role_pointers, [], 0)

        res += "\n".join(local_p + ['0']) + "\n\n"
        
    return res