from lark import Lark, Token, Tree, Transformer
import copy

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

expr_bnf = """\
?start: expression
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

Parser = Lark(BNF, parser="lalr", start="start", propagate_positions=True)

expr_Parser = Lark(expr_bnf, parser="lalr", start="start", propagate_positions=True)


class TreeToString(Transformer):
    def concat(self, items):
        return f"<{', '.join(items)}>"

    def arguments(self, items):
        return ", ".join(items)

    def param(self, items):
        try:
            assert isinstance(items[0], Token)
            # print(f'this is param: {items[0]}')
            return f"{items[0].value}"
        except:
            return ""

    def exp(self, items):
        return f"{items[0]} - {items[1]}"

    def xor(self, items):
        return f"{items[0]} * {items[1]}"

    def func(self, items):
        return f"{items[0]}({items[1]})"

    def match(self, items):
        return f"={items[0]}"

    def exp(self, items):
        return f"{items[0]}^{items[1]}"

    def nonce(self, items):
        return f"{items[0]}"

    def term(self, items):
        # [Token('NAME', 'gamma_0')]
        # [Token('NAME', 'gamma_0')]
        # print(items)
        return f"{items[0]}"

    def __default__(self, data, children, meta):
        return children[0] if children else data


def _vars(tree_node):
    if isinstance(tree_node, Token):
        return {tree_node.value}
    elif isinstance(tree_node, Tree):
        vars_set = set()
        start_index = 0
        if tree_node.data in ["func", "role"]:  # reduce the funcname
            start_index = 1
        for child in tree_node.children[start_index:]:
            vars_set.update(_vars(child))
        return vars_set
    else:
        return set()


def _vars_from_str(expr: str) -> set:
    root = expr_Parser.parse(expr)
    return _vars(root)


def binding_map(node: Tree, vdg, pointer):
    if node.data == "binding":
        left_vars = _vars(node.children[0])
        right_values = _vars(node.children[1])
        if len(left_vars) == 1:
            vdg[list(left_vars)[0]] = right_values
            pointer[list(left_vars)[0]] = node.children[1]
    elif node.data == "out":
        out_args = node.children[0]
        assert len(out_args.children) == 1
        msg = out_args.children[0]
        assert msg.data == "expression"
        if msg.children[0].data != "term":
            pass
    else:
        for child in node.children:
            if isinstance(child, Tree):
                binding_map(child, vdg, pointer)


def deconstruct_binding_map(node: Tree, vdg, pointers):
    if node.data == "binding":
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
    elif isinstance(node, Tree):
        transformer = TreeToString()
        reconstructed_string = transformer.transform(node)
    return reconstructed_string


# ghost variables
gamma = []


def rewrite(external_stack: list, vdg: dict, pointers: dict, j: int):
    init_re = []
    ##== `deconstructed_vars` used to include all variavles that have been unbinded ==##
    deconstructed_vars = set()

    stack = copy.deepcopy(external_stack)
    while stack:
        v = stack.pop()
        deconstructed_vars |= {v}
        if v in pointers.keys():
            ##== Suppose that v |-> p, i.e., let v = p in ==##
            p: Tree = pointers[v]

            p_type = p.children[0].data

            if p_type in ["func", "concat"]:
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
                    args_tree = p_tree

                for _, arg in enumerate(args_tree.children):
                    assert isinstance(arg, Tree)
                    if arg.children[0].data == "term":
                        pass

                    elif arg.children[0].data in ["func", "concat", "exp"]:
                        inter_vars = f"gamma_{j}"

                        if arg.children[0].data == "func":
                            # wrap it into an expression
                            pointers[inter_vars] = Tree("expression", [arg.children[0]])
                        else:
                            pointers[inter_vars] = arg.children[0]

                        ##== Update vdg and pointers ==##
                        ##== v|->t  ~~> v|->inter_v & inter_v|->t, ==##
                        ##== where t is nested function term, e.g., f(g(t)) ==##
                        vdg[inter_vars] = _vars(arg.children[0])
                        vdg[v] |= {inter_vars}  # update variable dependency
                        vdg[v] -= _vars(
                            arg.children[0]
                        )  # remove variables dependencies
                        j += 1

                        arg.children[0] = Tree(
                            "expression",
                            [
                                Tree(
                                    "term", [Tree("nonce", [Token("NAME", inter_vars)])]
                                )
                            ],
                        )

                        pass
            # elif p_type == "exp":
            #     inter_vars = f'gamma_{j}'
            #     pointers[inter_vars] = Tree('expression', [p_tree])
            #     j += 1

            elif p_type == "concat":
                pass

            reconstructed_string = tree2str(p)
            init_re += [f"let {reconstructed_string} = {v} in"]

            for vi in list(vdg[v]):
                if vi in pointers.keys():
                    if vi not in stack:
                        if vi not in list(deconstructed_vars):
                            stack += [vi]
                            deconstructed_vars |= {vi}

        else:
            pass
    return init_re, j


def var_mapping(node: Tree, mapping: dict):
    if node.data == "binding":
        right_vars = node.children[1]
        left_values = node.children[0]
        mapping[right_vars] = left_values
    else:
        for child in node.children:
            if isinstance(child, Tree):
                var_mapping(child, mapping)


def inverse_key(key: str) -> str:
    if key.startswith("sk"):
        return f"pk{key[-1]}"
    elif key.startswith("pk"):
        return f"sk{key[-1]}"
    else:
        return key


"""
def rewrite_receiv_event(mapping, pointers, K):
    res = []
    undeconstructed_vars = copy.deepcopy(mapping)
    for v in mapping.keys():
        p = mapping[v]
        assert p.data == "expression"
        p_tree = p.children[0]
        if p_tree.data == "func":
            func_name = p_tree.children[0].value
            args = p_tree.children[1]
            assert isinstance(args.children, list)
            
            if func_name == "senc":
                plain = tree2str(args.children[0])
                key = tree2str(args.children[1])
                if key in K:
                    if plain in K:
                        stmt = f'let ={plain} = sdec({tree2str(v)}, {key}) in'
                        
                        # remove key that has been deconstructed
                    else:
                        stmt = f'let {plain} = sdec({tree2str(v)}, {key}) in'
                    undeconstructed_vars.pop(v)
                    K |= { plain }
                else:
                    vds = _vars(p)
                    # for vd in vds:
                    #     vtree = Tree('expression', [Tree('term', [Tree('nonce', [Token('NAME', f'{vd}')])])])
                    #     unseen += [vtree]
                        
                    stmt = f'// let {plain} = sdec({tree2str(v)}, {key}) in'
                res += [stmt]

        
            elif func_name == "aenc":
                plain = tree2str(args.children[0])
                key = tree2str(args.children[1])
                if inverse_key(key) in K:
                    if plain in K:
                        stmt = f'let ={plain} = adec({tree2str(v)}, {inverse_key(key)}) in'
                    else:
                        stmt = f'let {plain} = adec({tree2str(v)}, {inverse_key(key)}) in'
                    K |= { plain }
                    undeconstructed_vars.pop(v)
                    
                else:
                    stmt = f'// let {plain} = adec({tree2str(v)}, {inverse_key(key)}) in'
                    pass
                res += [stmt]
            elif func_name == "sign":
                msg = tree2str(args.children[0])
                sig_key = tree2str(args.children[1])
                if msg in K and inverse_key(sig_key) in K:
                    # if inverse_key(sig_key) in K:
                    stmt = f'if verify({tree2str(v)}, {msg}, {inverse_key(sig_key)}) = true then'
                    undeconstructed_vars.pop(v)
                else:
                    stmt = f'// let {tree2str(p)} = {tree2str(v)} in'
                res += [stmt]
                
            # ===  one direction function === #   
            else:
                func_args:Tree = p_tree.children[1]
                func_vars:set = _vars(p_tree)
                flag = True
                for arg_v in func_vars:
                    '''
                    Here is an ad-hoc implementation
                    We need determine wether the args of func can be constructed
                    from knowledge set K
                    '''
                    gt = evaluate(pointers, arg_v)
                    vars = set([t for t in _vars_from_str(gt) if not t.startswith("'")])
                    if not vars.issubset(K):
                        flag = False
                    if arg_v in K:
                        flag = True 
                    
                    
                if not flag:
                    # print(f'this is K:{K}')
                    stmt = f'// let {tree2str(p)} = {tree2str(v)} in'
                else:
                    for i, f_arg in enumerate(func_args.children):
                        c = tree2str(f_arg)
                        if not c.startswith("'"):
                            if c in K:
                                func_args.children[i] = Tree('expression', [Tree('match', [Token('NAME', f'{c}')])])
                        K |= { tree2str(f_arg) }
                        
                    undeconstructed_vars.pop(v)
                    
                    K |= { tree2str(v) }
                    stmt = f'let {tree2str(p)} = {tree2str(v)} in'
                res += [stmt]
                    
        elif p_tree.data == "concat":
            if tree2str(v) in K:
                concat_args = p_tree.children[0]
                for i, c_arg in enumerate(concat_args.children):
                    c = tree2str(c_arg)
                    if not c.startswith("'"):
                        if c in K:
                            concat_args.children[i] = Tree('expression', [Tree('match', [Token('NAME', f'{c}')])])
                    K |= { tree2str(c_arg) }
                K |= { tree2str(v) }
                # print(K)
                undeconstructed_vars.pop(v)
                
                res += [f'let {tree2str(p)} = {tree2str(v)} in']
            else:
                res += [f'// let {tree2str(p)} = {tree2str(v)} in']
    
        elif p_tree.data == "term":
            res += [f'let {tree2str(p)} = {tree2str(v)} in']
        elif p_tree.data == "exp":
                res += [f'// let {tree2str(p)} = {tree2str(v)} in']
        # else:
        #     res += [f'// let {tree2str(p)} = {tree2str(v)} in']
        
    return res, undeconstructed_vars
"""


def rewrite_receiv_event(mapping, pointers, K, hasDeconstructed: list):
    res = []
    undeconstructed_vars = copy.deepcopy(mapping)

    stack = copy.deepcopy(mapping)

    from collections import deque

    queue = {}  # 延后处理的队列

    """
    #ISSURE 2 
     in(message3);
     let <package1, package2, =Nb> = message3 in
    `let gamma_0 = sdec(package2, Kbs) in'
     let <=idA, Kab, =Tb> = gamma_0 in
     // let gamma_1 = sdec(package1, Kas) in
     // let <idB, Na, Kab, Tb> = gamma_1 in
     // let gamma_1 = sdec(package1, Kas) in
     // let <idB, Na, Kab, Tb> = gamma_1 in
     in(message4);
     let <=package2, nbEncrypted> = message4 in
     `let =gamma_0 = sdec(package2, Kbs) in'
     let <=idA, =Kab, =Tb> = gamma_0 in
     let =Nb = sdec(nbEncrypted, Kab) in
    
    ##== Some variables are deconstructed twice, e.g., `package2` ==##
    
    #Solution
    ##== Maintain a stack which contains the variales that have been deconstructed ==##
    """

    """
    #ISSURE 1
    # let <package1, Na_encrypted, Nb> = messageBtoA in
    // let Na = sdec(Na_encrypted, Kab) in
    let gamma_0 = sdec(package1, Kas) in
    let <=idA, =idB, =Na, Kab> = gamma_0 in
    let =Na = sdec(Na_encrypted, Kab) in
    let Nb_encrypted = senc(Nb, Kab) in,
        
    ##== the variable `Na_encrypted' can be  deconstructed after `Kab' obtained ==##

    #Solution:
    ##== Design a greedy algorithm to maximize the number of deconstruction operations performed. ==##
    """
    while True:
        processed_count = 0

        for v in stack.keys():
            if tree2str(v) not in hasDeconstructed:
                p = stack[v]
                assert p.data == "expression"
                p_tree = p.children[0]
                if p_tree.data == "func":
                    func_name = p_tree.children[0].value
                    args = p_tree.children[1]
                    assert isinstance(args.children, list)

                    if func_name == "senc":
                        plain = tree2str(args.children[0])
                        key = tree2str(args.children[1])
                        if key in K:
                            if plain in K:
                                stmt = f"let ={plain} = sdec({tree2str(v)}, {key}) in"
                                ###== Once the variable is matched, it need no further deconstruction ==##
                                hasDeconstructed += [f"{plain}"]

                            else:
                                stmt = f"let {plain} = sdec({tree2str(v)}, {key}) in"

                            ##== remove key that has been deconstructed ==##
                            undeconstructed_vars.pop(v)
                            processed_count += 1
                            hasDeconstructed += [tree2str(v)]
                            K |= {plain}
                        else:
                            ##== can not be deconstructed ==##
                            stmt = f"// let {plain} = sdec({tree2str(v)}, {key}) in"
                            queue = {**queue, v: p}
                        res += [stmt]

                    elif func_name == "aenc":
                        plain = tree2str(args.children[0])
                        key = tree2str(args.children[1])
                        if inverse_key(key) in K:
                            if plain in K:
                                stmt = f"let ={plain} = adec({tree2str(v)}, {inverse_key(key)}) in"
                                hasDeconstructed += [f"{plain}"]
                            else:
                                stmt = f"let {plain} = adec({tree2str(v)}, {inverse_key(key)}) in"

                            K |= {plain}
                            hasDeconstructed += [tree2str(v)]
                            processed_count += 1
                            undeconstructed_vars.pop(v)

                        else:
                            stmt = f"// let {plain} = adec({tree2str(v)}, {inverse_key(key)}) in"
                            queue = {**queue, v: p}
                        res += [stmt]
                    elif func_name == "sign":
                        msg = tree2str(args.children[0])
                        sig_key = tree2str(args.children[1])
                        if msg in K and inverse_key(sig_key) in K:
                            stmt = f"if verify({tree2str(v)}, {msg}, {inverse_key(sig_key)}) = true then"

                            processed_count += 1
                            undeconstructed_vars.pop(v)
                            hasDeconstructed += [tree2str(v)]
                        else:
                            stmt = f"// let {tree2str(p)} = {tree2str(v)} in"
                            queue = {**queue, v: p}
                        res += [stmt]

                    ###==  one direction function ==##
                    else:
                        func_args: Tree = p_tree.children[1]
                        func_vars: set = _vars(p_tree)
                        flag = True
                        for arg_v in func_vars:
                            """
                            Here is an ad-hoc implementation
                            We need determine whether the args of func can be constructed
                            from knowledge set K
                            """
                            gt = evaluate(pointers, arg_v)
                            vars = set(
                                [t for t in _vars_from_str(gt) if not t.startswith("'")]
                            )
                            if not vars.issubset(K):
                                flag = False
                            if arg_v in K:
                                flag = True

                        if not flag:
                            stmt = f"// let {tree2str(p)} = {tree2str(v)} in"
                            queue = {**queue, v: p}
                        else:
                            for i, f_arg in enumerate(func_args.children):
                                c = tree2str(f_arg)
                                if not c.startswith("'"):
                                    if c in K:
                                        func_args.children[i] = Tree(
                                            "expression",
                                            [Tree("match", [Token("NAME", f"{c}")])],
                                        )
                                        hasDeconstructed += [f"{c}"]
                                K |= {tree2str(f_arg)}

                            undeconstructed_vars.pop(v)
                            hasDeconstructed += [tree2str(v)]
                            K |= {tree2str(v)}

                            processed_count += 1

                            stmt = f"let {tree2str(p)} = {tree2str(v)} in"
                        res += [stmt]

                elif p_tree.data == "concat":
                    if tree2str(v) in K:
                        concat_args = p_tree.children[0]
                        for i, c_arg in enumerate(concat_args.children):
                            c = tree2str(c_arg)
                            if not c.startswith("'"):
                                if c in K:
                                    concat_args.children[i] = Tree(
                                        "expression",
                                        [Tree("match", [Token("NAME", f"{c}")])],
                                    )
                                    
                                    hasDeconstructed += [f"{c}"]
                            K |= {tree2str(c_arg)}
                        K |= {tree2str(v)}
                        undeconstructed_vars.pop(v)

                        hasDeconstructed += [tree2str(v)]
                        processed_count += 1

                        res += [f"let {tree2str(p)} = {tree2str(v)} in"]
                    else:
                        res += [f"// let {tree2str(p)} = {tree2str(v)} in"]
                        queue = {**queue, v: p}

                elif p_tree.data == "term":
                    hasDeconstructed += [tree2str(v)]
                    processed_count += 1
                    res += [f"let {tree2str(p)} = {tree2str(v)} in"]
                    hasDeconstructed += [tree2str(v)]
                    K |= {tree2str(p)}
                elif p_tree.data == "exp":
                    res += [f"// let {tree2str(p)} = {tree2str(v)} in"]
                    queue = {**queue, v: p}

        if processed_count == 0:
            break

        stack = {**queue}
        queue.clear()

    return res, queue


def get_role_root(node: Tree, role_roots):
    if node.data == "role":
        role_roots += [node]
    else:
        for child in node.children:
            if isinstance(child, Tree):
                get_role_root(child, role_roots)


def evaluate(mem: dict, var: str) -> str:
    """ """
    if var not in mem.keys():
        return var
    else:
        value: str = tree2str(mem[var])
        for v in _vars(mem[var]):
            value = value.replace(v, evaluate(mem, v))
        return value


def rewrite_local_process(
    K: set,
    node: Tree,
    local_p: list,
    vdg,
    pointers,
    unconstructed,
    j,
    undeconstructed_mapping: dict,
    hasDeconstructed,
):
    if node.data == "new":
        nonce = tree2str(node.children[0])
        K |= {nonce}

        stmt = f"new {nonce};"
        local_p += [stmt]

    elif node.data == "binding":
        stmt = ""
        right_values = _vars(node.children[1])
        vars_without_str = set(
            [v for v in right_values if not (v.startswith("'") and v.endswith("'"))]
        )

        if not vars_without_str.issubset(K):
            stmt = f"// let {tree2str(node.children[0])} = {tree2str(node.children[1])} in \n// Can not construct {tree2str(node.children[0])} from {K}"
            unconstructed[tree2str(node.children[0])] = node.children[1]
        else:
            stmt = (
                f"let {tree2str(node.children[0])} = {tree2str(node.children[1])} in "
            )
            K |= _vars(tree2str(node.children[0]))
            K |= {tree2str(node.children[0])}

            ##== TODO: when new variable is constructed, K is enlarged,     ==##
            ##== some variable may be deconstructed at this timepoint,      ==##
            ##== so the greedy algorithm for deconstruction may be applied  ==##
            # res, undeconstructed_vars = rewrite_receiv_event(undeconstructed_mapping, pointers, K)
            # undeconstructed_mapping |=  undeconstructed_vars

            # for dcon_stmt in res:
            # local_p.append(dcon_stmt)

        local_p += [stmt]

    elif node.data == "out":
        stmt = ""
        sending_message = tree2str(node)
        if sending_message not in unconstructed.keys():
            stmt = f"out({sending_message});"
        else:
            stmt = f"// out({sending_message});"

            """
            #ISSURE:
            # This is an ad-hoc implementation, only handles a special case that:
                  ∃ v ∈ K(knowledge set) ==>  Mem[v]|->p && p == msg
            
            Find a term t which can be constructed from K, satisfying that: t|->gt && msg|->gt,
            where gt is a grounding term. The evaluation enviroments for t and msg are different.
            
            #TODO: implement an search algorithm to develop it.
            """
            for k in K:
                # print(k, evaluate(pointers, k))
                intended_out_msg = evaluate(
                    {**pointers, **unconstructed}, sending_message
                )
                if evaluate(pointers, k) == intended_out_msg:
                    stmt = f"out({k});"

        local_p += [stmt]

    elif node.data == "_in":

        in_args = node.children[0]
        assert len(in_args.children) == 1
        msg = in_args.children[0]
        assert msg.data == "expression"
        if msg.children[0].data != "term":
            # stack = list(_vars(msg))
            # K |= _vars(msg)

            inter_vars = f"gamma_{j}"
            if msg.children[0].data == "func":
                # wrap it into an expression
                pointers[inter_vars] = Tree("expression", [msg.children[0]])
            else:
                pointers[inter_vars] = msg.children[0]

            vdg[inter_vars] = _vars(msg.children[0])

            j += 1
            msg.children[0] = Tree(
                "expression",
                [Tree("term", [Tree("nonce", [Token("NAME", inter_vars)])])],
            )
            K |= {inter_vars}
            stack = [inter_vars]

        else:
            stack = [tree2str(msg)]
            K |= {tree2str(msg)}

        incoming_message = tree2str(node)
        local_p += [f"in({incoming_message});"]
        # j = 0
        rewrite_init, j = rewrite(stack, vdg, pointers, j)

        rewrite_init += ["0"]
        init_str = "\n".join(rewrite_init)
        last_str = init_str

        ##== Recursively rewrite the term ==##
        while True:
            init_root = Parser.parse(last_str)
            in_vdg, in_pointers = {}, {}

            deconstruct_binding_map(init_root, in_vdg, in_pointers)

            inter_res, k = rewrite(stack, in_vdg, in_pointers, j)

            inter_res += ["0"]
            inter_str = "\n".join(inter_res)

            if inter_str == last_str:
                break
            last_str = inter_str

        vdg = {**vdg, **in_vdg}
        pointers = {**pointers, **in_pointers}

        root = Parser.parse(last_str)
        mapping = {}
        var_mapping(root, mapping)

        # Todo: K should be updated when the process is rewriten
        res, undeconstructed_vars = rewrite_receiv_event(
            mapping, pointers, K, hasDeconstructed
        )
        undeconstructed_mapping |= undeconstructed_vars

        for dcon_stmt in res:
            local_p.append(dcon_stmt)

    else:
        for child in node.children:
            if isinstance(child, Tree):
                j, pointers = rewrite_local_process(
                    K,
                    child,
                    local_p,
                    vdg,
                    pointers,
                    unconstructed,
                    j,
                    undeconstructed_mapping,
                    hasDeconstructed,
                )

    # Take pointers as a return value to ensure it can update
    return j, pointers


def Rewriter(code: str) -> str:
    role_Parser = Lark(ROLE_BNF, parser="lalr", start="start", propagate_positions=True)
    root = role_Parser.parse(code)

    vdg = {}
    pointers = {}

    """
    #Todo: determine whether the program is well-formed globally
    Definition (Global well-formed):
        There is no free variables with expressions, 
        i.e., every variable is either a fresh nonoce, a binding variable or 
    """
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
        parameters: Tree = role.children[1]
        for k in parameters.children:

            ##== Initialize the knowledge set K  ==##
            # K |= {}
            K |= {tree2str(k)}

        stms: Tree = role.children[2]
        local_p = []
        local_p += [f'let {role_name}({", ".join(list(K))})=']

        undeconstructed_mapping = {}
        hasDeconstructed = []
        rewrite_local_process(
            K,
            stms,
            local_p,
            role_vdg,
            role_pointers,
            {},
            0,
            undeconstructed_mapping,
            hasDeconstructed,
        )

        print("This is hasdeconstructed variable:")
        print(hasDeconstructed)
        print()

        # print(undeconstructed_mapping)
        # r, _ = rewrite_receiv_event(undeconstructed_mapping, pointers, K)
        # Todo: K should be updated when the process is rewriten

        current_local_process = "\n".join(local_p + ["0"])
        res += current_local_process + "\n"

    return res


if __name__ == "__main__":
    code = """
let A(kas, A, B) =
  new I;
  new Na;
  let message1 = <I,A,B,senc(<Na,I,A,B>, kas)> in
  out(<message1, I>);
  in(message4); 0

let B(kbs, A, B) =
  in(<message1, I>);
  new Nb;
  let message2 = <message1,senc(<Nb,I,A,B>, kbs)> in
  out(message2);
  in(message3);
  let message4 = senc(<Na,kab>, kas) in
  out(message4); 0

let S(kbs, kas, A, B) =
  in(message2);
  new kab;
  let message3 = <I,senc(<Na,kab>, kas),senc(<Nb,kab>, kbs)> in
  out(message3); 0
  """
    code = """
let Alice(idB, Kas, idA) = 
  new Na;
  let message1 = <idA,idB,Na> in
  out(message1);
  in(messageBtoA);
  let Nb_encrypted = senc(Nb, Kab) in
  out(Nb_encrypted); 0

let Server(idB, Kas, Kbs, idA) = 
  in(message1);
  new Kab;
  let package1 = senc(<idA,idB,Na,Kab>, Kas) in
  let package2 = senc(<idA,idB,Na,Kab>, Kbs) in
  let compoundMessage = <package1,package2> in
  out(compoundMessage); 0

let Bob(idA, Kbs, idB) = 
  in(compoundMessage);
  new Nb;
  let Na_encrypted = senc(Na, Kab) in
  let messageBtoA = <package1,Na_encrypted,Nb> in
  out(messageBtoA);
  in(Nb_encrypted); 0



  """

    print(Rewriter(code))
