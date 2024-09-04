from lark import Lark, Tree, Token, Transformer

ROLE_BNF = """\
   ?start: role_spec
    role_spec       : role+
    role            : "let" NAME "(" parameters ")" "=" stms 
    parameters      : param ("," param)* 
    param           : NAME | FRESH |
    role_insts      : NAME "(" arguments ")"
    stms            : rep
                    | "(" stms ")"
                    | bind_stms
                    | cond_stms
                    | seq_stms
                    | par
                    | stmt
                    | END
    seq_stms        : stmt ";" stms
    bind_stms       : "let" binding "in" stms
    cond_stms       : "if" guard "then" stms
    par             : stms "||" stms
                    | stms "|" stms
    rep             : "!" stms
    END             : "0"
    stmt            : event | out | in | new | role_insts
    event           : "event" NAME "(" arguments ")"
    out             : "out" "(" arguments ")"
    in              : "in" "(" arguments ")" -> _in
    new             : "new" nonce
    binding         : expression "=" expression
    guard           : expression "=" expression
    expression      : term
                    | match
                    | xor
                    | exp
                    | func
                    | concat
    match           : "=" NAME
    xor             : expression "XOR" expression
    exp             : expression "^" expression
    concat          : "<" arguments ">"
    func            : NAME "(" arguments ")"
    arguments       : expression ("," expression)* |
    term            : nonce | NUMBER | STRING
    nonce           : fr | NAME           
    fr              : FRESH
    FRESH           : "~" NAME
    STRING          : /'[^']*'/
    NAME            : /[a-zA-Z_][a-zA-Z0-9_-]*/
    NUMBER          : /\d+/
    
    %import common.WS
    %ignore WS
"""

TOP_BNF = """\
    ?start: config_spec
    config_spec     : "process" ":" stms
    stms            : rep
                    | "(" stms ")"
                    | bind_stms
                    | cond_stms
                    | seq_stms
                    | par
                    | stmt
                    | END
    bind_stms       : "let" binding "in" stms
    seq_stms        : stmt ";" stms
    cond_stms       : "if" guard "then" stms
    par             : stms "||" stms
                    | stms "|" stms
    rep             : "!" stms
    END             : "0"
    stmt            : event | out | in | new | role_insts
    role_insts      : NAME "(" arguments ")"
    event           : "event" NAME "(" arguments ")"
    out             : "out" "(" arguments ")"
    in              : "in" "(" arguments ")" -> _in
    new             : "new" nonce
    binding         : expression "=" expression
    guard           : expression "=" expression
    expression      : term
                    | match
                    | xor
                    | exp
                    | func
                    | concat
    match           : "=" NAME
    xor             : expression "XOR" expression
    exp             : expression "^" expression
    concat          : "<" arguments ">"
    func            : NAME "(" arguments ")"
    arguments       : expression ("," expression)* |
    term            : nonce | NUMBER | STRING
    nonce           : fr | NAME           
    fr              : FRESH
    FRESH           : "~" NAME
    STRING          : /'[^']*'/
    NAME            : /[a-zA-Z_][a-zA-Z0-9_-]*/
    NUMBER          : /\d+/
    
    %import common.WS
    %ignore WS
"""



class TreeToStmt(Transformer):
    def event(self, items):
        name = items[0]
        body = items[1]
        return f'event {name}({body});'

    def new(self, items):
        nonce = items[0]
        return f'new {nonce};'
    
    def _in(self, items):
        msg = items[0]
        return f'in({msg});'
    
    def out(self, items):
        msg = items[0]
        return f'out({msg});'
    
    def binding(self, items):
        return f'let {items[0]} = {items[1]} in'
    
    def guard(self, items):
        return f'if {items[0]} = {items[1]} then'
    
    
    def concat(self, items):
        return f"<{', '.join(items)}>"
    
    def arguments(self, items):
        return ", ".join(items)
  
    def parameters(self, items):
        return ", ".join(items)
    
    def param(self, items):
      try:
        assert isinstance(items[0], Token)
        # print(f'this is param: {items[0]}')
        return f'{items[0].value}'
      except:
        return ""
        
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
      return f'{items[0]}'


    def __default__(self, data, children, meta):
      return children[0] if children else data
  
  

def tree2stmt(node) -> str:
    reconstructed_string = ""
    if isinstance(node, Token):
        reconstructed_string = node.value
        # print(reconstructed_string, type(reconstructed_string))
    elif isinstance(node, Tree):
        transformer = TreeToStmt()
        reconstructed_string = transformer.transform(node)
    return reconstructed_string



def pretty_stmts(root:Tree, stmt_list:list, indent_num=1) -> str:
    if root.data == 'role':
            # role            : "let" NAME "(" parameters ")" "=" stms 
        role_name:Token = root.children[0]
        parameters = root.children[1]
        stms_body = root.children[2]
        signature = f'\nlet {role_name.value}({tree2stmt(parameters)}) ='
        stmt_list += [signature]
        pretty_stmts(stms_body, stmt_list, indent_num)
        
    elif root.data == 'stms':
        if isinstance(root.children[0], Tree):
            stmt_type = root.children[0].data
            if stmt_type in ['seq_stms', 'bind_stms']:
                stmt_node = root.children[0].children[0]
                stmt_list += [ indent_num * '  ' + tree2stmt(stmt_node)]
                pretty_stmts(root.children[0].children[1], stmt_list, indent_num)   
            elif stmt_type == 'cond_stms' :
                guard = root.children[0].children[0]
                stmt_list += [ indent_num * '  ' + tree2stmt(guard)]
                pretty_stmts(root.children[0].children[1], stmt_list, indent_num + 1)  
                 
                        
        else:
            assert root.children[0].value == '0'
            stmt_list += [ indent_num * '  ' + root.children[0]]
    else:
        for child in root.children:
            if isinstance(child, Tree):
                pretty_stmts(child, stmt_list, indent_num)



if __name__ == "__main__":
    code2 ="""\
functions: id/1
let Alice(idB, Kas, idA)=
new Na;
let message1 = <idA, idB, Na> in 
out(message1);
in(message2);
let gamma_0 = sdec(message2, Kas) in
let <=Na, Kab, =idB, gamma_1> = gamma_0 in
// let gamma_2 = sdec(gamma_1, Kbs) in
// let <Kab, idA> = gamma_2 in
// let message3 = senc(<Kab, idA>, Kbs) in 
// Can not construct message3 from {'idB', 'idA', 'message1', 'Kas', 'Na', 'gamma_1', 'Kab', 'gamma_0', 'message2'}
out(gamma_1);
in(message4);
let Nb = sdec(message4, Kab) in
let message5 = senc(dec(Nb), Kab) in 
out(message5);
0

let Bob(idB, Kbs, idA)=
new Nb;
in(message3);
let gamma_0 = sdec(message3, Kbs) in
let <Kab, =idA> = gamma_0 in
let message4 = senc(Nb, Kab) in 
out(message4);
in(message5);
let gamma_1 = sdec(message5, Kab) in
let dec(=Nb) = gamma_1 in
0

let Server(idB, Kbs, Kas, idA)=
new Kab;
in(message1);
let <=idA, =idB, Na> = message1 in
let message2 = senc(<Na, Kab, idB, senc(<Kab, idA>, Kbs)>, Kas) in 
out(message2);
0
"""

  
    RoleParser = Lark(ROLE_BNF, parser='lalr', start='start', propagate_positions=True)
    
    root = RoleParser.parse(code2)
    
    assert root.data == 'role_spec'
    print(len(root.children))
    for role in root.children:
        assert role.data == 'role'
        print(role)
        RoleName = role.children[0] 
        print(RoleName)
        
   
    stmt = []
    pretty_stmts(root, stmt, 1)
    print("\n".join(stmt))