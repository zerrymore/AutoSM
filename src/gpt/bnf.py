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
                    | stmt ";" stms
                    | par
                    | stmt
                    | END
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
                    | stmt ";" stms
                    | par
                    | stmt
                    | END
    bind_stms       : "let" binding "in" stms
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