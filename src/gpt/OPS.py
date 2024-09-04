def __syn_Send(params:list[str], index=None, process_name=None):
    # print(params)
    if len(params) == 3:
        content = params[2]
        if content.startswith("(") and content.endswith(")"):
            """sample: send(A, B, (a, b, X))"""
            return f'out(<{content[1:-1]}>)'
        else:
            return f'out({params[2]})'
    elif len(params) > 3:
        """sample: send(A, B, a, b, X)"""
        return f'out(<{", ".join(params[2:])}>)'

def __syn_Recv(params, index=None, process_name=None):
    if len(params) == 3:
        content = params[2]
        if content.startswith("(") and content.endswith(")"):
            """sample: Recv(A, B, (a, b, X))"""
            return f'in(<{content[1:-1]}>)'
        else:
            return f'in({params[2]})'
    elif len(params) > 3:
        """sample: Recv(A, B, a, b, X)"""
        return f'in(<{", ".join(params[2:])}>)'

def __syn_concat(params, index=None, process_name=None):
    # print(params)
    return f'<{",".join(params)}>'

def __syn_aenc(params, index=None, process_name=None):
    m = params[0]
    k = params[1]
    return f'aenc({m}, {k})'

def __syn_var(params, index=None, process_name=None):
    return f'{params[0]}'

def __syn_role(params, index=None, proces_name=None):
    return f'{params[0]}'

func_op = ["aenc", "exp", "pow", "concat", "adec", "<", "hash", "senc", "sdec"]
def implicit_binding(arg:str) -> bool:
    for op in func_op:
        if arg.startswith(op):
            return True
        
def __syn_Op(params:list[str], index=None, process_name=None):
    if len(params) == 2:
        # print(f'PARAMS:{params}')
        if "=" in params[1]:
            return f'let {params[1]} in'
        elif implicit_binding(params[1]):
            index[process_name] += 1
            return f'let msg{index[process_name]} = {params[1]} in'
        else:
            return f'event {params[1][0].upper()+ params[1][1:]}'
    elif len(params) == 3:
        return "__"
    
def __syn_assign(params, index=None, process_name=None):
    if len(params) == 2:
        return f'{params[0]} = {params[1]}'
    else:
        args = ", ".join(params[1:])
        return f'{params[0]} = <{args}>'

def __syn_exp(params, index=None, process_name=None):
    # print("mul:", params)
    # if len(params) == 2:
    if params[0] == 'g':
        return f"'g'^{params[1]}"
    return f'{params[0]}^{params[1]}'

def __syn_Checks(params, index=None, process_name=None):
    # if len(params) == 2:
    return f'event Checks({params[0]}, {params[1]})'

def __syn_xor(params, index=None, process_name=None):
    # if len(params) == 2:
    return f'{params[0]} XOR {params[1]}'

def __syn_str(params, index=None, process_name=None):
    # if len(params) == 2:
    return f"'{params[0]}'"

def __syn_cond(params, index=None, process_name=None):
    # if len(params) == 2:
    return f"if {params[1]} then"

def __syn_glob_gen(params, index=None, process_name=None):
    # if len(params) == 2:
    return f"new {params[1]}"

def __syn_glob_send(params, index=None, process_name=None):
    # if len(params) == 2:
    return f"out({params[1]})"


def __syn_glob_op(params, index=None, process_name=None):
    # if len(params) == 2:
    return f"event {params[1]}"

def __syn_glob_par(params, index=None, process_name=None):
    # if len(params) == 2:
    return f"({'||'.join(params[1:])})"

def __syn_local(params, index=None, process_name=None):
    # if len(params) == 2:
    return f"={params[0]}"

def __syn_Gen(args, index=None, name=None):
    return f'new {args[1]}'

def __syn_role(args, index=None, name=None):
    return f'{args[0]}'

SYN_OPS = {
    "Send": __syn_Send,
    "Recv": __syn_Recv,
    "Op": __syn_Op,
    "Checks": __syn_Checks,
    "concat": __syn_concat,
    "aenc":__syn_aenc,
    "var": __syn_var,
    "role": __syn_role,
    "assign": __syn_assign,
    "bind": __syn_assign,
    "local": __syn_local,
    "compute": __syn_assign,
    "computes": __syn_assign,
    "exp": __syn_exp,
    "pow": __syn_exp,
    "xor": __syn_xor,
    "str": __syn_str,
    "mul":__syn_exp,
    "Cond": __syn_cond,
    "equal": __syn_assign,
    "Gen": __syn_Gen,
    "role": __syn_role,
    "GLOBAL_Gen": __syn_glob_gen,
    "GLOBAL_Send": __syn_glob_send,
    "GLOBAL_Op": __syn_glob_op,
    "PAR": __syn_glob_par,
}