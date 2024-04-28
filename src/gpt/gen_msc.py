import ast

def __syn_Send(params):
    return f'out({params[2]})'

def __syn_Recv(params):
    if len(params) == 3:
        return f'in({params[2]})'
    else: return f'in({params[1]})'
def __syn_concat(params:list):
    return f'<{", ".join(params)}>'

def __syn_aenc(params):
    m = params[0]
    k = params[1]
    return f'aenc({m}, {k})'

def __syn_var(params):
    return f'{params[0]}'

def __syn_role(params):
    return f'{params[0]}'

def __syn_Op(params):
    if len(params) == 2:
        if "=" in params[1]:
            return f'let {params[1]} in'
        else:
            return f'event {params[1]}'
    elif len(params) == 3:
        return "__"
    
def __syn_assign(params):
    if len(params) == 2:
        return f'{params[0]} = {params[1]}'
    else:
        return f'{params[0]} = {params[1]}'

def __syn_exp(params):
    # if len(params) == 2:
    return f'{params[0]}^{params[1]}'

def __syn_gen(params):
    # if len(params) == 2:
    return f'nonce {params[1]}'
SYN_OPS = {
    "Send": __syn_Send,
    "Recv": __syn_Recv,
    "concat": __syn_concat,
    "aenc":__syn_aenc,
    "var": __syn_var,
    "role": __syn_role,
    "Op": __syn_Op,
    "assign": __syn_assign,
    "compute": __syn_assign,
    "exp": __syn_exp,
    "Gen": __syn_gen,
}

def translate_call(node):
    if isinstance(node, ast.Call):
        func_name = node.func.id if isinstance(node.func, ast.Name) else translate_call(node.func)
        args = [translate_call(arg) for arg in node.args]
        
        if func_name == 'Knows':
            # initknow[process_name] =  args[1:]
            return None

        if func_name not in SYN_OPS:
            # print(f'func_name:{func_name}, args:{args}')                                                    
            return f"{func_name}({', '.join(args)})"
        else:
            return SYN_OPS[func_name](args)
        
    else:
        return ast.unparse(node)
    
def merge_nodes(nodes):
    # print(f'nodes:{nodes}')
    merged = []
    if nodes == [] or nodes is None:
        return []
    # print(f'nodes:{nodes}')
    merged.append(nodes[0])
    for i in range(1, len(nodes)):
        if nodes[i]["group"] == merged[-1]["group"]:
            if nodes[i]["pc"] == merged[-1]["pc"]+1:
                merged[-1]["pc"] += 1
                # print('nodes_i:', nodes[i]["text"])
                # print('merged:', merged)
                merged[-1]["text"] += f'\n{nodes[i]["text"]}'
            else: merged.append(nodes[i])
        else:
            merged.append(nodes[i])
    for m in merged:
        m["duration"] = (m["pc"] - m["start"] + 1) * 0.5
    # for i in merged:
    #     print(i)
    return merged

def calculus_to_msc(code):
    # print(f'codes:{code}')
    tree = ast.parse(code)
    links = []
    nodes = []
    initknow = {}
    groups = set()
    isGroup = []
    i = 1
    for node in ast.walk(tree):
        if isinstance(node, ast.Expr) and isinstance(node.value, ast.Call):
            first_arg = node.value.args[0]
            group_name = None
            
            if isinstance(first_arg, ast.Name):
                group_name = first_arg.id
            elif isinstance(first_arg, ast.Call) and first_arg.args:
                group_name = first_arg.args[0].id

            if group_name and group_name not in ["GLOBAL"]:
                # groups.setdefault(group_name, [])
                initknow.setdefault(group_name, [])
                groups.add(group_name)
                # translated_call = translate_call(node.value, group_name, processes, initknow)
                # if translated_call:
                #     groups.setdefault(group_name, []).append(translated_call)

            if isinstance(node, ast.Expr) and isinstance(node.value, ast.Call) and isinstance(node.value.func, ast.Name):
                if node.value.func.id == "Send":
                    sender = translate_call(node.value.args[0])
                    recevier = translate_call(node.value.args[1])
                    # groups.setdefault(recevier, [])
                    links.append({
                        "from": sender,
                        "to": recevier,
                        "text": translate_call(node.value.args[2]),
                        "time": i
                    })
                    i += 1
 
                elif node.value.func.id == "Op":
                    evt = translate_call(node.value.args[1])
                    nodes.append({
                        "group": group_name,
                        "text": evt,
                        "duration": 0.2,
                        "start": i,
                        "pc": i})
                    # print(nodes)
                    i += 1
                elif node.value.func.id == 'Gen':
                    evt = translate_call(node.value)
                    nodes.append({
                        "group": group_name,
                        "text": evt,
                        "duration": 0.2,
                        "start": i,
                        "pc": i})
                    # print(nodes)
                    i += 1
    merged = merge_nodes(nodes)
    if merged:
        max_duration = max(merged, key=lambda x: x['pc'])['pc']
    else:
        max_duration = i
    group_index = 0
    for group in groups:
        isGroup.append({
            "key": group,
            "text": f"Role:{group}",
            "isGroup": True,
            "loc": f"{group_index*100} 0",
            "duration": max_duration + 4,
        })
        group_index += 1

    data = {"class": "go.GraphLinksModel",
      "nodeDataArray": isGroup + merged,
      "linkDataArray": links
      }
    # print(data)
    return data

if __name__ == "__main__":
    code = f'''# A computes a nonce and sends it to B. (A -> B: ANonce)
Gen(role(A), var(ANonce))
Send(role(A), role(B), var(ANonce))

# When B receives A's nonce, B computes their own nonce and sends it to A. (B -> A: BNonce)
Recv(role(B), role(A), var(nonce))
Gen(role(B), var(BNonce))
Send(role(B), role(A), var(BNonce))

# When A receives B's nonce, A does two things:
Recv(role(A), role(B), var(nonce))
Op(role(A), Seq(action1, action2))

# A installs a session key SK, which is derived from ANonce and BNonce by applying a key derivation function (i.e., SK = kdf(ANonce, BNonce)).
Op(role(A), assign(var(SK), kdf(var(ANonce), var(BNonce))))

# Once the session key is installed, A sends a message with the string "ACK" to B (A -> B: "ACK") and switches to a 'DONE' state to indicate that the protocol has been executed successfully on A's side.
Send(role(A), role(B), "ACK")
Op(role(A), switch('DONE'))

# When B receives the "ACK" message, B also computes the session key SK = kdf(ANonce, BNonce), installs it, and switches to a 'DONE' state.
Recv(role(B), role(A), var(ACK))
Op(role(B), assign(var(SK), kdf(var(ANonce), var(BNonce))))
Op(role(B), switch('DONE'))
'''
    calculus_to_msc(code)