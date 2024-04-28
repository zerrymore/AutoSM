import ast
from gpt.OPS import SYN_OPS
import logging

process_template = """let {name}({parameters}) =
{body} 0

"""

config_template = """process:
{body}"""

class CodeExecutor(ast.NodeVisitor):
	def __init__(self):
		self.execution_order = []

	def visit(self, node):
		# Override the visit method to add each node to execution_order
		# if isinstance(node, ast.Expr):
		self.execution_order.append(node)
		super().visit(node)

def execute_code(code):
	tree = ast.parse(code)
	executor = CodeExecutor()
	executor.visit(tree)
	return executor.execution_order


def lambda_to_processes(code):
	execution_order = execute_code(code)
	
	def free_vars(node):
		VARS = []
		if isinstance(node, ast.Name):
			VARS += [node.id]
		elif isinstance(node, ast.Call):
			for v in node.args:
				VARS += free_vars(v)
		return VARS

	def Term(node):
		if isinstance(node, ast.Name):
			return node.id
		elif isinstance(node, ast.Constant):
			return f"'{node.value}'"
		elif isinstance(node, ast.Tuple):
			args = [Term(arg) for arg in node.elts]
			return f"<{', '.join(args)}>"
		elif isinstance(node, ast.BinOp):
			left = node.left
			right = node.right
			if isinstance(node.op, ast.BitXor):
				return f"{Term(left)}^{Term(right)}"
		elif isinstance(node, ast.Call):
			func_name = node.func.id
			args = [Term(arg) for arg in node.args]
			try:
				return SYN_OPS[func_name](args)
			except:
				return f'{func_name}({", ".join(args)})'
		else:
			return ast.unparse(node)
		
	processes = {}; fresh_vars = {}
	initknow = {}; index = {}; space = {}
	for node in execution_order:
		if isinstance(node, ast.Expr):
			event_name = node.value.func.id
			root = node.value
			role_name = Term(root.args[0])
			fresh_vars.setdefault(role_name, [])
			initknow.setdefault(role_name, [])
			index.setdefault(role_name, 0)
			space.setdefault(role_name, 0)
			processes.setdefault(role_name, [])
			if event_name == "Knows":
				initknow[role_name] +=  [Term(arg) for arg in root.args[1:]]
			else:
				args = [Term(arg) for arg in root.args]
				stmt = SYN_OPS[event_name](args, index, role_name)
				if not stmt.startswith("let") and not stmt.startswith("if")  and "||" not in stmt:
					stmt += ";"
				processes[role_name] += [space[role_name] * "  " + stmt]
				if stmt.startswith("if"):
					space[role_name] += 1
					
	try:
		config_spec = {"GLOBAL": config_template.format(
			body='\n'.join(["  " + line for line in processes["GLOBAL"]]),
		)}
	except:
		config_spec = {"GLOBAL": []}
		
	role_spec = {name: process_template.format(
		name=name,
		parameters=', '.join(initknow[name]),
		body='\n'.join(["  " + line for line in events]),
		space=space[name] * "\t"
		) for name, events in processes.items() if name not in ["GLOBAL"]}

	role_spec.update(config_spec) 
	roles_init = [f'{name}({", ".join(initknow[name])})' for name in processes.keys()]

	return role_spec, roles_init



def format_parse_output(spec:dict):
    role_sig = []
    roles_spec = ""
    for k in spec.keys():
        if k not in ["GLOBAL"]:
            roles_spec += spec[k]
            role_sig.append(spec[k].split("\n")[0])
    
    return roles_spec, None