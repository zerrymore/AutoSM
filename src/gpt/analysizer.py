import ast

class RoleBasedVariableAnalyzer(ast.NodeVisitor):
    def __init__(self):
        self.K = {}  # Dictionary to store known variables per role
        self.E = {}  # Dictionary to store error variables per role
        self.dependencies = {}  # Dictionary to store dependencies of each variable
        self.log = []  # List to store detailed logs of errors

    def visit_Call(self, node):
        if isinstance(node.func, ast.Name):
            function_name = node.func.id
            if len(node.args) > 0:
                role_arg = node.args[0]
                role = role_arg.id if isinstance(role_arg, ast.Name) else None

                if role:
                    if role not in self.K:
                        self.K[role] = set()
                    if role not in self.E:
                        self.E[role] = set()

                    if function_name == "Send":
                        if len(node.args) > 2:
                            send_var = node.args[2]
                            self._check_send_variables(role, send_var, node.lineno)

                    elif function_name == "Recv":
                        if len(node.args) > 2:
                            recv_var = node.args[2]
                            self._collect_variables(role, recv_var)
                            self._add_dependencies_to_knowledge(role, recv_var, node.lineno)

                    elif function_name in ["Knows", "Gen"]:
                        for arg in node.args[1:]:
                            self._collect_variables(role, arg)

                    elif function_name == "Op":
                        if len(node.args) > 1 and isinstance(node.args[1], ast.Call) and node.args[1].func.id == "assign":
                            assigned_var = node.args[1].args[0]
                            expression_arg = node.args[1].args[1]
                            if isinstance(assigned_var, ast.Name):
                                self.dependencies[assigned_var.id] = self._extract_variables(expression_arg)
                                self.K[role].add(assigned_var.id)
                            self._check_expression_variables(role, expression_arg, node.lineno)

    def _check_send_variables(self, role, node, lineno):
        vars_in_expression = self._extract_variables(node)
        print(vars_in_expression)
        print("~~~~~~~!!!!!")
        for var in vars_in_expression:
            if var not in self.K[role]:
                self.E[role].add(var)
                self.log.append({
                    'error': f"Variable '{var}' not known in role '{role}' when sending",
                    'line': lineno,
                    'code': self.source_code[lineno-1]
                })

    def _extract_variables(self, node):
        result = set()
        if isinstance(node, ast.Name):
            result.add(node.id)
        elif isinstance(node, ast.Call):
            function_name = node.func.id
            # if function_name == "exp":
            #     arg = node.args[0]
            #     # print(arg.id)
            # else:
            #     args = node.args      
                      
            for arg in node.args:
                result.update(self._extract_variables(arg))
        elif isinstance(node, ast.Attribute):
            result.add(self._get_full_attribute_name(node))
        return result

    def _collect_variables(self, role, node):
        if isinstance(node, ast.Name):
            self.K[role].add(node.id)
        elif isinstance(node, ast.Call):
            for arg in node.args:
                self._collect_variables(role, arg)
        elif isinstance(node, ast.Attribute):
            full_name = self._get_full_attribute_name(node)
            self.K[role].add(full_name)
            
    # def _add_dependencies_to_knowledge(self, role, node, lineno):
    #     vars = self._extract_variables(node)
    #     for var in vars:
    #         if var in self.dependencies:
    #             self.K[role].update(self.dependencies[var])
    
    def _add_all_dependencies(self, role, var, seen_vars):
        """
        Recursively adds all dependencies of a variable to the knowledge set of a role.
        :param role: The role for which dependencies are being added.
        :param var: The current variable being processed.
        :param seen_vars: A set to track variables that have been processed to avoid infinite recursion.
        """
        if var in seen_vars:  # Avoid infinite recursion by checking if we have already seen this variable
            return
        seen_vars.add(var)  # Mark this variable as seen

        if var in self.dependencies:
            # Update the knowledge base with the direct dependencies of the variable
            self.K[role].update(self.dependencies[var])
            # Recursively add dependencies of each dependency
            for dep_var in self.dependencies[var]:
                self._add_all_dependencies(role, dep_var, seen_vars)

    def _add_dependencies_to_knowledge(self, role, node, lineno):
        vars = self._extract_variables(node)  # Assuming this method extracts all variables used in the node
        for var in vars:
            self._add_all_dependencies(role, var, set())


    # Implement remaining methods...
    
    def _check_expression_variables(self, role, node, lineno):
        if isinstance(node, ast.Name):
            if node.id not in self.K[role]:
                self.E[role].add(node.id)
                # Log the error with detailed information
                self.log.append({
                    'error': f"Variable '{node.id}' not known in role '{role}'",
                    'line': lineno,
                    'code': self.source_code[lineno-1]
                })
        elif isinstance(node, ast.Call):
            for arg in node.args:
                self._check_expression_variables(role, arg, lineno)  # Pass lineno to recursive calls
        elif isinstance(node, ast.Attribute):
            base_name = self._get_full_attribute_name(node)
            if base_name not in self.K[role]:
                self.E[role].add(base_name)
                # Log the error with detailed information
                self.log.append({
                    'error': f"Variable '{base_name}' not known in role '{role}'",
                    'line': lineno,
                    'code': self.source_code[lineno-1]
                })



    def _get_full_attribute_name(self, node):
        if isinstance(node, ast.Attribute):
            return self._get_full_attribute_name(node.value) + '.' + node.attr
        elif isinstance(node, ast.Name):
            return node.id
        return ''

def analyze_code(code):
    tree = ast.parse(code)
    analyzer = RoleBasedVariableAnalyzer()
    analyzer.source_code = code.split("\n")  # Split source code into lines
    analyzer.visit(tree)
    return analyzer.K, analyzer.E, analyzer.log, analyzer.dependencies

def log_report(log_entries):
    return "\n".join([f"Error on line {entry['line']}: {entry['error']} in '{entry['code']}'" for entry in log_entries])


def errs_report(code):
    code = code.replace("exp(g,", "exp('g',").replace("pow(g,", "pow('g',")
    K, _, entries, dependency = analyze_code(code)
    print(dependency)
    print(K)
    return log_report(entries)



if __name__ == "__main__":
    code = """\
Gen(I, eskI)
Gen(R, eskR)
Op(I, assign(h1_eskI_lkI, h1(concat(eskI, lkI))))
Op(R, assign(h1_eskR_lkR, h1(concat(eskR, lkR))))
Op(I, assign(X, exp('g', h1_eskI_lkI)))
Op(R, assign(Y, exp(g, h1_eskR_lkR)))
Send(I, R, X)
Recv(R, I, X)
Send(R, I, Y)
Recv(I, R, Y)
Op(I, assign(x, exp(pkR, h1_eskI_lkI)))
Op(R, assign(y, exp(pkI, h1_eskR_lkR)))
Op(I, assign(kI, h2(concat(exp(Y, lkI), x, exp(Y, h1_eskI_lkI), idI, idR))))
Op(R, assign(kR, h2(concat(y, exp(X, lkR), exp(X, h1_eskR_lkR), idI, idR))))
"""
    print(errs_report(code))