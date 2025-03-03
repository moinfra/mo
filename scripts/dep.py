import sys
import ast
from collections import OrderedDict, defaultdict, deque

def main():
    """
    Main function to parse a BUILD file, perform topological sort on the dependencies,
    and generate a Graphviz DOT representation of the dependency graph.
    """
    # Check if the correct number of command-line arguments is provided.
    if len(sys.argv) != 2:
        print("Usage: python dep.py <BUILD_FILE>")
        sys.exit(1)
    
    # Get the file path from the command-line arguments.
    file_path = sys.argv[1]
    
    # Parse the BUILD file to extract dependencies.
    try:
        dependencies = parse_build_file(file_path)
    except FileNotFoundError:
        print(f"Error: File '{file_path}' not found.")
        sys.exit(1)
    except Exception as e:
        print(f"Error parsing BUILD file: {e}")
        sys.exit(1)
    
    # Build the dependency graph and perform topological sort.
    try:
        graph, in_degree, all_nodes = build_graph(dependencies)
        sorted_nodes = topological_sort(dependencies, graph, in_degree, all_nodes)
    except ValueError as e:
        print(f"Error: {e}")
        sys.exit(1)
    
    # Print the topological sort order.
    print("# Topological Sort Order:", end=' ')
    print(" -> ".join(sorted_nodes))
    
    # Generate the Graphviz DOT source code.
    dot_source = generate_dot(sorted_nodes, dependencies)
    print(dot_source)

def parse_build_file(file_path):
    """
    Parses the BUILD file to extract targets and their dependencies.
    
    Args:
        file_path (str): The path to the BUILD file.
        
    Returns:
        OrderedDict: An ordered dictionary where keys are target names and values are lists of their dependencies.
                     The order of targets is preserved as they appear in the BUILD file.
    """
    # Read the content of the BUILD file.
    with open(file_path, 'r', encoding='utf-8') as f:
        content = f.read()
    
    # Parse the content using the ast module.
    tree = ast.parse(content)
    
    # Use OrderedDict to maintain the order of targets as they appear in the file.
    dependencies = OrderedDict()
    
    # Traverse the abstract syntax tree (AST).
    for node in ast.walk(tree):
        # Check for function calls (e.g., cc_library, cc_binary).
        if isinstance(node, ast.Call):
            func = node.func
            # Check if the function is cc_library or cc_binary.
            if isinstance(func, ast.Name) and func.id in ('cc_library', 'cc_binary'):
                target_name = None
                deps = []
                
                # Extract the 'name' and 'deps' arguments from the function call.
                for keyword in node.keywords:
                    if keyword.arg == 'name':
                        # Get the target name.
                        if isinstance(keyword.value, ast.Constant):
                            target_name = keyword.value.value
                    elif keyword.arg == 'deps':
                        # Get the list of dependencies.
                        if isinstance(keyword.value, ast.List):
                            for item in keyword.value.elts:
                                if isinstance(item, ast.Constant):
                                    # Handle dependency format like ":lexer" -> "lexer".
                                    dep = item.value.split(':')[-1]
                                    deps.append(dep)
                
                # Add the target and its dependencies to the dictionary.
                if target_name is not None:
                    dependencies[target_name] = deps
    
    return dependencies

def build_graph(dependencies):
    """
    Builds a dependency graph and an in-degree table from the dependencies.
    
    Args:
        dependencies (OrderedDict): An ordered dictionary of targets and their dependencies.
        
    Returns:
        tuple: A tuple containing the dependency graph (defaultdict), the in-degree table (dict),
               and a set of all nodes in the graph.
    """
    # Collect all nodes (targets and dependencies).
    all_nodes = set()
    for target, deps in dependencies.items():
        all_nodes.add(target)
        all_nodes.update(deps)
    
    # Initialize the graph and in-degree table.
    graph = defaultdict(list)
    in_degree = {node: 0 for node in all_nodes}
    
    # Populate the graph and in-degree table.
    for target, deps in dependencies.items():
        for dep in deps:
            graph[dep].append(target)  # Edge: dep -> target
            in_degree[target] += 1
    
    return graph, in_degree, all_nodes

def topological_sort(dependencies, graph, in_degree, all_nodes):
    """
    Performs topological sort on the dependency graph to determine the build order.
    Detects cycles in the dependencies.
    
    Args:
        dependencies (OrderedDict): An ordered dictionary of targets and their dependencies.
        graph (defaultdict): The dependency graph.
        in_degree (dict): The in-degree table.
        all_nodes (set): A set of all nodes in the graph.
        
    Returns:
        list: A list of nodes in topological order.
        
    Raises:
        ValueError: If a cycle is detected in the dependencies.
    """
    # Create a copy of the in-degree table to modify.
    in_degree_copy = {k: v for k, v in in_degree.items()}
    
    # Initialize the queue and the list of sorted nodes.
    queue = deque()
    sorted_nodes = []
    
    # Initialize the queue with nodes that have an in-degree of 0.
    # Prioritize targets in the order they appear in the BUILD file.
    ordered_nodes = list(dependencies.keys()) + sorted(all_nodes - set(dependencies.keys()))
    
    for node in ordered_nodes:
        if in_degree_copy.get(node, 0) == 0:
            queue.append(node)
            in_degree_copy[node] = -1  # Mark as enqueued to avoid duplicates
    
    # Perform topological sort.
    while queue:
        current = queue.popleft()
        sorted_nodes.append(current)
        
        # Update the in-degrees of the neighbors.
        for neighbor in graph[current]:
            in_degree_copy[neighbor] -= 1
            if in_degree_copy[neighbor] == 0:
                queue.append(neighbor)
                in_degree_copy[neighbor] = -1  # Mark as enqueued
    
    # Check for cycles.
    if len(sorted_nodes) != len(all_nodes):
        raise ValueError("Cycle detected in dependencies")
    
    return sorted_nodes

def generate_dot(sorted_nodes, dependencies):
    """
    Generates the Graphviz DOT source code for the dependency graph.
    
    Args:
        sorted_nodes (list): A list of nodes in topological order.
        dependencies (OrderedDict): An ordered dictionary of targets and their dependencies.
        
    Returns:
        str: The Graphviz DOT source code.
    """
    # Collect all edges.
    edges = set()
    nodes = set(sorted_nodes)
    
    for target, deps in dependencies.items():
        for dep in deps:
            edges.add((dep, target))
    
    # Generate node declarations (in topological order).
    node_lines = [f'"{node}" [label="{node}"];' for node in sorted_nodes]
    
    # Generate edge declarations.
    edge_lines = [f'"{src}" -> "{dest}";' for src, dest in edges]
    
    # Combine the DOT source code.
    dot = [
        'digraph G {',
        '  rankdir="LR";',
        *['  ' + line for line in node_lines],
        *['  ' + line for line in edge_lines],
        '}'
    ]
    
    return '\n'.join(dot)

if __name__ == "__main__":
    main()
