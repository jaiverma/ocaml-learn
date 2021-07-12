class Node:
    # name     : string
    # children : list of Node
    def __init__(self, name, weight=0):
        self.name = name
        self.weight = weight
        self.subtree_weight = 0
        self.children = []

class Tree:
    _TMP = None

    def __init__(self):
        self.root = None
        self.nodes = set()

    # node    : Node (to add)
    # to      : string (node name to connect to)
    # weight  : int (weight of edge)
    def add_node(self, node, to, weight, cur=None):
        if cur is None:
            cur = self.root

        if self.root is None:
            print('Error, please set root first!')
            sys.exit(1)

        self.nodes.add(node.name)
        self.nodes.add(to)

        if cur.name == to:
            print(f'[DEBUG] add_node: {node.name} -> {to}')
            node.weight = weight
            cur.children.append(node)
        else:
            for child in cur.children:
                ret = self.add_node(node, to, weight, child)

    def _render_helper(self, cur, f):
        for child in cur.children:
            f.write(f'    {cur.name} -> {child.name}[label="{child.weight}"]\n')
        for child in cur.children:
            self._render_helper(child, f)

    def render(self):
        with open('/tmp/galaxy.dot', 'w') as f:
            f.write('digraph {\n')
            for node in self.nodes:
                f.write(f'    {node} [label="{node} ({Tree._TMP[node].subtree_weight})"]\n')
            self._render_helper(self.root, f)
            f.write('}\n')

    def propagate_weights(self):
        def _propagate_weights(cur):
            if len(cur.children) == 0:
                # leaf node
                cur.subtree_weight = 0
            else:
                w = []
                for child in cur.children:
                    w.append(child.weight + _propagate_weights(child))
                cur.subtree_weight = max(w)

            return cur.subtree_weight

        _propagate_weights(self.root)

def read_input():
    num_planets = int(input())
    num_wormholes = int(input())
    graph = Tree()
    done = dict()
    Tree._TMP = done

    for _ in range(num_wormholes):
        A, B, cost = list(map(int, input().split(' ')))
        ANode = None
        BNode = None
        if A not in done:
            ANode = Node(A)
            done[A] = ANode
        else:
            ANode = done[A]

        if B not in done:
            BNode = Node(B)
            done[B] = BNode
        else:
            BNode = done[B]

        if ANode.name in graph.nodes:
            # A is the parent
            graph.add_node(BNode, ANode.name, cost)
        elif BNode.name in graph.nodes:
            # B is the parent
            graph.add_node(ANode, BNode.name, cost)
        else:
            # set root
            graph.root = ANode
            graph.add_node(BNode, ANode.name, cost)

    return graph

G = read_input()
G.propagate_weights()
G.render()
