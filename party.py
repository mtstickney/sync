import json
import sys
from collections import deque

class Node:
    def __init__(self, name, score, boss):
        self.name = name
        self.score = score
        self.take_score = None
        self.leave_score = None
        self.children = []
        self.boss = boss

def load_graph(fh):
    nodes = dict()
    def load_person(dct):
        name = dct['name']
        nodes[name] = Node(name, dct['party-animal-score'], dct['boss'])

    # Load people
    json.load(fh, object_hook=load_person)

    # Wire them up
    ceo = None
    for name, person in nodes.items():
        if person.boss is not None:
            nodes[person.boss].children.append(person)
        else:
            ceo = person

    return ceo

def build_scores(node):
    if node.children == []:
        node.take_score = node.score
        node.leave_score = 0
        return

    for c in node.children:
        build_scores(c)
    # If this node gets taken, its immediate children can't go
    node.take_score = sum((c.leave_score) for c in node.children) + node.score
    node.leave_score = sum((max(c.leave_score, c.take_score) for c in node.children))


def select_invitees(node, invitees=None, may_go=True):
    if invitees is None:
        invitees = deque()

    if may_go == True and node.take_score > node.leave_score:
        invitees.append(node.name)
        going = True
    else:
        going = False

    for c in node.children:
        invitees = select_invitees(c, invitees=invitees, may_go=(not going))
    return invitees

def ceo_graph(ceo):
    new_root = Node(ceo.name, ceo.score, None)

    # Ensure the CEO should go
    new_root.take_score = 1
    new_root.leave_score = 0
    new_root.children = ceo.children
    return new_root

def print_invitees(file):
    with open(file) as fh:
        graph = load_graph(fh)
        build_scores(graph)
        with_ceo = ceo_graph(graph)

        print "Optimal party people:"
        for name in select_invitees(graph):
            print name

        print "\nOptimal party with CEO:"
        for name in select_invitees(with_ceo):
            print name

if __name__ == '__main__':
    print_invitees(sys.argv[1])
