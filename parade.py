import re
import itertools
import sys
from collections import defaultdict, deque

def leader_follower(line):
    bm = re.match(r"^(.*) comes before (.*)$", line)
    am = re.match(r"^(.*) comes after (.*)$", line)
    if bm is not None:
        return (bm.group(1), bm.group(2))
    if am is not None:
        return (am.group(2), am.group(1))
    else:
        raise Exception("Couldn't parse input file")

def load_requests(fh):
    leaders = set()
    non_leaders = set()
    followers = defaultdict(set)
    for line in fh:
        leader, follower = leader_follower(line)

        # Update the set of possible leaders
        non_leaders.add(follower)
        leaders.discard(follower)
        if leader not in non_leaders:
            leaders.add(leader)

        followers[leader].add(follower)

    return (followers, leaders)

def linear_extension(followers, leaders):
    processed = set()
    order = deque()

    def linearize(leader):
        q = deque([leader])
        linearization = deque()
        path_nodes = set()

        while len(q) != 0:
            country = q.popleft()

            if country in processed:
                continue
            if country in path_nodes:
                # Already traversed, time for post-processing
                linearization.appendleft(country)
                processed.add(country)
                path_nodes.remove(country)
            else:
                path_nodes.add(country)
                q.appendleft(country)
                for c in followers[country] - processed:
                    if c in path_nodes:
                        # cycle, bail
                        return None
                    q.appendleft(c)
        return linearization

    if len(leaders) == 0:
        return None

    for l in leaders:
        sub_order = linearize(l)
        if sub_order is None:
            return None
        order.extend(sub_order)
    return order

def print_order(filename):
    with open(filename) as fh:
        reqs, leaders = load_requests(fh)
        order = linear_extension(reqs, leaders)
        if order is None:
            print "Illegal request file!"
        else:
            for num, country in itertools.izip(itertools.count(1), order):
                print "%d. %s" % (num, country)

if __name__ == '__main__':
    print_order(sys.argv[1])
