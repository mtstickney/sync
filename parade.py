import re
import itertools
import sys
from collections import defaultdict, deque

def before_countries(line):
    bm = re.match(r"^(.*) comes before (.*)$", line)
    am = re.match(r"^(.*) comes after (.*)$", line)
    if bm is not None:
        return (bm.group(1), bm.group(2))
    if am is not None:
        return (am.group(2), am.group(1))
    else:
        raise Exception("Couldn't parse input file")

def load_requests(fh):
    leaders = frozenset()
    non_leaders = frozenset()
    followers = defaultdict(frozenset)
    for line in fh:
        follower, leader = before_countries(line)

        # Update the set of possible leaders
        non_leaders = non_leaders | frozenset([follower])
        leaders = leaders - frozenset([follower])
        if leader not in non_leaders:
            leaders = leaders | frozenset([leader])

        followers[leader] = followers[leader] | frozenset([follower])

    return (followers, leaders)

def linearize(followers, leaders):
    q = deque(leaders)
    order = deque()
    

    if len(leaders) == 0:
        print "No Leaders"
        return None

    while len(q) != 0:
        level, country = q.popleft()
        if country in visited:
            print "Already visited", country
            return None
        order.append(country)
        q.extend(followers[country])
    return order

def print_order(filename):
    with open(filename) as fh:
        reqs, leaders = load_requests(fh)
        order = linearize(reqs, leaders)
        if order is None:
            print "Illegal request file!"
        else:
            for num, country in itertools.izip(itertools.count(1), order):
                print "%d. %s" % (num, country)

if __name__ == '__main__':
    print_order(sys.argv[1])
