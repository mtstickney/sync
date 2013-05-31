import heapq
import sys

def jittered_sorter(max_jitter, iterable):
    queue = []
    input = iter(iterable)

    def time_window(queue):
        if len(queue) == 0 or len(queue) == 1:
            return 0
        min_time = queue[0][0]
        max_time = max(queue[-1][0], queue[-2][0])
        return max_time - min_time

    try:
        while True:
            while time_window(queue) <= max_jitter:
                heapq.heappush(queue, next(input))
            yield heapq.heappop(queue)
    except StopIteration:
        # No more data, set to yield the rest of the elements
        pass
    while len(queue) > 0:
        yield heapq.heappop(queue)

def event_stream(filename):
    with open(filename) as fh:
        lines = iter(fh)
        splitlines = (l.split() for l in lines)
        events = ((float(l[0]), ' '.join(l[1:])) for l in splitlines)
        for evt in jittered_sorter(300, events):
            yield evt

if __name__ == "__main__":
    for evt in event_stream(sys.argv[1]):
        print "%f   %s" % (evt[0], evt[1])
