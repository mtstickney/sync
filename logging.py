import heapq
import sys

def jittered_sorter(max_jitter, iterable):
    queue = []
    input = iter(iterable)

    def time_window(queue):
        max_time = heapq.nlargest(1, queue)
        min_time = heapq.nsmallest(1, queue)
        if len(queue) == 0:
            return 0
        return max_time[0][0] - min_time[0][0]

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
