import heapq
import sys

class SpanHeap:
    "A heap that keeps track of the range of values stored."
    def __init__(self):
        self.max = None
        self.queue = []

    def push(self, item):
        heapq.heappush(self.queue, item)
        if len(self.queue) == 1 or item > self.max:
            self.max = item

    def pop(self):
        return heapq.heappop(self.queue)

    def span(self):
        if len(self.queue) == 0:
            return 0
        maximum = self.max[0]
        minimum = self.queue[0][0]
        return maximum - minimum

    def size(self):
        return len(self.queue)

def jittered_sorter(max_jitter, iterable, heap=None):
    if heap is None:
        heap = SpanHeap()
    input = iter(iterable)

    try:
        while True:
            while heap.span() <= max_jitter:
                heap.push(next(input))
            yield heap.pop()
    except StopIteration:
        # No more data, set to yield the rest of the elements
        pass
    while heap.size() > 0:
        yield heap.pop()

def event_stream(filename):
    with open(filename) as fh:
        lines = iter(fh)
        splitlines = ((l.split()[0], l.rstrip()) for l in lines)
        events = ((float(key), line) for key, line in splitlines)
        for evt in jittered_sorter(300, events):
            yield evt

if __name__ == "__main__":
    for evt in event_stream(sys.argv[1]):
        print evt[1]
