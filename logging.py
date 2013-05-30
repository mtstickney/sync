import heapq

class JitteredSorter:
    def __init__(self, max_jitter, iterable):
        self.source = iterable
        self.window = max_jitter
        self.queue = []

    def __iter__(self):
        def time_window(queue):
            max_time = heapq.nlargest(1, queue)
            min_time = heapq.nsmallest(1, queue)
            if len(max_time) == 0 or len(min_time) == 0:
                return 0
            return max_time[0] - min_time[0]

        try:
            while True:
                while time_window(self.queue) <= self.window:
                    heapq.heappush(self.queue, next(self.source))
                yield heapq.heappop(self.queue)
        except StopIteration:
            # All the data's in, we're good to return an element
            pass

        while len(self.queue) > 0:
            yield heapq.heappop(self.queue)

