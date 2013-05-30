import heapq

class JitteredSorter:
    def __init__(self, max_jitter, iterable):
        self.source = iterable
        self.window = max_jitter
        self.queue = []

    def __next__(self):
        def time_window(queue):
            max_time = heapq.nlargest(1, queue)
            min_time = heapq.nsmallest(1, queue)

        try:
            while time_window(self.queue) <= self.window:
                heapq.heappush(self.queue, self.source.__next__())
        except StopIteration:
            # All the data's in, we're good to return an element
            pass

        return heapq.heappop(self.queue)

    def __iter__(self):
        return self
