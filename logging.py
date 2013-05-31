import heapq

def jittered_sorter(max_jitter, iterable):
    queue = []
    input = iter(iterable)

    def time_window(queue):
        max_time = heapq.nlargest(1, queue)
        min_time = heapq.nsmallest(1, queue)
        if len(queue) == 0:
            return 0
        return max_time[0] - min_time[0]

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
