class EulerTourTree:
    def __init__(self, tour):
        self.tour = tour
        n = len(tour)
        self.first = [0] * n
        self.last = [0] * n
        self.level = [0] * n
        stack = []
        for i, val in enumerate(tour):
            if not stack:
                stack.append(i)
                self.level[i] = 0
            else:
                j = stack[-1]
                if val == tour[j]:
                    stack.pop()
                    self.last[j] = i
                else:
                    stack.append(i)
                    self.level[i] = self.level[j] + 1
                    self.first[i] = j

    def build_euler_tour(root):
        tour = []
        EulerTourTree._build_euler_tour_helper(root, tour)
        return tour

    def _build_euler_tour_helper(node, tour):
        if node:
            tour.append(node.val)
            for child in node.children:
                EulerTourTree._build_euler_tour_helper(child, tour)
            tour.append(node.val)