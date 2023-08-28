class MyStack:

    def __init__(self):
        self.q1 = []
        self.q2 = []

    def push(self, x: int) -> None:
        if len(self.q1) > 0:
            self.q1.append(x)
        else:
            self.q2.append(x)

    def _helper(self):
        if len(self.q1) > 0:
            main = self.q1
            aux = self.q2
        else:
            main = self.q2
            aux = self.q1
        
        while len(main) > 1:
            aux.append(main.pop(0))
        return main.pop(0), aux, main

    def pop(self) -> int:
        val, _, _ = self._helper()
        return val
        

    def top(self) -> int:
        val, aux, main = self._helper()
        aux.append(val)
        return val

    def empty(self) -> bool:
        return len(self.q1) == 0 and len(self.q2) == 0


# Your MyStack object will be instantiated and called as such:
# obj = MyStack()
# obj.push(x)
# param_2 = obj.pop()
# param_3 = obj.top()
# param_4 = obj.empty()
