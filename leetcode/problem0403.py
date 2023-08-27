class Solution:
    def canCross(self, stones: List[int]) -> bool:
        pos = [(1, 0)]
        seen = set()
        seen.add((1, 0))

        for i in range(1, len(stones)+1):
            next_pos = []
            for k, last_pos in pos:

                if i == 1:
                    units = [1]
                else:
                    units = [k-1, k, k+1]

                for j in units:
                    jump = j + last_pos
                    if jump == stones[-1]:
                        return True
                    if jump in stones and jump > 0 and j > 0 and (j, jump) not in seen:
                        next_pos.append((j, jump))
                    seen.add((j, jump))

            if next_pos == []:
                break
            pos = next_pos

        return False