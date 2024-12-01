class Solution:
    def minimumPossibleSum(self, n: int, target: int) -> int:
        nums = []
        seen = set()
        i = 1
        while len(nums) < n:
            if target - i not in seen:
                seen.add(i)
                nums.append(i)
            i += 1
        return sum(nums)