class Solution:
    def largestOddNumber(self, num: str) -> str:
        start = 0
        end = None
        for i, digit in enumerate(num):
            d = int(digit)
            if d % 2 != 0:
                end = i+1

        if end is None:
            return ""
        return num[start:end]
