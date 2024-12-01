class Solution:
    def largestGoodInteger(self, num: str) -> str:
        ret = ""
        value = float('-inf')
        i = 0
        while i < len(num):
            digit = num[i]
            if i+2 < len(num) and digit == num[i+1] and digit == num[i+2]:
                good_int = num[i:i+3]
                if int(good_int) > value:
                    value = int(good_int)
                    ret = good_int
                i = i+3
                continue
            i += 1
        return ret
