class Solution:
    def totalMoney(self, n: int) -> int:
        if n == 1:
            return 1

        total = 1
        last_day = 1
        last_monday = 1
        
        for day in range(1, n):
            if day % 7 == 0:
                total += last_monday + 1
                last_monday = last_monday + 1
                last_day = last_monday
            else:
                total += last_day + 1
                last_day += 1
        
        return total

