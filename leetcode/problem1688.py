class Solution:
    def numberOfMatches(self, n: int) -> int:
        num_matches = 0
        while n > 1:
            if n % 2 == 0:
                num_matches += n // 2
                n //= 2
            else:
                num_matches += (n-1) // 2
                n = ((n-1) // 2) + 1

        return num_matches
