class Solution:
    def findTheDifference(self, s: str, t: str) -> str:
        s_map = {}

        for char in s:
            if char not in s_map:
                s_map[char] = 0
            s_map[char] += 1

        for char in t:
            if char not in s_map:
                return char
            s_map[char] -= 1
            if s_map[char] == 0:
                del s_map[char]
        
        for char in s_map:
            return char