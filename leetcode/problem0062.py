class Solution:
    def uniquePathsHelper(self, i, j, m, n, seen) -> int:
        if i >= m or j >= n:
            return 0
        
        if i == m-1 and j == n-1:
            return 1

        if (i,j) in seen:
            return seen[(i,j)]

        down = self.uniquePathsHelper(i+1, j, m, n, seen)
        right = self.uniquePathsHelper(i, j+1, m, n, seen)
        seen[(i,j)] = down + right

        return down + right

    def uniquePaths(self, m: int, n: int) -> int:
        return self.uniquePathsHelper(0, 0, m, n, {})