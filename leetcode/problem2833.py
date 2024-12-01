class Solution:
    def furthestDistanceFromOrigin(self, moves: str) -> int:
        numl = numr = 0
        underscores = 0
        
        for move in moves:
            if move == 'L':
                numl += 1
            if move == 'R':
                numr += 1
            if move == '_':
                underscores += 1
        
        return underscores + max(numl, numr) - min(numl, numr)