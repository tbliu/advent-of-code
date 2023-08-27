class Solution:
    def reorganizeString(self, s: str) -> str:
        freqs = {}
        for char in s:
            if char not in freqs:
                freqs[char] = 0
            freqs[char] += 1
        
        # Python uses min heaps
        heap = []
        for char in freqs:
            heap.append((-freqs[char], char))
        heapq.heapify(heap)
        
        rearranged = []
        while heap != []:
            freq1, char1 = heapq.heappop(heap)
            rearranged.append(char1)

            if heap != []:
                freq2, char2 = heapq.heappop(heap)
            else:
                break

            rearranged.append(char2)
            
            if freq1 + 1 < 0:
                heapq.heappush(heap, (freq1+1, char1))
            
            if freq2 + 1 < 0:
                heapq.heappush(heap, (freq2+1, char2))
        
        
        if len(rearranged) != len(s):
            return ""
        return ''.join(rearranged)
        