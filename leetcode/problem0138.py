"""
# Definition for a Node.
class Node:
    def __init__(self, x: int, next: 'Node' = None, random: 'Node' = None):
        self.val = int(x)
        self.next = next
        self.random = random
"""

class Solution:
    def copyRandomList(self, head: 'Optional[Node]') -> 'Optional[Node]':
        if head is None:
            return None
        
        node_to_index = {}
        copied_list = []
        original_list = []
        
        i = 0
        ptr = head 
        
        while ptr is not None:
            # Map the node to its index for easier `random`` retrieval
            node_to_index[ptr] = i
            
            # Copied nodes will have the same index as their corresponding original nodes
            copied_list.append(Node(ptr.val))
            original_list.append(ptr)

            ptr = ptr.next
            i += 1
            
        copied_list.append(None) # For the last node's next
        ptr = head
        
        for i in range(len(copied_list)-1):
            new_node = copied_list[i]
            new_node.next = copied_list[i+1]
            
            original = original_list[i]
            original_random = original.random
            if original_random is None:
                new_random = None
            else:
                index_of_random = node_to_index[original_random]
                new_random = copied_list[index_of_random]
            
            new_node.random = new_random
                
        return copied_list[0]