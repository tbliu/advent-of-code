# Definition for a binary tree node.
# class TreeNode:
#     def __init__(self, val=0, left=None, right=None):
#         self.val = val
#         self.left = left
#         self.right = right
class Solution:
    def traverse(self, node, traversal):
        if node is None:
            return 
        self.traverse(node.left, traversal)
        traversal.append(node.val)
        self.traverse(node.right, traversal)
        return
        
    def inorderTraversal(self, root: Optional[TreeNode]) -> List[int]:
        traversal = []
        self.traverse(root, traversal)
        return traversal
