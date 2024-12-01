# Definition for a binary tree node.
# class TreeNode:
#     def __init__(self, val=0, left=None, right=None):
#         self.val = val
#         self.left = left
#         self.right = right
class Solution:
    def evaluateTree(self, node: Optional[TreeNode]) -> bool:
        if node is None:
            return False
        if node.val == 0:
            return False
        if node.val == 1:
            return True
        
        if node.val == 2:
            return self.evaluateTree(node.left) or self.evaluateTree(node.right)

        if node.val == 3:
            return self.evaluateTree(node.left) and self.evaluateTree(node.right)
