# Definition for a binary tree node.
# class TreeNode:
#     def __init__(self, val=0, left=None, right=None):
#         self.val = val
#         self.left = left
#         self.right = right
class Solution:
    def postorder(self, node, parent, stack):
        if node is None:
            return
        self.postorder(node.left, node, stack)
        self.postorder(node.right, node, stack)
        stack.append((node, parent))

    def removeLeafNodes(self, root: Optional[TreeNode], target: int) -> Optional[TreeNode]:
        if root is None:
            return None

        # Run post-order traversal to visit both children first before parent
        stack = []
        self.postorder(root, None, stack)
        
        for node, parent in stack:
            if parent is None:  # edge case - handled below
                continue

            if node.val == target and node.left is None and node.right is None:
                if node == parent.left:
                    parent.left = None
                else:
                    parent.right = None

        
        if root.left is None and root.right is None and root.val == target:
            return None
        return root
