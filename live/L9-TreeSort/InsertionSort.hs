{-# LANGUAGE GADTs #-}

-- Advanced Functional Programming
-- Chalmers TDA342 / GU DIT260

-- | Binary search tree.

data BST a where
  Leaf :: BST a
  Node :: a         -- ^ Payload / pivot element @p@.
       -> BST a    -- ^ Left  subtree containing only elements @<= p@.
       -> BST a    -- ^ Right subtree containing only elements @>= p@.
       -> BST a

-- | Insert into binary search tree.

insert :: Ord a => a -> BST a -> BST a
insert p Leaf           = Node p Leaf Leaf
insert p (Node q lt rt)
  | p <= q    = Node q (insert p lt) rt
  | otherwise = Node q lt (insert p rt)

-- | Building a BST from a list.

tree :: Ord a => [a] -> BST a
tree []       = Leaf
tree (x : xs) = insert x (tree xs)

-- | In-order flattening of a BST.

flatten :: BST a -> [a]
flatten Leaf = []
flatten (Node p lt rt) = flatten lt ++ p : flatten rt

-- | Sorting is flatten ∘ tree.

sort :: Ord a => [a] -> [a]
sort xs = flatten (tree xs)
