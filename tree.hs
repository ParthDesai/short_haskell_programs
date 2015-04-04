data Tree a = EmptyTree | Node{val :: a, left :: Tree a, right :: Tree a}

treeInsert :: (Ord a) => Tree a -> a -> Tree a
treeInsert EmptyTree val = Node val EmptyTree EmptyTree
treeInsert (Node n left right) val = if n > val then (Node n left (treeInsert right val)) else (Node n (treeInsert left val) right) 

treeFind :: (Ord a) => Tree a -> a -> Bool
treeFind EmptyTree val = False
treeFind (Node n left right) val = case n `compare` val of EQ -> True
							   LT -> treeFind right val
							   GT -> treeFind left val 


instance Functor Tree where
	fmap f EmptyTree = EmptyTree
	fmap f (Node val left right) = Node (f val) (fmap f left) (fmap f right)
