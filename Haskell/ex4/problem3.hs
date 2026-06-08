data BTree = Leaf Char | BNode Int BTree BTree deriving (Show, Eq)

mirror :: BTree -> BTree
mirror (Leaf c)             = Leaf c
mirror (BNode x left right) = BNode x (mirror right) (mirror left)