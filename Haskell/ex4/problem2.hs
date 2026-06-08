data BTree = Leaf Char | BNode Int BTree BTree deriving (Show, Eq)

topmirror :: BTree -> BTree
topmirror (Leaf c)             = Leaf c
topmirror (BNode x left right) = BNode x right left