data Tree = Nil | Node Int Tree Tree

layer :: Tree -> Int -> [Int]
layer Nil _           = []
layer _ t | t < 0    = []
layer (Node x l r) 0 = [x]
layer (Node x l r) t = layer l (t-1) ++ layer r (t-1)

treesum :: Tree -> Int -> Integer
treesum _ t | t < 0 = 0
treesum b t         = sum [toInteger x | level <- [0..t], x <- layer b level]