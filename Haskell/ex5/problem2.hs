data Tree = Nil | Node Int Tree Tree

cutoff :: Tree -> Int -> Tree
cutoff Nil _           = Nil
cutoff _ t | t < 0    = Nil
cutoff (Node x l r) 0 = Node x Nil Nil
cutoff (Node x l r) t = Node x (cutoff l (t-1)) (cutoff r (t-1))