data Tree a = Nil | Node a (Tree a) (Tree a)

instance Eq a => Eq (Tree a) where
    Nil          == Nil          = True
    Node x l r   == Node y m s  = x == y && l == m && r == s
    _            == _            = False

instance Ord a => Ord (Tree a) where
    s <= t = case (s, t) of
        (Nil, _)                   -> True
        (Node _ _ _, Nil)          -> False
        (Node x l r, Node y m s') -> x <= y && l <= m && r <= s'