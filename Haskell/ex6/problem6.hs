data Tree a = Nil | Node a (Tree a) (Tree a) deriving Show
data HTree a b = HNil |
                 Node1 a (HTree a b) (HTree a b) |
                 Node2 a b (HTree a b) (HTree a b) deriving Show

class Sortable a where
    sortIncr :: a -> a
    sortDecr :: a -> a

class Sortable a => UniqueSorted a where
    usortIncr :: a -> a
    usortDecr :: a -> a

-- Shared sorting primitives
qsortIncr :: Ord a => [a] -> [a]
qsortIncr [] = []
qsortIncr (x:xs) = qsortIncr smaller ++ [x] ++ qsortIncr bigger
  where
    smaller = filter (<= x) xs
    bigger  = filter (> x) xs

qsortDecr :: Ord a => [a] -> [a]
qsortDecr = reverse . qsortIncr

nubOrd :: Eq a => [a] -> [a]
nubOrd [] = []
nubOrd (x:xs) = x : nubOrd (filter (/= x) xs)

-- List instances
instance Ord a => Sortable [a] where
    sortIncr = qsortIncr
    sortDecr = qsortDecr

instance Ord a => UniqueSorted [a] where
    usortIncr = qsortIncr . nubOrd
    usortDecr = qsortDecr . nubOrd

-- Tree helpers
prefix :: Tree a -> [a]
prefix Nil          = []
prefix (Node x l r) = x : prefix l ++ prefix r

treeSize :: Tree a -> Int
treeSize Nil          = 0
treeSize (Node _ l r) = 1 + treeSize l + treeSize r

rebuild :: Tree a -> [a] -> Tree a
rebuild Nil          _      = Nil
rebuild (Node _ l r) (x:xs) =
    let (lxs, rxs) = splitAt (treeSize l) xs
    in Node x (rebuild l lxs) (rebuild r rxs)
rebuild (Node _ _ _) []     = Nil

trimToSize :: Tree a -> Int -> Tree a
trimToSize Nil          _  = Nil
trimToSize _            0  = Nil
trimToSize (Node x l r) n
    | 1 + lsize + rsize <= n = Node x l r
    | 1 + lsize >= n         = Node x (trimToSize l (n-1)) Nil
    | otherwise              = Node x l (trimToSize r (n - 1 - lsize))
  where
    lsize = treeSize l
    rsize = treeSize r

instance Ord a => Sortable (Tree a) where
    sortIncr t = rebuild t (qsortIncr (prefix t))
    sortDecr t = rebuild t (qsortDecr (prefix t))

instance Ord a => UniqueSorted (Tree a) where
    usortIncr t = rebuild (trimToSize t n) (qsortIncr unique)
      where unique = nubOrd (qsortIncr (prefix t))
            n      = length unique
    usortDecr t = rebuild (trimToSize t n) (qsortDecr unique)
      where unique = nubOrd (qsortIncr (prefix t))
            n      = length unique

-- HTree helpers
hprefix :: HTree a b -> [a]
hprefix HNil             = []
hprefix (Node1 x l r)   = x : hprefix l ++ hprefix r
hprefix (Node2 x _ l r) = x : hprefix l ++ hprefix r

hprefix2 :: HTree a b -> [(a, Maybe b)]
hprefix2 HNil             = []
hprefix2 (Node1 x l r)   = (x, Nothing) : hprefix2 l ++ hprefix2 r
hprefix2 (Node2 x y l r) = (x, Just y)  : hprefix2 l ++ hprefix2 r

htreeSize :: HTree a b -> Int
htreeSize HNil             = 0
htreeSize (Node1 _ l r)   = 1 + htreeSize l + htreeSize r
htreeSize (Node2 _ _ l r) = 1 + htreeSize l + htreeSize r

hrebuild :: HTree a b -> [a] -> HTree a b
hrebuild HNil            _      = HNil
hrebuild (Node1 _ l r)   (x:xs) =
    let (lxs, rxs) = splitAt (htreeSize l) xs
    in Node1 x (hrebuild l lxs) (hrebuild r rxs)
hrebuild (Node2 _ y l r) (x:xs) =
    let (lxs, rxs) = splitAt (htreeSize l) xs
    in Node2 x y (hrebuild l lxs) (hrebuild r rxs)
hrebuild _ [] = HNil

htrimToSize :: HTree a b -> Int -> HTree a b
htrimToSize HNil            _  = HNil
htrimToSize _               0  = HNil
htrimToSize (Node1 x l r)   n
    | 1 + lsize + rsize <= n   = Node1 x l r
    | 1 + lsize >= n           = Node1 x (htrimToSize l (n-1)) HNil
    | otherwise                = Node1 x l (htrimToSize r (n - 1 - lsize))
  where
    lsize = htreeSize l
    rsize = htreeSize r
htrimToSize (Node2 x y l r) n
    | 1 + lsize + rsize <= n   = Node2 x y l r
    | 1 + lsize >= n           = Node2 x y (htrimToSize l (n-1)) HNil
    | otherwise                = Node2 x y l (htrimToSize r (n - 1 - lsize))
  where
    lsize = htreeSize l
    rsize = htreeSize r

instance (Ord a, Ord b) => Sortable (HTree a b) where
    sortIncr t = hrebuild t (qsortIncr (hprefix t))
    sortDecr t = hrebuild t (qsortDecr (hprefix t))

instance (Ord a, Ord b) => UniqueSorted (HTree a b) where
    usortIncr t = hrebuild (htrimToSize t n) (qsortIncr (map fst unique))
      where unique = nubOrd (hprefix2 t)
            n      = length unique
    usortDecr t = hrebuild (htrimToSize t n) (qsortDecr (map fst unique))
      where unique = nubOrd (hprefix2 t)
            n      = length unique