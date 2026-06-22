data Tree a = Nil | Node a (Tree a) (Tree a) deriving Show

class Sortable a where
    sortIncr :: a -> a
    sortDecr :: a -> a

class Sortable a => UniqueSorted a where
    usortIncr :: a -> a
    usortDecr :: a -> a

qsortIncr :: Ord a => [a] -> [a]
qsortIncr [] = []
qsortIncr (x:xs) = qsortIncr smaller ++ [x] ++ qsortIncr bigger
  where
    smaller = filter (<= x) xs
    bigger  = filter (> x) xs

qsortDecr :: Ord a => [a] -> [a]
qsortDecr = reverse . qsortIncr

nubOrd :: Ord a => [a] -> [a]
nubOrd [] = []
nubOrd (x:xs) = x : nubOrd (filter (/= x) xs)

instance Ord a => Sortable [a] where
    sortIncr = qsortIncr
    sortDecr = qsortDecr

instance Ord a => UniqueSorted [a] where
    usortIncr = qsortIncr . nubOrd
    usortDecr = qsortDecr . nubOrd

prefix :: Tree a -> [a]
prefix Nil          = []
prefix (Node x l r) = x : prefix l ++ prefix r

treeSize :: Tree a -> Int
treeSize Nil          = 0
treeSize (Node _ l r) = 1 + treeSize l + treeSize r

rebuild :: Tree a -> [a] -> Tree a
rebuild Nil          _      = Nil
rebuild (Node _ l r) (x:xs) =
    let lsize      = treeSize l
        (lxs, rxs) = splitAt lsize xs
    in Node x (rebuild l lxs) (rebuild r rxs)
rebuild (Node _ _ _) [] = Nil

trimToSize :: Tree a -> Int -> Tree a
trimToSize Nil          _              = Nil
trimToSize _            0              = Nil
trimToSize (Node x l r) n
    | 1 + lsize + rsize <= n           = Node x l r
    | 1 + lsize >= n                   = Node x (trimToSize l (n-1)) Nil
    | otherwise                        = Node x l (trimToSize r (n - 1 - lsize))
  where
    lsize = treeSize l
    rsize = treeSize r

rebuildUnique :: Tree a -> [a] -> Tree a
rebuildUnique t xs = rebuild (trimToSize t (length xs)) xs

instance Ord a => Sortable (Tree a) where
    sortIncr t = rebuild t (qsortIncr (prefix t))
    sortDecr t = rebuild t (qsortDecr (prefix t))

instance Ord a => UniqueSorted (Tree a) where
    usortIncr t = rebuildUnique t (qsortIncr (nubOrd (prefix t)))
    usortDecr t = rebuildUnique t (qsortDecr (nubOrd (prefix t)))