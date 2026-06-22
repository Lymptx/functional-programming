data Tree a = Nil | Node a (Tree a) (Tree a) deriving Show

class Sortable a where
    sortIncr :: a -> a
    sortDecr :: a -> a

-- List sorting
qsortIncr :: Ord a => [a] -> [a]
qsortIncr [] = []
qsortIncr (x:xs) = qsortIncr smaller ++ [x] ++ qsortIncr bigger
  where
    smaller = filter (<= x) xs
    bigger  = filter (> x) xs

qsortDecr :: Ord a => [a] -> [a]
qsortDecr = reverse . qsortIncr

instance Ord a => Sortable [a] where
    sortIncr = qsortIncr
    sortDecr = qsortDecr

-- Prefix traversal: root, left, right
prefix :: Tree a -> [a]
prefix Nil          = []
prefix (Node x l r) = x : prefix l ++ prefix r

-- Rebuild tree with same structure but new labels (prefix order)
rebuild :: Tree a -> [a] -> Tree a
rebuild Nil        _      = Nil
rebuild (Node _ l r) (x:xs) =
    let lsize        = treeSize l
        (lxs, rxs)   = splitAt lsize xs
    in Node x (rebuild l lxs) (rebuild r rxs)
rebuild (Node _ _ _) [] = Nil

treeSize :: Tree a -> Int
treeSize Nil          = 0
treeSize (Node _ l r) = 1 + treeSize l + treeSize r

instance Ord a => Sortable (Tree a) where
    sortIncr t = rebuild t (qsortIncr (prefix t))
    sortDecr t = rebuild t (qsortDecr (prefix t))