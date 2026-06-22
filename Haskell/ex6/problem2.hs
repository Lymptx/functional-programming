data Tree a = Nil | Leaf a | Node a (Tree a) (Tree a) deriving (Eq, Show)

lsortIncr :: Ord a => [a] -> [a]
lsortIncr [] = []
lsortIncr (x:xs) = lsortIncr smaller ++ [x] ++ lsortIncr bigger
  where
    smaller = filter (<= x) xs
    bigger  = filter (> x) xs

lsortDecr :: Ord a => [a] -> [a]
lsortDecr = reverse . lsortIncr

infix' :: Tree a -> [a]
infix' Nil          = []
infix' (Leaf x)     = [x]
infix' (Node x l r) = infix' l ++ [x] ++ infix' r

fromList :: [a] -> Tree a
fromList [] = Nil
fromList [x] = Leaf x
fromList xs =
  let n     = length xs
      -- root at index: ceil(n/2) - 1, so left gets ceil(n/2)-1 elements
      lsize = n `div` 2        -- left subtree element count
      mid   = xs !! lsize
      left  = take lsize xs
      right = drop (lsize + 1) xs
  in Node mid (fromList left) (fromList right)

tsortIncr :: Ord a => Tree a -> Tree a
tsortIncr = fromList . lsortIncr . infix'

tsortDecr :: Ord a => Tree a -> Tree a
tsortDecr = fromList . lsortDecr . infix'