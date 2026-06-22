data Tree a = Nil | Leaf a | Node a (Tree a) (Tree a) deriving (Eq, Show)

-- Remove duplicates preserving first occurrence
nubOrd :: Ord a => [a] -> [a]
nubOrd [] = []
nubOrd (x:xs) = x : nubOrd (filter (/= x) xs)

lsortIncr :: Ord a => [a] -> [a]
lsortIncr [] = []
lsortIncr (x:xs) = lsortIncr smaller ++ [x] ++ lsortIncr bigger
  where
    smaller = filter (<= x) xs
    bigger  = filter (> x) xs

lsortDecr :: Ord a => [a] -> [a]
lsortDecr = reverse . lsortIncr

-- List versions: deduplicate then sort
ulsortIncr :: Ord a => [a] -> [a]
ulsortIncr = lsortIncr . nubOrd

ulsortDecr :: Ord a => [a] -> [a]
ulsortDecr = lsortDecr . nubOrd

infix' :: Tree a -> [a]
infix' Nil          = []
infix' (Leaf x)     = [x]
infix' (Node x l r) = infix' l ++ [x] ++ infix' r

fromList :: [a] -> Tree a
fromList [] = Nil
fromList [x] = Leaf x
fromList xs =
  let n     = length xs
      lsize = n `div` 2
      mid   = xs !! lsize
      left  = take lsize xs
      right = drop (lsize + 1) xs
  in Node mid (fromList left) (fromList right)

-- Tree versions: deduplicate, ensure odd count by duplicating minimum, then sort
prepareTree :: Ord a => Tree a -> [a]
prepareTree t =
  let xs = lsortIncr (nubOrd (infix' t))
  in if even (length xs)
     then case xs of
            []    -> []
            (m:_) -> m : xs  -- duplicate minimum to make count odd
     else xs

utsortIncr :: Ord a => Tree a -> Tree a
utsortIncr = fromList . lsortIncr . prepareTree

utsortDecr :: Ord a => Tree a -> Tree a
utsortDecr = fromList . lsortDecr . prepareTree