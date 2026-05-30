import Data.List

gmean :: [Int] -> Int
gmean a
  | null a    = -1
  | any (>1) [length x | x <- group sorted] = -1
  | otherwise = head (snd (splitAt middle sorted))
  where
    sorted = sort a
    middle = div (length a - 1) 2