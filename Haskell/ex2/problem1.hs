import Data.List (nub)

outstanding :: [Int] -> [Int]
outstanding l = [x | x <- l, length (filter (== x) l) == 1]