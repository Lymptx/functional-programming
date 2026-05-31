import Data.List (nub)

ps :: [Int] -> [[Int]]
ps l
  | nub l /= l = [[], []]
  | otherwise  = foldr addElement [[]] l
  where
    addElement x subsets = subsets ++ map (x:) subsets