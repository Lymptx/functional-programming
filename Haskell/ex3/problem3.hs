import Data.List (sort)

type Graph = [[Int]]

isReachable :: Graph -> Int -> Int -> Bool
isReachable g m n
  | not (isValidGraph g) = False
  | m < 1 || m > dim    = False
  | n < 1 || n > dim    = False
  | otherwise            = n `elem` reachableFrom g m
  where
    dim = length g

    isValidGraph g =
      not (null g) &&
      all (\row -> length row == dim) g &&
      all (all (`elem` [0,1])) g

    neighbours node = [k | (k, val) <- zip [1..] (g !! (node-1)), val == 1]

    reachableFrom g m = bfs (neighbours m) []

    bfs [] visited = visited
    bfs (x:queue) visited
      | x `elem` visited = bfs queue visited
      | otherwise        = bfs (queue ++ neighbours x) (visited ++ [x])