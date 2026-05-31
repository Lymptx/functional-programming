import Data.List (sort)

type Graph = [[Int]]

reachableFrom :: Graph -> Int -> [Int]
reachableFrom g m
  | not (isValidGraph g) = []
  | m < 1 || m > dim    = []
  | otherwise            = sort (bfs (neighbours m) [])
  where
    dim = length g

    isValidGraph g =
      not (null g) &&
      all (\row -> length row == dim) g &&
      all (all (`elem` [0,1])) g

    neighbours node = [n | (n, val) <- zip [1..] (g !! (node-1)), val == 1]

    bfs [] visited = visited
    bfs (x:queue) visited
      | x `elem` visited = bfs queue visited
      | otherwise        = bfs (queue ++ neighbours x) (visited ++ [x])