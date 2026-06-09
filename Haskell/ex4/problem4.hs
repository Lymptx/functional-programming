data Classification = NoWay | OneWay | ManyWays | InvalidInput deriving Show
data Graph = Empty | GNode Int [Int] Graph

contains :: Graph -> Int -> Bool
contains Empty _ = False
contains (GNode v neighbors rest) x = x == v || x `elem` neighbors || contains rest x

neighborsOf :: Graph -> Int -> [Int]
neighborsOf Empty _ = []
neighborsOf (GNode v neighbors rest) x
    | v == x    = neighbors
    | otherwise = neighborsOf rest x

countPathes :: Graph -> Int -> Int -> Int -> Int
countPathes _ _ _ 2 = 2
countPathes g m n counter
    | n `elem` ns = 1 + sum [ countPathes g next n (counter+1) | next <- ns ]
    | otherwise   = sum [ countPathes g next n counter | next <- ns ]
  where
    ns = neighborsOf g m

pathes :: Graph -> Int -> Int -> Classification
pathes Empty _ _ = NoWay
pathes g m n
    | any (\x -> not (contains g x)) [m,n] = InvalidInput
    | count == 1 = OneWay
    | count == 0 = NoWay
    | otherwise = ManyWays
    where   
        count = countPathes g m n 0

--i would recommend not crossing this because my implementation is incomplete and difficult to understand