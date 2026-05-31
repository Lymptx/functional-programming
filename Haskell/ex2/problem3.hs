type Relation = [(Int, Int)]

isAntisym :: [(Int, Int)] -> Bool
isAntisym r = all check r
  where
    check (x, y) = if (y, x) `elem` r
                   then x == y
                   else Truep3