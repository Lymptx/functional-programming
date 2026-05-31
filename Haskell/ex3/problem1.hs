import Data.List (nub, sort)

toh :: Int -> Char -> Char -> Char -> [(Char, Char)]
toh n a b c
  | n <= 0                          = []
  | sort [a,b,c] /= ['A','B','C']   = []
  | nub [a,b,c] /= [a,b,c]         = []
  | otherwise                       = move n a b c
  where
    move 0 _ _ _ = []
    move n a b c = move (n-1) a c b
                ++ [(a, b)]
                ++ move (n-1) c b a