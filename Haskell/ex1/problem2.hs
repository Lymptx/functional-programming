import Data.List
import Data.Char

frequency :: String -> [(Char, Int)]
frequency s = [(head y, length y) | y <- list] 
    where 
        list = group [x | x <- sort s, isLower x]