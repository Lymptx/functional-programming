count :: String -> Char -> Int
count s c = length [x | x <- s, x == c]