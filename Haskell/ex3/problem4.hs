palFree :: String -> Int -> Bool
palFree t n = all notLongPalindrome (substrings t)
  where
    notLongPalindrome s = not (length s >= n && isPalindrome s)

    isPalindrome s = s == reverse s

    substrings s = [take len (drop start s)
                   | start <- [0..length s - 1]
                   , len   <- [1..length s - start]]