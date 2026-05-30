import Data.List (isPrefixOf)

type Editor = [Char]

replace :: Editor -> Int -> String -> String -> Editor
replace e i s t
  | i <= 0    = e
  | null s    = e
  | otherwise = go e i
  where
    sLen = length s
    go [] _         = []
    go remaining n
      | n <= 0              = remaining
      | s `isPrefixOf` remaining = t ++ go (drop sLen remaining) (n - 1)
      | otherwise           = head remaining : go (tail remaining) n