perfect :: Integer -> Integer -> [Integer]
perfect m n
  | m <= 0 || m > n = [0]
  | otherwise       = [x | x <- [m..n], isPerfect x]
  where
    isPerfect x = x == sumDivisors x
    sumDivisors x = sum [d | d <- [1..x-1], x `mod` d == 0]