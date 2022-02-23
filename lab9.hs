-- James Schader
-- 20220223

sumOfFirstN :: Integral a => a -> a
sumOfFirstN n = n * (n+1) `div` 2

divBy15 :: Integral x => Double -> Bool
divBy15 x = (0 == (x `mod` 15))

points :: Integral a => a -> a
points n
  | (9 < n) || (n < 1) = 0
  | (n <= 3)           = 12 - 2*n
  | otherwise          = 9 - n
