-- James Schader
-- 20220223

sumOfFirstN :: Integral a => a -> a
sumOfFirstN n = n * (n+1) `div` 2

divBy15 :: Integral a => a -> Bool
divBy15 x = (0 == (x `mod` 15))

points :: Integral a => a -> a
points n
  | (9 < n) || (n < 1) = 0
  | (n <= 3)           = 12 - 2*n
  | otherwise          = 9 - n

nextOdd :: Num p => p -> Integer
nextOdd n
	| (1 == n `mod` 2) = 2 + n
	| otherwise        = 1 + n

sumList :: Num p => [p] -> p
sumList [] = 0
sumList (x:xs) = x + (sumList xs)

--myHead :: Integral a => [a] -> a
--myHead (x:xs) = x

--myLength :: [a] -> Int
--myLength [a]
--	| null list = 0
--	| otherwise = length list

--myLast :: ...

fib :: Num p => p -> Integer
fib n
	| 0 = 1
	| 1 = 1
	| otherwise = fib (n-1) + fib (n-2) 
