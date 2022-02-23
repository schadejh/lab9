-- James Schader
-- 20220223

sumOfFirstN :: Integral a => a -> a
sumOfFirstN n = n * (n+1) `div` 2

divBy15 :: Integral a => a -> Bool
divBy15 x = (0 == (x `mod` 15))

points :: Integral a => a -> a
points n
  | (n < 1) || (n > 9) = 0
  | (n <= 3)           = 12 - 2*n
  | otherwise          = 9 - n

nextOdd :: Integral p => p -> p
nextOdd p
  | (1 == p `mod` 2) = 2 + p
  | otherwise        = 1 + p

sumList :: Num p => [p] -> p
sumList [] = 0
sumList (x:xs) = x + (sumList xs)

myHead :: [a] -> a
myHead (x:xs) = x

myLength :: [a] -> Int
myLength list
  | null list = 0
  | otherwise = 1 + myLength (tail list)

myLast :: [a] -> a
myLast (x:xs)
  | length (x:xs) == 1 = x
  | otherwise = myLast (tail (x:xs))

fib :: Integral a => a -> a
fib p
  | p == 0 = 1
  | p == 1 = 1
  | otherwise = fib (p-1) + fib (p-2)

ffib :: Integral a => a -> a
ffib n = fibAccum 1 1 n
fibAccum e0 e1 k
  | (k == 0) = e0
  | (k == 1) = e1
  | otherwise = fibAccum e1 (e0+e1) (k-1)

