myGcd :: Int -> Int -> Int
myGcd a 0 = abs a
myGcd 0 b = abs b
myGcd a b = myGcd b (a `mod` b)

isPrime :: Int -> Bool
isPrime n
    | n < 2     = False
    | n == 2    = True
    | even n    = False
    | otherwise = checkDivisors 3
    where
        checkDivisors d
            | d * d > n   = True
            | n `mod` d == 0 = False
            | otherwise   = checkDivisors (d + 2)

myLcm :: Int -> Int -> Int
myLcm a b
    | a == 0 || b == 0 = 0
    | otherwise = abs (a * b) `div` myGcd a b