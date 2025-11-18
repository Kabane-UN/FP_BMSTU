flatten :: [[Integer]] -> (Integer -> Bool) -> [Integer]
flatten lists predicate = concat $ filter (predicate . toInteger . length) lists

partition :: [Integer] -> Integer -> ([Integer], [Integer])
partition list n = ([x | x <- list, x < n], [x | x <- list, x >= n])

sorted :: (Integer -> Integer -> Bool) -> [Integer] -> Bool
sorted cmp list = all (uncurry cmp) (zip list (tail list))

chunksOf :: Integer -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = fxs : chunksOf n sxs
    where (fxs, sxs) = splitAt (fromIntegral n) xs

slices :: Integer -> [Integer] -> [[Integer]]
slices n list
    | n <= 0 = []
    | otherwise = chunksOf n list


data RegExp
    = EmptySet
    | Epsilon
    | Symbol Char
    | Union RegExp RegExp
    | Concat RegExp RegExp
    | Star RegExp
    deriving (Show, Eq)

simplify :: RegExp -> RegExp
simplify EmptySet = EmptySet
simplify Epsilon = Epsilon
simplify (Symbol c) = Symbol c
simplify (Concat r1 r2) =
    case (simplify r1, simplify r2) of
        (EmptySet, _) -> EmptySet
        (_, EmptySet) -> EmptySet
        (Epsilon, r) -> r
        (r, Epsilon) -> r
        (r1', r2') -> Concat r1' r2'
simplify (Union r1 r2) =
    case (simplify r1, simplify r2) of
        (EmptySet, r) -> r
        (r, EmptySet) -> r
        (r1', r2') -> Union r1' r2'
simplify (Star r) =
    case simplify r of
        EmptySet -> Epsilon
        Epsilon -> Epsilon
        r' -> Star r'

bubbleSort :: Ord a => [a] -> [a]
bubbleSort s = case bubbleSort' s of
               t | t == s    -> t
                 | otherwise -> bubbleSort t
  where bubbleSort' (x:x2:xs) | x > x2    = x2:bubbleSort' (x:xs)
                         | otherwise = x:bubbleSort' (x2:xs)
        bubbleSort' s = s