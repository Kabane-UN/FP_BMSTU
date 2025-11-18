data Tree k v = Leaf | Node k v (Tree k v) (Tree k v) deriving (Show, Eq)

minKV :: Ord k => Tree k v -> Maybe (k, v)
minKV Leaf = Nothing
minKV (Node k v Leaf _) = Just (k, v)
minKV (Node _ _ left _) = minKV left

maxKV :: Ord k => Tree k v -> Maybe (k, v)
maxKV Leaf = Nothing
maxKV (Node k v _ Leaf) = Just (k, v)
maxKV (Node _ _ _ right) = maxKV right

remove :: Ord k => Tree k v -> k -> (Tree k v, Maybe v)
remove Leaf key = (Leaf, Nothing)
remove (Node k v left right) key
    | key < k =
        let (newLeft, result) = remove left key
        in (Node k v newLeft right, result)
    | key > k =
        let (newRight, result) = remove right key
        in (Node k v left newRight, result)
    | otherwise =
        case (left, right) of
            (Leaf, Leaf) -> (Leaf, Just v)
            (Leaf, _) -> (right, Just v)
            (_, Leaf) -> (left, Just v)
            _ ->
                let Just (minKey, minVal) = minKV right
                    (newRight, _) = remove right minKey
                in (Node minKey minVal left newRight, Just v)

exampleTree :: Tree Int String
exampleTree =
    Node 5 "five"
        (Node 3 "three"
            (Node 1 "one" Leaf Leaf)
            (Node 4 "four" Leaf Leaf))
        (Node 8 "eight"
            (Node 6 "six" Leaf Leaf)
            (Node 10 "ten" Leaf Leaf))


main :: IO ()
main = do
    putStrLn "Исходное дерево:"
    print exampleTree
   
    putStrLn "\nМинимальная пара:"
    print $ minKV exampleTree  -- Just (1,"one")
   
    putStrLn "\nМаксимальная пара:"
    print $ maxKV exampleTree -- Just (10, "ten")
    putStrLn "\nУдаление:"
    let (newTree, value) = remove exampleTree 10
    print newTree
    print value
    let (newTree, value) = remove exampleTree 8
    print newTree
    print value



