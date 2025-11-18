% Лабораторная работа № 3. Решение задач на языке Haskell:
  программирование без побочных эффектов
% 16 октября 2025 г.
% Андрей Кабанов, ИУ9-11М

# Цель работы
Получение навыков составления программ на языке Haskell.

# Индивидуальный вариант
* Функция `flatten :: [[Integer]] -> (Integer -> Boolean) -> [Integer]`, 
выполняющая конкатенацию списков целых чисел, находящихся в списке 
списков целых чисел и имеющих длину, удовлетворяющую предикату.

* Функция `partition :: [Integer] -> Integer -> ([Integer], [Integer])`, 
разделяющая список целых чисел на два списка :: в первый список помещаются 
числа, которые меньше указанного числа, а во второй — числа, которые не меньше.

* Функция `sorted :: (Integer -> Integer -> Boolean) -> [Integer] -> Boolean`, 
принимающая функцию сравнения двух целых чисел и возвращающая функцию, 
определяющую, является ли список целых чисел отсортированным в соответствии 
с функцией сравнения.

* Закаренная функция `slices :: Integer -> [Integer] -> [[Integer]]`, 
выполняющая разбиение списка целых чисел на фрагменты указанной 
в качестве параметра функции длины.

Регулярное выражение описано следующим абстрактным синтаксисом:

`RegEx → ∅ | ε | SYMBOL | RegEx ⋃ RegEx | RegEx · RegEx | RegEx*`  
Здесь ∅ — пустое множество, ⋃ — объединение, · — конкатенация.

Требуется написать функцию `simplify :: RegExp -> RegExp`, 
упрощающую регулярное выражение:
```
r∅ = ∅r = ∅
r ∪ ∅ = ∅ ∪ r = r
rε = εr = r
ε* = ∅* = ε
```


# Реализация

```haskell
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
```

# Тестирование

```
GHCi, version 9.6.7: https://www.haskell.org/ghc/  :? for help
ghci> :l Main.hs
[1 of 2] Compiling Main             ( Main.hs, interpreted )
Ok, one module loaded.
ghci> flatten [[1,2,3], [4,5], [6,7,8,9], [10]] (\len -> len `mod` 2 == 0)
[4,5,6,7,8,9]
ghci> let li = [1, 1000, 3, 4, 7, 6, 7, -30, 9, 10]
ghci> bubbleSort li
[-30,1,3,4,6,7,7,9,10,1000]
ghci> bubbleSort li
[-30,1,3,4,6,7,7,9,10,1000]
ghci> li
[1,1000,3,4,7,6,7,-30,9,10]
ghci> sorted (<) li
False
ghci> sorted (<) (bubbleSort li)
False
ghci> sorted (>) (bubbleSort li)
False
ghci> sorted (<) (bubbleSort li)
False
ghci> sorted (<) [1, 2, 3]
True
ghci> sorted (<=) (bubbleSort li)
True
ghci> sorted (<=) (li)
False
ghci> slices 3 (li)
[[1,1000,3],[4,7,6],[7,-30,9],[10]]
ghci> slices 2 (li)
[[1,1000],[3,4],[7,6],[7,-30],[9,10]]
ghci> partition li 2
([1,-30],[1000,3,4,7,6,7,9,10])
ghci> partition li 10
([1,3,4,7,6,7,-30,9],[1000,10])
ghci> simplify (Concat (Symbol 'a') EmptySet)
EmptySet
ghci> simplify (Concat (Symbol 'a') Epsilon)
Symbol 'a'
ghci> simplify (Union (Symbol 'a') EmptySet)
Symbol 'a'
```

# Вывод
В ходе выполнения данной лабораторной работы студент научился писать 
чистые функции на языке программирования Haskell. Освоение данного навыка 
не вызвало значительных трудностей, так как встроенные функции и используемые 
техники в Haskell во многом схожи с возможностями некоторых 
мультипарадигмальных языков программирования, таких как Python 
(например, функции zip, filter, а также конструкции list comprehension).