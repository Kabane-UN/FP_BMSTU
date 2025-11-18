% Лабораторная работа № 5. Решение задач на языке Haskell:
  программирование с побочными эффектами
% 6 ноября 2025 г.
% Андрей Кабанов, ИУ9-11М

# Цель работы
Получение навыков вычислений в монадах на языке программирования Haskell.

Реализуйте один из алгоритмов сортировки списка (см. индивидуальный вариант), 
например, быстрой, выбором, вставками. Пусть функции сортировки принимают два 
аргумента: предикат для сравнения элементов и исходный список. В реализациях 
не обращайтесь к элементу списка по его индексу. При необходимости используйте 
конструкции языка Haskell для включения элементов в список (списковые включения, 
list comprehension). Оцените вычислительную сложность. Экспериментально определите
 время, затрачиваемое вашими функциями на сортировку списков различной длины. Чем 
 вы можете объяснить низкую эффективность полученных реализаций?

Предложите альтернативные реализации тех же алгоритмов сортировки на основе модулей 
Data.Array из стандартной библиотеки Haskell Platform или Data.Vector из кодовой базы 
Hackage. Снова определите время, затрачиваемое затрачиваемое полученными функциями 
на сортировку списков различной длины. Достигнуто ли повышение производительности?

# Индивидуальный вариант
Сортировка прямым выбором.

# Реализация и тестирование

```haskell
{-# LANGUAGE AllowAmbiguousTypes #-}

import Control.DeepSeq (NFData, rnf)
import Control.Exception (evaluate)
import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.IO
import Data.Array.ST
import Data.List (minimumBy)
import Data.Ord (comparing)
import System.CPUTime
import Text.Printf

selectionSort :: (Eq a) => (a -> a -> Bool) -> [a] -> [a]
selectionSort _ [] = []
selectionSort cmp xs =
  let x = select cmp xs
   in x : selectionSort cmp (remove x xs)

select :: (a -> a -> Bool) -> [a] -> a
select _ [x] = x
select cmp (x : xs) =
  let m = select cmp xs
   in if cmp x m then x else m

remove :: (Eq a) => a -> [a] -> [a]
remove _ [] = []
remove y (x : xs)
  | x == y = xs
  | otherwise = x : remove y xs

selectionSortArray ::
  (Ix i, Ord a, Enum i) =>
  (a -> a -> Bool) ->
  Array i a ->
  Array i a
selectionSortArray cmp arr = go startIdx arr
  where
    (startIdx, endIdx) = bounds arr
    idxs = range (startIdx, endIdx)

    go current arr'
      | current == endIdx = arr'
      | otherwise =
          let minIdx =
                findMinIndex
                  cmp
                  arr'
                  current
                  (tail $ dropWhile (/= current) idxs)
           in if minIdx == current
                then go (succ current) arr'
                else go (succ current) (swapElements arr' current minIdx)

    findMinIndex cmp arr' current [] = current
    findMinIndex cmp arr' current (j : js) =
      if cmp (arr' ! j) (arr' ! current)
        then findMinIndex cmp arr' j js
        else findMinIndex cmp arr' current js

    swapElements arr' i j = arr' // [(i, arr' ! j), (j, arr' ! i)]

timeIt :: (NFData a) => IO a -> IO (a, Double)
timeIt action = do
  start <- getCPUTime
  result <- action
  evaluate (rnf result)
  end <- getCPUTime
  let diff = fromIntegral (end - start) / (10 ^ 12)
  return (result, diff)

randlist :: Int -> Int -> (Int, Int) -> [Int]
randlist 0 _ _ = []
randlist n cur (mul, max) = cur : randlist (n - 1) (cur * mul `mod` max) (mul, max)

main :: IO ()
main = do
  let sizes = [500, 1000, 2000, 5000, 10000]
  forM_ sizes $ \n -> do
    putStrLn $ "\nРазмер списка: " ++ show n
    let xs = randlist n 7 (13, 101)

    (returnL, tList) <- timeIt $ evaluateList xs
    (returnA, tArray) <- timeIt $ evaluateArray xs

    printf "List sort:     %.4f sec\n" tList
    printf "Array sort:    %.4f sec\n" tArray

  putStrLn "\nСравнение завершено."
  print $ randlist 20 7 (13, 101)
  print $ selectionSort (<) [5, 2, 4, 1, 3]
  print $ selectionSortArray (<) (listArray (0, 5 - 1) [5, 2, 4, 1, 3])

evaluateList :: [Int] -> IO [Int]
evaluateList xs = return $ selectionSort (<) xs

evaluateArray :: [Int] -> IO (Array Int Int)
evaluateArray xs = return $ selectionSortArray (<) (listArray (0, length xs - 1) xs)

```

# Вывод
В ходе работы были получены навыки реализации алгоритма сортировки прямым выбором в 
функциональном стиле. Сравнение производительности показало, что переход от 
списков к массивам ухудшает производительность за счет создания каждый раз 
нового массива.