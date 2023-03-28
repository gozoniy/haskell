
logList [] = []
logList (x:xs)=log x:logList xs

sumList :: Num a => [a] -> a
sumList [] = 0
sumList (x:xs) = x + sumList xs
multList [] = 1 
multList (x:xs) = x * multList xs

getNegative x = filter (< 0) x




--1.1--
avGList :: [Double] -> Double
avGList x = foldr (*) 1 x ** 1/len x
len [] = 0
len (x:xs)=1 + len xs

--1.2--
scalar a b = foldr (+) 0 (zipWith (*) a b)

--1.3--
countNegat x = len (getNegative x)

--1.4--
quicksort [] = []
quicksort (x:xs) = quicksort(filter (<x) xs) ++ [x] ++ quicksort (filter (>=x) xs)

--1.5--
bSort :: (a -> a -> Bool) -> [a] -> [a]
bSort a [] = []
bSort a (x:xs) = bSort a (filter (a x) xs) ++ [x] ++ bSort a (filter (not . a x) xs)

-- Вторая часть Задание 12
-- 2.12.Функция, которая меняет знак всех положительных элементов списка чисел, 
-- например: по [-1, 0, 5, -10, -20] дает [-1,0,-5,-10,-20]
negative :: [Int] -> [Int]
negative= map (\n -> (abs n)*(-1))

-- Вторая часть Задание 4
-- 2.4.Функция вычленения n -го элемента из заданного списка. 
vichlenit' x n = map snd(filter (\(a,b)-> a /=n) (zip [0..] x))