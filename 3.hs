
logList [] = []
logList (x:xs)=log x:logList xs

sumList :: Num a => [a] -> a
sumList [] = 0
sumList (x:xs) = x + sumList xs
multList [] = 1 
multList (x:xs) = x * multList xs

getNegative x = filter (< 0) x

avGList :: [Double] -> Double
avGList x = foldr (*) 1 x ** 1/len x
len [] = 0
len (x:xs)=1 + len xs

scalar a b = foldr (+) 0 (zipWith (*) a b)

countNegat x = len (getNegative x)

quicksort [] = []
quicksort (x:xs) = quicksort(filter (<x) xs) ++ [x] ++ quicksort (filter (>=x) xs)

bSort a [] = []
bSort a (x:xs) = bSort a (filter (a x) xs) ++ [x] ++ bSort a (filter (not . a x) xs)

-- Вторая часть Задание 12
-- 2.12.Функция, которая меняет знак всех положительных элементов списка чисел, 
-- например: по [-1, 0, 5, -10, -20] дает [-1,0,-5,-10,-20]

negative :: [Int] -> [Int]
negative [] = []
negative (x:xs) = if (x > 0) then (-x):negative xs
else x:negative xs

-- Вторая часть Задание 4
-- 2.4.Функция вычленения n -го элемента из заданного списка. 



clr (x:xs) n n0 = if n0 /= n then x:clr xs n (n0+1)
else xs
del [] _ = []
del l n = clr l n 0





main = print (multList[1,2,5,1])