
logList [] = []
logList (x:xs)=log x:logList xs

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


main = print (bSort (<) [1,2,5,1])