faq :: Integer->Integer
faq 0 = 1
faq x = x*faq(x-1)

len :: Num a1 => [a2] -> a1
len [] = 0
len l = 1+len(tail l)
su [] = 0
su (x:xs) = x + su(xs)
t1_2 n = [1..n]

t1_3 0 = []
t1_3 n = [i*2-1 |i<-[1..n]]


t1_4 0 = []
t1_4 n = [i*2 |i<-[1..n]]

t1_5 0 = []
t1_5 n = [i^3 |i<-[1..n]]

t1_6 0 = []
t1_6 n = t1_6(n-1) ++ [faq n]


t1_7 0 = []
t1_7 n = t1_7(n-1) ++ [10^n]

t1_8_1 :: Integer -> Integer
t1_8_1 1 = 1
t1_8_1 n = n + t1_8_1 (n-1)

t1_8 :: Integer -> [Integer]
t1_8 0 = []
t1_8 n = [t1_8_1 (i) |i<-[1..n]]

t1_9_1 :: Integer -> Integer
t1_9_1 1 = 1
t1_9_1 n = t1_8_1 (n) + t1_9_1 (n-1)

t1_9 0 = []
t1_9 n = [t1_9_1 (i) |i<-[1..n]]

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

