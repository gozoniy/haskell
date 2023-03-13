square1 n =[i^2|i<-[1..n]]
square2 n =[2*i-1 |i<-[1..n]]
square3 n =[2*i |i<-[1..n]]
square4 n =[i^3 |i<-[1..n]]
factorial :: Integer -> Integer
factorial n = if n == 0 then 1 else n * factorial (n-1)
square5 n =[factorial (i) |i<-[1..n]]
square6 n =[10^i |i<-[1..n]]
treygol :: Integer -> Integer
treygol n = if n == 1 then 1 else (n + treygol (n-1))
square7 n =[treygol (i) |i<-[1..n]]
cherygol :: Integer -> Integer
cherygol n = if n == 1 then 1 else (treygol (n) + cherygol (n-1))
square8 n =[cherygol (i) |i<-[1..n]]