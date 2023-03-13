square :: Integer->Integer
square x=x*x

signum :: Integer->Integer
signum x = if x > 0 then 1
    else if x < 0 then -1
        else 0

isPositive :: Integer->Bool
isPositive x = if x > 0 then True else False




eqCords :: (Float, Float) -> (Float, Float)
eqCords m = quadX((-2*fst(m)) + (-2*snd(m)) , ((fst(m)**2) + (snd(m)**2)))



quadX :: (Float,Float)->(Float,Float)
quadX (x,y) = (((-x-(x**2-4*1*y)**0.5)/2),((-x+(x**2-4*1*y)**0.5)/2))

main 