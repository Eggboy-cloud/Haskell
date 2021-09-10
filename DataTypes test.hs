import MeasureTime

fib :: Integer -> Integer 
fib 0 = 0
fib 1 = 1
fib n =  fib(n -2) + fib(n-1)


fibAux :: Integer -> Integer -> Integer -> Integer 

--fibAux 0 a b = ...
--fibAux i a b | i>0=
fibAux = undefined
--fibAux i (fib n) (fib (n+1)) == fib (n+1)




data Figure = Triangle Double Double Double | Rectangle Double Double | Circle {radius:: Double}

circumn :: Figure -> Double
circumn(Triangle a b c) = a + b + c
circumn (Rectangle a b) = 2 * (a + b)
circumn               c = 2* pi * radius c 

main = do
   let a = Circle 3
   print(circumn a)