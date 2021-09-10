{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
import MeasureTime




{- Lab 1
   Authors: Nils Ekström, Egil Guting, Carl Malmström
   Lab group: 82
 -}
---------------------------------------------
power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k-1)

-- A -------------------------
-- stepsPower n k gives the number of steps that
-- power n k takes to compute

stepsPower :: Integer -> Integer -> Integer
stepsPower n k = k+1


-- B -------------------------
-- power1

power1 :: Integer -> Integer -> Integer

power1 n k = product([n | k <- [1..k]])


-- C -------------------------
-- power2

power2 :: Integer -> Integer -> Integer
power2 n k
   | k < 0 = error "negative"
power2 n 0 = 1
power2 n k
   | even k = power2 (n * n)(div k 2)
   | odd k = n * power2 n (k-1)


-- D -------------------------
{- 

<Describe your test cases here>
we tested different values for n and k and they all returned true
print(and [comparePower1 n a | a <- [1..k]])
print(and [comaprePower2 n a | a <- [1..k]])
specific test values for n = 2, 20, -4
specific test values for k = 6, 30, 3
Since the values test for both higher and lower values as well as negative and positive values.
 -}

-- comparePower1
comparePower1 :: Integer -> Integer -> Bool
comparePower1 n k
   |(power n k) == (power1 n k) = True
   |otherwise    = False



-- comparePower2
comparePower2 :: Integer -> Integer -> Bool
comparePower2 n k
   |(power n k) == (power2 n k) = True
   |otherwise    = False


-- Test functions: 
testFunc :: Bool
testFunc = and[comparePower1 n k == comparePower2 n k| n <- nList , k <- kList]
   where nList = [2, 20, -4]
         kList = [6, 30, 3] 


-- Part E

-- Use measureTime2 'function' 'n' 'k'

-- Part F




funcFunctions :: Integer -> Integer -> String
funcFunctions n k = (show k ++ "\t" ++ show(power n k) ++"\t"++ show(power1 n k)++"\t"++ show(power2 n k))

table :: Integer -> Integer -> IO()
table n k = putStr("k p p1 p2\n" ++ unlines([funcFunctions n a | a <- [0..k]]))

