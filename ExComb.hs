module Comb ( arrange
            , perm
            , comb
            , combRep
            , bicoefs
            , pascal
            , prettyPascal
            , derange
            ) where

-- factorial (might be useful)
fact :: Integral a => a -> a
fact 0  = 1
fact n  = n * fact (n - 1) 

-- no. of permutations of n items
arrange :: Integral a => a -> a
arrange = fact

-- P(n,r)
perm :: Integral a => a -> a -> a
perm n r = (fact n) `div` fact (n - r)

-- C(n,r)
comb :: Integral a => a -> a -> a
comb n r = fact n `div` ( fact r * fact (n - r) )

-- Combinations n over r with repetitions
combRep :: Integral a => a -> a -> a
combRep n r = (fact (n + r - 1)) `div` ((fact r) * fact (n - 1))

-- Binomial coefficients
bicoefs :: Integral a => a -> [a]
bicoefs = undefined

-- derangements: how many permutations of n objects
-- do not leave ANY item in its original position
derange :: Integral a => a -> a
derange = undefined

-- Pascal triangle (infinite!) (See below its first 10 terms)
pascal :: Integral a => [[a]]
pascal = undefined

-- given to pretty-print the first n terms of your pascal funcion
prettyPascal :: Int -> IO ()
prettyPascal n = putStrLn $ unlines $ map show $ take n pascal

{-

Example output:

λ> prettyPascal 10
[1]
[1,1]
[1,2,1]
[1,3,3,1]
[1,4,6,4,1]
[1,5,10,10,5,1]
[1,6,15,20,15,6,1]
[1,7,21,35,35,21,7,1]
[1,8,28,56,70,56,28,8,1]
[1,9,36,84,126,126,84,36,9,1]

-}

