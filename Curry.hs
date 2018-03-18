module Curry where

pythagorean :: Double -> Double -> Double
pythagorean x y = sqrt (x ** 2 + y ** 2)

pythagorean' :: (Double, Double) -> Double
pythagorean' (x, y) = sqrt (x ** 2 + y ** 2)


myCurry :: ((Double, Double) -> Double) -> (Double -> Double -> Double)
myCurry f x y = f (x,y)

myUncurry :: (Double -> Double -> Double) -> ((Double, Double) -> Double)
myUncurry f (x,y) = f x y
