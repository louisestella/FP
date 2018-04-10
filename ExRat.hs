module ExRat
    ( rat
    , (//)
    , denominator
    , numerator
    ) where

data Rat = Rat Integer Integer


instance Show Rat where
    show (Rat a b) = show a ++ "/" ++ show b

instance Eq Rat where
    (Rat a b) == (Rat c d) = a * d == b * c

instance Num Rat where
    (Rat a b) + (Rat c d) = rat ((a * d) + (b * c)) (b * d)
    (Rat a b) * (Rat c d) = rat (a * c) (b * d)
    negate (Rat x y) = Rat (negate  x) y
    abs (Rat x y) = Rat (abs x) (abs y)
    fromInteger n = rat n 1 
    signum (Rat x y) = fromInteger (signum (numerator (rat x y)))

instance Ord Rat where
    compare (Rat x y) (Rat w z) = compare (x * z) (w * y)

rat :: Integer -> Integer -> Rat
rat x 0         = error "you can't put a zero on the denominator"
rat x y
    | samesign  = rat' (abs x) (abs y)
    | otherwise = rat' r s
    where
      samesign = x * y > 0
      r = (-(abs x))
      s = abs y


rat' :: Integer -> Integer -> Rat
rat' a b
    | gcd a b /= 1 = Rat (a `div` (gcd a b)) (b `div` (gcd a b)) 
    | otherwise    = Rat a b

(//) :: Rat -> Rat -> Rat
Rat a b // Rat c d = rat (a * d) (b * c)

denominator :: Rat -> Integer
denominator (Rat a b) = b 

numerator :: Rat -> Integer
numerator (Rat a b) = a

