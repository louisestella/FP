module ExNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral(..)
    , Bool(..)
    , not
    , (&&)
    , (||)
    , (++)
    , ($)
    , (.)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat = Zero | Succ Nat

instance Show Nat where

    show Zero     = "0"
    show (Succ n) = "S" ++ show n

instance Eq Nat where

    Zero   == Zero    = True
    Zero   == Succ _  = False
    Succ _ == Zero    = False
    Succ m == Succ n  = m == n

instance Ord Nat where

    -- (<=) :: Nat -> Nat -> Bool
    Zero   <=  n     = True
    Succ _ <= Zero   = False
    Succ n <= Succ m = n <= m
    
    -- Ord does not require defining min and max.
    -- Howevener, you should define them without using (<=).
    -- Both are binary functions: max m n = ..., etc.

    min Zero n            = Zero
    min n Zero            = Zero
    min (Succ n) (Succ m) = Succ (min n m) 

    max Zero n            = n
    max n Zero            = n
    max (Succ n) (Succ m) = Succ (max n m) 

isZero :: Nat -> Bool
isZero Zero = True
isZero _    = False

pred :: Nat -> Nat
pred Zero     = Zero
pred (Succ x) = x

even :: Nat -> Bool
even Zero            = True
even (Succ Zero)     = False
even (Succ (Succ x)) = even x

odd :: Nat -> Bool
odd = not . even

-- addition
(<+>) :: Nat -> Nat -> Nat
n <+> Zero     = n
n <+> (Succ m) = Succ (n <+> m) 

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when subtraction returns a negative number.
(<->) :: Nat -> Nat -> Nat
n        <-> Zero     = n
Zero     <-> m        = Zero
(Succ n) <-> (Succ m) = n <-> m


-- multiplication
(<*>) :: Nat -> Nat -> Nat
n <*> Zero     = Zero
n <*> (Succ m) = n <+> (n <*> m)  

-- exponentiation
(<^>) :: Nat -> Nat -> Nat
_ <^> Zero     = (Succ Zero)
n <^> (Succ m) = n <*> (n <^> m)

-- quotient
(</>) :: Nat -> Nat -> Nat
_ </> Zero = error "YOU SHALL NOT DIVIDE BY ZERO!" 
m </> n 
   | (n <= m)  = Succ((m <-> n) </> n)
   | otherwise = Zero



-- remainder
(<%>) :: Nat -> Nat -> Nat
(<%>) _ Zero = error "WHY ARE YOU TRYING TO DO THAT?"
(<%>) n m
    | (m <= n)  = (<%>) (n <-> m) m
    | otherwise = n

-- divides
(<|>) :: Nat -> Nat -> Bool
(<|>) _ Zero = True
(<|>) Zero _ = True
(<|>)a b 
    | (a <%> b == Zero) = True
    | otherwise         = False

divides = (<|>)


-- x `absDiff` y = |x - y|
-- (Careful: here this - is the real minus operator!)
absDiff :: Nat -> Nat -> Nat
n        `absDiff` Zero     = n
Zero     `absDiff` m        = m
(Succ n) `absDiff` (Succ m) = n `absDiff` m


(|-|) = absDiff

factorial :: Nat -> Nat
factorial Zero     = Succ Zero
factorial (Succ n) = (Succ n) <*> factorial n

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg Zero = Zero
sg n    = (Succ Zero)

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo b a
    | (a >= b)  = Succ Zero <+> lo b (a </> b)
    | otherwise = Zero

--
-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!
--

toNat :: Integral a => a -> Nat
toNat 0  = Zero
toNat n  = (Succ Zero) <+> (toNat (n - 1))

fromNat :: Integral a => Nat -> a
fromNat Zero     = 0
fromNat (Succ n) = 1 + (fromNat n)

instance Integral Nat where
    toInteger = fromNat
    quotRem n m = (n </> m, n <%> m)

-- Obs: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger x
        | x < 0     = toNat 0
        | x == 0    = Zero
        | otherwise = toNat x

