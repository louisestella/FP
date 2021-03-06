module ExList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C


head :: [a] -> a
head []    = error "head of empty list"
head (x:_) = x

tail :: [a] -> [a]
tail []     = error "tail of empty list"
tail (_:xs) = xs

null :: [a] -> Bool
null [] = True
null _  = False

length :: Integral i => [a] -> i
length []     = 0
length (_:xs) = 1 + length xs

sum :: Num a => [a] -> a
sum []     = 0
sum (x:xs) = x + sum xs

product :: Num a => [a] -> a
product []     = 1
product (x:xs) = x * product xs

reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]

(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

infixr 5 ++


-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc w []     = [w]
snoc w (x:xs) = x : snoc w xs

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

minimum :: Ord a => [a] -> a
minimum []  = error "minimum of empty list"
minimum [x] = x
minimum (x:xs)
  | x < m     = x
  | otherwise = m
  where m = minimum xs

maximum :: Ord a => [a] -> a
maximum []  = error "maximum of empty list"
maximum [x] = x
maximum (x:xs) =
    let m = maximum xs
     in if x > m then x else m

take :: Integral i => i -> [a] -> [a]
take _  []    = []
take 0  _     = []
take n (x:xs) = x : take (n - 1) xs


-- drop
drop :: Integral i => i -> [a] -> [a]
drop _ []     = []
drop 0 x      = x
drop n (x:xs) = drop (n - 1) xs 


-- takeWhile
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ []       = []
takeWhile p (x:xs)
    | (p x == True)  = [x] ++ takeWhile p xs
    | (p x == False) = [] 


-- dropWhile
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ []       = []
dropWhile p (x:xs)
    | (p x == False) = xs
    | (p x == True)  = dropWhile p xs


-- tails
tails :: [a] -> [[a]]
tails []     = [[]]
tails (x:xs) = [(x:xs)] ++ tails xs


-- init
init :: [a] -> [a]
init []     = []
init [x]    = []
init (x:xs) = x:(init xs)


-- inits
inits :: [a] -> [[a]]
inits []     = [[]]
inits (x:xs) = inits (init (x:xs)) ++ [(x:xs)]
  

-- any
any :: (a -> Bool) -> [a] -> Bool
any _ []     = False
any p (x:xs) = if p x then True else any p xs 

-- all
all :: (a -> Bool) -> [a] -> Bool
all _ []     = True
all p (x:xs) = if p x then all p xs else False

-- and
and :: [Bool] -> Bool
and []     = True
and (x:xs) = if x then and xs else False

-- or
or :: [Bool] -> Bool
or []     = False
or (x:xs) = x || or xs 

-- concat
concat :: [[a]] -> [a]
concat []     = []
concat (x:xs) = x ++ concat xs

-- elem
elem :: Eq a => a -> [a] -> Bool
elem _ []     = False
elem n (x:xs) = n == x || elem n xs

-- (!!)
(!!) :: [a] -> Int -> a
[] !! _     = error "index too large"
(x:_) !! 0  = x
(_:xs) !! n = xs !! (n - 1) 

-- filter
filter :: (a -> Bool) -> [a] -> [a]
filter _ []     = []
filter p (x:xs) = if p x then x : filter p xs else filter p xs

-- map
map :: (a -> b) -> [a] -> [b]
--map f xs = [ f x | x <- xs]
map _ []     = []
map f (x:xs) = f x : map f xs

-- cycle
cycle :: [a] -> [a]
cycle [] = error "empty list."
cycle xs = xs ++ cycle xs 

-- repeat
repeat :: a -> [a]
repeat n = n : repeat n

-- replicate
replicate :: Integral i => i -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n - 1) x


-- isPrefixOf
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _          = True
isPrefixOf _ []          = False 
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

-- isInfixOf
isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf [] _          = True
isInfixOf _ []          = False
isInfixOf xs ys'@(_:ys) = if not (isPrefixOf xs ys') then isInfixOf xs ys else True 

-- isSuffixOf
isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf xs ys = isPrefixOf (reverse xs) (reverse ys) 

-- zip
zip :: [a] -> [b] -> [(a, b)]
zip [] _          = []
zip _ []          = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys
 
-- zipWith
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _          = []
zipWith _ _ []          = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

--unzip
unzip :: [(a, b)] -> ([a], [b])
unzip []          = ([],[])
unzip [(x,y)]     = ([x],[y])
unzip ((x,y):xys) = (x:xs,y:ys)
    where
          (xs,ys) = unzip xys
         

--subsequences
subsequences :: [a] -> [[a]]
subsequences []       = [[]]
subsequences [x]      = [[],[x]]
--subsequences (x:y:[]) = [[],[x],[y],[x,y]]
--subsequences (x:y:xs) = 


-- intercalate
-- nub


-- splitAt
splitAt :: Int -> [a] -> ([a], [a])
--splitAt n xs = (take n xs, drop n xs)
splitAt 0 xs     = ([], xs)
splitAt n []     = ([], [])
splitAt n (x:xs) = (x: ys, zs)
    where
        (ys,zs) = splitAt (n - 1) xs
    

-- break

-- lines
-- words
-- unlines
-- unwords

-- transpose

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome = undefined

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}

