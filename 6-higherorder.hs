--- curried functions ---

multThree :: (Num a) => a -> a -> a -> a  
multThree x y z = x * y * z  

-- (((multThree x) y) z)

--- you can do weird shit like this
-- ghci> let multTwoWithNine = multThree 9  
-- ghci> multTwoWithNine 2 3  
-- 54  
-- ghci> let multWithEighteen = multTwoWithNine 2  
-- ghci> multWithEighteen 10  
-- 180  

-- we may define functions with parts of curried functions
compareWithHundred :: (Num a, Ord a) => a -> Ordering  
compareWithHundred x = compare 100 x  

-- infix function "sectioning"
divideByTen :: (Floating a) => a -> a  
divideByTen = (/10) 
-- divideByTen 200 == 200 / 10 == (/10) 200
-- exception: (-4) means negative four, (subtract 4) properly sections

--- higher orderism ---

-- functions may take functions, and return functions
applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x)  

{- oh yeah, we can do block comments too
ghci> applyTwice (++ " HAHA") "HEY"  
"HEY HAHA HAHA"  
ghci> applyTwice ("HAHA " ++) "HEY"  
"HAHA HAHA HEY"  
-}

-- pointwise application of function, for two lists
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys  

-- can do goofy stuff like flipping function arguments
flip' :: (a -> b -> c) -> (b -> a -> c)  
flip' f = g  
    where g x y = f y x  

-- function, list, applies function to all elements of list
map :: (a -> b) -> [a] -> [b]  
map _ [] = []  
map f (x:xs) = f x : map f xs  
-- kinda similar to ilst comp, map (+3) [1,2,3] is the same as
-- [x+3 | x <- [1,2,3]]

--- lambdas ---

-- anonymous functions
numLongChains :: Int  
numLongChains = length (filter isLong (map chain [1..100]))  
    where isLong xs = length xs > 15  
-- can be written as
numLongChains :: Int  
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))  

-- limits:
-- if pattern matching fails in a lambda, you get a runtime error

-- these are equivalent because lambdas extend all the way to right
addThree :: (Num a) => a -> a -> a -> a  
addThree x y z = x + y + z  
addThree :: (Num a) => a -> a -> a -> a  
addThree = \x -> \y -> \z -> x + y + z  

--- folds ---

-- like maps, but reduce to single value
-- foldl = left fold
sum' :: (Num a) => [a] -> a  
sum' xs = foldl (\acc x -> acc + x) 0 xs  
-- foldl takes 3 arguments:
-- binary function, with accumulator: \acc x -> acc + x
-- starting value: 0
-- list to be folded: xs

-- the binary function is applied between the starting value and head of list.
-- then with the result and the head of the remaining list,
-- etc etc.

-- more succinctly:
sum' :: (Num a) => [a] -> a  
sum' = foldl (+) 0 
-- this works because foldl (+) 0 returns a function that takes a list
-- in general, 'foo a = bar b a' <=> 'foo = bar b'

-- another example
elem' :: (Eq a) => a -> [a] -> Bool  
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

-- foldr works similarly, but the binary op has accumulator as first param,
-- current val as the second.
map' :: (a -> b) -> [a] -> [b]  
map' f xs = foldr (\x acc -> f x : acc) [] xs  

-- foldl1 and foldr1 work similarly, but assume the head or tail
-- to be the starting value.
sum'' :: (Num a) => [a] -> a
sum'' = foldl1 (+)

--some more examples
maximum' :: (Ord a) => [a] -> a  
maximum' = foldr1 (\x acc -> if x > acc then x else acc)  
  
reverse' :: [a] -> [a]  
reverse' = foldl (\acc x -> x : acc) []  
  
product' :: (Num a) => [a] -> a  
product' = foldr1 (*)  
  
filter' :: (a -> Bool) -> [a] -> [a]  
filter' p = foldr (\x acc -> if p x then x : acc else acc) []  
  
head' :: [a] -> a  
head' = foldr1 (\x _ -> x)  
  
last' :: [a] -> a  
last' = foldl1 (\_ x -> x)  

-- scan is like fold, but makes a list of all intermediate acc steps
{-
ghci> scanl (+) 0 [3,5,2,1]  
[0,3,8,10,11]  
ghci> scanr (+) 0 [3,5,2,1]  
[11,8,3,1,0]  
ghci> scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1]  
[3,4,5,5,7,9,9,9]  
ghci> scanl (flip (:)) [] [3,2,1]  
[[],[3],[2,3],[1,2,3]]
-}

--- $ operator ---

($) :: (a -> b) -> a -> b  
f $ x = f x  
-- why? because lower precedence than normal function operation
sqrt 3 + 4 + 9 = (sqrt 3) + 4 + 9
sqrt $ 3 + 4 + 9 = sqrt(3 + 4 + 9)
-- just think of it as a parentheses

--- function composition ---

(.) :: (b -> c) -> (a -> b) -> a -> c  
f . g = \x -> f (g x)  

f (g (z x)) <=> (f . g . z) x



-- finally some examples of the same function 3 times
-- with varying readability
oddSquareSum :: Integer  
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))   

oddSquareSum :: Integer  
oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

oddSquareSum :: Integer  
oddSquareSum =   
    let oddSquares = filter odd $ map (^2) [1..]  
        belowLimit = takeWhile (<10000) oddSquares  
    in  sum belowLimit    