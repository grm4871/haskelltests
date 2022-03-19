-- haskell has static types
-- but, inferred

-- :t - gives type in ghci

-- type declarations
removeNonUppercase :: [Char] -> [Char]  
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]   

-- curried functions etc
addThree :: Int -> Int -> Int -> Int  
addThree x y z = x + y + z  

-- Int: integer, bounded
-- Integer: unbounded
-- float: single precision
-- double: you get the idea
-- bool: t/f
-- char: a character
-- tuple: dependent on length and type of compenents
-- list: dependent on type of component

-- we have generics
head :: [a] -> a

-- typeclasses
-- an interface that defines some behavior
(==) :: (Eq a) => a -> a -> Bool  
-- can only compare things of same type and typeclass Eq
-- (class constraint) => type -> type -> type

--   some typeclasses
-- Eq: support equality testing
-- Ord: can be ordered
-- Show: can be toString'd
-- Read: can be fromString'd
-- Enum: can be enumerated
-- Bounded: have upper & lower bound
-- Num: "number"
-- Integral: integers
-- Floating: floats

-- type annotations:
(read "5" :: Float) * 4 

-- useful to cast int to float
fromIntegral :: (Num b, Integral a) => a -> b