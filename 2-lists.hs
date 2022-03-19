-- lists are homogenous (single type)
numbers = [4,2,3,5,6,7,7]

-- strings are lists of characters
-- hello = ['h','e' ... 

-- concatenating lists: ++ operator
[1,2,3,4] ++ [5]
-- note: long list ++ short list = slow

-- but we also have : operator
'a':" small cat"
-- [1,2,3] == 1:2:3:[]

-- getting an element out of a list by index: use !!
"Steve Buscem" !! 6 -- = 'B'

-- and of course lists can contain lists

-- some sample functions:
head [5,4,3] -- 5
tail [5,4,3] -- [4,3]
last [5,4,3] -- 3
init [5,4,3] -- [5,4]
length [5,4,3] -- 3
null [5,4,3] -- False (not empty)
reverse [5,4,3] -- [3,4,5]
take 2 [5,4,3] -- [5,4]
drop 2 [5,4,3] -- [3]
maximum, minimum, sum, product --(obvious)
4 `elem` [5,4,3] -- True

-- ranges
[1..20] -- 1 to 20
[2,1..20] -- step size 2
[1..] -- infinite list!

-- infinite list functions
cycle [1,2,3] -- [1,2,3,1,2,3,1,2,3 . . .
repeat 1 -- [1,1,1,1 . . .
-- finite version: replicate 3 1 = [1,1,1]

-- list comprehensions
-- used for building specific sets from general sets
-- examples:
[x*2 | x <- [1..10]]
[x | x <- [50..100], x `mod` 7 == 3]
[ x | x <- [10..20], x /= 13, x /= 15, x /= 19]
[[ x | x <- xs, even x ] | xs <- xxs]  

-- tuples: heterogeneous, fixed size
-- each size is a different type
fst (8,11) == 8
snd (8,11) == 11