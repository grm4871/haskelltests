-- now how to make our own types
-- note: start with uppercase

-- one way: use the data keyword
data Bool = False | True

data Shape = Circle Float Float Float | Rectangle Float Float Float Float
-- this defines a constructor
Circle :: Float -> Float -> Float -> Shape
-- and similarly for the rectangle

-- and we can now define functions on it
surface :: Shape -> Float  
surface (Circle _ _ r) = pi * r ^ 2  
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)  

-- make printable
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
-- this will automatically make a tostring
-- and since value constructors are functions, we can curry them
map (Circle 10 20) [4,5,6,7]

--- record syntax ---
data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     , height :: Float  
                     , phoneNumber :: String  
                     , flavor :: String  
                     } deriving (Show)   
-- now we have named fields!
-- and this will print nicer

--- type constructor ---
data Maybe a = Nothing | Just a

-- example type
data Vector a = Vector a a a deriving (Show)  
  
vplus :: (Num t) => Vector t -> Vector t -> Vector t  
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)  
  
vectMult :: (Num t) => Vector t -> t -> Vector t  
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)  
  
scalarMult :: (Num t) => Vector t -> Vector t -> t  
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n  

-- we can also derive Eq, Read, Bounded, it just inherits to the members


--- type keyword ---

type String = [Char]
-- creates an alias

-- fixity declarations:
-- where the operator binds, whether it is left associative or roght associative
-- mult is infixl 7 *
-- plus is infixl 6 +
-- they are both left associative, but * binds tighter than +

--- typeclasses (advanced) ---

-- an example
class Eq a where  
    (==) :: a -> a -> Bool  
    (/=) :: a -> a -> Bool  
    x == y = not (x /= y)  
    x /= y = not (x == y)  

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where  
    Red == Red = True  
    Green == Green = True  
    Yellow == Yellow = True  
    _ == _ = False  

instance Show TrafficLight where  
    show Red = "Red light"  
    show Yellow = "Yellow light"  
    show Green = "Green light"  

--- Functors!!! ---
class Functor f where  
    fmap :: (a -> b) -> f a -> f b  
-- generalized map
-- take a function, apply it to a type constructor and get a new type constructor
-- where the function is applied to all elements

-- type kinds
-- *: Concrete type. Doesn't take type parameters, values can
-- only have types that are concrete types
-- * -> *: Type cons takes a concrete type, returns a concrete type
-- ex: Maybe:: * -> *
-- but Maybe Int:: *
-- you should just read this part of the chapter, it gives me a headache
-- essentially though you can be a concrete type, or a higher type composed of
-- functions of concrete types