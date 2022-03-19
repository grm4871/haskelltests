--- monads ---

-- have a rich mathematical definition
-- that is not really touched on here
(>>=) :: (Monad m) => m a -> (a -> m b) -> m b  
-- a monad is an applicative functor that also implements this
-- so if you have a function that takes a value and returns a lifted value,
-- then a monad is something where you can take a lifted input, lower it, and
-- then lift it again. aka, it is an applicative functor where you can also
-- go backwards.

-- functor: you can take functions from the base domain and lift them to the functor
-- domain
-- (a -> b) -> f a -> f b

-- applicative: you can take an already lifted function f (a -> b) and pull it
-- back to the base domain
-- fa (a -> b) -> fa a -> fa b

-- monad: you can take a function that takes a base value and returns a lifted
-- value, and instead it to a lifted value instead
-- this is sort of an "inverse functor" in a very abstract sense 
-- m a -> (a -> m b) -> m b


--- do notation ---
-- this is not just for io actions, it works on any monad
-- essentially, we can now create chains of functions, even though
-- these functions cross from base to monad
-- can you see why?

instance Monad [] where  
    return x = [x]  
    xs >>= f = concat (map f xs)  
    fail _ = []

-- the rest of the book sort of "puts it all together"