--- functors (again) ---

-- a functor is essentially a context that you can apply functions over
-- for example, a list is a box that you can fmap function onto
-- so a list is a functor

-- must be kind * -> *

-- io is a functor
instance Functor IO where  
    fmap f action = do  
        result <- action  
        return (f result)  

-- lifting a function: taking a function that takes a param, and returns a param
-- the lifted function: takes a functor and return a functor
-- fmap f results in f lifted to functors
-- note: you don't have to map to a specific functor, this lifted function will work
--       on any functor with the typeclass

-- functor laws:
-- 1-must remain unchanged with id function
-- 2-composition and mapping are associative
--formally:
fmap (f . g) = fmap f . fmap g


--- applicative functors ---
class (Functor f) => Applicative f where  
    pure :: a -> f a  
    (<*>) :: f (a -> b) -> f a -> f b  

-- pure: takes something, puts it in the functor
-- <*>: like fmap, but takes a functor with a function, and another functor
-- (same type of functor) with a val, and applies the function over the value

instance Applicative Maybe where  
    pure = Just  
    Nothing <*> _ = Nothing  
    (Just f) <*> something = fmap f something  

-- pure f <*> x: wrap f in functor, then apply it to what's in x
-- so, this is the same as fmap f x or f <$> x

-- Control.Applicative exports <$>, which is fmap but infix

-- applicative functor laws:
-- pure f <*> x = fmap f x
-- pure id <*> v = v
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- pure f <*> pure x =  pure (f x)
-- u <*> pure y = pure ($ y) <*> u

-- don't worry about these too much

--- newtype keyword ---
-- create type out of existing types

-- so, for instance, there are two ways to make list inherit applicative
-- creating a power set, or element by element
-- so we can use newtype to make a type that wraps a list

newtype ZipList a = ZipList { getZipList :: [a] }  

-- bit confusing, just readthe page

--- monoids ---
-- monoid: you have an associative binary function,
-- an identity value, and that's it
-- and we have a type class for things of this form
-- so that we have generic functions that can act on any monoid
-- pretty cool!
-- and the chapter just introduces increasingly esoteric cases
-- where these laws apply