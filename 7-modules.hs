-- a haskell program is a collection of modules where the main module loads
-- other modules
-- a module is a collection of functions, types, typeclasses

-- syntax: import <module name>
-- ex)
import Data.Map
-- ghci> :m + <module name> <module name> . . .
-- import <module name> (function, function
-- import <module name> hiding (function to exclude)
-- namespaces: import qualified <module name> [as <new namespace>]

-- the bulk of this chapter is just a big list of default functions
-- i am just going to link it
-- http://learnyouahaskell.com/modules

-- making a module:
module ModuleName
( function
, function2
, function'
) where

function :: Num -> Num
etc

-- note, we don't have to export everything

-- folder structures:
-- Geometry / 
--  - Sphere.hs
--  - Cuboid.hs
--  - Cube.hs

-- so now we can import eg, Geometry.Sphere