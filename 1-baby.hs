---- begin notes ----
-- why write these as haskell files?
-- syntax highlighting
-- do any of these compile?
-- probably not
-- if i end up releasing these, you can use them as a summary
-- for the learnyouahaskell.com book

-- example functions
doubleMe x = x + x

doubleUs x y = x*2 + y*2
doubleUs' x y = x + x + y + y
doubleUs'' x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
    then x
    else x*2

doubleSmallNumberPlusOne x = (if x > 100 then x else x^2) + 1

-- note, apostrophe valid in func name
conanO'Brien = "It's a-me, Conan O'Brien!"   

-- but functions cannot begin with uppercase letters
-- Hello = "invalid"

-- io actions will only be performed if they're in main or in ghci

import Data.Char  
  
-- you can use let bindings in do blocks as well
main = do  
    putStrLn "What's your first name?"  
    firstName <- getLine  
    putStrLn "What's your last name?"  
    lastName <- getLine  
    let bigFirstName = map toUpper firstName  
        bigLastName = map toUpper lastName  
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"  

-- ex) program that reads lines and prints reversed. stops when we input a blank line
main = do   
    line <- getLine  
    if null line  
        then return () -- this line makes an io action that does nothing
        else do  
            putStrLn $ reverseWords line  
            main -- we loop by recursing

reverseWords :: String -> String  
reverseWords = unwords . map reverse . words  

-- return in haskell: in IO actions, creates an IO action out of that value.
-- no correspondance to how return works in other languages

-- the chapter then goes over some io functions
-- again, the page itself is a better reference than i can be
-- http://learnyouahaskell.com/input-and-output

-- exceptions
-- while we can use type logic in safe code, io code works best with exceptions
-- pure code chan through exceptions, but can only be caught in the i/o part
-- anyway

-- example:

import System.Environment  
import System.IO  
import System.IO.Error  
  
main = toTry `catch` handler  
              
toTry :: IO ()  
toTry = do (fileName:_) <- getArgs  
           contents <- readFile fileName  
           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"  
  
handler :: IOError -> IO ()  
handler e  
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"  
    | otherwise = ioError e  

-- we use the catch function