-- finally, hello world
main = putStrLn "hello, world"

-- do syntax: make a block of actions
main = do  
    putStrLn "Hello, what's your name?"  
    name <- getLine  
    putStrLn ("Hey " ++ name ++ ", you rock!")  
-- now we can code like we're in a normal language

-- this glues 3 io actions into one, with type IO () because putstrlen returns
-- IO ().
-- getLine is an IO action that results in a String
-- var <- getLine: Take the value from getline, bind it to car
-- so, <- gets the result from the IO "box"

nameTag = "Hello, my name is " ++ getLine  
-- this line is INVALID!!!
-- because IO String != String

-- every IO action has a result encapsulated, but sometimes a meaningless one
-- also the last action cannot be bound