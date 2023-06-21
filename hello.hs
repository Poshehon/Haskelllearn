import Data.Char

main = do 
    putStrLn "What is your name?"
    name <- getLine
    let big = map toUpper name
    putStrLn ("Hello, " ++ big)
    putStrLn "Enter three times"
    res <- (sequence . take 3. cycle) [getLine]
    print res
    mapM print [1, 7, 9]