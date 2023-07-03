import Control.Monad.State

fibstep :: State (Int, Int) ()
fibstep = do
    (a,b) <- get
    put (b, a+b)

counter :: Int -> (Int, Int) -> (Int, Int)
counter n x = execState (sequence (replicate n fibstep)) x

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fst (counter n (0,1))

main = do
    putStrLn "Enter the number of element"
    number <- getLine
    putStr "Your number is equal to "
    let n = read number :: Int
        m = (show . fib) n
    putStrLn m