import Control.Monad
import Data.List

choose :: String -> String -> IO ()
choose "view" = view
choose "add" = add
choose "remove" = remove
choose _ = error "I cannot do this"

view :: String -> IO ()
view name = do
    datum <- readFile name
    let info = unlines (zipWith (\x y -> show x ++ " " ++ y) [1..] (lines datum))
    putStr info

add :: String -> IO ()
add name = do
    putStrLn "What do you want to add?"
    todo <- getLine
    appendFile name (todo ++ "\n")

remove :: String -> IO ()
remove name = do
    putStrLn "Enter number of case"
    num <- getLine
    datum <- readFile name
    let n = read num
        info = lines datum
        m = length info
    if n > m then error "This business does not exist" else 
        writeFile name (unlines (delete (info !! (n-1)) info))

main = do
    putStrLn ("Enter filename of your todo list")
    name <- getLine
    forever (do
        putStrLn ("What do you want to do?")
        business <- getLine
        choose business name)