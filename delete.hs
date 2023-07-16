import Data.List (isInfixOf)
main :: IO ()
main = do
    putStr "Substring: "
    name <- getLine
    if name == "" then putStrLn "Cancelled"
        else do
        lst <- getDirectoryContents (".")
        let p = isInfixOf name
        let ls = filter p lst
        mapM_ removeFile ls
        mapM_ (\x -> putStrLn ("Remove file: " ++ x)) ls