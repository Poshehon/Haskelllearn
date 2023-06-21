import System.IO
{-
main = do
    handle <- openFile "poem.txt" ReadMode -- handle says the location of our file
    datum <- hGetContents handle -- get data
    putStr datum
    hClose handle -- we must close our file
-}
main = do
    withFile "poem.txt" ReadMode (\handle -> do
        datum <- hGetContents handle
        putStr datum) -- close automatically