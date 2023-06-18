import qualified Data.Char as C

numbers = [0,1..]
limit :: Int -> Int
limit x = head [ n | n <- (take x numbers), x <= n^2] + 1 -- no need to check big numbers
choose :: Int -> Int -> Int -> Int -> Int -> Bool
choose x a b c d = (x == a^2 + b^2 + c^2 + d^2) && (a <= b) && (b <= c) && (c <= d) -- choosing the appropriate sets
represent :: Int -> [[Int]]
represent x
    | x <= 0 = error "Only natural numbers!"
represent x = [ [n1,n2,n3,n4] |
 n1 <- (take ((limit x `div` 2) + 1) numbers), n2 <- (take (limit x) numbers),
  n3 <- (take (limit x) numbers), n4 <- (take (limit x) numbers), choose x n1 n2 n3 n4]

health :: Float -> Float -> String
health weight height
    | (height <= 0) || (weight <= 0) = error "You are lying to me"
    | bmi weight height <= 18.5 = "You need to eat!"
    | bmi weight height <= 25 = "Great!"
    | otherwise = "More sport, less food!"
    where bmi :: Float -> Float -> Float 
          bmi x y = x / y ^ 2

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = (quicksort [n | n <- xs, n < x]) ++ [x]
 ++ [n | n <- xs, n == x] ++ (quicksort [n | n <- xs, n > x])

harmonic :: Int -> Double
harmonic n
    | n <= 0 = 0.0
harmonic n = foldl (\acc x -> acc + 1/x) 0 (take n num)
    where num = map fromIntegral [1,2..]

caesarcode :: Int -> String -> String
caesarcode _ [] = []
caesarcode n st = map (C.chr . (`mod` 160) . (+n) . C.ord) st

caesardecode :: String -> [String]
caesardecode [] = [[]]
caesardecode st = foldl (\acc n -> shift st n : acc) [] [0..159]
    where shift st n = map (C.chr . (`mod` 160) . (subtract n) . C.ord) st