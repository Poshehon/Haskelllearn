numbers = [0,1..]
limit :: Int -> Int
limit x = head [ n | n <- (take x numbers), x <= n^2] + 1 -- no need to check big numbers
choose :: Int -> Int -> Int -> Int -> Int -> Bool
choose x a b c d = (x == a^2 + b^2 + c^2 + d^2) && (a <= b) && (b <= c) && (c <= d) -- choosing the appropriate sets
represent :: Int -> [[Int]]
represent x = [ [n1,n2,n3,n4] |
 n1 <- (take ((limit x `div` 2) + 1) numbers), n2 <- (take (limit x) numbers),
  n3 <- (take (limit x) numbers), n4 <- (take (limit x) numbers), choose x n1 n2 n3 n4]
health :: Float -> Float -> String
health weight height
    | height == 0 = error "You lying to me"
    | bmi weight height <= 18.5 = "You need to eat!"
    | bmi weight height <= 25 = "Great!"
    | otherwise = "You need more sex and sport!"
    where bmi :: Float -> Float -> Float 
          bmi x y = x / y ^ 2