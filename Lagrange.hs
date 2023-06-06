numbers = [0,1..]
limit :: Int -> Int
limit x = last [ n | n <- (take x numbers), x >= n^2] -- no need to check big numbers
choose :: Int -> Int -> Int -> Int -> Int -> Bool
choose x a b c d = (x == a^2 + b^2 + c^2 + d^2) && (a <= b) && (b <= c) && (c <= d) -- choosing the appropriate sets
represent :: Int -> [[Int]]
represent x = [ [n1,n2,n3,n4] |
 n1 <- (take (limit x) numbers), n2 <- (take (limit x) numbers),
  n3 <- (take (limit x) numbers), n4 <- (take (limit x) numbers), choose x n1 n2 n3 n4]