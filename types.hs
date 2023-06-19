data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Triangle Point Point Point deriving (Show)

norm :: Point -> Point -> Float
norm (Point x1 y1) (Point x2 y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

perimeter :: Shape ->  Float
perimeter (Circle _ r) = if r > 0 then 2 * pi * r else 0
perimeter (Triangle a b c) = norm b a + norm c a + norm b c

-- now want to do parallel translation

translation :: Shape -> Float -> Float -> Shape
translation (Triangle (Point x1 y1) (Point x2 y2) (Point x3 y3)) a b = 
    Triangle (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b)) (Point (x3 + a) (y3 + b))
translation (Circle (Point x y) r) a b = Circle (Point (x + a) (y + b)) r

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)
start :: (Ord a) => a -> Tree a
start a = Empty -- create elementary tree
