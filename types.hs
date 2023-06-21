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
start :: a -> Tree a
start a = Node a Empty Empty -- create elementary tree

input :: (Ord a) => a -> Tree a -> Tree a
input a Empty = start a
input a (Node x left right)
    | a < x = Node x (input a left) right
    | a > x = Node x left (input a right)
    | otherwise = (Node a) left right

search :: (Ord a) => a -> Tree a -> Bool
search _ Empty = False
search a (Node x left right)
    | a == x = True
    | a < x = search a left
    | a > x = search a right

maketree :: (Ord a) => [a] -> Tree a
maketree x = foldr input Empty (reverse x)

data TrafficLight = Red | Yellow | Green

instance Show TrafficLight where
    show x = case x of
        Red -> "The light is red. You need to stop"
        Yellow -> "The light is yellow. Wait for the color change"
        _ -> "The light is green. You can go now"

type Name = String --  synonym

data Person = Person {firstName :: Name, lastName :: Name, age :: Int}

instance Show Person where
    show x = "Hello " ++ firstName x ++ " " ++ lastName x ++ ". You are " ++ (show . age) x ++ " years old."