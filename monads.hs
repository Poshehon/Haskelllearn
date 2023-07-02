import Control.Monad.Writer
import Control.Monad.State
{-
newtype Writer w a = Writer { runWriter :: (a, w)}
instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    Writer (x,v) >>= f = let
        (Writer (y,v')) = f x in Writer (y, v `mappend v')
-}
data Id a = Id a deriving (Show)
instance Functor Id where
    fmap f (Id a) = Id (f a)
instance Applicative Id where
    pure x = Id x
    (Id f) <*> (Id x) = Id (f x)
instance Monad Id where
    (Id x) >>= f = f (x)

type Birds = Int
type Pole = (Birds, Birds)

landleft :: Birds -> Pole -> Maybe Pole
landleft n (x, y)
    | abs (x + n - y) > 3 = Nothing
    | otherwise = Just (x + n, y)

landright :: Birds -> Pole -> Maybe Pole
landright n (x, y)
    | abs (x - y - n) > 3 = Nothing
    | otherwise = Just (x, y + n)
{-
instance Functor Maybe where
    fmap f (Just a) = Just (f a)
    fmap _ Nothing = Nothing
instance Applicative Maybe where
    pure x = Just x
    Nothing <*> _ = Nothing
    (Just f) <*> Nothing = Nothing
    (Just f) <*> Just x = Just (f x)
instance Monad Maybe where
    Nothing >>= _ = Nothing
    (Just x) >>= f = f x
-}
-- we can write return (0,0) >>= landleft 3 >>= landright 2 >>= ...
{-
pole = do
    start <- return (0,0)
    first <- landleft 3 start
    second <- landright 2 first
    return second
-}

getfirst :: Maybe Char
getfirst = do
    (x:xs) <- Just ""
    return x

{-
instance Functor [] where
    fmap = map
instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x | f <- sx, x <- xs]
instance Monad [] where
    xs >>= f = concat (map f xs)
-}
euclid :: Int -> Int -> Writer [String] Int
euclid x y
    | y == 0 = do
        tell ["Algoritm is over"]
        return x
    | otherwise = do
        tell [show x ++ " mod " ++ show y ++ " = " ++ show (x `mod` y)]
        euclid y (x `mod` y)
{-
instance Monad ((->) r) where
    return x = \_ -> x
    h >>= f = \w -> f (h w) w
-}
safehead :: [a] -> Maybe a
safehead = do
    a <- null
    if a then return Nothing
    else do
        h <- head
        return (Just h)
-- we can write e <- id
{-
newtype State s a = State {runState :: s -> (a,s)}
instance Monad (State s) where
    return x = State (\s -> (x,s))
    m >>= k = State (\s -> 
        let (a, s') = runState m st
        m' = k m
        in runState m' st')
get :: State s s
get = State (\s -> (s,s))

put :: s -> State s ()
put s = State (\x -> ((), s))
-}
tick :: State Int Int
tick = do
    n <- get
    put (n+1)
    return n
