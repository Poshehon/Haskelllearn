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
