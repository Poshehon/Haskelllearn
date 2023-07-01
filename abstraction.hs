{-
class Functor f where
    fmap :: (a -> b) -> f a -> f b

instance Functor [] where
    fmap = map

instance Functor Maybe where
    fmap f (Just a) = Just (f a)
    fmap _ Nothing = Nothing

instance Functor IO where
    fmap f action = do
        result <- action
        return (f result)

1. fmap id = id
2. fmap (f . g) = fmap f . fmap g

class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

instance Applicative Maybe where
    pure x = Just x
    Nothing <*> _ = Nothing
    (Just f) <*> Nothing = Nothing
    (Just f) <*> Just x = Just (f x)

instance Applicative Maybe where
    pure x = Just x
    Nothing <*> _ = Nothing
    (Just f) <*> x = fmap f x

instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x | f <- sx, x <- xs]

instance Applicative IO where
    pure = return
    a <*> b = do
        f <- a
        x <- b
        return (f x)

1. pure f <*> x = fmap f x
2. pure id <*> v = v
3. pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
4. pure f <*> pure x = pure (f x)
5. u <*> pure y = pure ($ y) <*> u

class Monoid a where
    mempty :: a
    mappend :: a -> a -> a
    mconcat :: [a] -> a
    mconcat = foldr mappend mempty

instance Monoid a => Monoid (Maybe a) where
    mempty = Nothing
    mappend Nothing x = x
    mappend x Nothing = x
    mappend (Just a) (Just b) = Just (a `mappend` b)

1. mempty `mappend` x = x
2. x `mappend` mempty = x
3. (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)

newtype SumInt = SumInt Integer deriving (Show, Num)
newtype ProdInt = ProdInt Integer deriving (Show, Num)

instance Monoid SumInt where
    mempty = 0
    mappend (SumInt x) (SumInt y) = SumInt (x + y)

instance Monoid ProdInt where
    mempty = 1
    mappend (ProdInt x) (ProdInt y) = ProdInt (x*y)

class (Applicative m) => Monad m where
    (>>=)  :: m a -> (a -> m b) -> m b
    return = pure

1. return a >>= k = k a
2. m >>= return = m
3. m >>= (\x -> f x >>= g) = m >>= f >>= g
-}
