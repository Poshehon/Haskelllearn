{-
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

class Functor f where
    fmap :: (a -> b) -> f a -> f b

instance Functor [] where
    fmap = map

instance Functor ((,) s) where
    fmap g (x,y) = (x, g y)

class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b
    x >> y = x >>= \_ -> y

    fail :: String -> m a
    fail s = error s
-}
-- Это работало раньше, но теперь так нельзя
-- Надо сначала делать аппликативные функторы
{-newtype Id a = Id {get :: a} deriving (Show)

instance Monad Id where
    return = Id
    (>>=) (Id x) f = f x
    -}
