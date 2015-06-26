module Control.Monad.Query where

import Control.Monad
import Control.Applicative
import Data.Traversable
import Control.Monad.Trans
import Data.Functor.Identity

data QueryT a b m r = QPure r
                    | QLift (m (QueryT a b m r))
                    | QReq a (b -> QueryT a b m r)

type Query a b = QueryT a b Identity

instance Functor m => Functor (QueryT a b m) where
    fmap f (QPure x)  = QPure (f x)
    fmap f (QLift mx) = QLift (fmap f <$> mx)
    fmap f (QReq r g) = QReq r $ fmap f . g

instance Applicative m => Applicative (QueryT a b m) where
    pure = QPure
    QPure f <*> q  = fmap f q
    QLift mf <*> q = case q of
                       QPure x  -> QLift (fmap ($ x) <$> mf)
                       QLift mx -> QLift (liftA2 (<*>) mf mx)   -- dangerous?
                       QReq r g -> QReq r $ \s -> QLift $ fmap (<*> g s) mf -- dangerous?
    QReq r g <*> q = QReq r $ \s -> g s <*> q

-- instance Alternative m => Alternative (QueryT a b m) where
--     empty   = QLift empty
--     QPure x  <|> _ = QPure x
--     QLift mx <|> y = QLift $ mx <|> pure y          -- what about case of QLift (pure (QLift empty)) ?

instance Monad m => Monad (QueryT a b m) where
    return = QPure
    QPure x >>= f  = f x
    QLift mx >>= f = QLift $ fmap (>>= f) mx        -- dangerous?
    QReq r g >>= f = QReq r $ \s -> case g s of
                                      QPure x    -> f x
                                      QLift mx   -> QLift $ fmap (>>= f) mx   -- dangerous?
                                      QReq r' g' -> QReq r' (f <=< g')

instance MonadTrans (QueryT a b) where
    lift = QLift . fmap QPure

query :: a -> QueryT a b m b
query r = QReq r QPure

runQueryTM :: (Traversable t, Monad t, Monad m) => (a -> m b) -> QueryT a b t r -> m (t r)
runQueryTM f q = case q of
                   QPure x  -> return (return x)
                   QLift mx -> fmap join . sequence . fmap (runQueryTM f) $ mx      -- better way?
                   QReq r g -> runQueryTM f . g =<< f r

runQueryM :: Monad m => (a -> m b) -> Query a b r -> m r
runQueryM f = fmap runIdentity . runQueryTM f

runQueryT :: Monad t => (a -> b) -> QueryT a b t r -> t r
runQueryT f q = case q of
                  QPure x  -> return x
                  QLift mx -> runQueryT f =<< mx
                  QReq r g -> runQueryT f . g $ f r

runQuery :: (a -> b) -> Query a b r -> r
runQuery f = runIdentity . runQueryT f

-- onLookup :: (a -> Maybe b) -> QueryT a b Maybe r -> Maybe r
-- onLookup f = runQueryM

-- data Foo = Foo { fooBar :: String
--                , fooBaz :: Int
--                }


