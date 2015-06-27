{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Monad.Query where

import Control.Applicative
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Writer.Class
import Data.Functor.Identity

data QueryT a b t r = QueryT { runQueryTM :: forall m. Monad m => (a -> m (t b)) -> m (t r) }

type Query a b = QueryT a b Identity

instance Functor t => Functor (QueryT a b t) where
    fmap f (QueryT q) = QueryT $ (fmap . fmap) f . q

instance Applicative t => Applicative (QueryT a b t) where
    pure x = QueryT $ const (return (pure x))
    QueryT f <*> QueryT x = QueryT $ \g -> liftA2 (<*>) (f g) (x g)

instance Alternative t => Alternative (QueryT a b t) where
    empty = QueryT $ const (return empty)
    QueryT x <|> QueryT y = QueryT $ \g -> liftA2 (<|>) (x g) (y g)

instance (Monad t, Traversable t) => Monad (QueryT a b t) where
    return x = QueryT $ const (return (return x))
    QueryT q >>= f = QueryT $ \g -> do
        QueryT x <- traverse f <$> q g
        join <$> x g

instance (MonadPlus t, Traversable t) => MonadPlus (QueryT a b t) where
    mzero = empty
    mplus = (<|>)

instance MonadTrans (QueryT a b) where
    lift x = QueryT $ const (return x)

instance (MonadError e t, Traversable t) => MonadError e (QueryT a b t) where
    throwError = lift . throwError
    catchError (QueryT q) f = QueryT $ \g -> do
      x <- q g
      let QueryT q' = sequence $ fmap return x `catchError` \e -> return (f e)
      join <$> q' g

instance (MonadReader r t, Traversable t) => MonadReader r (QueryT a b t) where
    ask = lift ask
    reader = lift . reader
    local = hoistQ . local

instance (MonadState s t, Traversable t) => MonadState s (QueryT a b t) where
    get = lift get
    put = lift . put
    state = lift . state

instance (MonadWriter w t, Traversable t) => MonadWriter w (QueryT a b t) where
    writer = lift . writer
    tell = lift . tell
    listen = hoistQ listen
    pass = hoistQ pass

hoistQ :: (t r -> t s) -> QueryT a b t r -> QueryT a b t s
hoistQ f (QueryT q) = QueryT $ fmap f . q

query :: Applicative t => a -> QueryT a b t b
query r = QueryT ($ r)

runQueryM :: Monad m => Query a b r -> (a -> m b) -> m r
runQueryM (QueryT q) f = runIdentity <$> q (fmap Identity . f)

runQueryT :: QueryT a b t r -> (a -> t b) -> t r
runQueryT (QueryT q) f = runIdentity $ q (Identity . f)

runQuery :: Query a b r -> (a -> b) -> r
runQuery (QueryT q) f = runIdentity . runIdentity $ q (Identity . Identity . f)

