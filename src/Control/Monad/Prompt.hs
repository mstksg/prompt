{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Monad.Prompt where

import Control.Applicative
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Writer.Class
import Data.Functor.Identity
import Text.Read (readMaybe)

data PromptT a b t r = PromptT { runPromptTM :: forall m. Monad m => (a -> m (t b)) -> m (t r) }

type Prompt a b = PromptT a b Identity

instance Functor t => Functor (PromptT a b t) where
    fmap f (PromptT p) = PromptT $ (fmap . fmap) f . p

instance Applicative t => Applicative (PromptT a b t) where
    pure x = PromptT $ const (return (pure x))
    PromptT f <*> PromptT x = PromptT $ \g -> liftA2 (<*>) (f g) (x g)

instance Alternative t => Alternative (PromptT a b t) where
    empty = PromptT $ const (return empty)
    PromptT x <|> PromptT y = PromptT $ \g -> liftA2 (<|>) (x g) (y g)

instance (Monad t, Traversable t) => Monad (PromptT a b t) where
    return x = PromptT $ const (return (return x))
    PromptT p >>= f = PromptT $ \g -> do
        PromptT x <- traverse f <$> p g
        join <$> x g

instance (MonadPlus t, Traversable t) => MonadPlus (PromptT a b t) where
    mzero = empty
    mplus = (<|>)

instance MonadTrans (PromptT a b) where
    lift x = PromptT $ const (return x)

instance (MonadError e t, Traversable t) => MonadError e (PromptT a b t) where
    throwError = lift . throwError
    catchError (PromptT p) f = PromptT $ \g -> do
      x <- p g
      let PromptT p' = sequence $ fmap return x `catchError` \e -> return (f e)
      join <$> p' g

instance (MonadReader r t, Traversable t) => MonadReader r (PromptT a b t) where
    ask = lift ask
    reader = lift . reader
    local = hoistP . local

instance (MonadState s t, Traversable t) => MonadState s (PromptT a b t) where
    get = lift get
    put = lift . put
    state = lift . state

instance (MonadWriter w t, Traversable t) => MonadWriter w (PromptT a b t) where
    writer = lift . writer
    tell = lift . tell
    listen = hoistP listen
    pass = hoistP pass

hoistP :: (t r -> t s) -> PromptT a b t r -> PromptT a b t s
hoistP f (PromptT p) = PromptT $ fmap f . p

prompt :: a -> PromptT a b t b
prompt r = PromptT ($ r)

runPromptM :: Monad m => Prompt a b r -> (a -> m b) -> m r
runPromptM (PromptT p) f = runIdentity <$> p (fmap Identity . f)

runPromptT :: PromptT a b t r -> (a -> t b) -> t r
runPromptT (PromptT p) f = runIdentity $ p (Identity . f)

runPrompt :: Prompt a b r -> (a -> b) -> r
runPrompt (PromptT p) f = runIdentity . runIdentity $ p (Identity . Identity . f)

interactPT :: Applicative t => PromptT String String t r -> IO (t r)
interactPT = flip runPromptTM $ \str -> do
    putStrLn str
    pure <$> getLine

interactP :: Prompt String String r -> IO r
interactP = flip runPromptM $ \str -> do
    putStrLn str
    getLine
