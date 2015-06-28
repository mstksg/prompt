{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Monad.Prompt (
    MonadPrompt(..)
  , PromptT(..)
  , Prompt
  , runPromptM
  , runPromptT
  , runPrompt
  , prompt'
  , prompts'
  , interactP
  , interactPT
  , mapPromptT
  , hoistIsoP
  , liftP
  , promptP
  , promptsP
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Prompt.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Writer.Class
import Data.Functor.Identity

newtype PromptT a b t r = PromptT { runPromptTM :: forall m. Monad m => (a -> m (t b)) -> m (t r) }

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
    local = mapPromptT . local

instance (MonadState s t, Traversable t) => MonadState s (PromptT a b t) where
    get = lift get
    put = lift . put
    state = lift . state

instance (MonadWriter w t, Traversable t) => MonadWriter w (PromptT a b t) where
    writer = lift . writer
    tell = lift . tell
    listen = mapPromptT listen
    pass = mapPromptT pass

instance Applicative t => MonadPrompt a b (PromptT a b t) where
    prompt = promptP
    prompts = promptsP

mapPromptT :: (t r -> t s) -> PromptT a b t r -> PromptT a b t s
mapPromptT f (PromptT p) = PromptT $ fmap f . p

hoistIsoP :: (forall s. t s -> u s)
          -> (forall s. u s -> t s)
          -> PromptT a b t r
          -> PromptT a b u r
hoistIsoP to from (PromptT p) = PromptT $ \g -> to <$> p (fmap from . g)

liftP :: Applicative t => t r -> PromptT a b t r
liftP x = PromptT $ const (return x)

promptP :: a -> PromptT a b t b
promptP r = PromptT ($ r)

promptsP :: Functor t => (b -> c) -> a -> PromptT a b t c
promptsP f r = PromptT $ (fmap . fmap) f . ($ r)

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

