{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Control.Monad.Prompt
-- Description : Prompt monad and transformer
-- Copyright   : (c) Justin Le 2015
-- License     : MIT
-- Maintainer  : justin@jle.im
-- Stability   : unstable
-- Portability : portable
--

module Control.Monad.Prompt (
    MonadPrompt(..)
  , PromptT(..)
  , Prompt
  , runPromptTM
  , runPromptM
  , runPromptT
  , runPrompt
  , prompt'
  , prompts'
  , interactP
  , interactPT
  , mapPromptT
  , hoistP
  , liftP
  , promptP
  , promptsP
  , promptP'
  , promptsP'
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

-- | Prompt type, providing the ability to "prompt" or "query" by
-- presenting/asking with an @a@ and receiving a @b@ response.
--
-- @
-- 'prompt' :: a -> (PromptT a b t) b
-- @
--
-- "Ask with an @a@, get a @b@."
--
-- Note that we defer the process of specifying /how/ 'prompt' delivers its
-- @b@.  It can take place in IO, or in any other effectful setting...but
-- 'PromptT' doesn't care, and it never involves IO or any arbitrary IO
-- itself.
--
-- 'PromptT' can perform its "pure" computations in the context of
-- a 'Traversable' @t@, to absorb short-circuiting behvaior with 'Maybe' or
-- 'Either', logging with 'Writer', etc., but this is in general completely
-- unrelated to the effectful monad where the prompting will eventually
-- take place.
--
newtype PromptT a b t r = PromptT { runPromptTM :: forall m. Monad m => (a -> m (t b)) -> m (t r) }

-- | Provides the ability to "prompt" and "query", asking with an @a@ for
-- a @b@.  A type synonym for 'PromptT' without any underlying
-- 'Traversable'.
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

-- | Maps the underying @t a@ returned by 'PromptT'.  Cannot change @t@.
mapPromptT :: (t r -> t s) -> PromptT a b t r -> PromptT a b t s
mapPromptT f (PromptT p) = PromptT $ fmap f . p

-- | Swap out the 'Traversable' @t@ with a pair of natural transformations.
-- The first maps the output @t a@, and the second maps the result of the
-- prompting function.
hoistP :: (forall s. t s -> u s)
          -> (forall s. u s -> t s)
          -> PromptT a b t r
          -> PromptT a b u r
hoistP to from (PromptT p) = PromptT $ \g -> to <$> p (fmap from . g)

-- | Like 'lift', but without the 'Monad' constraint.
liftP :: t r -> PromptT a b t r
liftP x = PromptT $ const (return x)

-- | Like 'prompt', but specialized to 'PromptT' and without
-- a 'Applicative' constraint.
promptP :: a -> PromptT a b t b
promptP r = PromptT ($ r)

-- | Like 'prompts', but specialized to 'PromptT' and downgrading the
-- 'Applicative' constraint to a 'Functor' constraint.
promptsP :: Functor t => (b -> c) -> a -> PromptT a b t c
promptsP f r = PromptT $ (fmap . fmap) f . ($ r)

-- | Like 'prompt'', but specialized to 'PromptT' and without the
-- 'Applicative' constraint.  Is a 'promptP' strict on its argument.
promptP' :: a -> PromptT a b t b
promptP' x = x `seq` promptP x

-- | Like 'prompts'', but specialized to 'PromptT' and downgrading the
-- 'Applicative' constraint to a 'Functor' constraint.  Is a 'promptsP'
-- strict on its argument.
promptsP' :: Functor t => (b -> c) -> a -> PromptT a b t c
promptsP' f x = x `seq` promptsP f x

-- | Run a @'Prompt' a b r@ with a given effectful @a -> m b@ function, to
-- get the resulting @r@ in @m@.  Note that the @Prompt@ itself in general
-- has nothing to do with @m@, and cannot execute arbitrary @m@ other than
-- that given in the prompt response function.
runPromptM :: Monad m => Prompt a b r -> (a -> m b) -> m r
runPromptM (PromptT p) f = runIdentity <$> p (fmap Identity . f)

-- | Run a @'PromptT' a b t r@ with a given @a -> t b@ function, with
-- 'Traversable' @t@.  The effects take place in the same context as the
-- underlying context of the 'PromptT'.
runPromptT :: PromptT a b t r -> (a -> t b) -> t r
runPromptT (PromptT p) f = runIdentity $ p (Identity . f)

-- | Run a @'Prompt' a b r@ with a pure @a -> b@ prompt response function.
-- More or less reduces @'Prompt' a b@ to a @'Reader' (a -> b)@.
runPrompt :: Prompt a b r -> (a -> b) -> r
runPrompt (PromptT p) f = runIdentity . runIdentity $ p (Identity . Identity . f)

-- | Run a @'PromptT' String String@ in IO by sending the request to stdout
-- and reading the response from stdin.
interactPT :: Applicative t => PromptT String String t r -> IO (t r)
interactPT = flip runPromptTM $ \str -> do
    putStrLn str
    pure <$> getLine

-- | Run a @'Prompt' String String@ in IO by sending the request to stdout
-- and reading the response from stdin.
interactP :: Prompt String String r -> IO r
interactP = flip runPromptM $ \str -> do
    putStrLn str
    getLine

