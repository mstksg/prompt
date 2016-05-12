{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Control.Monad.Prompt
-- Description : Prompt monad and transformer
-- Copyright   : (c) Justin Le 2015
-- License     : MIT
-- Maintainer  : justin@jle.im
-- Stability   : unstable
-- Portability : portable
--
-- Provides the 'PromptT' type, which allows you to program computations
-- that can "ask" or "prompt" with values to get values in return.  The
-- computation doesn't care about the process of prompting, or how it
-- works, and has nothing to do with the effectful monad where the
-- prompting will eventually take place.
--
-- For example, sometimes you might want a computation to be able to query
-- or database, or talk with stdio, but you don't want your type to involve
-- arbitrary IO or be over IO, opening the door to a mess of IO.  'Prompt'
-- lets you write programs that can query "something", and then at a later
-- point in time, run it, providing the method of fulfilling each prompt.
-- Write your program independent of IO, or databases, or stdio, etc.; only
-- later "fill in" what it means.  You can even run the same 'Prompt' with
-- different ways to fulfill the prompts --- pure, effectful, etc.
--
-- For usage examples and a more detailed explanation, see
-- <https://github.com/mstksg/prompt the README>.

module Control.Monad.Prompt (
  -- * Prompt
    Prompt
  , runPromptM
  , runPrompt
  , interactP
  -- * PromptT
  , PromptT
  , runPromptTM
  , runPromptT
  , interactPT
  -- * Prompting
  , MonadPrompt(..)
  , prompt'
  , prompts'
  -- ** Specialized
  , promptP
  , promptsP
  , promptP'
  , promptsP'
  -- * Low level
  , mapPromptT
  , hoistP
  , liftP
  , mkPromptT
  , mkPrompt
  ) where

import Control.Applicative
import Control.Monad.Compat hiding (sequence, mapM, msum)
import Control.Monad.Error.Class
import Control.Monad.Prompt.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Writer.Class
import Data.Foldable
import Data.Functor.Identity
import Prelude.Compat


-- | Like 'Prompt', but can perform its "pure" computations in the context
-- of a 'Traversable' @t@, to absorb short-circuiting behvaior with 'Maybe'
-- or 'Either', logging with 'Writer', etc., but this is in general
-- completely unrelated to the effectful monad where the prompting will
-- eventually take place.  Specify short-circuiting and logging logic,
-- without worrying about IO or anything relating to the prompting effect.
--
-- @
-- 'prompt' :: a -> (PromptT a b t) b
-- @
--
-- Implements several useful typeclasses for working with the underlying
-- 'Traversable' and integrating effects, like 'Alternative', 'MonadError',
-- 'MonadWriter', etc.
--
-- Constructor is hidden, but a direct constructing function is exported as
-- 'mkPrompT' in the rare case it is needed or wanted.
--
newtype PromptT a b t r = PromptT (forall m. Monad m => (a -> m (t b)) -> m (t r))

-- | Prompt type, providing the ability to "prompt" or "query" by
-- presenting/asking with an @a@ and receiving a @b@ response.
--
-- @
-- 'prompt' :: a -> (Prompt a b) b
-- @
--
-- "Ask with an @a@, get a @b@."
--
-- Has a 'Monad', 'Applicative', 'Functor', etc. instance so it can be
-- sequenced monadically or applicatively, so you can sequence and bind
-- from 'prompt'.
--
-- Note that we defer the process of specifying /how/ 'prompt' delivers its
-- @b@.  It can take place in IO, or in any other effectful setting...but
-- 'Prompt' doesn't care, and it never involves IO or any arbitrary IO
-- itself.
--
-- Can be "constructed directly" using 'mkPrompt', but typically using
-- 'prompt' and the 'Applicative', 'Monad' instances etc. is better.
--
type Prompt a b = PromptT a b Identity

instance Functor t => Functor (PromptT a b t) where
#if MIN_VERSION_base(4,8,0)
    fmap f (PromptT p) = PromptT $ (fmap . fmap) f . p
#else
    fmap f (PromptT p) = PromptT $ (liftM . fmap) f . p
#endif

instance Applicative t => Applicative (PromptT a b t) where
    pure x = PromptT $ const (return (pure x))
#if MIN_VERSION_base(4,8,0)
    PromptT f <*> PromptT x = PromptT $ \g -> liftA2 (<*>) (f g) (x g)
#else
    PromptT f <*> PromptT x = PromptT $ \g -> liftM2 (<*>) (f g) (x g)
#endif

instance (Alternative t, Traversable t) => Alternative (PromptT a b t) where
    empty = PromptT $ const (return empty)
    PromptT x <|> PromptT y = PromptT $ \g -> do
        x' <- x g
        let c = (return . pure <$> x') <|> pure (y g)
#if MIN_VERSION_base(4,8,0)
        -- TODO: Is this okay?????
        -- join <$> sequence c
        asum <$> sequence c
#else
        asum `liftM` sequence c
#endif

instance (Monad t, Traversable t) => Monad (PromptT a b t) where
    return x = PromptT $ const (return (return x))
    PromptT p >>= f = PromptT $ \g -> do
#if MIN_VERSION_base(4,8,0)
        PromptT x <- traverse f <$> p g
        join <$> x g
#else
        PromptT x <- mapM f `liftM` p g
        join `liftM` x g
#endif

instance (MonadPlus t, Traversable t) => MonadPlus (PromptT a b t) where
#if MIN_VERSION_base(4,8,0)
    mzero = empty
    mplus = (<|>)
#else
    mzero = PromptT $ const (return mzero)
    PromptT x `mplus` PromptT y = PromptT $ \g -> do
        x' <- x g
        let c = (return . return <$> x') `mplus` return (y g)
        msum `liftM` sequence c
#endif

instance MonadTrans (PromptT a b) where
    lift x = PromptT $ const (return x)

instance (MonadError e t, Traversable t) => MonadError e (PromptT a b t) where
    throwError = lift . throwError
    catchError (PromptT p) f = PromptT $ \g -> do
      x <- p g
      let PromptT p' = sequence $ fmap return x `catchError` \e -> return (f e)
#if MIN_VERSION_base(4,8,0)
      join <$> p' g
#else
      join `liftM` p' g
#endif

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

-- | Directly construct a 'PromptT'.  Has to be able to take a @(a - m (t
-- b)) -> m (t r)@ that can work on /any/ 'Monad'.
--
-- Typically this won't be used, but is provided for completion; using
-- 'prompt' and its 'Applicative', 'Monad' instances, etc., is more clear.
--
-- @
-- 'prompt' r = 'mkPromptT' $ \g -> g r
-- @
mkPromptT :: (forall m. Monad m => (a -> m (t b)) -> m (t r)) -> PromptT a b t r
mkPromptT = PromptT

-- | Directly construct a 'Prompt'.  Has to be able to take a @(a -> m b)
-- -> m r@ that can work on /any/ 'Monad'.
--
-- Typically this won't be used, but is provided for completion; using
-- 'prompt' and its 'Applicative', 'Monad' instances, etc., is more clear.
mkPrompt :: (forall m. Monad m => (a -> m b) -> m r) -> Prompt a b r
#if MIN_VERSION_base(4,8,0)
mkPrompt f = PromptT $ \g -> Identity <$> f (fmap runIdentity . g)
#else
mkPrompt f = PromptT $ \g -> Identity `liftM` f (liftM runIdentity . g)
#endif

-- | Maps the underying @t a@ returned by 'PromptT'.  Cannot change @t@.
mapPromptT :: (t r -> t s) -> PromptT a b t r -> PromptT a b t s
#if MIN_VERSION_base(4,8,0)
mapPromptT f (PromptT p) = PromptT $ fmap f . p
#else
mapPromptT f (PromptT p) = PromptT $ liftM f . p
#endif

-- | Swap out the 'Traversable' @t@ with a pair of natural transformations.
-- The first maps the output @t a@, and the second maps the result of the
-- prompting function.
hoistP :: (forall s. t s -> u s)    -- ^ forward natural transformation
       -> (forall s. u s -> t s)    -- ^ backwards natural transformation
       -> PromptT a b t r
       -> PromptT a b u r
#if MIN_VERSION_base(4,8,0)
hoistP to from (PromptT p) = PromptT $ \g -> to <$> p (fmap from . g)
#else
hoistP to from (PromptT p) = PromptT $ \g -> to `liftM` p (liftM from . g)
#endif

-- | Like 'lift', but without the 'Monad' constraint.
liftP :: t r -> PromptT a b t r
liftP x = PromptT $ const (return x)

-- | Like 'prompt', but specialized to 'PromptT' and without
-- the 'Applicative' constraint.
promptP :: a                    -- ^ prompting value
        -> PromptT a b t b
promptP r = PromptT ($ r)

-- | Like 'prompts', but specialized to 'PromptT' and downgrading the
-- 'Applicative' constraint to a 'Functor' constraint.
promptsP :: Functor t
         => (b -> c)            -- ^ to be applied to response value
         -> a                   -- ^ prompting value
         -> PromptT a b t c
#if MIN_VERSION_base(4,8,0)
promptsP f r = PromptT $ (fmap . fmap) f . ($ r)
#else
promptsP f r = PromptT $ (liftM . fmap) f . ($ r)
#endif

-- | Like 'prompt'', but specialized to 'PromptT' and without the
-- 'Applicative' constraint.  Is a 'promptP' strict on its argument.
promptP' :: a                   -- ^ prompting value (strict)
         -> PromptT a b t b
promptP' x = x `seq` promptP x

-- | Like 'prompts'', but specialized to 'PromptT' and downgrading the
-- 'Applicative' constraint to a 'Functor' constraint.  Is a 'promptsP'
-- strict on its argument.
promptsP' :: Functor t
          => (b -> c)           -- ^ to be applied to response value
          -> a                  -- ^ prompting value (strict)
          -> PromptT a b t c
promptsP' f x = x `seq` promptsP f x

-- | Run a @'PromptT' a b t r@ with a given effectful @a -> m (t b)@
-- "prompt response" function, to get the resulting @r@ in @m@ and @t@.
-- The "prompt response" function is able to interact with the underlying
-- 'Traversable' @t@.
--
-- Note that the 'PromptT' in general has nothing to do with the @m@, and
-- cannot execute arbitrary @m@ other than that given in the prompt
-- response function.
runPromptTM :: Monad m
            => PromptT a b t r
            -> (a -> m (t b))   -- ^ "Prompt response function",
                                -- effectfully responding to a given @a@ with a @b@.
            -> m (t r)
runPromptTM (PromptT p) = p

-- | Run a @'Prompt' a b r@ with a given effectful @a -> m b@ "prompt
-- response" function, to get the resulting @r@ in @m@.  Note that the
-- 'Prompt' itself in general has nothing to do with @m@, and cannot
-- execute arbitrary @m@ other than that given in the prompt response
-- function.
--
-- Effectively treats a @'Prompt' a b@ as a @forall m. ReaderT (a -> m b) m@
runPromptM :: Monad m
           => Prompt a b r
           -> (a -> m b)   -- ^ "Prompt response function", effectfully
                           -- responding to a given @a@ with a @b@.
           -> m r
#if MIN_VERSION_base(4,8,0)
runPromptM (PromptT p) f = runIdentity <$> p (fmap Identity . f)
#else
runPromptM (PromptT p) f = runIdentity `liftM` p (liftM Identity . f)
#endif

-- | Run a @'PromptT' a b t r@ with a given @a -> t b@ function, with
-- 'Traversable' @t@.  The effects take place in the same context as the
-- underlying context of the 'PromptT'.
runPromptT :: PromptT a b t r
           -> (a -> t b)    -- ^ "Prompt response function", "purely"
                            -- responding to a given @a@ with a @b@ in
                            -- context of 'Traversable' @t@.
           -> t r
runPromptT (PromptT p) f = runIdentity $ p (Identity . f)

-- | Run a @'Prompt' a b r@ with a pure @a -> b@ prompt response function.
-- More or less reduces @'Prompt' a b@ to a @'Reader' (a -> b)@.
runPrompt :: Prompt a b r
          -> (a -> b)   -- ^ "Prompt response function", purely responding
                        -- to a given @a@ with a @b@.
          -> r
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

