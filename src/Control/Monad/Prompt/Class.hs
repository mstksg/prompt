{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

-- |
-- Module      : Control.Monad.Prompt.Class
-- Description : Typeclass for contexts with prompting ability.
-- Copyright   : (c) Justin Le 2015
-- License     : MIT
-- Maintainer  : justin@jle.im
-- Stability   : unstable
-- Portability : portable
--
-- Provides a typeclass for 'Applicative' and 'Monad' types that give you
-- the ability to, at any time, "prompt" with an @a@ and get a @b@ in
-- response.  The power of this instance is that each type gets to define
-- its own way to deliver a response.
--
-- This library provides instances for 'PromptT' from
-- "Control.Monad.Prompt" and the monad transformers in /transformers/ and
-- /mtl/.  Feel free to create your own instances too.
--
-- @
-- data Interactive a = Interactive ((String -> String) -> a)
--
-- -- at any time, ask with a string to get a string
-- instance MonadPrompt String String Interactive where
--     prompt str = Interactive $ \f -> f str
-- @

module Control.Monad.Prompt.Class (
    MonadPrompt(..)
  , prompt'
  , prompts'
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Error
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Prelude.Compat
import qualified Control.Monad.Trans.RWS.Lazy      as RWSL
import qualified Control.Monad.Trans.RWS.Strict    as RWSS
import qualified Control.Monad.Trans.State.Lazy    as SL
import qualified Control.Monad.Trans.State.Strict  as SS
import qualified Control.Monad.Trans.Writer.Lazy   as WL
import qualified Control.Monad.Trans.Writer.Strict as WS


-- | An 'Applicative' (and possibly 'Monad') where you can, at any time,
-- "prompt" with an @a@ and receive a @b@ in response.
--
-- Instances include 'PromptT' and any /transformers/ monad transformer
-- over another 'MonadPrompt'.
class Applicative m => MonadPrompt a b m | m -> a b where
    -- | "Prompt" with an @a@ for a @b@ in the context of the type.
    prompt  :: a        -- ^ prompting value
            -> m b
    prompt = prompts id
    -- | "Prompt" with an @a@ for a @b@ in the context of the type, and
    -- apply the given function to receive a @c@.
    prompts :: (b -> c) -- ^ mapping function
            -> a        -- ^ prompting value
            -> m c
    prompts f = fmap f . prompt
#if MIN_VERSION_base(4,7,0)
    {-# MINIMAL prompt | prompts #-}
#endif

-- | A version of 'prompt' strict on its prompting value.
prompt' :: MonadPrompt a b m => a -> m b
prompt' x = x `seq` prompt x

-- | A version of 'prompts' strict on its prompting value.
prompts' :: MonadPrompt a b m => (b -> c) -> a -> m c
prompts' f x = x `seq` prompts f x

instance (Monad m, MonadPrompt a b m) => MonadPrompt a b (ReaderT r m) where
    prompt    = lift . prompt
    prompts f = lift . prompts f

instance (Monad m, MonadPrompt a b m) => MonadPrompt a b (ExceptT e m) where
    prompt    = lift . prompt
    prompts f = lift . prompts f

instance (Monad m, MonadPrompt a b m) => MonadPrompt a b (ErrorT e m) where
    prompt    = lift . prompt
    prompts f = lift . prompts f

instance (Monad m, MonadPrompt a b m) => MonadPrompt a b (SS.StateT s m) where
    prompt    = lift . prompt
    prompts f = lift . prompts f

instance (Monad m, MonadPrompt a b m) => MonadPrompt a b (SL.StateT s m) where
    prompt    = lift . prompt
    prompts f = lift . prompts f

instance (Monad m, MonadPrompt a b m, Monoid w) => MonadPrompt a b (WS.WriterT w m) where
    prompt    = lift . prompt
    prompts f = lift . prompts f

instance (Monad m, MonadPrompt a b m, Monoid w) => MonadPrompt a b (WL.WriterT w m) where
    prompt    = lift . prompt
    prompts f = lift . prompts f

instance (Monad m, MonadPrompt a b m, Monoid w) => MonadPrompt a b (RWSS.RWST r w s m) where
    prompt    = lift . prompt
    prompts f = lift . prompts f

instance (Monad m, MonadPrompt a b m, Monoid w) => MonadPrompt a b (RWSL.RWST r w s m) where
    prompt    = lift . prompt
    prompts f = lift . prompts f

instance (Monad m, MonadPrompt a b m) => MonadPrompt a b (MaybeT m) where
    prompt    = lift . prompt
    prompts f = lift . prompts f
