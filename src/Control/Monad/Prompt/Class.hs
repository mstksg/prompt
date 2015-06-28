{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Control.Monad.Prompt.Class (
    MonadPrompt(..)
  , prompt'
  , prompts'
  ) where

import Control.Monad.Error
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import qualified Control.Monad.RWS.Lazy      as RWSL
import qualified Control.Monad.RWS.Strict    as RWSS
import qualified Control.Monad.State.Lazy    as SL
import qualified Control.Monad.State.Strict  as SS
import qualified Control.Monad.Writer.Lazy   as WL
import qualified Control.Monad.Writer.Strict as WS

class Applicative m => MonadPrompt a b m | m -> a b where
    prompt  :: a -> m b
    prompt = prompts id
    prompts :: (b -> c) -> a -> m c
    prompts f = fmap f . prompt
    {-# MINIMAL prompt | prompts #-}

prompt' :: MonadPrompt a b m => a -> m b
prompt' x = x `seq` prompt x

prompts' :: MonadPrompt a b m => (b -> c) -> a -> m c
prompts' f x = x `seq` prompts f x

instance (Monad m, MonadPrompt a b m) => MonadPrompt a b (ReaderT r m) where
    prompt    = lift . prompt
    prompts f = lift . prompts f

instance (Monad m, MonadPrompt a b m) => MonadPrompt a b (ExceptT e m) where
    prompt    = lift . prompt
    prompts f = lift . prompts f

instance (Monad m, MonadPrompt a b m, Error e) => MonadPrompt a b (ErrorT e m) where
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
