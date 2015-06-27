{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Control.Monad.Prompt.Class where

class MonadPrompt a b m | m -> a b where
    prompt :: a -> m b

