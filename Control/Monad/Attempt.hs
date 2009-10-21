{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
---------------------------------------------------------
--
-- Module        : Control.Monad.Attempt
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Unstable
-- Portability   : portable
--
---------------------------------------------------------
module Control.Monad.Attempt
    ( AttemptT (..)
    , evalAttemptT
    ) where

import Data.Attempt
import Control.Applicative
import Control.Monad
import Control.Monad.Attempt.Class
import Control.Monad.Trans

newtype AttemptT m v = AttemptT {
    runAttemptT :: m (Attempt v)
}

instance (Functor m, Monad m) => Functor (AttemptT m) where
    fmap f = AttemptT . fmap (fmap f) . runAttemptT
instance (Functor m, Monad m) => Applicative (AttemptT m) where
    pure = return
    (<*>) = ap
instance (Functor m, Monad m) => Monad (AttemptT m) where
    return = AttemptT . return . Success
    (AttemptT mv) >>= f = AttemptT $
        mv >>= attempt (return . Failure) (runAttemptT . f)
instance (Functor m, Monad m) => MonadAttempt (AttemptT m) where
    failure = AttemptT . return . Failure
instance MonadTrans AttemptT where
    lift = AttemptT . fmap' Success where
        fmap' f m = m >>= return . f
instance (Functor m, MonadIO m) => MonadIO (AttemptT m) where
    liftIO = AttemptT . fmap' Success where
        fmap' f m = liftIO m >>= return . f
instance (Functor m, Monad m) => FromAttempt (AttemptT m) where
    fromAttempt = attempt failure return

evalAttemptT :: (Monad m, FromAttempt m)
             => AttemptT m v
             -> m v
evalAttemptT = join . fmap' fromAttempt . runAttemptT where
    fmap' :: Monad m => (a -> b) -> m a -> m b
    fmap' f ma = ma >>= return . f
