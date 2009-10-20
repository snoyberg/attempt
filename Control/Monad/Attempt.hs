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
    ) where

import Data.Attempt
import Control.Applicative
import Control.Monad
import Control.Monad.Attempt.Class
import Control.Monad.Trans

newtype AttemptT m v = AttemptT {
    runAttemptT :: m (Attempt v)
}

instance Functor m => Functor (AttemptT m) where
    fmap f = AttemptT . fmap (fmap f) . runAttemptT
instance Applicative m => Applicative (AttemptT m) where
    pure = AttemptT . pure . Success
    (AttemptT mf) <*> (AttemptT mv) = AttemptT $ fmap (<*>) mf <*> mv
instance Monad m => Monad (AttemptT m) where
    return = AttemptT . return . Success
    (AttemptT mv) >>= f = AttemptT $
        mv >>= attempt (return . Failure) (runAttemptT . f)
instance Monad m => MonadAttempt (AttemptT m) where
    failure = AttemptT . return . Failure
instance MonadTrans AttemptT where
    lift = AttemptT . fmap' Success where
        fmap' f m = m >>= return . f
instance MonadIO m => MonadIO (AttemptT m) where
    liftIO = AttemptT . fmap' Success where
        fmap' f m = liftIO m >>= return . f
