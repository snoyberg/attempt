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

-- | Provide a monad transformer for the attempt monad, which allows the
-- reporting of errors using extensible exceptions.
module Control.Monad.Attempt
    ( AttemptT (..)
    , evalAttemptT
    , module Data.Attempt
    ) where

import Data.Attempt
import Control.Applicative
import Control.Monad
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
    wrapFailure f (AttemptT mv) = AttemptT $ liftM (wrapFailure f) mv
instance MonadTrans AttemptT where
    lift = AttemptT . liftM Success where
instance (Functor m, MonadIO m) => MonadIO (AttemptT m) where
    liftIO = AttemptT . liftM Success . liftIO where
instance (Functor m, Monad m) => FromAttempt (AttemptT m) where
    fromAttempt = attempt failure return

-- | Instances of 'FromAttempt' specify a manner for embedding 'Attempt'
-- failures directly into the target data type. For example, the 'IO' instance
-- simply throws a runtime error. This is a convenience wrapper when you simply
-- want to use that default action.
--
-- So given a type 'AttemptT' 'IO' 'Int', this function will convert it to 'IO'
-- 'Int', throwing any exceptions in the original value.
evalAttemptT :: (Monad m, FromAttempt m)
             => AttemptT m v
             -> m v
evalAttemptT = join . liftM fromAttempt . runAttemptT where
