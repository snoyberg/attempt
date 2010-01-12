{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
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
    , attemptT
    , catchRuntime
    , module Data.Attempt
    ) where

import Data.Attempt
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Exception (Exception, handle)

newtype AttemptT m v = AttemptT {
    runAttemptT :: m (Attempt v)
}

instance Monad m => Functor (AttemptT m) where
    fmap f = AttemptT . liftM (liftM f) . runAttemptT
instance Monad m => Applicative (AttemptT m) where
    pure = return
    (<*>) = ap
instance Monad m => Monad (AttemptT m) where
    return = AttemptT . return . return
    (AttemptT mv) >>= f = AttemptT $ do
        v <- mv
        attempt (return . failure) (runAttemptT . f) v
instance (Exception e, Monad m) => Failure e (AttemptT m) where
    failure = AttemptT . return . failure
instance (Monad m, Exception e) => WrapFailure e (AttemptT m) where
    wrapFailure f (AttemptT mv) = AttemptT $ liftM (wrapFailure f) mv
instance MonadTrans AttemptT where
    lift = AttemptT . liftM return where
instance MonadIO m => MonadIO (AttemptT m) where
    liftIO = AttemptT . liftM return . liftIO where
instance Monad m => FromAttempt (AttemptT m) where
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

-- | The equivalent of 'attempt' for transformers. Given a success and failure
-- handler, eliminates the 'AttemptT' portion of the transformer stack.
attemptT :: Monad m
         => (forall e. Exception e => e -> b)
         -> (a -> b)
         -> AttemptT m a
         -> m b
attemptT s f = liftM (attempt s f) . runAttemptT

-- | Catches runtime (ie, IO) exceptions and represents them in an 'AttemptT'
-- transformer.
--
-- Like 'handle', the first argument to this function must explicitly state the
-- type of its input.
catchRuntime :: (Exception eIn, Exception eOut)
             => (eIn -> eOut)
             -> IO v
             -> AttemptT IO v
catchRuntime f =
      AttemptT
    . handle (return . Failure . f)
    . fmap Success
