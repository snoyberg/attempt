{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
---------------------------------------------------------
--
-- Module        : Data.Attempt
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Unstable
-- Portability   : portable
--
-- A universal data type for computations which may fail.
---------------------------------------------------------
module Data.Attempt
    ( Attempt (..)
    , FromAttempt (..)
    , attempt
    , AttemptHandler
    , makeHandler
    ) where

import qualified Control.Exception as E
import Control.Monad (ap)
import Control.Applicative
import Data.Generics
import Control.Monad.Attempt.Class

data Attempt v =
    Success v
    | forall e. E.Exception e => Failure e
    deriving (Typeable)

instance Show v => Show (Attempt v) where
    show (Success v) = "Success " ++ show v
    show (Failure e) = "Failure " ++ show e
instance MonadAttempt Attempt where
    success = Success
    failure = Failure

instance Functor Attempt where
    fmap f (Success v) = Success $ f v
    fmap _ (Failure e) = Failure e
instance Applicative Attempt where
    pure = Success
    (<*>) = ap
instance Monad Attempt where
    return = Success
    (Success v) >>= f = f v
    (Failure e) >>= _ = Failure e

class FromAttempt a where
    fromAttempt :: Attempt v -> a v

instance FromAttempt IO where
    fromAttempt (Success v) = return v
    fromAttempt (Failure e) = E.throwIO e
instance FromAttempt Maybe where
    fromAttempt (Success v) = Just v
    fromAttempt (Failure _) = Nothing
instance FromAttempt [] where
    fromAttempt (Success v) = [v]
    fromAttempt (Failure _) = []
instance FromAttempt (Either String) where
    fromAttempt (Success v) = Right v
    fromAttempt (Failure e) = Left $ show e
instance FromAttempt (Either E.SomeException) where
    fromAttempt (Success v) = Right v
    fromAttempt (Failure e) = Left $ E.SomeException e

attempt :: (forall e. E.Exception e => e -> b) -> (a -> b) -> Attempt a -> b
attempt _ f (Success v) = f v
attempt f _ (Failure e) = f e

data AttemptHandler v = forall e. E.Exception e => AttemptHandler (e -> v)

makeHandler :: [AttemptHandler v] -> v -> (forall e. E.Exception e => e -> v)
makeHandler [] v _ = v
makeHandler (AttemptHandler h:hs) v e =
    case cast e of
        Nothing -> makeHandler hs v e
        Just e' -> h e'
