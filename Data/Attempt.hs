{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
---------------------------------------------------------
--
-- Module        : Data.Object
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
    , FailedAttempt (..)
    , FromAttempt (..)
    , attemptJoin
    , attemptLookup
    , attempt
    , AttemptHandler
    , makeHandler
    ) where

import qualified Control.Exception as E
import Control.Monad (ap)
import Control.Applicative
import Data.Generics

data Attempt v =
    Success v
    | forall e. E.Exception e => Failure e
    deriving (Typeable)

instance Show v => Show (Attempt v) where
    show (Success v) = "Success " ++ show v
    show (Failure e) = "Failure " ++ show e

newtype FailedAttempt = FailedAttempt { getMessage :: String }
    deriving (Show, Eq, Data, Typeable)
instance E.Exception FailedAttempt

instance Functor Attempt where
    fmap f (Success v) = Success $ f v
    fmap _ (Failure e) = Failure e
instance Applicative Attempt where
    pure = Success
    (<*>) = ap
instance Monad Attempt where
    return = Success
    fail = Failure . FailedAttempt
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

attemptJoin :: (FromAttempt m, Monad m) => m (Attempt v) -> m v
attemptJoin m = m >>= fromAttempt

data KeyNotFound k v = KeyNotFound k [(k, v)]
    deriving Typeable
instance Show k => Show (KeyNotFound k v) where
    show (KeyNotFound key _) = "Could not find requested key: " ++ show key
instance (Typeable k, Typeable v, Show k) => E.Exception (KeyNotFound k v)

attemptLookup :: (Typeable k, Typeable v, Show k, Eq k)
              => k
              -> [(k, v)]
              -> Attempt v
attemptLookup k m = maybe (Failure $ KeyNotFound k m) Success $ lookup k m

attempt :: Attempt v -> (forall e. E.Exception e => e -> v) -> v
attempt (Success v) _ = v
attempt (Failure e) f = f e

data AttemptHandler v = forall e. E.Exception e => AttemptHandler (e -> v)

makeHandler :: [AttemptHandler v] -> v -> (forall e. E.Exception e => e -> v)
makeHandler [] v _ = v
makeHandler (AttemptHandler h:hs) v e =
    case cast e of
        Nothing -> makeHandler hs v e
        Just e' -> h e'
