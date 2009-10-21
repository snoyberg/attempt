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
    fromAttempt = attempt E.throwIO return
instance FromAttempt Maybe where
    fromAttempt = attempt (const Nothing) Just
instance FromAttempt [] where
    fromAttempt = attempt (const []) (: [])
instance FromAttempt (Either String) where
    fromAttempt = attempt (Left . show) Right
instance FromAttempt (Either E.SomeException) where
    fromAttempt = attempt (Left . E.SomeException) Right

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
