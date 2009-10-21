{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
---------------------------------------------------------
--
-- Module        : Control.Monad.Attempt.Class
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Unstable
-- Portability   : portable
--
---------------------------------------------------------
module Control.Monad.Attempt.Class
    ( MonadAttempt (..)
    , StringException (..)
    ) where

import Control.Exception
import Data.Generics

class (Functor m, Monad m) => MonadAttempt m where
    failure :: Exception e => e -> m v
    failureString :: String -> m v
    failureString = failure . StringException

newtype StringException = StringException String
    deriving Typeable
instance Show StringException where
    show (StringException s) = s
instance Exception StringException
