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

-- | Defines a type class for any monads which may report failure using
-- extensible exceptions.
module Control.Monad.Attempt.Class
    ( MonadAttempt (..)
    , StringException (..)
    ) where

import Control.Exception
import Data.Generics

-- | Any 'Monad' which may report failure using extensible exceptions. This
-- most obviously applies to the Attempt data type, but you should just as well
-- use this for arbitrary 'Monad's.
--
-- Usage should be straight forward: 'return' successes and 'failure' errors.
-- If you simply want to send a string error message, use 'failureString'.
-- Although tempting to do so, 'fail' is *not* used as a synonym for
-- 'failureString'; 'fail' should not be used at all.
--
-- Minimal complete definition: 'failure'.
class (Functor m, Monad m) => MonadAttempt m where
    failure :: Exception e => e -> m v
    failureString :: String -> m v
    failureString = failure . StringException

-- | A simple exception which simply contains a string. Note that the 'Show'
-- instance simply returns the contained string.
newtype StringException = StringException String
    deriving Typeable
instance Show StringException where
    show (StringException s) = s
instance Exception StringException
