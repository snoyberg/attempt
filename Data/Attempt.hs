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

-- | A universal data type for computations which may fail. Errors are reported
-- using extensible exceptions. These exceptions are not explicitly stated; if
-- you want this kind of functionality, something like control-monad-exception
-- might be a more appropriate fit.
module Data.Attempt
    ( -- * Data type and type class
      Attempt (..)
    , FromAttempt (..)
    , fa
    , joinAttempt
      -- * General handling of 'Attempt's
    , attempt
    , makeHandler
    , AttemptHandler (..)
      -- * Individual 'Attempt's
    , isFailure
    , isSuccess
    , fromSuccess
      -- * Lists of 'Attempt's
    , successes
    , failures
    , partitionAttempts
      -- * Reimport the 'MonadAttempt' class
    , module Control.Monad.Attempt.Class
    ) where

import qualified Control.Exception as E
import Control.Monad (ap)
import Control.Applicative
import Data.Generics
import Control.Monad.Attempt.Class
import Data.Either (lefts)
import Control.Monad.Loc

type StackTrace = [String]
showStack :: StackTrace -> String
showStack = concatMap ((++) "\n    at ")

-- | Contains either a 'Success' value or a 'Failure' exception.
data Attempt v =
    Success v
    | Failure StackTrace E.SomeException
    deriving (Typeable)

instance Show v => Show (Attempt v) where
    show (Success v) = "Success " ++ show v
    show (Failure st e) = "Failure " ++ show e ++ showStack st

instance Functor Attempt where
    fmap f (Success v) = Success $ f v
    fmap _ (Failure s e) = Failure s e
instance Applicative Attempt where
    pure = Success
    (<*>) = ap
instance Monad Attempt where
    return = Success
    (Success v) >>= f = f v
    (Failure st e) >>= _ = Failure st e
instance E.Exception e => MonadFailure e Attempt where
    failure = Failure [] . E.SomeException
instance WrapFailure Attempt where
    wrapFailure _ (Success v) = Success v
    wrapFailure f (Failure st (E.SomeException e)) =
        Failure st $ E.SomeException $ f e
instance MonadLoc Attempt where
    withLoc _ (Success v) = Success v
    withLoc s (Failure st e) = Failure (s:st) e

-- | Any type which can be converted from an 'Attempt'. The included instances are your \"usual suspects\" for dealing with error handling. They include:
--
-- 'IO': For the IO instance, any exceptions in the 'Attempt' are thrown as
-- runtime exceptions.
--
-- 'Maybe': Returns 'Nothing' on 'Failure', or 'Just' on 'Success'.
--
-- List: Returns the empty list on 'Failure', or a singleton list on 'Success'.
--
-- 'Either' 'String': Returns 'Left' ('show' exception) on 'Failure', or 'Right' on
-- 'Success'.
--
-- 'Either' 'E.Exception': Returns 'Left' exception on 'Failure', or 'Right' on
-- 'Success'.
class FromAttempt a where
    fromAttempt :: Attempt v -> a v

-- | A shortcut for 'fromAttempt'.
fa :: FromAttempt a => Attempt v -> a v
fa = fromAttempt

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

-- | This is not a simple translation of the Control.Monad.join function.
-- Instead, for 'Monad's which are instances of 'FromAttempt', it removes the
-- inner 'Attempt' type, reporting errors as defined in the 'FromAttempt'
-- instance.
--
-- For example, join (Just (failureString \"foo\")) == Nothing.
joinAttempt :: (FromAttempt m, Monad m) => m (Attempt v) -> m v
joinAttempt = (>>= fromAttempt)

-- | Process either the exception or value in an 'Attempt' to produce a result.
--
-- This function is modeled after 'maybe' and 'either'. The first argument must
-- accept any instances of 'E.Exception'. If you want to handle multiple types
-- of exceptions, see 'makeHandler'. The second argument converts the success
-- value.
attempt :: (forall e. E.Exception e => e -> b) -- ^ error handler
        -> (a -> b) -- ^ success handler
        -> Attempt a
        -> b
attempt _ f (Success v) = f v
attempt f _ (Failure _ e) = f e

-- | Convert multiple 'AttemptHandler's and a default value into an exception
-- handler.
--
-- This is a convenience function when you want to have special handling for a
-- few types of 'E.Exception's and provide another value for anything else.
makeHandler :: [AttemptHandler v] -> v -> (forall e. E.Exception e => e -> v)
makeHandler [] v _ = v
makeHandler (AttemptHandler h:hs) v e =
    case cast e of
        Nothing -> makeHandler hs v e
        Just e' -> h e'

-- | A simple wrapper value necesary due to the Haskell type system. Wraps a
-- function from a *specific* 'E.Exception' type to some value.
data AttemptHandler v = forall e. E.Exception e => AttemptHandler (e -> v)


-- | Tests for a 'Failure' value.
isFailure :: Attempt v -> Bool
isFailure = attempt (const True) (const False)

-- | Tests for a 'Success' value.
isSuccess :: Attempt v -> Bool
isSuccess = attempt (const False) (const True)

-- | This is an unsafe, partial function which should only be used if you
-- either know that a function will succeed or don't mind the occassional
-- runtime exception.
fromSuccess :: Attempt v -> v
fromSuccess = attempt (error . show) id

-- | Returns only the 'Success' values.
successes :: [Attempt v] -> [v]
successes l = [ v | Success v <- l ]

-- | Returns only the 'Failure' values, each wrapped in a 'SomeException'.
failures :: [Attempt v] -> [E.SomeException]
failures = lefts . map eitherExceptionFromAttempt where
    eitherExceptionFromAttempt :: Attempt v -> Either E.SomeException v
    eitherExceptionFromAttempt = fa

-- | Return all of the 'Failure's and 'Success'es separately in a tuple.
partitionAttempts :: [Attempt v] -> ([E.SomeException], [v])
partitionAttempts = foldr (attempt f s) ([],[])
 where
  f a (l, r) = (E.SomeException a:l, r)
  s a (l, r) = (l, a:r)
