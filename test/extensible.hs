{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.Failure
import Control.Exception
import Data.Typeable
import Prelude hiding (catch)
import Control.Monad.Attempt

data ParentException = forall e. Exception e => ParentException e
    deriving (Typeable)
instance Show ParentException where
    show (ParentException e) = "ParentException: " ++ show e
instance Exception ParentException

data ChildException = ChildException
    deriving (Typeable, Show)
instance Exception ChildException where
    toException = toException . ParentException
    fromException x = do
        ParentException e <- fromException x
        cast e

foo :: MonadFailure StackFrame m => m a
foo = failureAt "test location" ChildException

main = do
    catch foo handle1
    catch foo handle2
    catch foo handle3
    let ahandler1 :: Exception e => e -> IO ()
        ahandler1 = makeHandler
                        [ AttemptHandler handle1
                        ]
                        (putStrLn "Not caught1")
    attempt ahandler1 (\() -> putStrLn "Success... weird") foo
    let ahandler2 :: Exception e => e -> IO ()
        ahandler2 = makeHandler
                        [ AttemptHandler handle1
                        , AttemptHandler handle2
                        ]
                        (putStrLn "Not caught2")
    attempt ahandler2 (\() -> putStrLn "Success... weird") foo
    let ahandler3 :: Exception e => e -> IO ()
        ahandler3 = makeHandler
                        [ AttemptHandler handle1
                        , AttemptHandler handle2
                        , AttemptHandler handle3
                        ]
                        (putStrLn "Not caught3")
    attempt ahandler3 (\() -> putStrLn "Success... weird") foo
    let ahandler4 :: Exception e => e -> IO ()
        ahandler4 = makeHandler
                        [ AttemptHandler handle2
                        , AttemptHandler handle3
                        ]
                        (putStrLn "Not caught4")
    attempt ahandler4 (\() -> putStrLn "Success... weird") foo
    let ahandler5 :: Exception e => e -> IO ()
        ahandler5 = makeHandler
                        [ AttemptHandler handle3
                        ]
                        (putStrLn "Not caught5")
    attempt ahandler5 (\() -> putStrLn "Success... weird") foo
    runAttemptT foo

handle1 :: ChildException -> IO ()
handle1 e = putStrLn $ "handle1: " ++ show e

handle2 :: ParentException -> IO ()
handle2 e = putStrLn $ "handle2: " ++ show e

handle3 :: SomeException -> IO ()
handle3 e = putStrLn $ "handle3: " ++ show e
