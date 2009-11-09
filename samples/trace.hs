{-# OPTIONS_GHC -F -pgmF MonadLoc #-}
import Data.Attempt
import qualified Safe.Failure as A
import Control.Monad.Loc
import Control.Monad.Attempt

--foo :: String -> AttemptT IO Int
foo x = do
    A.read x

--bar :: String -> Attempt Int
bar x = do
    y <- foo x
    return $ y + 2

--baz :: String -> Attempt Int
baz x = do
    y <- bar x
    return $ y * 3

main = do
    print $ (baz "hello" :: Attempt Int)
    print $ (baz "5" :: Attempt Int)
    runAttemptT (baz "hello" :: AttemptT IO Int) >>= print
    runAttemptT (baz "5" :: AttemptT IO Int) >>= print
