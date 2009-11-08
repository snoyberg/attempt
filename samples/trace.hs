{-# OPTIONS_GHC -F -pgmF MonadLoc #-}
import Data.Attempt
import qualified Safe.Failure as A
import Control.Monad.Loc

foo :: String -> Attempt Int
foo x = do
    A.read x

bar :: String -> Attempt Int
bar x = do
    y <- foo x
    return $ y + 2

baz :: String -> Attempt Int
baz x = do
    y <- bar x
    return $ y * 3

main = do
    print $ baz "hello"
    print $ baz "5"
