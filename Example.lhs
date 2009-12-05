<p>This file is an example of how to use the attempt library, as literate
Haskell. We'll start off with some import statements.</p>

> {-# LANGUAGE DeriveDataTypeable #-}
> {-# LANGUAGE ExistentialQuantification #-}
> import Control.Monad.Attempt
> import qualified Safe.Failure as A
> import System.Environment (getArgs)
> import Safe (readMay)
> import Data.Generics
> import qualified Control.Exception as E

<p>We're going to deal with a very simplistic example. Let's say you have some
text files that need processing. The files are each three lines long. The
first and last line are integers; the second is a mathematical operator (one
of +, -, * and /). Your goal with each file is to simply perform the
mathematical operator on the two numbers. Let's start with the Operator data
type.</p>

> data Operator = Add | Sub | Mul | Div
> instance Read Operator where
>   readsPrec _ "+" = [(Add, "")]
>   readsPrec _ "-" = [(Sub, "")]
>   readsPrec _ "*" = [(Mul, "")]
>   readsPrec _ "/" = [(Div, "")]
>   readsPrec _ s = []
>
> toFunc :: Operator -> Int -> Int -> Int
> toFunc Add = (+)
> toFunc Sub = (-)
> toFunc Mul = (*)
> toFunc Div = div

<p>Nothing special here (besides some sloppy programming). Let's go ahead and
write the first version of our process function.</p>

> process1 :: FilePath -> IO Int
> process1 filePath = do
>   contents <- readFile filePath -- IO may fail for some reason
>   let [num1S, opS, num2S] = lines contents -- maybe there aren't 3 lines?
>       num1 = read num1S -- read might fail
>       op   = read opS   -- read might fail
>       num2 = read num2S -- read might fail
>   return $ toFunc op num1 num2

<p>If you test this function out on a valid file, it works just fine. But what
happens when you call it with invalid data? In fact, there are five things
which could go wrong that I'd be interested in dealing with in the above code.</p>

<p>So now we need some way to deal with these issues. There's a few standard ones
in the Haskell toolbelt:</p>

<ol>
<li>Wrap the response in Maybe. Disadvantage: can't give any indication what he
error was.</li>

<li>Wrap the response in an Either String. Disadvantage: error type is simply a
string, which isn't necesarily very informative. Also, Either is not defined
by the standard library to be a Monad, making this type of processing clumsy.</li>

<li>Wrap in a more exotic Either SomeException or some such. Disadvantage:
still not a Monad.</li>

<li>Declare your own error type. Disadvantage: ad-hoc, and makes it very
difficult to compose different libraries together.</li>

</ol>

<p>In steps the attempt library. It's essentially option 4 wrapped in a library
for general consumption. Features include:</p>

<ol>
<li>Uses extensible exceptions so you can report whatever information you want.</li>

<li>Exceptions are not explicitly typed, so you don't need to wrap insanely
long function signatures to explain what exceptions you might be throwing.</li>

<li>Defines all the standard instances you want, including providing a monad
transformers.</li>

<p>Let's transform the above example to use the attempt library in its most basic
form:</p>

> data ProcessError = NotThreeLines String | NotInt String | NotOperator String
>   deriving (Show, Typeable)
> instance E.Exception ProcessError
>
> process2 :: FilePath -> IO (Attempt Int)
> process2 filePath =
>   E.handle (\e -> return $ failure (e :: E.IOException)) $ do
>       contents <- readFile filePath
>       return $ case lines contents of
>           [num1S, opS, num2S] ->
>               case readMay num1S of
>                   Just num1 ->
>                       case readMay opS of
>                           Just op ->
>                               case readMay num2S of
>                                   Just num2 -> return $ toFunc op num1 num2
>                                   Nothing -> failure $ NotInt num2S
>                           Nothing -> failure $ NotOperator opS
>                   Nothing -> failure $ NotInt num1S
>           _ -> failure $ NotThreeLines contents

<p>If you run these on the sample files in the input directory, you'll see that
we're getting the right result; the program in not erroring out, simply
returning a failure message. However, this wasn't very satisfactory with all of
those nested case statements. Let's use two facts to our advantage:</p>

<ol>
<li>Attempt is a Monad.</li>
<li>There is a Data.Attempt.Helper module which provides a special read
function.</li>
</ol>

> data ProcessErrorWrapper =
>   forall e. E.Exception e => BadIntWrapper e
>   | forall e. E.Exception e => BadOperatorWrapper e
>   deriving (Typeable)
> instance Show ProcessErrorWrapper where
>   show (BadIntWrapper e) = "BadInt: " ++ show e
>   show (BadOperatorWrapper e) = "BadOperator: " ++ show e
> instance E.Exception ProcessErrorWrapper
> process3 :: FilePath -> IO (Attempt Int)
> process3 filePath =
>   E.handle (\e -> return $ failure (e :: E.IOException)) $ do
>       contents <- readFile filePath
>       return $ case lines contents of
>           [num1S, opS, num2S] -> do
>               num1 <- wrapFailure BadIntWrapper $ A.read num1S
>               op   <- wrapFailure BadOperatorWrapper $ A.read opS
>               num2 <- wrapFailure BadIntWrapper $ A.read num2S
>               return $ toFunc op num1 num2
>           _ -> failure $ NotThreeLines contents

<p>That certainly cleaned stuff up. The special read function works just as you
would expected: if the read succeeds, it returns a Success value. Otherwise,
it returns a Failure.</p>

<p>But what's going on with that wrapFailure stuff? This is just to clean up the
output. The read function will return an exception of type "CouldNotRead",
which let's you know that you failed a read attempt, but doesn't let you know
what you were trying to read.</p>

<p>So far, so good. But that "case lines contents" bit is still a little
annoying. Let's get rid of it.</p>

> process4 :: FilePath -> IO (Attempt Int)
> process4 filePath =
>   E.handle (\e -> return $ failure (e :: E.IOException)) $ do
>       contents <- readFile filePath
>       return $ do
>           let contents' = lines contents
>           [num1S, opS, num2S] <-
>               A.assert (length contents' == 3)
>                        contents'
>                        (NotThreeLines contents)
>           num1 <- wrapFailure BadIntWrapper $ A.read num1S
>           op   <- wrapFailure BadOperatorWrapper $ A.read opS
>           num2 <- wrapFailure BadIntWrapper $ A.read num2S
>           return $ toFunc op num1 num2

<p>There's unfortunately no simple way to catch pattern match fails, but an
assertion works almost as well. The only thing which is still a bit irksome is
the whole exception handling business. Let's be rid of that next.</p>

> process5 :: FilePath -> AttemptT IO Int
> process5 filePath = do
>   contents <- A.readFile filePath
>   let contents' = lines contents
>   [num1S, opS, num2S] <-
>       A.assert (length contents' == 3)
>                contents'
>                (NotThreeLines contents)
>   num1 <- wrapFailure BadIntWrapper $ A.read num1S
>   op   <- wrapFailure BadOperatorWrapper $ A.read opS
>   num2 <- wrapFailure BadIntWrapper $ A.read num2S
>   return $ toFunc op num1 num2

<p>There's a built-in readFile function that handles all that handling of error
garbage for you. If you compare this version of the function to the first, you
should notice that it's very similar. You can avoid a lot of the common
sources of runtime errors by simply replacing unsafe functions (Prelude.read)
with safe ones (Data.Attempt.Helper.read).</p>

<p>However, there's still one other different between process5 and process2-4:
the return type. process2-4 return (IO (Attempt Int)), while process5 returns
an (AttemptT IO Int). This is the monad transformer version of Attempt; read
the documentation for more details. To get back to the same old return type as
before:</p>

> process6 :: FilePath -> IO (Attempt Int)
> process6 = runAttemptT . process5

<p>And just to come full circle: let's say that you have an Attempt that you would like to disappear. Well, clearly you have to do <em>something</em> with those exceptions. There is a FromAttempt class which is used for containers which can contain those exceptions in some way. IO is one such container. So, to get all the way back to those runtime exceptions, try either of the following:</p>

> process7 :: FilePath -> IO Int
> process7 = evalAttemptT . process5
>
> process8 :: FilePath -> IO Int
> process8 = joinAttempt . process6

<p>Below is a simple main function for testing out these various functions. Try
them out on the files in the input directory. Also, to simulate an IO error,
call them on a non-existant file.</p>

> main = do
>   args <- getArgs
>   if length args /= 2
>       then error "Usage: Example.lhs <process> <file path>"
>       else return ()
>   let [processNum, filePath] = args
>   case processNum of
>       "1" -> process1 filePath >>= print
>       "2" -> process2 filePath >>= print
>       "3" -> process3 filePath >>= print
>       "4" -> process4 filePath >>= print
>       "5" -> runAttemptT (process5 filePath) >>= print
>       "6" -> process6 filePath >>= print
>       "7" -> process7 filePath >>= print
>       "8" -> process8 filePath >>= print
>       x -> error $ "Invalid process function: " ++ x
