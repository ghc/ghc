{- |
Module      :  Control.Monad.Cont
Copyright   :  (c) The University of Glasgow 2001,
               (c) Jeff Newbern 2003-2007,
               (c) Andriy Palamarchuk 2007
License     :  BSD-style (see the file LICENSE)

Maintainer  :  libraries@haskell.org
Stability   :  experimental
Portability :  portable

[Computation type:] Computations which can be interrupted and resumed.

[Binding strategy:] Binding a function to a monadic value creates
a new continuation which uses the function as the continuation of the monadic
computation.

[Useful for:] Complex control structures, error handling,
and creating co-routines.

[Zero and plus:] None.

[Example type:] @'Cont' r a@

The Continuation monad represents computations in continuation-passing style
(CPS).
In continuation-passing style function result is not returned,
but instead is passed to another function,
received as a parameter (continuation).
Computations are built up from sequences
of nested continuations, terminated by a final continuation (often @id@)
which produces the final result.
Since continuations are functions which represent the future of a computation,
manipulation of the continuation functions can achieve complex manipulations
of the future of the computation,
such as interrupting a computation in the middle, aborting a portion
of a computation, restarting a computation, and interleaving execution of
computations.
The Continuation monad adapts CPS to the structure of a monad.

Before using the Continuation monad, be sure that you have
a firm understanding of continuation-passing style
and that continuations represent the best solution to your particular
design problem.
Many algorithms which require continuations in other languages do not require
them in Haskell, due to Haskell's lazy semantics.
Abuse of the Continuation monad can produce code that is impossible
to understand and maintain.
-}

module Control.Monad.Cont (
    -- * MonadCont class
    MonadCont(..),
    -- * The Cont monad
    Cont,
    cont,
    runCont,
    mapCont,
    withCont,
    -- * The ContT monad transformer
    ContT(ContT),
    runContT,
    mapContT,
    withContT,
    module Control.Monad,
    module Control.Monad.Trans,
    -- * Example 1: Simple Continuation Usage
    -- $simpleContExample

    -- * Example 2: Using @callCC@
    -- $callCCExample
    
    -- * Example 3: Using @ContT@ Monad Transformer
    -- $ContTExample
  ) where

import Control.Monad.Cont.Class

import Control.Monad.Trans
import Control.Monad.Trans.Cont

import Control.Monad

{- $simpleContExample
Calculating length of a list continuation-style:

>calculateLength :: [a] -> Cont r Int
>calculateLength l = return (length l)

Here we use @calculateLength@ by making it to pass its result to @print@:

>main = do
>  runCont (calculateLength "123") print
>  -- result: 3

It is possible to chain 'Cont' blocks with @>>=@.

>double :: Int -> Cont r Int
>double n = return (n * 2)
>
>main = do
>  runCont (calculateLength "123" >>= double) print
>  -- result: 6
-}

{- $callCCExample
This example gives a taste of how escape continuations work, shows a typical
pattern for their usage.

>-- Returns a string depending on the length of the name parameter.
>-- If the provided string is empty, returns an error.
>-- Otherwise, returns a welcome message.
>whatsYourName :: String -> String
>whatsYourName name =
>  (`runCont` id) $ do                      -- 1
>    response <- callCC $ \exit -> do       -- 2
>      validateName name exit               -- 3
>      return $ "Welcome, " ++ name ++ "!"  -- 4
>    return response                        -- 5
>
>validateName name exit = do
>  when (null name) (exit "You forgot to tell me your name!")

Here is what this example does:

(1) Runs an anonymous 'Cont' block and extracts value from it with
@(\`runCont\` id)@. Here @id@ is the continuation, passed to the @Cont@ block.

(1) Binds @response@ to the result of the following 'Control.Monad.Cont.Class.callCC' block,
binds @exit@ to the continuation.

(1) Validates @name@.
This approach illustrates advantage of using 'Control.Monad.Cont.Class.callCC' over @return@.
We pass the continuation to @validateName@,
and interrupt execution of the @Cont@ block from /inside/ of @validateName@.

(1) Returns the welcome message from the 'Control.Monad.Cont.Class.callCC' block.
This line is not executed if @validateName@ fails.

(1) Returns from the @Cont@ block.
-}

{-$ContTExample
'ContT' can be used to add continuation handling to other monads.
Here is an example how to combine it with @IO@ monad:

>import Control.Monad.Cont
>import System.IO
>
>main = do
>  hSetBuffering stdout NoBuffering
>  runContT (callCC askString) reportResult
>
>askString :: (String -> ContT () IO String) -> ContT () IO String
>askString next = do
>  liftIO $ putStrLn "Please enter a string"
>  s <- liftIO $ getLine
>  next s
>
>reportResult :: String -> IO ()
>reportResult s = do
>  putStrLn ("You entered: " ++ s)

Action @askString@ requests user to enter a string,
and passes it to the continuation.
@askString@ takes as a parameter a continuation taking a string parameter,
and returning @IO ()@.
Compare its signature to 'runContT' definition.
-}
