%
% (c) The GRASP/AQUA Project, Glasgow University, 1994
%
\section[PrelMonIO]{Monadic I/O Primitives}

This module defines the basic monadic framework for Haskell 1.3 I/O. 

\begin{code}

module PreludeMonadicIO (
    (>>), 
    (>>=), 
    accumulate,
    either, 
    fail, 
    failWith, 
    handle, 
    return, 
    sequence,
    try,

    IO(..), 
    Either(..)

    ) where

import Cls
import Core
import IChar
import IInt
import IList
import List		( (++) )
import Prel		( (.), not )
import PS		( _PackedString, _unpackPS )
import Text
import TyArray
import TyComplex

import PreludeGlaST
import PreludeIOError

infixr 1 >>, >>=

\end{code}

\subsection[IOMonad]{The IO Monad}

I/O operations may need to indicate errors, and implementations may
need to handle these errors.  The $IO$ monad extends existing practice
by making this functionality primitive.  The exact errors which may
occur are defined in $PreludeIOError13$.

\begin{code}

type IO a = PrimIO (Either IOError13 a)

data Either a b =  Left a | Right b deriving (Text, Eq, Ord)

\end{code}

An expression of type $IO a$, for some type {\em a}, denotes a
computation whose answer is either a result of type {\em a} or an
<em>error</em> of type $IOError13$.  The computation succeeds with
result {\em succ} if its answer is $Right succ$, and fails with
result {\em fail} if its answer is $Left fail$.  Note that the
type system delimits the possibility of failure: only expressions of
some type $IO a$ can <em>fail</em> in the sense defined here.

\begin{code}

{-# INLINE return #-}
{-# INLINE failWith #-}

return   :: a       -> IO a
failWith :: IOError13 -> IO a

return = returnPrimIO . Right
failWith = returnPrimIO . Left

\end{code}

There are two primitives to create trivial computations, one for
each of the two possibilities, success or failure.

$return result$ is a computation that succeeds with result 
{\em result}.

$failWith fail$ is a computation that fails with the error 
{\em fail}.

\begin{code}

{-# INLINE (>>=) #-}

(>>=) :: IO a -> (a -> IO b) -> IO b 
m >>= k = m `thenPrimIO` \ r -> k' r
  where
    k' (Right x)  = k x
    k' (Left err) = returnPrimIO (Left err)

\end{code}

The $>>=$ operation is used to sequence two computations, where the
second computation is parameterised on the result of the first.

\begin{code}

{-# INLINE (>>) #-}

(>>) :: IO a -> IO b -> IO b
m >> k = m >>= \ _ -> k

\end{code}

The restricted form of $>>=$, $>>$, is used when the result of the
first computation is uninteresting.

\subsection[Error-Handling]{Error Handling}

\begin{code}

handle :: IO a -> (IOError13 -> IO a) -> IO a
handle m k = m `thenPrimIO` \ r -> k' r
  where
    k' (Left err) = k err
    k' result = returnPrimIO result

\end{code}

The construct $handle comp handler$ can be used to handle a
simple error during a computation {\em comp}.  Its usefulness is
limited in that the replacement value must be of the same type as the
result of {\em comp}.

\begin{code}

try :: IO a -> IO (Either IOError13 a) 
try p = handle (p >>= (return . Right)) (return . Left)

\end{code}

The construct $try comp$ exposes errors which occur within a
computation, and which are not fully handled.  It always succeeds.

\subsection[UserErrors]{User-Defined Errors}

\begin{code}

fail :: String -> IO a 
fail = failWith . UserError

\end{code}

As a convention for user-generated errors, to return an error message
$msg :: String$, return the error value $UserError msg$
via the computation $fail msg$.

This construct should be used instead of Haskell's $error :: String -> a$ 
operation wherever convenient.

\subsection[HOFs]{Higher-Order Utility Functions}

\begin{code}

either :: (a -> c) -> (b -> c) -> (Either a b) -> c
either kl kr x = case x of {Left a -> kl a; Right b -> kr b}

\end{code}

The construct $either a b$ can be used to generate functions on types
of the form $Either a b$.

\begin{code}

accumulate :: [IO a] -> IO [a] 

accumulate [] = return []
accumulate (f:fs) 
  = f		  >>= \ x ->
    accumulate fs >>= \ xs ->
    return (x:xs)

{- partain: this may be right, but I'm going w/ a more-certainly-right version
accumulate = foldr mcons (return [])
  where
    mcons :: IO a -> IO [a] -> IO [a]
    mcons p q = p >>= \x -> q >>= \y -> return (x : y)
-}

\end{code}

The $accumulate$ computation is used to process a list of computations
of the same type, and to return a list of their results when executed
in sequence.

\begin{code}

sequence :: [IO a] -> IO () 
sequence = foldr (>>) (return ())

\end{code}

The $sequence$ computation is used for the simpler case when the
computations are executed entirely for their external effect, and the
results are therefore uninteresting.

