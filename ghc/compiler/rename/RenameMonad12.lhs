%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[RenameMonad12]{The monad used by the renamer passes 1 and 2}

\begin{code}
#include "HsVersions.h"

module RenameMonad12 (
	Rn12M(..),
	initRn12, thenRn12, returnRn12,
	mapRn12, zipWithRn12, foldrRn12,
	addErrRn12, getModuleNameRn12, recoverQuietlyRn12,

	-- and to make the interface self-sufficient...
	Bag, Pretty(..), PprStyle, PrettyRep
    ) where

import Bag
import Errors
import Outputable
import Pretty		-- for type Pretty
import Util		-- for pragmas only

infixr 9 `thenRn12`
\end{code}

In this monad, we pass down the name of the module we are working on,
and we thread the collected errors.

\begin{code}
type Rn12M result
  =  FAST_STRING{-module name-}
  -> Bag Error
  -> (result, Bag Error)

#ifdef __GLASGOW_HASKELL__
{-# INLINE thenRn12 #-}
{-# INLINE returnRn12 #-}
#endif

initRn12 :: FAST_STRING{-module name-} -> Rn12M a -> (a, Bag Error)
initRn12 mod action = action mod emptyBag

thenRn12 :: Rn12M a -> (a -> Rn12M b) -> Rn12M b
thenRn12 expr continuation mod errs_so_far
  = case (expr mod errs_so_far) of
     (res1, errs1) -> continuation res1 mod errs1

returnRn12 :: a -> Rn12M a
returnRn12 x mod errs_so_far = (x, errs_so_far)

mapRn12 :: (a -> Rn12M b) -> [a] -> Rn12M [b]

mapRn12 f []     = returnRn12 []
mapRn12 f (x:xs)
  = f x		 `thenRn12` \ r ->
    mapRn12 f xs `thenRn12` \ rs ->
    returnRn12 (r:rs)

zipWithRn12 :: (a -> b -> Rn12M c) -> [a] -> [b] -> Rn12M [c]

zipWithRn12 f []     [] = returnRn12 []
zipWithRn12 f (x:xs) (y:ys)
  = f x y	     	`thenRn12` \ r ->
    zipWithRn12 f xs ys `thenRn12` \ rs ->
    returnRn12 (r:rs)

foldrRn12 :: (a -> b -> Rn12M b) -> b -> [a] -> Rn12M b

foldrRn12 f z []     = returnRn12 z
foldrRn12 f z (x:xs)
 = foldrRn12 f z xs  `thenRn12` \ rest ->
   f x rest

addErrRn12 :: Error -> Rn12M ()
addErrRn12 err mod errs_so_far
 = ( (), errs_so_far `snocBag` err )

getModuleNameRn12 :: Rn12M FAST_STRING
getModuleNameRn12 mod errs_so_far = (mod, errs_so_far)
\end{code}

\begin{code}
recoverQuietlyRn12 :: a -> Rn12M a -> Rn12M a

recoverQuietlyRn12 use_this_if_err action mod errs_so_far
  = let
	(result, errs_out)
	  = case (action mod emptyBag{-no errors-}) of { (res, errs) ->
	    if isEmptyBag errs then
		(res, errs_so_far)  -- retain incoming errs
	    else
		(use_this_if_err, errs_so_far)
	    }
    in
    (result, errs_out)
\end{code}
