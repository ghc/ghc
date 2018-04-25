{-# LANGUAGE Trustworthy #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.STRef
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (uses Control.Monad.ST)
--
-- Mutable references in the (strict) ST monad.
--
-----------------------------------------------------------------------------

module Data.STRef (
        -- * STRefs
        STRef,          -- abstract
        newSTRef,
        readSTRef,
        writeSTRef,
        modifySTRef,
        modifySTRef'
 ) where

import GHC.ST
import GHC.STRef

-- | Mutate the contents of an 'STRef'.
--
-- >>> :{
-- runST (do
--     ref <- newSTRef ""
--     modifySTRef ref (const "world")
--     modifySTRef ref (++ "!")
--     modifySTRef ref ("Hello, " ++)
--     readSTRef ref )
-- :}
-- "Hello, world!"
--
-- Be warned that 'modifySTRef' does not apply the function strictly.  This
-- means if the program calls 'modifySTRef' many times, but seldomly uses the
-- value, thunks will pile up in memory resulting in a space leak.  This is a
-- common mistake made when using an STRef as a counter.  For example, the
-- following will leak memory and may produce a stack overflow:
--
-- >>> import Control.Monad (replicateM_)
-- >>> :{
-- print (runST (do
--     ref <- newSTRef 0
--     replicateM_ 1000 $ modifySTRef ref (+1)
--     readSTRef ref ))
-- :}
-- 1000
--
-- To avoid this problem, use 'modifySTRef'' instead.
modifySTRef :: STRef s a -> (a -> a) -> ST s ()
modifySTRef ref f = writeSTRef ref . f =<< readSTRef ref

-- | Strict version of 'modifySTRef'
--
-- @since 4.6.0.0
modifySTRef' :: STRef s a -> (a -> a) -> ST s ()
modifySTRef' ref f = do
    x <- readSTRef ref
    let x' = f x
    x' `seq` writeSTRef ref x'
