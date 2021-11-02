{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE UnboxedTuples             #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.StaticPtr
-- Copyright   :  (C) 2014 I/O Tweag
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Symbolic references to values.
--
-- References to values are usually implemented with memory addresses, and this
-- is practical when communicating values between the different pieces of a
-- single process.
--
-- When values are communicated across different processes running in possibly
-- different machines, though, addresses are no longer useful since each
-- process may use different addresses to store a given value.
--
-- To solve such concern, the references provided by this module offer a key
-- that can be used to locate the values on each process. Each process maintains
-- a global table of references which can be looked up with a given key. This
-- table is known as the Static Pointer Table. The reference can then be
-- dereferenced to obtain the value.
--
-- The various communicating processes need to aggree on the keys used to refer
-- to the values in the Static Pointer Table, or lookups will fail. Only
-- processes launched from the same program binary are guaranteed to use the
-- same set of keys.
--
-----------------------------------------------------------------------------

module GHC.StaticPtr
  ( StaticPtr
  , deRefStaticPtr
  , StaticKey
  , staticKey
  , unsafeLookupStaticPtr
  , StaticPtrInfo(..)
  , staticPtrInfo
  , staticPtrKeys
  , IsStatic(..)
  ) where

import Data.Typeable       (Typeable)
import Foreign.C.Types     (CInt(..))
import Foreign.Marshal     (allocaArray, peekArray, withArray)
import GHC.Ptr             (Ptr(..), nullPtr)
import GHC.Fingerprint     (Fingerprint(..))
import GHC.Prim
import GHC.Word            (Word64(..))


-- | A reference to a value of type @a@.
data StaticPtr a = StaticPtr Word64# Word64# -- The flattened Fingerprint is
                                             -- convenient in the compiler.
                             StaticPtrInfo a

-- | Dereferences a static pointer.
deRefStaticPtr :: StaticPtr a -> a
deRefStaticPtr (StaticPtr _ _ _ v) = v

-- | A key for 'StaticPtr's that can be serialized and used with
-- 'unsafeLookupStaticPtr'.
type StaticKey = Fingerprint

-- | The 'StaticKey' that can be used to look up the given 'StaticPtr'.
staticKey :: StaticPtr a -> StaticKey
staticKey (StaticPtr w0 w1 _ _) = Fingerprint (W64# w0) (W64# w1)

-- | Looks up a 'StaticPtr' by its 'StaticKey'.
--
-- If the 'StaticPtr' is not found returns @Nothing@.
--
-- This function is unsafe because the program behavior is undefined if the type
-- of the returned 'StaticPtr' does not match the expected one.
--
unsafeLookupStaticPtr :: StaticKey -> IO (Maybe (StaticPtr a))
unsafeLookupStaticPtr (Fingerprint w1 w2) = do
    ptr@(Ptr addr) <- withArray [w1, w2] hs_spt_lookup
    if (ptr == nullPtr)
    then return Nothing
    else case addrToAny# addr of
           (# spe #) -> return (Just spe)

foreign import ccall unsafe hs_spt_lookup :: Ptr Word64 -> IO (Ptr a)

-- | A class for things buildable from static pointers.
--
-- GHC wraps each use of the 'static' keyword with
-- 'fromStaticPtr'. Because the 'static' keyword requires its argument
-- to be an instance of 'Typeable', 'fromStaticPtr' carries a
-- 'Typeable' constraint as well.
class IsStatic p where
    fromStaticPtr :: Typeable a => StaticPtr a -> p a

-- | @since 4.9.0.0
instance IsStatic StaticPtr where
    fromStaticPtr = id

-- | Miscellaneous information available for debugging purposes.
data StaticPtrInfo = StaticPtrInfo
    { -- | Package key of the package where the static pointer is defined
      spInfoUnitId  :: String
      -- | Name of the module where the static pointer is defined
    , spInfoModuleName :: String
      -- | Source location of the definition of the static pointer as a
      -- @(Line, Column)@ pair.
    , spInfoSrcLoc     :: (Int, Int)
    }
  deriving Show -- ^ @since 4.8.0.0

-- | 'StaticPtrInfo' of the given 'StaticPtr'.
staticPtrInfo :: StaticPtr a -> StaticPtrInfo
staticPtrInfo (StaticPtr _ _ n _) = n

-- | A list of all known keys.
staticPtrKeys :: IO [StaticKey]
staticPtrKeys = do
    keyCount <- hs_spt_key_count
    allocaArray (fromIntegral keyCount) $ \p -> do
      count <- hs_spt_keys p keyCount
      peekArray (fromIntegral count) p >>=
        mapM (\pa -> peekArray 2 pa >>= \[w1, w2] -> return $ Fingerprint w1 w2)
{-# NOINLINE staticPtrKeys #-}

foreign import ccall unsafe hs_spt_key_count :: IO CInt

foreign import ccall unsafe hs_spt_keys :: Ptr a -> CInt -> IO CInt
