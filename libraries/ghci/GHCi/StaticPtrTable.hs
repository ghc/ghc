{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module GHCi.StaticPtrTable ( sptAddEntry ) where

import Data.Word
import Foreign
import GHC.Fingerprint
import GHCi.RemoteTypes

-- | Used by GHCi to add an SPT entry for a set of interactive bindings.
sptAddEntry :: Fingerprint -> HValue -> IO ()
sptAddEntry (Fingerprint a b) (HValue x) = do
    sptr <- newStablePtr x
    withArray [a,b] $ \fpr_ptr -> do
        ent_ptr <- malloc
        poke ent_ptr (castStablePtrToPtr sptr)
        spt_insert_stableptr fpr_ptr ent_ptr

foreign import ccall "hs_spt_insert_stableptr"
    spt_insert_stableptr :: Ptr Word64 -> Ptr (Ptr ()) -> IO ()
