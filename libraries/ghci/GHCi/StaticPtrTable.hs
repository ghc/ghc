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
    -- We own the memory holding the key (fingerprint) which gets inserted into
    -- the static pointer table and can't free it until the SPT entry is removed
    -- (which is currently never).
    fpr_ptr <- newArray [a,b]
    sptr <- newStablePtr x
    ent_ptr <- malloc
    poke ent_ptr (castStablePtrToPtr sptr)
    spt_insert_stableptr fpr_ptr ent_ptr

foreign import ccall "hs_spt_insert_stableptr"
    spt_insert_stableptr :: Ptr Word64 -> Ptr (Ptr ()) -> IO ()
