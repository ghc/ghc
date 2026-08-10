{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module GHCi.StaticPtrTable ( sptAddEntry, sptRemoveEntry ) where

import Prelude -- See note [Why do we import Prelude here?]
import Data.Word
import Foreign
import GHC.Fingerprint
import GHCi.RemoteTypes

-- | Used by GHCi to add an SPT entry for a set of interactive bindings.
sptAddEntry :: Fingerprint -> HValue -> IO ()
sptAddEntry (Fingerprint a b) (HValue x) = do
    -- We own the memory holding the key (fingerprint) which gets inserted into
    -- the static pointer table and can't free it, not even when the entry is
    -- removed, since we don't keep track of it.
    fpr_ptr <- newArray [a,b]
    sptr <- newStablePtr x
    ent_ptr <- malloc
    poke ent_ptr (castStablePtrToPtr sptr)
    spt_insert_stableptr fpr_ptr ent_ptr

foreign import ccall "hs_spt_insert_stableptr"
    spt_insert_stableptr :: Ptr Word64 -> Ptr (Ptr ()) -> IO ()

sptRemoveEntry :: Fingerprint -> IO ()
sptRemoveEntry (Fingerprint a b) = withArray [a,b] spt_remove

foreign import ccall "hs_spt_remove"
    spt_remove :: Ptr Word64 -> IO ()
