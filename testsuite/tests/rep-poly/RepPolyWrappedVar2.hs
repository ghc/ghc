{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}

module RepPolyWrappedVar2 where

import GHC.Exts

foo = unsafeCoerce# @IntRep @AddrRep
