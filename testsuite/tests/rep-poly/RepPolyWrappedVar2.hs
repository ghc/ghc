{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}

module RepPolyWrappedVar2 where

import GHC.Exts

foo = unsafeCoerce# @IntRep @AddrRep
