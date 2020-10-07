{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE GADTs #-}

module UnliftedNewtypesUnassociatedFamily where

import GHC.Int (Int(I#))
import GHC.Word (Word(W#))
import GHC.Exts (Int#,Word#)
import GHC.Exts (TYPE,RuntimeRep(IntRep,WordRep,TupleRep))

data family DF :: TYPE (r :: RuntimeRep)

-- All these fail: see #18891 and !4419
-- See Note [Kind inference for data family instances]
-- in GHC.Tc.TyCl.Instance
newtype instance DF = MkDF1a Int#
newtype instance DF = MkDF2a Word#
newtype instance DF = MkDF3a (# Int#, Word# #)
