{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE GADTs #-}

module T25611c where

import GHC.Int (Int(I#))
import GHC.Word (Word(W#))
import GHC.Exts (Int#,Word#)
import GHC.Exts (TYPE,RuntimeRep(IntRep,WordRep,TupleRep))

data family DF :: TYPE (r :: RuntimeRep)

-- it used to be failed: see #18891 and !4419
-- See Note [Kind inference for data family instances]
-- in GHC.Tc.TyCl.Instance
-- but succ now see #25611
newtype instance DF = MkDF1a Int#
newtype instance DF = MkDF2a Word#
newtype instance DF = MkDF3a (# Int#, Word# #)

go = 1
  where
    x :: DF @IntRep
    x = MkDF1a 3#
