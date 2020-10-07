{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module UnliftedNewtypesUnassociatedFamily where

import GHC.Int (Int(I#))
import GHC.Word (Word(W#))
import GHC.Exts (Int#,Word#)
import GHC.Exts (TYPE,Levity(Lifted))
import GHC.Exts (RuntimeRep(BoxedRep,IntRep,WordRep,TupleRep))

data family DFT (r :: RuntimeRep) :: TYPE r
newtype instance DFT 'IntRep = MkDFT1 Int#
newtype instance DFT 'WordRep = MkDFT2 Word#
newtype instance DFT ('TupleRep '[ 'IntRep, 'WordRep])
  = MkDFT3 (# Int#, Word# #)
data instance DFT ('BoxedRep 'Lifted) = MkDFT4 | MkDFT5

data family DF :: TYPE (r :: RuntimeRep)

-- Use a type application
newtype instance DF @IntRep = MkDF1 Int#

-- Use a kind signature
newtype instance DF :: TYPE 'WordRep where
  MkDF2 :: Word# -> DF

-- Also uses a kind signature
newtype instance DF :: TYPE ('TupleRep '[ 'IntRep, 'WordRep ]) where
  MkDF3 :: (# Int#, Word# #) -> DF

data instance DF = MkDF4 | MkDF5
