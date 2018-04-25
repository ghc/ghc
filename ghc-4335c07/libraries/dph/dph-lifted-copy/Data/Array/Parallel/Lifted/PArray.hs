{-# LANGUAGE CPP, FlexibleContexts #-}

#include "fusion-phases.h"

--
module Data.Array.Parallel.Lifted.PArray (
  PArray(..), PData,

  PA(..),
  lengthPA#, dataPA#, replicatePA#, replicatelPA#, repeatPA#,
  emptyPA, indexPA#, extractPA#, bpermutePA#, appPA#, applPA#,
  packByTagPA#, combine2PA#, updatePA#, fromListPA#, fromListPA, nfPA,

  replicatePD, replicatelPD, repeatPD, emptyPD,
  indexPD, extractPD, bpermutePD, appPD, applPD,
  packByTagPD, combine2PD, updatePD, fromListPD, nfPD,

  PRepr, PR(..),

  Scalar(..),
  replicatePRScalar, replicatelPRScalar, repeatPRScalar, emptyPRScalar,
  indexPRScalar, extractPRScalar, bpermutePRScalar, appPRScalar, applPRScalar,
  packByTagPRScalar, combine2PRScalar, updatePRScalar, fromListPRScalar,
  nfPRScalar,
) where
import Data.Array.Parallel.PArray.PRepr
import Data.Array.Parallel.PArray.Scalar
import Data.Array.Parallel.PArray.PData
import Data.Array.Parallel.PArray.Base




