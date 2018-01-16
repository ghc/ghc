{-# LANGUAGE MagicHash #-}
#include "fusion-phases.h"

module Data.Array.Parallel.Unlifted (
  -- * Types
  Elt,  Array,
  
  -- * Constructors
  empty,
  generate,
  replicate, replicate_s, replicate_rs,
  repeat,
  indexed,
  (+:+),
  append_s, append_vs,
  indices_s,
  enumFromTo,
  enumFromThenTo,
  enumFromStepLen,
  enumFromStepLenEach,

  -- * Projections
  length,
  index,
  indexs,
  indexs_avs,

  extract,
  extracts_nss,
  extracts_ass,
  extracts_avs,
  drop,
  
  -- * Update
  update,

  -- * Permutation
  permute,
  bpermute,
  mbpermute,
  bpermuteDft,
      
  -- * Zipping and Unzipping
  zip,   zip3,
  unzip, unzip3,
  fsts,  snds,  

  -- * Map and ZipWith
  map,
  zipWith,
  zipWith3,
  zipWith4,
  zipWith5,
  zipWith6,
  zipWith7,
  zipWith8,

  -- * Scans and Folds
  scan,
  fold,  fold_s,  fold_ss,   fold_vs, fold_r,  
  fold1, fold1_s, fold1_ss,  fold1_vs,
  sum,   sum_s,   sum_ss,    sum_r,  
  count, count_s, count_ss,
  and, 

  -- * Pack and Filter
  pack,
  packByTag,
  filter,
  pick,
  
  -- * Combine and Interleave
  combine, combine2,
  interleave,

  -- * Selectors
  Sel2,
  mkSel2, 
  tagsSel2,
  indicesSel2,
  elementsSel2_0,
  elementsSel2_1,
  repSel2,
  tagsToSel2,
  
  -- * Selector Representations
  SelRep2,
  mkSelRep2,
  indicesSelRep2,
  elementsSelRep2_0,
  elementsSelRep2_1,
    
  -- * Segment Descriptors
  Segd,
  mkSegd,
  validSegd,
  emptySegd,
  singletonSegd,
  lengthsToSegd,
  lengthSegd,
  lengthsSegd,
  indicesSegd,
  elementsSegd,
  plusSegd, 

  -- * Scattered Segment Descriptors
  SSegd,
  mkSSegd,
  validSSegd,
  emptySSegd,
  singletonSSegd,
  promoteSegdToSSegd,
  isContiguousSSegd,
  lengthOfSSegd,
  lengthsOfSSegd,
  indicesOfSSegd,
  startsOfSSegd,
  sourcesOfSSegd,
  getSegOfSSegd,
  appendSSegd,
  
  -- * Virtual Segment Descriptors
  VSegd,
  mkVSegd,
  validVSegd,
  emptyVSegd,
  singletonVSegd,
  replicatedVSegd,
  promoteSegdToVSegd,
  promoteSSegdToVSegd,
  isManifestVSegd,
  isContiguousVSegd,
  lengthOfVSegd,
  takeVSegidsOfVSegd,
  takeVSegidsRedundantOfVSegd,
  takeSSegdOfVSegd,
  takeSSegdRedundantOfVSegd,
  takeLengthsOfVSegd,
  getSegOfVSegd,
  unsafeDemoteToSSegdOfVSegd,
  unsafeDemoteToSegdOfVSegd,
  updateVSegsOfVSegd,
  updateVSegsReachableOfVSegd,
  appendVSegd,
  combine2VSegd,
  
  -- * Irregular two dimensional arrays
  Elts, Arrays,
  emptys,
  singletons,
  lengths,
  unsafeIndexs,
  unsafeIndex2s,
  appends,
  fromVectors,
  toVectors,
  
  -- * Random arrays
  randoms, randomRs,
  
  -- * Array IO
  IOElt, hGet, hPut,
  toList, fromList,
) where

import Prelude                    (Num, Int, Bool, Float, Double)
import System.IO                  (IO, Handle)
import Data.Word                  (Word8)
import qualified System.Random
import qualified Prelude
import qualified Data.Vector       as VV
