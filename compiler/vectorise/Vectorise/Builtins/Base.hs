-- |Builtin types and functions used by the vectoriser. These are all defined in
-- 'Data.Array.Parallel.Prim'.

module Vectorise.Builtins.Base (
  -- * Hard config
  mAX_DPH_PROD,
  mAX_DPH_SUM,
  mAX_DPH_COMBINE,
  mAX_DPH_SCALAR_ARGS,
  aLL_DPH_PRIM_TYCONS,
  
  -- * Builtins
  Builtins(..),
  
  -- * Projections
  selTy, selsTy,
  selReplicate,
  selTags,
  selElements,
  selsLength,
  sumTyCon,
  prodTyCon,
  prodDataCon,
  replicatePD_PrimVar,
  emptyPD_PrimVar,
  packByTagPD_PrimVar,
  combinePDVar,
  combinePD_PrimVar,
  scalarZip,
  closureCtrFun
) where

import TysPrim
import BasicTypes
import Class
import CoreSyn
import TysWiredIn
import Type
import TyCon
import DataCon
import NameEnv
import Name
import Outputable

import Data.Array


-- Cardinality of the various families of types and functions exported by the DPH library.

mAX_DPH_PROD :: Int
mAX_DPH_PROD = 5

mAX_DPH_SUM :: Int
mAX_DPH_SUM = 2

mAX_DPH_COMBINE :: Int
mAX_DPH_COMBINE = 2

mAX_DPH_SCALAR_ARGS :: Int
mAX_DPH_SCALAR_ARGS = 8

-- Types from 'GHC.Prim' supported by DPH
--
aLL_DPH_PRIM_TYCONS :: [Name]
aLL_DPH_PRIM_TYCONS = map tyConName [intPrimTyCon, {- floatPrimTyCon, -} doublePrimTyCon]


-- |Holds the names of the types and functions from 'Data.Array.Parallel.Prim' that are used by the
-- vectoriser.
--
data Builtins 
        = Builtins 
        { parrayTyCon          :: TyCon                     -- ^ PArray
        , pdataTyCon           :: TyCon                     -- ^ PData
        , pdatasTyCon          :: TyCon                     -- ^ PDatas
        , prClass              :: Class                     -- ^ PR
        , prTyCon              :: TyCon                     -- ^ PR
        , preprTyCon           :: TyCon                     -- ^ PRepr
        , paClass              :: Class                     -- ^ PA
        , paTyCon              :: TyCon                     -- ^ PA
        , paDataCon            :: DataCon                   -- ^ PA
        , paPRSel              :: Var                       -- ^ PA
        , replicatePDVar       :: Var                       -- ^ replicatePD
        , replicatePD_PrimVars :: NameEnv Var               -- ^ replicatePD_Int# etc.
        , emptyPDVar           :: Var                       -- ^ emptyPD
        , emptyPD_PrimVars     :: NameEnv Var               -- ^ emptyPD_Int# etc.
        , packByTagPDVar       :: Var                       -- ^ packByTagPD
        , packByTagPD_PrimVars :: NameEnv Var               -- ^ packByTagPD_Int# etc.
        , combinePDVars        :: Array Int Var             -- ^ combinePD
        , combinePD_PrimVarss  :: Array Int (NameEnv Var)   -- ^ combine2PD_Int# etc.
        , scalarClass          :: Class                     -- ^ Scalar
        , scalarZips           :: Array Int Var             -- ^ map, zipWith, zipWith3
        , voidTyCon            :: TyCon                     -- ^ Void
        , voidVar              :: Var                       -- ^ void
        , fromVoidVar          :: Var                       -- ^ fromVoid
        , sumTyCons            :: Array Int TyCon           -- ^ Sum2 .. Sum3
        , wrapTyCon            :: TyCon                     -- ^ Wrap
        , pvoidVar             :: Var                       -- ^ pvoid
        , pvoidsVar            :: Var                       -- ^ pvoids
        , closureTyCon         :: TyCon                     -- ^ :->
        , closureVar           :: Var                       -- ^ closure
        , liftedClosureVar     :: Var                       -- ^ liftedClosure
        , applyVar             :: Var                       -- ^ $: 
        , liftedApplyVar       :: Var                       -- ^ liftedApply
        , closureCtrFuns       :: Array Int Var             -- ^ closure1 .. closure3
        , selTys               :: Array Int Type            -- ^ Sel2
        , selsTys              :: Array Int Type            -- ^ Sels2
        , selsLengths          :: Array Int CoreExpr        -- ^ lengthSels2
        , selReplicates        :: Array Int CoreExpr        -- ^ replicate2
        , selTagss             :: Array Int CoreExpr        -- ^ tagsSel2
        , selElementss         :: Array (Int, Int) CoreExpr -- ^ elementsSel2_0 .. elementsSel_2_1
        , liftingContext       :: Var                       -- ^ lc
        }


-- Projections ----------------------------------------------------------------
-- We use these wrappers instead of indexing the `Builtin` structure directly
-- because they give nicer panic messages if the indexed thing cannot be found.

selTy :: Int -> Builtins -> Type
selTy           = indexBuiltin "selTy" selTys

selsTy :: Int -> Builtins -> Type
selsTy          = indexBuiltin "selsTy" selsTys

selsLength :: Int -> Builtins -> CoreExpr
selsLength      = indexBuiltin "selLength" selsLengths

selReplicate :: Int -> Builtins -> CoreExpr
selReplicate    = indexBuiltin "selReplicate" selReplicates 

selTags :: Int -> Builtins -> CoreExpr
selTags         = indexBuiltin "selTags" selTagss

selElements :: Int -> Int -> Builtins -> CoreExpr
selElements i j = indexBuiltin "selElements" selElementss (i, j)

sumTyCon :: Int -> Builtins -> TyCon
sumTyCon        = indexBuiltin "sumTyCon" sumTyCons

prodTyCon :: Int -> Builtins -> TyCon
prodTyCon n _
  | n >= 2 && n <= mAX_DPH_PROD 
  = tupleTyCon BoxedTuple n
  | otherwise
  = pprPanic "prodTyCon" (ppr n)

prodDataCon :: Int -> Builtins -> DataCon
prodDataCon n bi 
 = case tyConDataCons (prodTyCon n bi) of
    [con] -> con
    _ -> pprPanic "prodDataCon" (ppr n)

replicatePD_PrimVar :: TyCon -> Builtins -> Var
replicatePD_PrimVar tc bi
  = lookupEnvBuiltin "replicatePD_PrimVar" (replicatePD_PrimVars bi) (tyConName tc)

emptyPD_PrimVar :: TyCon -> Builtins -> Var
emptyPD_PrimVar tc bi
  = lookupEnvBuiltin "emptyPD_PrimVar" (emptyPD_PrimVars bi) (tyConName tc)

packByTagPD_PrimVar :: TyCon -> Builtins -> Var
packByTagPD_PrimVar tc bi
  = lookupEnvBuiltin "packByTagPD_PrimVar" (packByTagPD_PrimVars bi) (tyConName tc)

combinePDVar :: Int -> Builtins -> Var
combinePDVar = indexBuiltin "combinePDVar" combinePDVars

combinePD_PrimVar :: Int -> TyCon -> Builtins -> Var
combinePD_PrimVar i tc bi
  = lookupEnvBuiltin "combinePD_PrimVar" 
      (indexBuiltin "combinePD_PrimVar" combinePD_PrimVarss i bi) (tyConName tc)

scalarZip :: Int -> Builtins -> Var
scalarZip = indexBuiltin "scalarZip" scalarZips

closureCtrFun :: Int -> Builtins -> Var
closureCtrFun = indexBuiltin "closureCtrFun" closureCtrFuns

-- | Get an element from one of the arrays of `Builtins`.
--   Panic if the indexed thing is not in the array.
indexBuiltin :: (Ix i, Outputable i) 
             => String                   -- ^ Name of the selector we've used, for panic messages.
             -> (Builtins -> Array i a)  -- ^ Field selector for the `Builtins`.
             -> i                        -- ^ Index into the array.
             -> Builtins 
             -> a
indexBuiltin fn f i bi
  | inRange (bounds xs) i = xs ! i
  | otherwise       
  = pprSorry "Vectorise.Builtins.indexBuiltin" 
    (vcat [ text ""
    , text "DPH builtin function '" <> text fn <> text "' of size '" <> ppr i <> 
      text "' is not yet implemented."
    , text "This function does not appear in your source program, but it is needed"
    , text "to compile your code in the backend. This is a known, current limitation"
    , text "of DPH. If you want it to to work you should send mail to cvs-ghc@haskell.org"
    , text "and ask what you can do to help (it might involve some GHC hacking)."])
  where xs = f bi


-- | Get an entry from one of a 'NameEnv' of `Builtins`. Panic if the named item is not in the array.
lookupEnvBuiltin :: String                    -- Function name for error messages
                 -> NameEnv a                 -- Name environment
                 -> Name                      -- Index into the name environment
                 -> a
lookupEnvBuiltin fn env n
  | Just r <- lookupNameEnv env n = r
  | otherwise 
  = pprSorry "Vectorise.Builtins.lookupEnvBuiltin" 
    (vcat [ text ""
    , text "DPH builtin function '" <> text fn <> text "_" <> ppr n <> 
      text "' is not yet implemented."
    , text "This function does not appear in your source program, but it is needed"
    , text "to compile your code in the backend. This is a known, current limitation"
    , text "of DPH. If you want it to to work you should send mail to cvs-ghc@haskell.org"
    , text "and ask what you can do to help (it might involve some GHC hacking)."])
