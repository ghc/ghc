
-- | Builtin types and functions used by the vectoriser.
--   These are all defined in the DPH package.
module Vectorise.Builtins.Base (
	-- * Hard config
	mAX_DPH_PROD,
	mAX_DPH_SUM,
	mAX_DPH_COMBINE,
	mAX_DPH_SCALAR_ARGS,
	
	-- * Builtins
	Builtins(..),
	indexBuiltin,
	
	-- * Projections
	selTy,
	selReplicate,
	selPick,
	selTags,
	selElements,
	sumTyCon,
	prodTyCon,
	prodDataCon,
	combinePDVar,
	scalarZip,
	closureCtrFun
) where
import Vectorise.Builtins.Modules
import BasicTypes
import Class
import CoreSyn
import TysWiredIn
import Type
import TyCon
import DataCon
import Var
import Outputable
import Data.Array


-- Numbers of things exported by the DPH library.
mAX_DPH_PROD :: Int
mAX_DPH_PROD = 5

mAX_DPH_SUM :: Int
mAX_DPH_SUM = 2

mAX_DPH_COMBINE :: Int
mAX_DPH_COMBINE = 2

mAX_DPH_SCALAR_ARGS :: Int
mAX_DPH_SCALAR_ARGS = 3


-- | Holds the names of the builtin types and functions used by the vectoriser.
data Builtins 
        = Builtins 
        { dphModules       :: Modules

	-- From dph-common:Data.Array.Parallel.Lifted.PArray
        , parrayTyCon      :: TyCon			-- ^ PArray
        , parrayDataCon    :: DataCon			-- ^ PArray
        , pdataTyCon       :: TyCon			-- ^ PData
        , paClass          :: Class                     -- ^ PA
        , paTyCon          :: TyCon			-- ^ PA
        , paDataCon        :: DataCon			-- ^ PA
        , paPRSel          :: Var                       -- ^ PA
        , preprTyCon       :: TyCon			-- ^ PRepr
        , prClass          :: Class                     -- ^ PR
        , prTyCon          :: TyCon			-- ^ PR
        , prDataCon        :: DataCon			-- ^ PR
        , replicatePDVar   :: Var			-- ^ replicatePD
        , emptyPDVar       :: Var			-- ^ emptyPD
        , packByTagPDVar   :: Var			-- ^ packByTagPD
        , combinePDVars    :: Array Int Var		-- ^ combinePD
        , scalarClass      :: Class			-- ^ Scalar

        -- From dph-common:Data.Array.Parallel.Lifted.Closure
        , closureTyCon     :: TyCon			-- ^ :->
        , closureVar       :: Var			-- ^ closure
        , applyVar         :: Var			-- ^ $: 
        , liftedClosureVar :: Var			-- ^ liftedClosure
        , liftedApplyVar   :: Var			-- ^ liftedApply
        , closureCtrFuns   :: Array Int Var		-- ^ closure1 .. closure2

	-- From dph-common:Data.Array.Parallel.Lifted.Repr
        , voidTyCon        :: TyCon			-- ^ Void
        , wrapTyCon        :: TyCon			-- ^ Wrap
        , sumTyCons        :: Array Int TyCon           -- ^ Sum2 .. Sum3
        , voidVar          :: Var			-- ^ void
        , pvoidVar         :: Var			-- ^ pvoid
        , fromVoidVar      :: Var			-- ^ fromVoid
        , punitVar         :: Var			-- ^ punit

	-- From dph-common:Data.Array.Parallel.Lifted.Selector
        , selTys           :: Array Int Type		-- ^ Sel2
        , selReplicates    :: Array Int CoreExpr	-- ^ replicate2
        , selPicks         :: Array Int CoreExpr	-- ^ pick2
        , selTagss         :: Array Int CoreExpr	-- ^ tagsSel2
        , selEls           :: Array (Int, Int) CoreExpr	-- ^ elementsSel2_0 .. elementsSel_2_1

	-- From dph-common:Data.Array.Parallel.Lifted.Scalar
	-- NOTE: map is counted as a zipWith fn with one argument array.
        , scalarZips       :: Array Int Var		-- ^ map, zipWith, zipWith3

	-- A Fresh variable
        , liftingContext   :: Var			-- ^ lc
        }


-- | Get an element from one of the arrays of contained by a `Builtins`.
--   If the indexed thing is not in the array then panic.
indexBuiltin 
	:: (Ix i, Outputable i) 
	=> String 			-- ^ Name of the selector we've used, for panic messages.
	-> (Builtins -> Array i a)	-- ^ Field selector for the `Builtins`.
	-> i				-- ^ Index into the array.
	-> Builtins 
	-> a

indexBuiltin fn f i bi
  | inRange (bounds xs) i = xs ! i
  | otherwise 		  
  = pprSorry "Vectorise.Builtins.indexBuiltin" 
  	(vcat	[ text ""
		, text "DPH builtin function '" <> text fn <> text "' of size '" <> ppr i <> text "' is not yet implemented."
		, text "This function does not appear in your source program, but it is needed"
		, text "to compile your code in the backend. This is a known, current limitation"
		, text "of DPH. If you want it to to work you should send mail to cvs-ghc@haskell.org"
		, text "and ask what you can do to help (it might involve some GHC hacking)."])

  where	xs = f bi


-- Projections ----------------------------------------------------------------
-- We use these wrappers instead of indexing the `Builtin` structure directly
-- because they give nicer panic messages if the indexed thing cannot be found.

selTy :: Int -> Builtins -> Type
selTy 		= indexBuiltin "selTy" selTys

selReplicate :: Int -> Builtins -> CoreExpr
selReplicate 	= indexBuiltin "selReplicate" selReplicates 

selPick :: Int -> Builtins -> CoreExpr
selPick 	= indexBuiltin "selPick" selPicks

selTags :: Int -> Builtins -> CoreExpr
selTags 	= indexBuiltin "selTags" selTagss

selElements :: Int -> Int -> Builtins -> CoreExpr
selElements i j = indexBuiltin "selElements" selEls (i,j)

sumTyCon :: Int -> Builtins -> TyCon
sumTyCon 	= indexBuiltin "sumTyCon" sumTyCons

prodTyCon :: Int -> Builtins -> TyCon
prodTyCon n _
	| n >= 2 && n <= mAX_DPH_PROD 
	= tupleTyCon Boxed n

  	| otherwise
	= pprPanic "prodTyCon" (ppr n)

prodDataCon :: Int -> Builtins -> DataCon
prodDataCon n bi 
 = case tyConDataCons (prodTyCon n bi) of
	[con]	-> con
	_	-> pprPanic "prodDataCon" (ppr n)

combinePDVar :: Int -> Builtins -> Var
combinePDVar 	= indexBuiltin "combinePDVar" combinePDVars

scalarZip :: Int -> Builtins -> Var
scalarZip 	= indexBuiltin "scalarZip" scalarZips

closureCtrFun :: Int -> Builtins -> Var
closureCtrFun 	= indexBuiltin "closureCtrFun" closureCtrFuns


