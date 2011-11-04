\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

{-# LANGUAGE BangPatterns, DeriveDataTypeable #-}

module CostCentre (
        CostCentre(..), CcName, IsCafCC(..),
		-- All abstract except to friend: ParseIface.y

	CostCentreStack,
	CollectedCCs,
        noCCS, currentCCS, dontCareCCS,
	noCostCentre, noCCAttached,
        noCCSAttached, isCurrentCCS,
        maybeSingletonCCS,

	mkUserCC, mkAutoCC, mkAllCafsCC, 
        mkSingletonCCS,
        isCafCCS, isCafCC, isSccCountCC, sccAbleCC, ccFromThisModule,

	pprCostCentreCore,
	costCentreUserName,

	cmpCostCentre	-- used for removing dups in a list
    ) where

import Var
import Name
import Module
import Unique
import Outputable	
import FastTypes
import FastString
import Util

import Data.Data

-----------------------------------------------------------------------------
-- Cost Centres

-- | A Cost Centre is the argument of an _scc_ expression.
 
data CostCentre
  = NoCostCentre	-- Having this constructor avoids having
			-- to use "Maybe CostCentre" all the time.

  | NormalCC {  
		cc_name :: CcName,	-- Name of the cost centre itself
		cc_mod  :: Module,	-- Name of module defining this CC.
                cc_is_caf  :: IsCafCC   -- see below
    }

  | AllCafsCC {	
		cc_mod  :: Module	-- Name of module defining this CC.
    }
  deriving (Data, Typeable)

type CcName = FastString

data IsCafCC = NotCafCC | CafCC
  deriving (Eq, Ord, Data, Typeable)

noCostCentre :: CostCentre
noCostCentre = NoCostCentre


instance Eq CostCentre where
	c1 == c2 = case c1 `cmpCostCentre` c2 of { EQ -> True; _ -> False }

instance Ord CostCentre where
	compare = cmpCostCentre

cmpCostCentre :: CostCentre -> CostCentre -> Ordering

cmpCostCentre (AllCafsCC  {cc_mod = m1}) (AllCafsCC  {cc_mod = m2})
  = m1 `compare` m2

cmpCostCentre (NormalCC {cc_name = n1, cc_mod =  m1, cc_is_caf = c1}) 
	      (NormalCC {cc_name = n2, cc_mod =  m2, cc_is_caf = c2}) 
    -- first key is module name, then the name, then the cafness
  = (m1 `compare` m2) `thenCmp` (n1 `compare` n2) `thenCmp` (c1 `compare` c2)

cmpCostCentre other_1 other_2
  = let
	!tag1 = tag_CC other_1
	!tag2 = tag_CC other_2
    in
    if tag1 <# tag2 then LT else GT
  where
    tag_CC NoCostCentre    = _ILIT(0)
    tag_CC (NormalCC   {}) = _ILIT(1)
    tag_CC (AllCafsCC  {}) = _ILIT(2)


-----------------------------------------------------------------------------
-- Predicates on CostCentre

isCafCC :: CostCentre -> Bool
isCafCC (AllCafsCC {})                   = True
isCafCC (NormalCC {cc_is_caf = CafCC}) = True
isCafCC _		                 = False

-- | Is this a cost-centre which records scc counts
isSccCountCC :: CostCentre -> Bool
isSccCountCC cc | isCafCC cc  = False
                | otherwise   = True

-- | Is this a cost-centre which can be sccd ?
sccAbleCC :: CostCentre -> Bool
sccAbleCC NoCostCentre  = panic "sccAbleCC:NoCostCentre"
sccAbleCC cc | isCafCC cc = False
             | otherwise  = True

ccFromThisModule :: CostCentre -> Module -> Bool
ccFromThisModule cc m = cc_mod cc == m


-----------------------------------------------------------------------------
-- Building cost centres

mkUserCC :: FastString -> Module -> CostCentre
mkUserCC cc_name mod
  = NormalCC { cc_name = cc_name, cc_mod =  mod,
               cc_is_caf = NotCafCC {-might be changed-}
    }

mkAutoCC :: Id -> Module -> IsCafCC -> CostCentre
mkAutoCC id mod is_caf
  = NormalCC { cc_name = str, cc_mod =  mod,
               cc_is_caf = is_caf
    }
  where 
        name = getName id
        -- beware: only external names are guaranteed to have unique
        -- Occnames.  If the name is not external, we must append its
        -- Unique.
        -- See bug #249, tests prof001, prof002,  also #2411
        str | isExternalName name = occNameFS (getOccName id)
            | otherwise           = mkFastString $ showSDoc $
                                      ftext (occNameFS (getOccName id))
                                      <> char '_' <> pprUnique (getUnique name)
mkAllCafsCC :: Module -> CostCentre
mkAllCafsCC m = AllCafsCC { cc_mod = m }

-----------------------------------------------------------------------------
-- Cost Centre Stacks

-- | A Cost Centre Stack is something that can be attached to a closure.
-- This is either:
-- 	
--      * the current cost centre stack (CCCS)
--      * a pre-defined cost centre stack (there are several
-- 	  pre-defined CCSs, see below).

data CostCentreStack
  = NoCCS

  | CurrentCCS		-- Pinned on a let(rec)-bound 
			-- thunk/function/constructor, this says that the 
			-- cost centre to be attached to the object, when it 
			-- is allocated, is whatever is in the 
			-- current-cost-centre-stack register.

  | DontCareCCS         -- We need a CCS to stick in static closures
			-- (for data), but we *don't* expect them to
			-- accumulate any costs.  But we still need
			-- the placeholder.  This CCS is it.

  | SingletonCCS CostCentre

  deriving (Eq, Ord)	-- needed for Ord on CLabel


-- synonym for triple which describes the cost centre info in the generated
-- code for a module.
type CollectedCCs
  = ( [CostCentre]       -- local cost-centres that need to be decl'd
    , [CostCentre]       -- "extern" cost-centres
    , [CostCentreStack]  -- pre-defined "singleton" cost centre stacks
    )


noCCS, currentCCS, dontCareCCS :: CostCentreStack

noCCS 			= NoCCS
currentCCS              = CurrentCCS
dontCareCCS             = DontCareCCS

-----------------------------------------------------------------------------
-- Predicates on Cost-Centre Stacks

noCCSAttached :: CostCentreStack -> Bool
noCCSAttached NoCCS			= True
noCCSAttached _				= False

noCCAttached :: CostCentre -> Bool
noCCAttached NoCostCentre		= True
noCCAttached _				= False

isCurrentCCS :: CostCentreStack -> Bool
isCurrentCCS CurrentCCS			= True
isCurrentCCS _	      			= False

isCafCCS :: CostCentreStack -> Bool
isCafCCS (SingletonCCS cc)              = isCafCC cc
isCafCCS _				= False

maybeSingletonCCS :: CostCentreStack -> Maybe CostCentre
maybeSingletonCCS (SingletonCCS cc)     = Just cc
maybeSingletonCCS _			= Nothing

mkSingletonCCS :: CostCentre -> CostCentreStack
mkSingletonCCS cc = SingletonCCS cc


-----------------------------------------------------------------------------
-- Printing Cost Centre Stacks.

-- The outputable instance for CostCentreStack prints the CCS as a C
-- expression.

instance Outputable CostCentreStack where
  ppr NoCCS		= ptext (sLit "NO_CCS")
  ppr CurrentCCS	= ptext (sLit "CCCS")
  ppr DontCareCCS       = ptext (sLit "CCS_DONT_CARE")
  ppr (SingletonCCS cc) = ppr cc <> ptext (sLit "_ccs")


-----------------------------------------------------------------------------
-- Printing Cost Centres
-- 
-- There are several different ways in which we might want to print a
-- cost centre:
-- 
-- 	- the name of the cost centre, for profiling output (a C string)
-- 	- the label, i.e. C label for cost centre in .hc file.
-- 	- the debugging name, for output in -ddump things
-- 	- the interface name, for printing in _scc_ exprs in iface files.
-- 
-- The last 3 are derived from costCentreStr below.  The first is given
-- by costCentreName.

instance Outputable CostCentre where
  ppr cc = getPprStyle $ \ sty ->
	   if codeStyle sty
  	   then ppCostCentreLbl cc
	   else text (costCentreUserName cc)

-- Printing in Core
pprCostCentreCore :: CostCentre -> SDoc
pprCostCentreCore NoCostCentre
  = text "__no_cc"
pprCostCentreCore (AllCafsCC {cc_mod = m})
  = text "__sccC" <+> braces (ppr m)
pprCostCentreCore (NormalCC {cc_name = n, cc_mod = m,
                             cc_is_caf = caf})
  = text "__scc" <+> braces (hsep [
	ftext (zEncodeFS n),
	ppr m,
        pp_caf caf
    ])

pp_caf :: IsCafCC -> SDoc
pp_caf CafCC = text "__C"
pp_caf _     = empty

-- Printing as a C label
ppCostCentreLbl :: CostCentre -> SDoc
ppCostCentreLbl (NoCostCentre)		  = text "NONE_cc"
ppCostCentreLbl (AllCafsCC  {cc_mod = m}) = ppr m <> text "_CAFs_cc"
ppCostCentreLbl (NormalCC {cc_name = n, cc_mod = m, cc_is_caf = is_caf}) 
  = ppr m <> char '_' <> ftext (zEncodeFS n) <> 
	text (case is_caf of { CafCC -> "_CAF"; _ -> "" }) <> text "_cc"

-- This is the name to go in the user-displayed string, 
-- recorded in the cost centre declaration
costCentreUserName :: CostCentre -> String
costCentreUserName (NoCostCentre)  = "NO_CC"
costCentreUserName (AllCafsCC {})  = "CAF"
costCentreUserName (NormalCC {cc_name = name, cc_is_caf = is_caf})
  =  case is_caf of { CafCC -> "CAF:";   _ -> "" } ++ unpackFS name

\end{code}
