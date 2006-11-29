%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%

\begin{code}
module IfaceSyn (
	module IfaceType,		-- Re-export all this

	IfaceDecl(..), IfaceClassOp(..), IfaceConDecl(..), IfaceConDecls(..),
	IfaceExpr(..), IfaceAlt, IfaceNote(..),
	IfaceBinding(..), IfaceConAlt(..), IfaceIdInfo(..),
	IfaceInfoItem(..), IfaceRule(..), IfaceInst(..), IfaceFamInst(..),

	-- Misc
        ifaceDeclSubBndrs, visibleIfConDecls,

	-- Equality
	GenIfaceEq(..), IfaceEq, (&&&), bool, eqListBy, eqMaybeBy,
	eqIfDecl, eqIfInst, eqIfFamInst, eqIfRule, checkBootDecl,
	
	-- Pretty printing
	pprIfaceExpr, pprIfaceDeclHead 
    ) where

#include "HsVersions.h"

import CoreSyn
import IfaceType

import NewDemand
import Class
import UniqFM
import Unique
import NameSet 
import Name
import CostCentre
import Literal
import ForeignCall
import SrcLoc
import BasicTypes
import Outputable
import FastString
import Module

import Data.List
import Data.Maybe

infixl 3 &&&
infix  4 `eqIfExt`, `eqIfIdInfo`, `eqIfType`
\end{code}


%************************************************************************
%*									*
		Data type declarations
%*									*
%************************************************************************

\begin{code}
data IfaceDecl 
  = IfaceId { ifName   :: OccName,
	      ifType   :: IfaceType, 
	      ifIdInfo :: IfaceIdInfo }

  | IfaceData { ifName       :: OccName,	-- Type constructor
		ifTyVars     :: [IfaceTvBndr],	-- Type variables
		ifCtxt	     :: IfaceContext,	-- The "stupid theta"
		ifCons	     :: IfaceConDecls,	-- Includes new/data info
	        ifRec	     :: RecFlag,	-- Recursive or not?
		ifGadtSyntax :: Bool,		-- True <=> declared using
						-- GADT syntax 
		ifGeneric    :: Bool,		-- True <=> generic converter
						--          functions available
    						-- We need this for imported
    						-- data decls, since the
    						-- imported modules may have
    						-- been compiled with
    						-- different flags to the
    						-- current compilation unit 
                ifFamInst    :: Maybe (IfaceTyCon, [IfaceType])
                                                -- Just <=> instance of family
    }

  | IfaceSyn  {	ifName    :: OccName,		-- Type constructor
		ifTyVars  :: [IfaceTvBndr],	-- Type variables
		ifOpenSyn :: Bool,		-- Is an open family?
		ifSynRhs  :: IfaceType		-- Type for an ordinary
						-- synonym and kind for an
						-- open family
    }

  | IfaceClass { ifCtxt    :: IfaceContext, 	-- Context...
		 ifName    :: OccName,		-- Name of the class
		 ifTyVars  :: [IfaceTvBndr],	-- Type variables
		 ifFDs     :: [FunDep FastString], -- Functional dependencies
		 ifATs	   :: [IfaceDecl],	-- Associated type families
		 ifSigs    :: [IfaceClassOp],	-- Method signatures
	         ifRec	   :: RecFlag		-- Is newtype/datatype associated with the class recursive?
    }

  | IfaceForeign { ifName :: OccName,           -- Needs expanding when we move
                                                -- beyond .NET
		   ifExtName :: Maybe FastString }

data IfaceClassOp = IfaceClassOp OccName DefMeth IfaceType
	-- Nothing    => no default method
	-- Just False => ordinary polymorphic default method
	-- Just True  => generic default method

data IfaceConDecls
  = IfAbstractTyCon		-- No info
  | IfOpenDataTyCon		-- Open data family
  | IfOpenNewTyCon		-- Open newtype family
  | IfDataTyCon [IfaceConDecl]	-- data type decls
  | IfNewTyCon  IfaceConDecl	-- newtype decls

visibleIfConDecls :: IfaceConDecls -> [IfaceConDecl]
visibleIfConDecls IfAbstractTyCon  = []
visibleIfConDecls IfOpenDataTyCon  = []
visibleIfConDecls IfOpenNewTyCon   = []
visibleIfConDecls (IfDataTyCon cs) = cs
visibleIfConDecls (IfNewTyCon c)   = [c]

data IfaceConDecl 
  = IfCon {
	ifConOcc     :: OccName,   		-- Constructor name
	ifConInfix   :: Bool,			-- True <=> declared infix
	ifConUnivTvs :: [IfaceTvBndr],		-- Universal tyvars
	ifConExTvs   :: [IfaceTvBndr],		-- Existential tyvars
	ifConEqSpec  :: [(OccName,IfaceType)],	-- Equality contraints
	ifConCtxt    :: IfaceContext,		-- Non-stupid context
	ifConArgTys  :: [IfaceType],		-- Arg types
	ifConFields  :: [OccName],		-- ...ditto... (field labels)
	ifConStricts :: [StrictnessMark]}	-- Empty (meaning all lazy),
						-- or 1-1 corresp with arg tys

data IfaceInst 
  = IfaceInst { ifInstCls  :: Name,     		-- See comments with
		ifInstTys  :: [Maybe IfaceTyCon],	-- the defn of Instance
		ifDFun     :: Name,     		-- The dfun
		ifOFlag    :: OverlapFlag,		-- Overlap flag
		ifInstOrph :: Maybe OccName }		-- See is_orph in defn of Instance
	-- There's always a separate IfaceDecl for the DFun, which gives 
	-- its IdInfo with its full type and version number.
	-- The instance declarations taken together have a version number,
	-- and we don't want that to wobble gratuitously
	-- If this instance decl is *used*, we'll record a usage on the dfun;
	-- and if the head does not change it won't be used if it wasn't before

data IfaceFamInst
  = IfaceFamInst { ifFamInstFam   :: Name                -- Family tycon
		 , ifFamInstTys   :: [Maybe IfaceTyCon]  -- Rough match types
		 , ifFamInstTyCon :: IfaceTyCon		 -- Instance decl
		 }

data IfaceRule
  = IfaceRule { 
	ifRuleName   :: RuleName,
	ifActivation :: Activation,
	ifRuleBndrs  :: [IfaceBndr],	-- Tyvars and term vars
	ifRuleHead   :: Name,   	-- Head of lhs
	ifRuleArgs   :: [IfaceExpr],	-- Args of LHS
	ifRuleRhs    :: IfaceExpr,
	ifRuleOrph   :: Maybe OccName	-- Just like IfaceInst
    }

data IfaceIdInfo
  = NoInfo			-- When writing interface file without -O
  | HasInfo [IfaceInfoItem]	-- Has info, and here it is

-- Here's a tricky case:
--   * Compile with -O module A, and B which imports A.f
--   * Change function f in A, and recompile without -O
--   * When we read in old A.hi we read in its IdInfo (as a thunk)
--	(In earlier GHCs we used to drop IdInfo immediately on reading,
--	 but we do not do that now.  Instead it's discarded when the
--	 ModIface is read into the various decl pools.)
--   * The version comparsion sees that new (=NoInfo) differs from old (=HasInfo *)
--	and so gives a new version.

data IfaceInfoItem
  = HsArity	 Arity
  | HsStrictness StrictSig
  | HsInline     Activation
  | HsUnfold	 IfaceExpr
  | HsNoCafRefs
  | HsWorker	 Name Arity	-- Worker, if any see IdInfo.WorkerInfo
					-- for why we want arity here.
	-- NB: we need IfaceExtName (not just OccName) because the worker
	--     can simplify to a function in another module.
-- NB: Specialisations and rules come in separately and are
-- only later attached to the Id.  Partial reason: some are orphans.

--------------------------------
data IfaceExpr
  = IfaceLcl 	FastString
  | IfaceExt    Name
  | IfaceType   IfaceType
  | IfaceTuple 	Boxity [IfaceExpr]		-- Saturated; type arguments omitted
  | IfaceLam 	IfaceBndr IfaceExpr
  | IfaceApp 	IfaceExpr IfaceExpr
  | IfaceCase	IfaceExpr FastString IfaceType [IfaceAlt]
  | IfaceLet	IfaceBinding  IfaceExpr
  | IfaceNote	IfaceNote IfaceExpr
  | IfaceCast   IfaceExpr IfaceCoercion
  | IfaceLit	Literal
  | IfaceFCall	ForeignCall IfaceType

data IfaceNote = IfaceSCC CostCentre
	       | IfaceInlineMe
               | IfaceCoreNote String

type IfaceAlt = (IfaceConAlt, [FastString], IfaceExpr)
	-- Note: FastString, not IfaceBndr (and same with the case binder)
	-- We reconstruct the kind/type of the thing from the context
	-- thus saving bulk in interface files

data IfaceConAlt = IfaceDefault
 		 | IfaceDataAlt Name
		 | IfaceTupleAlt Boxity
		 | IfaceLitAlt Literal

data IfaceBinding
  = IfaceNonRec	IfaceIdBndr IfaceExpr
  | IfaceRec 	[(IfaceIdBndr, IfaceExpr)]

-- -----------------------------------------------------------------------------
-- Utils on IfaceSyn

ifaceDeclSubBndrs :: IfaceDecl -> [OccName]
--  *Excludes* the 'main' name, but *includes* the implicitly-bound names
-- Deeply revolting, because it has to predict what gets bound,
-- especially the question of whether there's a wrapper for a datacon

ifaceDeclSubBndrs (IfaceClass {ifCtxt = sc_ctxt, ifName = cls_occ, 
			       ifSigs = sigs, ifATs = ats })
  = co_occs ++
    [tc_occ, dc_occ, dcww_occ] ++
    [op | IfaceClassOp op  _ _ <- sigs] ++
    [ifName at | at <- ats ] ++
    [mkSuperDictSelOcc n cls_occ | n <- [1..n_ctxt]] 
  where
    n_ctxt = length sc_ctxt
    n_sigs = length sigs
    tc_occ  = mkClassTyConOcc cls_occ
    dc_occ  = mkClassDataConOcc cls_occ	
    co_occs | is_newtype = [mkNewTyCoOcc tc_occ]
	    | otherwise  = []
    dcww_occ -- | is_newtype = mkDataConWrapperOcc dc_occ	-- Newtypes have wrapper but no worker
	     | otherwise  = mkDataConWorkerOcc dc_occ	-- Otherwise worker but no wrapper
    is_newtype = n_sigs + n_ctxt == 1			-- Sigh 

ifaceDeclSubBndrs IfaceData {ifCons = IfAbstractTyCon}
  = []
-- Newtype
ifaceDeclSubBndrs (IfaceData {ifName = tc_occ,
			      ifCons = IfNewTyCon (
				         IfCon { ifConOcc = con_occ, 
							   ifConFields = fields
							 }),
			      ifFamInst = famInst}) 
  = fields ++ [con_occ, mkDataConWorkerOcc con_occ, mkNewTyCoOcc tc_occ]
    ++ famInstCo famInst tc_occ

ifaceDeclSubBndrs (IfaceData {ifName = tc_occ,
			      ifCons = IfDataTyCon cons, 
			      ifFamInst = famInst})
  = nub (concatMap ifConFields cons) 	-- Eliminate duplicate fields
    ++ concatMap dc_occs cons
    ++ famInstCo famInst tc_occ
  where
    dc_occs con_decl
	| has_wrapper = [con_occ, work_occ, wrap_occ]
	| otherwise   = [con_occ, work_occ]
	where
	  con_occ = ifConOcc con_decl
	  strs    = ifConStricts con_decl
	  wrap_occ = mkDataConWrapperOcc con_occ
	  work_occ = mkDataConWorkerOcc con_occ
	  has_wrapper = any isMarkedStrict strs	-- See MkId.mkDataConIds (sigh)
			|| not (null . ifConEqSpec $ con_decl)
			|| isJust famInst
		-- ToDo: may miss strictness in existential dicts

ifaceDeclSubBndrs _other = []

-- coercion for data/newtype family instances
famInstCo Nothing  baseOcc = []
famInstCo (Just _) baseOcc = [mkInstTyCoOcc baseOcc]

----------------------------- Printing IfaceDecl ------------------------------

instance Outputable IfaceDecl where
  ppr = pprIfaceDecl

pprIfaceDecl (IfaceId {ifName = var, ifType = ty, ifIdInfo = info})
  = sep [ ppr var <+> dcolon <+> ppr ty, 
	  nest 2 (ppr info) ]

pprIfaceDecl (IfaceForeign {ifName = tycon})
  = hsep [ptext SLIT("foreign import type dotnet"), ppr tycon]

pprIfaceDecl (IfaceSyn {ifName = tycon, ifTyVars = tyvars, 
		        ifOpenSyn = False, ifSynRhs = mono_ty})
  = hang (ptext SLIT("type") <+> pprIfaceDeclHead [] tycon tyvars)
       4 (equals <+> ppr mono_ty)

pprIfaceDecl (IfaceSyn {ifName = tycon, ifTyVars = tyvars, 
		        ifOpenSyn = True, ifSynRhs = mono_ty})
  = hang (ptext SLIT("type family") <+> pprIfaceDeclHead [] tycon tyvars)
       4 (dcolon <+> ppr mono_ty)

pprIfaceDecl (IfaceData {ifName = tycon, ifGeneric = gen, ifCtxt = context,
			 ifTyVars = tyvars, ifCons = condecls, 
			 ifRec = isrec, ifFamInst = mbFamInst})
  = hang (pp_nd <+> pprIfaceDeclHead context tycon tyvars)
       4 (vcat [pprRec isrec, pprGen gen, pp_condecls tycon condecls,
	        pprFamily mbFamInst])
  where
    pp_nd = case condecls of
		IfAbstractTyCon -> ptext SLIT("data")
		IfOpenDataTyCon -> ptext SLIT("data family")
		IfDataTyCon _   -> ptext SLIT("data")
		IfNewTyCon _  	-> ptext SLIT("newtype")
		IfOpenNewTyCon 	-> ptext SLIT("newtype family")

pprIfaceDecl (IfaceClass {ifCtxt = context, ifName = clas, ifTyVars = tyvars, 
			  ifFDs = fds, ifATs = ats, ifSigs = sigs, 
			  ifRec = isrec})
  = hang (ptext SLIT("class") <+> pprIfaceDeclHead context clas tyvars <+> pprFundeps fds)
       4 (vcat [pprRec isrec,
	        sep (map ppr ats),
		sep (map ppr sigs)])

pprRec isrec = ptext SLIT("RecFlag") <+> ppr isrec
pprGen True  = ptext SLIT("Generics: yes")
pprGen False = ptext SLIT("Generics: no")

pprFamily Nothing        = ptext SLIT("FamilyInstance: none")
pprFamily (Just famInst) = ptext SLIT("FamilyInstance:") <+> ppr famInst

instance Outputable IfaceClassOp where
   ppr (IfaceClassOp n dm ty) = ppr n <+> ppr dm <+> dcolon <+> ppr ty

pprIfaceDeclHead :: IfaceContext -> OccName -> [IfaceTvBndr] -> SDoc
pprIfaceDeclHead context thing tyvars
  = hsep [pprIfaceContext context, parenSymOcc thing (ppr thing), 
	  pprIfaceTvBndrs tyvars]

pp_condecls tc IfAbstractTyCon  = ptext SLIT("{- abstract -}")
pp_condecls tc IfOpenNewTyCon   = empty
pp_condecls tc (IfNewTyCon c)   = equals <+> pprIfaceConDecl tc c
pp_condecls tc IfOpenDataTyCon  = empty
pp_condecls tc (IfDataTyCon cs) = equals <+> sep (punctuate (ptext SLIT(" |"))
							     (map (pprIfaceConDecl tc) cs))

pprIfaceConDecl tc
	(IfCon { ifConOcc = name, ifConInfix = is_infix, 
		 ifConUnivTvs = univ_tvs, ifConExTvs = ex_tvs, 
		 ifConEqSpec = eq_spec, ifConCtxt = ctxt, ifConArgTys = arg_tys, 
		 ifConStricts = strs, ifConFields = fields })
  = sep [main_payload,
	 if is_infix then ptext SLIT("Infix") else empty,
	 if null strs then empty 
	      else nest 4 (ptext SLIT("Stricts:") <+> hsep (map ppr strs)),
	 if null fields then empty
	      else nest 4 (ptext SLIT("Fields:") <+> hsep (map ppr fields))]
  where
    main_payload = ppr name <+> dcolon <+> 
		   pprIfaceForAllPart (univ_tvs ++ ex_tvs) (eq_ctxt ++ ctxt) (ppr con_tau)

    eq_ctxt = [(IfaceEqPred (IfaceTyVar (occNameFS tv)) ty) 
	      | (tv,ty) <- eq_spec] 
    con_tau = foldr1 IfaceFunTy (arg_tys ++ [tc_app])
    tc_app  = IfaceTyConApp (IfaceTc tc_name)
			    [IfaceTyVar tv | (tv,_) <- univ_tvs]
    tc_name = mkInternalName (mkBuiltinUnique 1) tc noSrcLoc
	-- Really Gruesome, but just for debug print

instance Outputable IfaceRule where
  ppr (IfaceRule { ifRuleName = name, ifActivation = act, ifRuleBndrs = bndrs,
		   ifRuleHead = fn, ifRuleArgs = args, ifRuleRhs = rhs }) 
    = sep [hsep [doubleQuotes (ftext name), ppr act,
		 ptext SLIT("forall") <+> pprIfaceBndrs bndrs],
	   nest 2 (sep [ppr fn <+> sep (map (pprIfaceExpr parens) args),
		        ptext SLIT("=") <+> ppr rhs])
      ]

instance Outputable IfaceInst where
  ppr (IfaceInst {ifDFun = dfun_id, ifOFlag = flag, 
		  ifInstCls = cls, ifInstTys = mb_tcs})
    = hang (ptext SLIT("instance") <+> ppr flag 
		<+> ppr cls <+> brackets (pprWithCommas ppr_rough mb_tcs))
         2 (equals <+> ppr dfun_id)

instance Outputable IfaceFamInst where
  ppr (IfaceFamInst {ifFamInstFam = fam, ifFamInstTys = mb_tcs,
		     ifFamInstTyCon = tycon_id})
    = hang (ptext SLIT("family instance") <+> 
	    ppr fam <+> brackets (pprWithCommas ppr_rough mb_tcs))
         2 (equals <+> ppr tycon_id)

ppr_rough :: Maybe IfaceTyCon -> SDoc
ppr_rough Nothing   = dot
ppr_rough (Just tc) = ppr tc
\end{code}


----------------------------- Printing IfaceExpr ------------------------------------

\begin{code}
instance Outputable IfaceExpr where
    ppr e = pprIfaceExpr noParens e

pprIfaceExpr :: (SDoc -> SDoc) -> IfaceExpr -> SDoc
	-- The function adds parens in context that need
	-- an atomic value (e.g. function args)

pprIfaceExpr add_par (IfaceLcl v)       = ppr v
pprIfaceExpr add_par (IfaceExt v)       = ppr v
pprIfaceExpr add_par (IfaceLit l)       = ppr l
pprIfaceExpr add_par (IfaceFCall cc ty) = braces (ppr cc <+> ppr ty)
pprIfaceExpr add_par (IfaceType ty)     = char '@' <+> pprParendIfaceType ty

pprIfaceExpr add_par app@(IfaceApp _ _) = add_par (pprIfaceApp app [])
pprIfaceExpr add_par (IfaceTuple c as)  = tupleParens c (interpp'SP as)

pprIfaceExpr add_par e@(IfaceLam _ _)   
  = add_par (sep [char '\\' <+> sep (map ppr bndrs) <+> arrow,
		  pprIfaceExpr noParens body])
  where 
    (bndrs,body) = collect [] e
    collect bs (IfaceLam b e) = collect (b:bs) e
    collect bs e              = (reverse bs, e)

pprIfaceExpr add_par (IfaceCase scrut bndr ty [(con, bs, rhs)])
  = add_par (sep [ptext SLIT("case") <+> char '@' <+> pprParendIfaceType ty
			<+> pprIfaceExpr noParens scrut <+> ptext SLIT("of") 
			<+> ppr bndr <+> char '{' <+> ppr_con_bs con bs <+> arrow,
  		  pprIfaceExpr noParens rhs <+> char '}'])

pprIfaceExpr add_par (IfaceCase scrut bndr ty alts)
  = add_par (sep [ptext SLIT("case") <+> char '@' <+> pprParendIfaceType ty
		 	<+> pprIfaceExpr noParens scrut <+> ptext SLIT("of") 
			<+> ppr bndr <+> char '{',
  		  nest 2 (sep (map ppr_alt alts)) <+> char '}'])

pprIfaceExpr add_par (IfaceCast expr co)
  = sep [pprIfaceExpr parens expr,
	 nest 2 (ptext SLIT("`cast`")),
	 pprParendIfaceType co]

pprIfaceExpr add_par (IfaceLet (IfaceNonRec b rhs) body)
  = add_par (sep [ptext SLIT("let {"), 
		  nest 2 (ppr_bind (b, rhs)),
		  ptext SLIT("} in"), 
		  pprIfaceExpr noParens body])

pprIfaceExpr add_par (IfaceLet (IfaceRec pairs) body)
  = add_par (sep [ptext SLIT("letrec {"),
		  nest 2 (sep (map ppr_bind pairs)), 
		  ptext SLIT("} in"),
		  pprIfaceExpr noParens body])

pprIfaceExpr add_par (IfaceNote note body) = add_par (ppr note <+> pprIfaceExpr parens body)

ppr_alt (con, bs, rhs) = sep [ppr_con_bs con bs, 
			      arrow <+> pprIfaceExpr noParens rhs]

ppr_con_bs (IfaceTupleAlt tup_con) bs = tupleParens tup_con (interpp'SP bs)
ppr_con_bs con bs		      = ppr con <+> hsep (map ppr bs)
  
ppr_bind ((b,ty),rhs) = sep [ppr b <+> dcolon <+> ppr ty, 
			     equals <+> pprIfaceExpr noParens rhs]

------------------
pprIfaceApp (IfaceApp fun arg) args = pprIfaceApp fun (nest 2 (pprIfaceExpr parens arg) : args)
pprIfaceApp fun	 	       args = sep (pprIfaceExpr parens fun : args)

------------------
instance Outputable IfaceNote where
    ppr (IfaceSCC cc)     = pprCostCentreCore cc
    ppr IfaceInlineMe     = ptext SLIT("__inline_me")
    ppr (IfaceCoreNote s) = ptext SLIT("__core_note") <+> pprHsString (mkFastString s)


instance Outputable IfaceConAlt where
    ppr IfaceDefault	  = text "DEFAULT"
    ppr (IfaceLitAlt l)   = ppr l
    ppr (IfaceDataAlt d)  = ppr d
    ppr (IfaceTupleAlt b) = panic "ppr IfaceConAlt" 
	-- IfaceTupleAlt is handled by the case-alternative printer

------------------
instance Outputable IfaceIdInfo where
   ppr NoInfo       = empty
   ppr (HasInfo is) = ptext SLIT("{-") <+> fsep (map ppr_hs_info is) <+> ptext SLIT("-}")

ppr_hs_info (HsUnfold unf) 	= ptext SLIT("Unfolding:") <+>
				  	parens (pprIfaceExpr noParens unf)
ppr_hs_info (HsInline act)      = ptext SLIT("Inline:") <+> ppr act
ppr_hs_info (HsArity arity)     = ptext SLIT("Arity:") <+> int arity
ppr_hs_info (HsStrictness str)  = ptext SLIT("Strictness:") <+> pprIfaceStrictSig str
ppr_hs_info HsNoCafRefs		= ptext SLIT("HasNoCafRefs")
ppr_hs_info (HsWorker w a)	= ptext SLIT("Worker:") <+> ppr w <+> int a
\end{code}


%************************************************************************
%*									*
	Equality, for interface file version generaion only
%*									*
%************************************************************************

Equality over IfaceSyn returns an IfaceEq, not a Bool.  The new
constructor is EqBut, which gives the set of things whose version must
be equal for the whole thing to be equal.  So the key function is
eqIfExt, which compares Names.

Of course, equality is also done modulo alpha conversion.

\begin{code}
data GenIfaceEq a
  = Equal 		-- Definitely exactly the same
  | NotEqual		-- Definitely different
  | EqBut a       -- The same provided these Names have not changed

type IfaceEq = GenIfaceEq NameSet

instance Outputable IfaceEq where
  ppr Equal          = ptext SLIT("Equal")
  ppr NotEqual       = ptext SLIT("NotEqual")
  ppr (EqBut occset) = ptext SLIT("EqBut") <+> ppr (nameSetToList occset)

bool :: Bool -> IfaceEq
bool True  = Equal
bool False = NotEqual

toBool :: IfaceEq -> Bool
toBool Equal     = True
toBool (EqBut _) = True
toBool NotEqual  = False

zapEq :: IfaceEq -> IfaceEq	-- Used to forget EqBut information
zapEq (EqBut _) = Equal
zapEq other	= other

(&&&) :: IfaceEq -> IfaceEq -> IfaceEq
Equal       &&& x 	    = x
NotEqual    &&& x	    = NotEqual
EqBut nms   &&& Equal       = EqBut nms
EqBut nms   &&& NotEqual    = NotEqual
EqBut nms1  &&& EqBut nms2  = EqBut (nms1 `unionNameSets` nms2)

-- This function is the core of the EqBut stuff
-- ASSUMPTION: The left-hand argument is the NEW CODE, and hence
-- any Names in the left-hand arg have the correct parent in them.
eqIfExt :: Name -> Name -> IfaceEq
eqIfExt name1 name2 
  | name1 == name2 = EqBut (unitNameSet name1)
  | otherwise      = NotEqual

---------------------
checkBootDecl :: IfaceDecl 	-- The boot decl
	      -> IfaceDecl	-- The real decl
	      -> Bool		-- True <=> compatible
checkBootDecl (IfaceId s1 t1 _) (IfaceId s2 t2 _)
  = ASSERT( s1==s2 ) toBool (t1 `eqIfType` t2)

checkBootDecl d1@(IfaceForeign {}) d2@(IfaceForeign {})
  = ASSERT (ifName d1 == ifName d2 ) ifExtName d1 == ifExtName d2

checkBootDecl d1@(IfaceSyn {}) d2@(IfaceSyn {})
  = ASSERT( ifName d1 == ifName d2 )
    toBool $ eqWith (ifTyVars d1) (ifTyVars d2) $ \ env -> 
          eq_ifType env (ifSynRhs d1) (ifSynRhs d2)

checkBootDecl d1@(IfaceData {}) d2@(IfaceData {})
-- We don't check the recursion flags because the boot-one is
-- recursive, to be conservative, but the real one may not be.
-- I'm not happy with the way recursive flags are dealt with.
  = ASSERT( ifName d1    == ifName d2 ) 
    toBool $ eqWith (ifTyVars d1) (ifTyVars d2) $ \ env -> 
	eq_ifContext env (ifCtxt d1) (ifCtxt d2) &&& 
	case ifCons d1 of
	    IfAbstractTyCon -> Equal
	    cons1	    -> eq_hsCD env cons1 (ifCons d2)

checkBootDecl d1@(IfaceClass {}) d2@(IfaceClass {})
  = ASSERT( ifName d1 == ifName d2 )
    toBool $ eqWith (ifTyVars d1) (ifTyVars d2) $ \ env -> 
	  eqListBy (eq_hsFD env)    (ifFDs d1)  (ifFDs d2) &&&
	  case (ifCtxt d1, ifSigs d1) of
	     ([], [])      -> Equal
	     (cxt1, sigs1) -> eq_ifContext env cxt1 (ifCtxt d2)  &&&
			      eqListBy (eq_cls_sig env) sigs1 (ifSigs d2)

checkBootDecl _ _ = False	-- default case

---------------------
eqIfDecl :: IfaceDecl -> IfaceDecl -> IfaceEq
eqIfDecl (IfaceId s1 t1 i1) (IfaceId s2 t2 i2)
  = bool (s1 == s2) &&& (t1 `eqIfType` t2) &&& (i1 `eqIfIdInfo` i2)

eqIfDecl d1@(IfaceForeign {}) d2@(IfaceForeign {})
  = bool (ifName d1 == ifName d2 && ifExtName d1 == ifExtName d2)

eqIfDecl d1@(IfaceData {}) d2@(IfaceData {})
  = bool (ifName d1    == ifName d2 && 
	  ifRec d1     == ifRec   d2 && 
	  ifGadtSyntax d1 == ifGadtSyntax   d2 && 
	  ifGeneric d1 == ifGeneric d2) &&&
    ifFamInst d1 `eqIfTc_fam` ifFamInst d2 &&&
    eqWith (ifTyVars d1) (ifTyVars d2) (\ env -> 
	    eq_ifContext env (ifCtxt d1) (ifCtxt d2) &&& 
    	    eq_hsCD env (ifCons d1) (ifCons d2) 
	)
	-- The type variables of the data type do not scope
	-- over the constructors (any more), but they do scope
	-- over the stupid context in the IfaceConDecls
  where
    Nothing             `eqIfTc_fam` Nothing             = Equal
    (Just (fam1, tys1)) `eqIfTc_fam` (Just (fam2, tys2)) = 
      fam1 `eqIfTc` fam2 &&& eqListBy eqIfType tys1 tys2
    _		        `eqIfTc_fam` _                   = NotEqual

eqIfDecl d1@(IfaceSyn {}) d2@(IfaceSyn {})
  = bool (ifName d1 == ifName d2) &&&
    eqWith (ifTyVars d1) (ifTyVars d2) (\ env -> 
          eq_ifType env (ifSynRhs d1) (ifSynRhs d2)
        )

eqIfDecl d1@(IfaceClass {}) d2@(IfaceClass {})
  = bool (ifName d1 == ifName d2 && 
	  ifRec d1  == ifRec  d2) &&&
    eqWith (ifTyVars d1) (ifTyVars d2) (\ env -> 
   	  eq_ifContext env (ifCtxt d1) (ifCtxt d2)  &&&
	  eqListBy (eq_hsFD env)    (ifFDs d1)  (ifFDs d2) &&&
	  eqListBy eqIfDecl         (ifATs d1)  (ifATs d2) &&&
	  eqListBy (eq_cls_sig env) (ifSigs d1) (ifSigs d2)
       )

eqIfDecl _ _ = NotEqual	-- default case

-- Helper
eqWith :: [IfaceTvBndr] -> [IfaceTvBndr] -> (EqEnv -> IfaceEq) -> IfaceEq
eqWith = eq_ifTvBndrs emptyEqEnv

-----------------------
eqIfInst d1 d2 = bool (ifDFun d1 == ifDFun d2 && ifOFlag d1 == ifOFlag d2)
-- All other changes are handled via the version info on the dfun

eqIfFamInst d1 d2 = bool (ifFamInstTyCon d1 == ifFamInstTyCon d2)
-- All other changes are handled via the version info on the tycon

eqIfRule (IfaceRule n1 a1 bs1 f1 es1 rhs1 o1)
	 (IfaceRule n2 a2 bs2 f2 es2 rhs2 o2)
       = bool (n1==n2 && a1==a2 && o1 == o2) &&&
	 f1 `eqIfExt` f2 &&&
         eq_ifBndrs emptyEqEnv bs1 bs2 (\env -> 
	 zapEq (eqListBy (eq_ifaceExpr env) es1 es2) &&&
		-- zapEq: for the LHSs, ignore the EqBut part
         eq_ifaceExpr env rhs1 rhs2)

eq_hsCD env (IfDataTyCon c1) (IfDataTyCon c2) 
  = eqListBy (eq_ConDecl env) c1 c2

eq_hsCD env (IfNewTyCon c1)  (IfNewTyCon c2)  = eq_ConDecl env c1 c2
eq_hsCD env IfAbstractTyCon  IfAbstractTyCon  = Equal
eq_hsCD env IfOpenDataTyCon  IfOpenDataTyCon  = Equal
eq_hsCD env IfOpenNewTyCon   IfOpenNewTyCon   = Equal
eq_hsCD env d1		     d2		      = NotEqual

eq_ConDecl env c1 c2
  = bool (ifConOcc c1     == ifConOcc c2 && 
	  ifConInfix c1   == ifConInfix c2 && 
	  ifConStricts c1 == ifConStricts c2 && 
	  ifConFields c1  == ifConFields c2) &&&
    eq_ifTvBndrs env (ifConUnivTvs c1) (ifConUnivTvs c2) (\ env ->
    eq_ifTvBndrs env (ifConExTvs c1) (ifConExTvs c2) (\ env ->
	eq_ifContext env (ifConCtxt c1) (ifConCtxt c2) &&&
	eq_ifTypes env (ifConArgTys c1) (ifConArgTys c2)))

eq_hsFD env (ns1,ms1) (ns2,ms2)
  = eqListBy (eqIfOcc env) ns1 ns2 &&& eqListBy (eqIfOcc env) ms1 ms2

eq_cls_sig env (IfaceClassOp n1 dm1 ty1) (IfaceClassOp n2 dm2 ty2)
  = bool (n1==n2 && dm1 == dm2) &&& eq_ifType env ty1 ty2
\end{code}


\begin{code}
-----------------
eqIfIdInfo NoInfo	 NoInfo	       = Equal
eqIfIdInfo (HasInfo is1) (HasInfo is2) = eqListBy eq_item is1 is2
eqIfIdInfo i1 		 i2 = NotEqual

eq_item (HsInline a1)	   (HsInline a2)      = bool (a1 == a2)
eq_item (HsArity a1)	   (HsArity a2)	      = bool (a1 == a2)
eq_item (HsStrictness s1)  (HsStrictness s2)  = bool (s1 == s2)
eq_item (HsUnfold u1)   (HsUnfold u2)         = eq_ifaceExpr emptyEqEnv u1 u2
eq_item HsNoCafRefs        HsNoCafRefs	      = Equal
eq_item (HsWorker wkr1 a1) (HsWorker wkr2 a2) = bool (a1==a2) &&& (wkr1 `eqIfExt` wkr2)
eq_item _ _ = NotEqual

-----------------
eq_ifaceExpr :: EqEnv -> IfaceExpr -> IfaceExpr -> IfaceEq
eq_ifaceExpr env (IfaceLcl v1)	      (IfaceLcl v2)	   = eqIfOcc env v1 v2
eq_ifaceExpr env (IfaceExt v1)	      (IfaceExt v2)	   = eqIfExt v1 v2
eq_ifaceExpr env (IfaceLit l1)        (IfaceLit l2) 	   = bool (l1 == l2)
eq_ifaceExpr env (IfaceFCall c1 ty1)  (IfaceFCall c2 ty2)  = bool (c1==c2) &&& eq_ifType env ty1 ty2
eq_ifaceExpr env (IfaceType ty1)      (IfaceType ty2)	   = eq_ifType env ty1 ty2
eq_ifaceExpr env (IfaceTuple n1 as1)  (IfaceTuple n2 as2)  = bool (n1==n2) &&& eqListBy (eq_ifaceExpr env) as1 as2
eq_ifaceExpr env (IfaceLam b1 body1)  (IfaceLam b2 body2)  = eq_ifBndr env b1 b2 (\env -> eq_ifaceExpr env body1 body2)
eq_ifaceExpr env (IfaceApp f1 a1)     (IfaceApp f2 a2)	   = eq_ifaceExpr env f1 f2 &&& eq_ifaceExpr env a1 a2
eq_ifaceExpr env (IfaceCast e1 co1)   (IfaceCast e2 co2)   = eq_ifaceExpr env e1 e2 &&& eq_ifType env co1 co2
eq_ifaceExpr env (IfaceNote n1 r1)    (IfaceNote n2 r2)    = eq_ifaceNote env n1 n2 &&& eq_ifaceExpr env r1 r2

eq_ifaceExpr env (IfaceCase s1 b1 ty1 as1) (IfaceCase s2 b2 ty2 as2)
  = eq_ifaceExpr env s1 s2 &&&
    eq_ifType env ty1 ty2 &&&
    eq_ifNakedBndr env b1 b2 (\env -> eqListBy (eq_ifaceAlt env) as1 as2)
  where
    eq_ifaceAlt env (c1,bs1,r1) (c2,bs2,r2)
	= bool (eq_ifaceConAlt c1 c2) &&& 
	  eq_ifNakedBndrs env bs1 bs2 (\env -> eq_ifaceExpr env r1 r2)

eq_ifaceExpr env (IfaceLet (IfaceNonRec b1 r1) x1) (IfaceLet (IfaceNonRec b2 r2) x2)
  = eq_ifaceExpr env r1 r2 &&& eq_ifIdBndr env b1 b2 (\env -> eq_ifaceExpr env x1 x2)

eq_ifaceExpr env (IfaceLet (IfaceRec as1) x1) (IfaceLet (IfaceRec as2) x2)
  = eq_ifIdBndrs env bs1 bs2 (\env -> eqListBy (eq_ifaceExpr env) rs1 rs2 &&& eq_ifaceExpr env x1 x2)
  where
    (bs1,rs1) = unzip as1
    (bs2,rs2) = unzip as2


eq_ifaceExpr env _ _ = NotEqual

-----------------
eq_ifaceConAlt :: IfaceConAlt -> IfaceConAlt -> Bool
eq_ifaceConAlt IfaceDefault	  IfaceDefault		= True
eq_ifaceConAlt (IfaceDataAlt n1)  (IfaceDataAlt n2)	= n1==n2
eq_ifaceConAlt (IfaceTupleAlt c1) (IfaceTupleAlt c2)	= c1==c2
eq_ifaceConAlt (IfaceLitAlt l1)	  (IfaceLitAlt l2)	= l1==l2
eq_ifaceConAlt _ _ = False

-----------------
eq_ifaceNote :: EqEnv -> IfaceNote -> IfaceNote -> IfaceEq
eq_ifaceNote env (IfaceSCC c1)    (IfaceSCC c2)        = bool (c1==c2)
eq_ifaceNote env IfaceInlineMe    IfaceInlineMe        = Equal
eq_ifaceNote env (IfaceCoreNote s1) (IfaceCoreNote s2) = bool (s1==s2)
eq_ifaceNote env _ _ = NotEqual
\end{code}

\begin{code}
---------------------
eqIfType t1 t2 = eq_ifType emptyEqEnv t1 t2

-------------------
eq_ifType env (IfaceTyVar n1)         (IfaceTyVar n2)         = eqIfOcc env n1 n2
eq_ifType env (IfaceAppTy s1 t1)      (IfaceAppTy s2 t2)      = eq_ifType env s1 s2 &&& eq_ifType env t1 t2
eq_ifType env (IfacePredTy st1)       (IfacePredTy st2)       = eq_ifPredType env st1 st2
eq_ifType env (IfaceTyConApp tc1 ts1) (IfaceTyConApp tc2 ts2) = tc1 `eqIfTc` tc2 &&& eq_ifTypes env ts1 ts2
eq_ifType env (IfaceForAllTy tv1 t1)  (IfaceForAllTy tv2 t2)  = eq_ifTvBndr env tv1 tv2 (\env -> eq_ifType env t1 t2)
eq_ifType env (IfaceFunTy s1 t1)      (IfaceFunTy s2 t2)      = eq_ifType env s1 s2 &&& eq_ifType env t1 t2
eq_ifType env _ _ = NotEqual

-------------------
eq_ifTypes env = eqListBy (eq_ifType env)

-------------------
eq_ifContext env a b = eqListBy (eq_ifPredType env) a b

-------------------
eq_ifPredType env (IfaceClassP c1 tys1) (IfaceClassP c2 tys2) = c1 `eqIfExt` c2 &&&  eq_ifTypes env tys1 tys2
eq_ifPredType env (IfaceIParam n1 ty1) (IfaceIParam n2 ty2)   = bool (n1 == n2) &&& eq_ifType env ty1 ty2
eq_ifPredType env _ _ = NotEqual

-------------------
eqIfTc (IfaceTc tc1) (IfaceTc tc2) = tc1 `eqIfExt` tc2
eqIfTc IfaceIntTc    IfaceIntTc	   = Equal
eqIfTc IfaceCharTc   IfaceCharTc   = Equal
eqIfTc IfaceBoolTc   IfaceBoolTc   = Equal
eqIfTc IfaceListTc   IfaceListTc   = Equal
eqIfTc IfacePArrTc   IfacePArrTc   = Equal
eqIfTc (IfaceTupTc bx1 ar1) (IfaceTupTc bx2 ar2) = bool (bx1==bx2 && ar1==ar2)
eqIfTc IfaceLiftedTypeKindTc   IfaceLiftedTypeKindTc   = Equal
eqIfTc IfaceOpenTypeKindTc     IfaceOpenTypeKindTc     = Equal
eqIfTc IfaceUnliftedTypeKindTc IfaceUnliftedTypeKindTc = Equal
eqIfTc IfaceUbxTupleKindTc     IfaceUbxTupleKindTc     = Equal
eqIfTc IfaceArgTypeKindTc      IfaceArgTypeKindTc      = Equal
eqIfTc _		       _		       = NotEqual
\end{code}

-----------------------------------------------------------
	Support code for equality checking
-----------------------------------------------------------

\begin{code}
------------------------------------
type EqEnv = UniqFM FastString	-- Tracks the mapping from L-variables to R-variables

eqIfOcc :: EqEnv -> FastString -> FastString -> IfaceEq
eqIfOcc env n1 n2 = case lookupUFM env n1 of
			Just n1 -> bool (n1 == n2)
			Nothing -> bool (n1 == n2)

extendEqEnv :: EqEnv -> FastString -> FastString -> EqEnv
extendEqEnv env n1 n2 | n1 == n2  = env
		      | otherwise = addToUFM env n1 n2

emptyEqEnv :: EqEnv
emptyEqEnv = emptyUFM

------------------------------------
type ExtEnv bndr = EqEnv -> bndr -> bndr -> (EqEnv -> IfaceEq) -> IfaceEq

eq_ifNakedBndr :: ExtEnv FastString
eq_ifBndr      :: ExtEnv IfaceBndr
eq_ifTvBndr    :: ExtEnv IfaceTvBndr
eq_ifIdBndr    :: ExtEnv IfaceIdBndr

eq_ifNakedBndr env n1 n2 k = k (extendEqEnv env n1 n2)

eq_ifBndr env (IfaceIdBndr b1) (IfaceIdBndr b2) k = eq_ifIdBndr env b1 b2 k
eq_ifBndr env (IfaceTvBndr b1) (IfaceTvBndr b2) k = eq_ifTvBndr env b1 b2 k
eq_ifBndr _ _ _ _ = NotEqual

eq_ifTvBndr env (v1, k1) (v2, k2) k = eq_ifType env k1 k2 &&& k (extendEqEnv env v1 v2)
eq_ifIdBndr env (v1, t1) (v2, t2) k = eq_ifType env t1 t2 &&& k (extendEqEnv env v1 v2)

eq_ifBndrs   	:: ExtEnv [IfaceBndr]
eq_ifIdBndrs 	:: ExtEnv [IfaceIdBndr]
eq_ifTvBndrs 	:: ExtEnv [IfaceTvBndr]
eq_ifNakedBndrs :: ExtEnv [FastString]
eq_ifBndrs   	= eq_bndrs_with eq_ifBndr
eq_ifIdBndrs 	= eq_bndrs_with eq_ifIdBndr
eq_ifTvBndrs 	= eq_bndrs_with eq_ifTvBndr
eq_ifNakedBndrs = eq_bndrs_with eq_ifNakedBndr

eq_bndrs_with eq env []       []       k = k env
eq_bndrs_with eq env (b1:bs1) (b2:bs2) k = eq env b1 b2 (\env -> eq_bndrs_with eq env bs1 bs2 k)
eq_bndrs_with eq env _	      _	       _ = NotEqual
\end{code}

\begin{code}
eqListBy :: (a->a->IfaceEq) -> [a] -> [a] -> IfaceEq
eqListBy eq []     []     = Equal
eqListBy eq (x:xs) (y:ys) = eq x y &&& eqListBy eq xs ys
eqListBy eq xs     ys     = NotEqual

eqMaybeBy :: (a->a->IfaceEq) -> Maybe a -> Maybe a -> IfaceEq
eqMaybeBy eq Nothing Nothing   = Equal
eqMaybeBy eq (Just x) (Just y) = eq x y
eqMaybeBy eq x        y        = NotEqual
\end{code}
