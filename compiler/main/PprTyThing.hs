-----------------------------------------------------------------------------
--
-- Pretty-printing TyThings
--
-- (c) The GHC Team 2005
--
-----------------------------------------------------------------------------

module PprTyThing (
	PrintExplicitForalls,
	pprTyThing,
	pprTyThingInContext, pprTyThingParent_maybe,
	pprTyThingLoc,
	pprTyThingInContextLoc,
	pprTyThingHdr,
  	pprTypeForUser
  ) where

import qualified GHC

import GHC ( TyThing(..) )
import DataCon
import Id
import IdInfo
import TyCon
import Coercion( pprCoAxiom )
import TcType
import Name
import Outputable
import FastString

-- -----------------------------------------------------------------------------
-- Pretty-printing entities that we get from the GHC API

-- This should be a good source of sample code for using the GHC API to
-- inspect source code entities.

type PrintExplicitForalls = Bool

type ShowMe = Name -> Bool
-- The ShowMe function says which sub-components to print
--   True  <=> print
--   False <=> elide to "..."

----------------------------
-- | Pretty-prints a 'TyThing' with its defining location.
pprTyThingLoc :: PrintExplicitForalls -> TyThing -> SDoc
pprTyThingLoc pefas tyThing
  = showWithLoc loc (pprTyThing pefas tyThing)
  where loc = pprNameLoc (GHC.getName tyThing)

-- | Pretty-prints a 'TyThing'.
pprTyThing :: PrintExplicitForalls -> TyThing -> SDoc
pprTyThing pefas thing = ppr_ty_thing pefas (const True) thing

ppr_ty_thing :: PrintExplicitForalls -> ShowMe -> TyThing -> SDoc
ppr_ty_thing pefas _ 	 (AnId id)          = pprId         pefas id
ppr_ty_thing pefas _ 	 (ADataCon dataCon) = pprDataConSig pefas dataCon
ppr_ty_thing pefas show_me (ATyCon tyCon)   = pprTyCon      pefas show_me tyCon
ppr_ty_thing _     _       (ACoAxiom ax)    = pprCoAxiom    ax
ppr_ty_thing pefas show_me (AClass cls)     = pprClass      pefas show_me cls

-- | Pretty-prints a 'TyThing' in context: that is, if the entity
-- is a data constructor, record selector, or class method, then
-- the entity's parent declaration is pretty-printed with irrelevant
-- parts omitted.
pprTyThingInContext :: PrintExplicitForalls -> TyThing -> SDoc
pprTyThingInContext pefas thing
  | Just parent <- pprTyThingParent_maybe thing
  = ppr_ty_thing pefas (== GHC.getName thing) parent
  | otherwise
  = pprTyThing pefas thing

-- | Like 'pprTyThingInContext', but adds the defining location.
pprTyThingInContextLoc :: PrintExplicitForalls -> TyThing -> SDoc
pprTyThingInContextLoc pefas tyThing
  = showWithLoc (pprNameLoc (GHC.getName tyThing))
                (pprTyThingInContext pefas tyThing)

pprTyThingParent_maybe :: TyThing -> Maybe TyThing
-- (pprTyThingParent_maybe x) returns (Just p)
-- when pprTyThingInContext sould print a declaration for p
-- (albeit with some "..." in it) when asked to show x
pprTyThingParent_maybe (ADataCon dc) = Just (ATyCon (dataConTyCon dc))
pprTyThingParent_maybe (AnId id)     = case idDetails id of
      				      	 RecSelId { sel_tycon = tc } -> Just (ATyCon tc)
      				      	 ClassOpId cls               -> Just (AClass cls)
                                      	 _other                      -> Nothing
pprTyThingParent_maybe _other = Nothing

-- | Pretty-prints the 'TyThing' header. For functions and data constructors
-- the function is equivalent to 'pprTyThing' but for type constructors
-- and classes it prints only the header part of the declaration.
pprTyThingHdr :: PrintExplicitForalls -> TyThing -> SDoc
pprTyThingHdr pefas (AnId id)          = pprId         pefas id
pprTyThingHdr pefas (ADataCon dataCon) = pprDataConSig pefas dataCon
pprTyThingHdr pefas (ATyCon tyCon)     = pprTyConHdr   pefas tyCon
pprTyThingHdr _     (ACoAxiom ax)      = pprCoAxiom ax
pprTyThingHdr pefas (AClass cls)       = pprClassHdr   pefas cls

pprTyConHdr :: PrintExplicitForalls -> TyCon -> SDoc
pprTyConHdr _ tyCon
  | Just (_fam_tc, tys) <- tyConFamInst_maybe tyCon
  = ptext keyword <+> ptext (sLit "instance") <+> pprTypeApp tyCon tys
  | otherwise
  = ptext keyword <+> opt_family <+> opt_stupid <+> ppr_bndr tyCon <+> hsep (map ppr vars)
  where
    vars | GHC.isPrimTyCon tyCon ||
	   GHC.isFunTyCon tyCon = take (GHC.tyConArity tyCon) GHC.alphaTyVars
	 | otherwise = GHC.tyConTyVars tyCon

    keyword | GHC.isSynTyCon tyCon = sLit "type"
            | GHC.isNewTyCon tyCon = sLit "newtype"
            | otherwise            = sLit "data"

    opt_family
      | GHC.isFamilyTyCon tyCon = ptext (sLit "family")
      | otherwise             = empty

    opt_stupid 	-- The "stupid theta" part of the declaration
	| isAlgTyCon tyCon = GHC.pprThetaArrowTy (tyConStupidTheta tyCon)
	| otherwise	   = empty	-- Returns 'empty' if null theta

pprDataConSig :: PrintExplicitForalls -> GHC.DataCon -> SDoc
pprDataConSig pefas dataCon
  = ppr_bndr dataCon <+> dcolon <+> pprTypeForUser pefas (GHC.dataConType dataCon)

pprClassHdr :: PrintExplicitForalls -> GHC.Class -> SDoc
pprClassHdr _ cls
  = ptext (sLit "class") <+>
    GHC.pprThetaArrowTy (GHC.classSCTheta cls) <+>
    ppr_bndr cls <+>
    hsep (map ppr tyVars) <+>
    GHC.pprFundeps funDeps
  where
     (tyVars, funDeps) = GHC.classTvsFds cls

pprId :: PrintExplicitForalls -> Var -> SDoc
pprId pefas ident
  = hang (ppr_bndr ident <+> dcolon)
	 2 (pprTypeForUser pefas (GHC.idType ident))

pprTypeForUser :: PrintExplicitForalls -> GHC.Type -> SDoc
-- We do two things here.
-- a) We tidy the type, regardless
-- b) If PrintExplicitForAlls is True, we discard the foralls
-- 	but we do so `deeply'
-- Prime example: a class op might have type
--	forall a. C a => forall b. Ord b => stuff
-- Then we want to display
--	(C a, Ord b) => stuff
pprTypeForUser print_foralls ty
  | print_foralls = ppr tidy_ty
  | otherwise     = ppr (mkPhiTy ctxt ty')
  where
    tidy_ty     = tidyTopType ty
    (_, ctxt, ty') = tcSplitSigmaTy tidy_ty

pprTyCon :: PrintExplicitForalls -> ShowMe -> TyCon -> SDoc
pprTyCon pefas show_me tyCon
  | GHC.isSynTyCon tyCon
  = if GHC.isFamilyTyCon tyCon
    then pprTyConHdr pefas tyCon <+> dcolon <+> 
	 pprTypeForUser pefas (GHC.synTyConResKind tyCon)
    else
      let rhs_type = GHC.synTyConType tyCon
      in hang (pprTyConHdr pefas tyCon <+> equals) 2 (pprTypeForUser pefas rhs_type)
  | otherwise
  = pprAlgTyCon pefas show_me tyCon

pprAlgTyCon :: PrintExplicitForalls -> ShowMe -> TyCon -> SDoc
pprAlgTyCon pefas show_me tyCon
  | gadt      = pprTyConHdr pefas tyCon <+> ptext (sLit "where") $$
		   nest 2 (vcat (ppr_trim show_con datacons))
  | otherwise = hang (pprTyConHdr pefas tyCon)
    		   2 (add_bars (ppr_trim show_con datacons))
  where
    datacons = GHC.tyConDataCons tyCon
    gadt = any (not . GHC.isVanillaDataCon) datacons

    ok_con dc = show_me (dataConName dc) || any show_me (dataConFieldLabels dc)
    show_con dc
      | ok_con dc = Just (pprDataConDecl pefas show_me gadt dc)
      | otherwise = Nothing

pprDataConDecl :: PrintExplicitForalls -> ShowMe -> Bool -> GHC.DataCon -> SDoc
pprDataConDecl pefas show_me gadt_style dataCon
  | not gadt_style = ppr_fields tys_w_strs
  | otherwise      = ppr_bndr dataCon <+> dcolon <+>
			sep [ pp_foralls, GHC.pprThetaArrowTy theta, pp_tau ]
	-- Printing out the dataCon as a type signature, in GADT style
  where
    (forall_tvs, theta, tau) = tcSplitSigmaTy (GHC.dataConUserType dataCon)
    (arg_tys, res_ty)        = tcSplitFunTys tau
    labels     = GHC.dataConFieldLabels dataCon
    stricts    = GHC.dataConStrictMarks dataCon
    tys_w_strs = zip stricts arg_tys
    pp_foralls | pefas     = GHC.pprForAll forall_tvs
               | otherwise = empty

    pp_tau = foldr add (ppr res_ty) tys_w_strs
    add str_ty pp_ty = pprParendBangTy str_ty <+> arrow <+> pp_ty

    pprParendBangTy (bang,ty) = ppr bang <> GHC.pprParendType ty

    pprBangTy bang ty = ppr bang <> ppr ty

    maybe_show_label (lbl,(strict,tp))
	| show_me lbl = Just (ppr lbl <+> dcolon <+> pprBangTy strict tp)
	| otherwise   = Nothing

    ppr_fields [ty1, ty2]
	| GHC.dataConIsInfix dataCon && null labels
	= sep [pprParendBangTy ty1, pprInfixName dataCon, pprParendBangTy ty2]
    ppr_fields fields
	| null labels
	= ppr_bndr dataCon <+> sep (map pprParendBangTy fields)
	| otherwise
	= ppr_bndr dataCon <+>
		braces (sep (punctuate comma (ppr_trim maybe_show_label
					(zip labels fields))))

pprClass :: PrintExplicitForalls -> ShowMe -> GHC.Class -> SDoc
pprClass pefas show_me cls
  | null methods
  = pprClassHdr pefas cls
  | otherwise
  = hang (pprClassHdr pefas cls <+> ptext (sLit "where"))
       2 (vcat (ppr_trim show_meth methods))
  where
    methods = GHC.classMethods cls
    show_meth id | show_me (idName id) = Just (pprClassMethod pefas id)
	         | otherwise           = Nothing

pprClassMethod :: PrintExplicitForalls -> Id -> SDoc
pprClassMethod pefas id
  = hang (ppr_bndr id <+> dcolon) 2 (pprTypeForUser pefas op_ty)
  where
  -- Here's the magic incantation to strip off the dictionary
  -- from the class op type.  Stolen from IfaceSyn.tyThingToIfaceDecl.
  --
  -- It's important to tidy it *before* splitting it up, so that if
  -- we have	class C a b where
  --	          op :: forall a. a -> b
  -- then the inner forall on op gets renamed to a1, and we print
  -- (when dropping foralls)
  --		class C a b where
  --		  op :: a1 -> b

  tidy_sel_ty = tidyTopType (GHC.idType id)
  (_sel_tyvars, rho_ty) = GHC.splitForAllTys tidy_sel_ty
  op_ty = GHC.funResultTy rho_ty

ppr_trim :: (a -> Maybe SDoc) -> [a] -> [SDoc]
ppr_trim show xs
  = snd (foldr go (False, []) xs)
  where
    go x (eliding, so_far)
	| Just doc <- show x = (False, doc : so_far)
	| otherwise = if eliding then (True, so_far)
		                 else (True, ptext (sLit "...") : so_far)

add_bars :: [SDoc] -> SDoc
add_bars []      = empty
add_bars [c]     = equals <+> c
add_bars (c:cs)  = sep ((equals <+> c) : map (char '|' <+>) cs)

-- Wrap operators in ()
ppr_bndr :: GHC.NamedThing a => a -> SDoc
ppr_bndr a = GHC.pprParenSymName a

showWithLoc :: SDoc -> SDoc -> SDoc
showWithLoc loc doc
    = hang doc 2 (char '\t' <> comment <+> loc)
		-- The tab tries to make them line up a bit
  where
    comment = ptext (sLit "--")

