-----------------------------------------------------------------------------
--
-- Pretty-printing TyThings
--
-- (c) The GHC Team 2005
--
-----------------------------------------------------------------------------

{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module PprTyThing (
	PrintExplicitForalls,
	pprTyThing,
	pprTyThingInContext, 
	pprTyThingLoc,
	pprTyThingInContextLoc,
	pprTyThingHdr,
  	pprTypeForUser
  ) where

import qualified GHC

import GHC ( TyThing(..) )
import DataCon
import Id
import TyCon
import Coercion( pprCoAxiom )
import HscTypes( tyThingParent_maybe )
import TcType
import Name
import StaticFlags( opt_PprStyle_Debug )
import Outputable
import FastString

-- -----------------------------------------------------------------------------
-- Pretty-printing entities that we get from the GHC API

-- This should be a good source of sample code for using the GHC API to
-- inspect source code entities.

type PrintExplicitForalls = Bool

type ShowSub = [Name]
--   []     <=> print all sub-components of the current thing
--   (n:ns) <=> print sub-component 'n' with ShowSub=ns
--              elide other sub-components to "..."
showAll :: ShowSub
showAll = []

showSub :: NamedThing n => ShowSub -> n -> Bool
showSub []    _     = True
showSub (n:_) thing = n == getName thing

showSub_maybe :: NamedThing n => ShowSub -> n -> Maybe ShowSub
showSub_maybe []     _     = Just []
showSub_maybe (n:ns) thing = if n == getName thing then Just ns
                                                   else Nothing

----------------------------
-- | Pretty-prints a 'TyThing' with its defining location.
pprTyThingLoc :: PrintExplicitForalls -> TyThing -> SDoc
pprTyThingLoc pefas tyThing
  = showWithLoc (pprDefinedAt (GHC.getName tyThing)) (pprTyThing pefas tyThing)

-- | Pretty-prints a 'TyThing'.
pprTyThing :: PrintExplicitForalls -> TyThing -> SDoc
pprTyThing pefas thing = ppr_ty_thing pefas showAll thing

-- | Pretty-prints a 'TyThing' in context: that is, if the entity
-- is a data constructor, record selector, or class method, then
-- the entity's parent declaration is pretty-printed with irrelevant
-- parts omitted.
pprTyThingInContext :: PrintExplicitForalls -> TyThing -> SDoc
pprTyThingInContext pefas thing
  = go [] thing
  where
    go ss thing = case tyThingParent_maybe thing of
                    Just parent -> go (getName thing : ss) parent
                    Nothing     -> ppr_ty_thing pefas ss thing

-- | Like 'pprTyThingInContext', but adds the defining location.
pprTyThingInContextLoc :: PrintExplicitForalls -> TyThing -> SDoc
pprTyThingInContextLoc pefas tyThing
  = showWithLoc (pprDefinedAt (GHC.getName tyThing))
                (pprTyThingInContext pefas tyThing)

-- | Pretty-prints the 'TyThing' header. For functions and data constructors
-- the function is equivalent to 'pprTyThing' but for type constructors
-- and classes it prints only the header part of the declaration.
pprTyThingHdr :: PrintExplicitForalls -> TyThing -> SDoc
pprTyThingHdr pefas (AnId id)          = pprId         pefas id
pprTyThingHdr pefas (ADataCon dataCon) = pprDataConSig pefas dataCon
pprTyThingHdr pefas (ATyCon tyCon)     = pprTyConHdr   pefas tyCon
pprTyThingHdr _     (ACoAxiom ax)      = pprCoAxiom ax

------------------------
ppr_ty_thing :: PrintExplicitForalls -> ShowSub -> TyThing -> SDoc
ppr_ty_thing pefas _  (AnId id)          = pprId         pefas id
ppr_ty_thing pefas _  (ADataCon dataCon) = pprDataConSig pefas dataCon
ppr_ty_thing pefas ss (ATyCon tyCon)   	 = pprTyCon      pefas ss tyCon
ppr_ty_thing _     _  (ACoAxiom ax)    	 = pprCoAxiom    ax
pprTyConHdr :: PrintExplicitForalls -> TyCon -> SDoc
pprTyConHdr pefas tyCon
  | Just (fam_tc, tys) <- tyConFamInst_maybe tyCon
  = ptext keyword <+> ptext (sLit "instance") <+> pprTypeApp fam_tc tys
  | Just cls <- tyConClass_maybe tyCon
  = pprClassHdr pefas cls
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

pprTyCon :: PrintExplicitForalls -> ShowSub -> TyCon -> SDoc
pprTyCon pefas ss tyCon
  | Just syn_rhs <- GHC.synTyConRhs_maybe tyCon
  = case syn_rhs of
      SynFamilyTyCon {} -> pprTyConHdr pefas tyCon <+> dcolon <+> 
                           pprTypeForUser pefas (GHC.synTyConResKind tyCon)
      SynonymTyCon rhs_ty -> hang (pprTyConHdr pefas tyCon <+> equals) 
                                2 (pprTypeForUser pefas rhs_ty)

  | Just cls <- GHC.tyConClass_maybe tyCon
  = pprClass pefas ss cls
  | otherwise
  = pprAlgTyCon pefas ss tyCon

pprAlgTyCon :: PrintExplicitForalls -> ShowSub -> TyCon -> SDoc
pprAlgTyCon pefas ss tyCon
  | gadt      = pprTyConHdr pefas tyCon <+> ptext (sLit "where") $$
		   nest 2 (vcat (ppr_trim (map show_con datacons)))
  | otherwise = hang (pprTyConHdr pefas tyCon)
    		   2 (add_bars (ppr_trim (map show_con datacons)))
  where
    datacons = GHC.tyConDataCons tyCon
    gadt = any (not . GHC.isVanillaDataCon) datacons

    ok_con dc = showSub ss dc || any (showSub ss) (dataConFieldLabels dc)
    show_con dc
      | ok_con dc = Just (pprDataConDecl pefas ss gadt dc)
      | otherwise = Nothing

pprDataConDecl :: PrintExplicitForalls -> ShowSub -> Bool -> GHC.DataCon -> SDoc
pprDataConDecl pefas ss gadt_style dataCon
  | not gadt_style = ppr_fields tys_w_strs
  | otherwise      = ppr_bndr dataCon <+> dcolon <+>
			sep [ pp_foralls, GHC.pprThetaArrowTy theta, pp_tau ]
	-- Printing out the dataCon as a type signature, in GADT style
  where
    (forall_tvs, theta, tau) = tcSplitSigmaTy (GHC.dataConUserType dataCon)
    (arg_tys, res_ty)        = tcSplitFunTys tau
    labels     = GHC.dataConFieldLabels dataCon
    stricts    = GHC.dataConStrictMarks dataCon
    tys_w_strs = zip (map user_ify stricts) arg_tys
    pp_foralls | pefas     = GHC.pprForAll forall_tvs
               | otherwise = empty

    pp_tau = foldr add (ppr res_ty) tys_w_strs
    add str_ty pp_ty = pprParendBangTy str_ty <+> arrow <+> pp_ty

    pprParendBangTy (bang,ty) = ppr bang <> GHC.pprParendType ty
    pprBangTy       (bang,ty) = ppr bang <> ppr ty

    -- See Note [Printing bangs on data constructors]
    user_ify :: HsBang -> HsBang
    user_ify bang | opt_PprStyle_Debug = bang
    user_ify HsStrict                  = HsBang False
    user_ify (HsUnpack {})             = HsBang True
    user_ify bang                      = bang

    maybe_show_label (lbl,bty)
	| showSub ss lbl = Just (ppr lbl <+> dcolon <+> pprBangTy bty)
	| otherwise      = Nothing

    ppr_fields [ty1, ty2]
	| GHC.dataConIsInfix dataCon && null labels
	= sep [pprParendBangTy ty1, pprInfixName dataCon, pprParendBangTy ty2]
    ppr_fields fields
	| null labels
	= ppr_bndr dataCon <+> sep (map pprParendBangTy fields)
	| otherwise
	= ppr_bndr dataCon
	  <+> (braces $ sep $ punctuate comma $ ppr_trim $
               map maybe_show_label (zip labels fields))

pprClass :: PrintExplicitForalls -> ShowSub -> GHC.Class -> SDoc
pprClass pefas ss cls
  | null methods && null assoc_ts
  = pprClassHdr pefas cls
  | otherwise
  = vcat [ pprClassHdr pefas cls <+> ptext (sLit "where")
         , nest 2 (vcat $ ppr_trim $ 
                   map show_at assoc_ts ++ map show_meth methods)]
  where
    methods  = GHC.classMethods cls
    assoc_ts = GHC.classATs cls
    show_meth id | showSub ss id  = Just (pprClassMethod pefas id)
	         | otherwise      = Nothing
    show_at tc = case showSub_maybe ss tc of
                      Just ss' -> Just (pprTyCon pefas ss' tc)
                      Nothing  -> Nothing

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

ppr_trim :: [Maybe SDoc] -> [SDoc]
-- Collapse a group of Nothings to a single "..."
ppr_trim xs
  = snd (foldr go (False, []) xs)
  where
    go (Just doc) (_,     so_far) = (False, doc : so_far)
    go Nothing    (True,  so_far) = (True, so_far)
    go Nothing    (False, so_far) = (True, ptext (sLit "...") : so_far)

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

{- 
Note [Printing bangs on data constructors] 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For imported data constructors the dataConStrictMarks are the
representation choices (see Note [Bangs on data constructor arguments]
in DataCon.lhs). So we have to fiddle a little bit here to turn them
back into user-printable form.
-}
