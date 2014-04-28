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
--     http://ghc.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module PprTyThing (
	pprTyThing,
	pprTyThingInContext,
	pprTyThingLoc,
	pprTyThingInContextLoc,
	pprTyThingHdr,
  	pprTypeForUser
  ) where

import TypeRep ( TyThing(..) )
import DataCon
import Id
import TyCon
import Class
import Coercion( pprCoAxBranch )
import CoAxiom( CoAxiom(..), brListMap )
import HscTypes( tyThingParent_maybe )
import Type( tidyTopType, tidyOpenType, splitForAllTys, funResultTy )
import Kind( synTyConResKind )
import TypeRep( pprTvBndrs, pprForAll, suppressKinds )
import TysPrim( alphaTyVars )
import MkIface ( tyThingToIfaceDecl )
import TcType
import Name
import VarEnv( emptyTidyEnv )
import StaticFlags( opt_PprStyle_Debug )
import DynFlags
import Outputable
import FastString

-- -----------------------------------------------------------------------------
-- Pretty-printing entities that we get from the GHC API

-- This should be a good source of sample code for using the GHC API to
-- inspect source code entities.

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
pprTyThingLoc :: TyThing -> SDoc
pprTyThingLoc tyThing
  = showWithLoc (pprDefinedAt (getName tyThing)) (pprTyThing tyThing)

-- | Pretty-prints a 'TyThing'.
pprTyThing :: TyThing -> SDoc
pprTyThing thing = ppr_ty_thing (Just showAll) thing

-- | Pretty-prints a 'TyThing' in context: that is, if the entity
-- is a data constructor, record selector, or class method, then
-- the entity's parent declaration is pretty-printed with irrelevant
-- parts omitted.
pprTyThingInContext :: TyThing -> SDoc
pprTyThingInContext thing
  = go [] thing
  where
    go ss thing = case tyThingParent_maybe thing of
                    Just parent -> go (getName thing : ss) parent
                    Nothing     -> ppr_ty_thing (Just ss) thing

-- | Like 'pprTyThingInContext', but adds the defining location.
pprTyThingInContextLoc :: TyThing -> SDoc
pprTyThingInContextLoc tyThing
  = showWithLoc (pprDefinedAt (getName tyThing))
                (pprTyThingInContext tyThing)

-- | Pretty-prints the 'TyThing' header. For functions and data constructors
-- the function is equivalent to 'pprTyThing' but for type constructors
-- and classes it prints only the header part of the declaration.
pprTyThingHdr :: TyThing -> SDoc
pprTyThingHdr = ppr_ty_thing Nothing

------------------------
-- NOTE: We pretty-print 'TyThing' via 'IfaceDecl' so that we can reuse the
-- 'TyCon' tidying happening in 'tyThingToIfaceDecl'. See #8776 for details.
ppr_ty_thing :: Maybe ShowSub -> TyThing -> SDoc
ppr_ty_thing mss tyThing = case tyThing of
    AnId id -> pprId id
    ATyCon tyCon -> case mss of
        Nothing -> pprTyConHdr tyCon
        Just ss -> pprTyCon ss tyCon
    _ -> ppr $ tyThingToIfaceDecl tyThing

pprTyConHdr :: TyCon -> SDoc
pprTyConHdr tyCon
  | Just (fam_tc, tys) <- tyConFamInst_maybe tyCon
  = ptext keyword <+> ptext (sLit "instance") <+> pprTypeApp fam_tc tys
  | Just cls <- tyConClass_maybe tyCon
  = pprClassHdr cls
  | otherwise
  = sdocWithDynFlags $ \dflags ->
    ptext keyword <+> opt_family <+> opt_stupid <+> ppr_bndr tyCon
    <+> pprTvBndrs (suppressKinds dflags (tyConKind tyCon) vars)
  where
    vars | isPrimTyCon tyCon ||
	   isFunTyCon tyCon = take (tyConArity tyCon) alphaTyVars
	 | otherwise = tyConTyVars tyCon

    keyword | isSynTyCon tyCon = sLit "type"
            | isNewTyCon tyCon = sLit "newtype"
            | otherwise            = sLit "data"

    opt_family
      | isFamilyTyCon tyCon = ptext (sLit "family")
      | otherwise             = empty

    opt_stupid 	-- The "stupid theta" part of the declaration
	| isAlgTyCon tyCon = pprThetaArrowTy (tyConStupidTheta tyCon)
	| otherwise	   = empty	-- Returns 'empty' if null theta

pprClassHdr :: Class -> SDoc
pprClassHdr cls
  = sdocWithDynFlags $ \dflags ->
    ptext (sLit "class") <+>
    sep [ pprThetaArrowTy (classSCTheta cls)
        , ppr_bndr cls
          <+> pprTvBndrs (suppressKinds dflags (tyConKind (classTyCon cls)) tvs)
        , pprFundeps funDeps ]
  where
     (tvs, funDeps) = classTvsFds cls

pprId :: Var -> SDoc
pprId ident
  = hang (ppr_bndr ident <+> dcolon)
	 2 (pprTypeForUser (idType ident))

pprTypeForUser :: Type -> SDoc
-- We do two things here.
-- a) We tidy the type, regardless
-- b) Swizzle the foralls to the top, so that without
--    -fprint-explicit-foralls we'll suppress all the foralls
-- Prime example: a class op might have type
--	forall a. C a => forall b. Ord b => stuff
-- Then we want to display
--	(C a, Ord b) => stuff
pprTypeForUser ty
  = pprSigmaType (mkSigmaTy tvs ctxt tau)
  where
    (tvs, ctxt, tau) = tcSplitSigmaTy tidy_ty
    (_, tidy_ty)   = tidyOpenType emptyTidyEnv ty
     -- Often the types/kinds we print in ghci are fully generalised
     -- and have no free variables, but it turns out that we sometimes
     -- print un-generalised kinds (eg when doing :k T), so it's
     -- better to use tidyOpenType here

pprTyCon :: ShowSub -> TyCon -> SDoc
pprTyCon ss tyCon
  | Just syn_rhs <- synTyConRhs_maybe tyCon
  = case syn_rhs of
      OpenSynFamilyTyCon    -> pp_tc_with_kind
      BuiltInSynFamTyCon {} -> pp_tc_with_kind

      ClosedSynFamilyTyCon (CoAxiom { co_ax_branches = branches })
         -> hang closed_family_header
              2  (vcat (brListMap (pprCoAxBranch tyCon) branches))

      AbstractClosedSynFamilyTyCon
         -> closed_family_header <+> ptext (sLit "..")

      SynonymTyCon rhs_ty
         -> hang (pprTyConHdr tyCon <+> equals)
               2 (ppr rhs_ty)   -- Don't suppress foralls on RHS type!

                                                 -- e.g. type T = forall a. a->a
  | Just cls <- tyConClass_maybe tyCon
  = (pp_roles (== Nominal)) $$ pprClass ss cls

  | otherwise
  = (pp_roles (== Representational)) $$ pprAlgTyCon ss tyCon

  where
      -- if, for each role, suppress_if role is True, then suppress the role
      -- output
    pp_roles :: (Role -> Bool) -> SDoc
    pp_roles suppress_if
      = sdocWithDynFlags $ \dflags ->
        let roles = suppressKinds dflags (tyConKind tyCon) (tyConRoles tyCon)
        in ppUnless (isFamInstTyCon tyCon || all suppress_if roles) $
             -- Don't display roles for data family instances (yet)
             -- See discussion on Trac #8672.
           ptext (sLit "type role") <+> ppr tyCon <+> hsep (map ppr roles)

    pp_tc_with_kind = vcat [ pp_roles (const True)
                           , pprTyConHdr tyCon <+> dcolon
                             <+> pprTypeForUser (synTyConResKind tyCon) ]
    closed_family_header
       = pp_tc_with_kind <+> ptext (sLit "where")

pprAlgTyCon :: ShowSub -> TyCon -> SDoc
pprAlgTyCon ss tyCon
  | gadt      = pprTyConHdr tyCon <+> ptext (sLit "where") $$
		   nest 2 (vcat (ppr_trim (map show_con datacons)))
  | otherwise = hang (pprTyConHdr tyCon)
    		   2 (add_bars (ppr_trim (map show_con datacons)))
  where
    datacons = tyConDataCons tyCon
    gadt = any (not . isVanillaDataCon) datacons

    ok_con dc = showSub ss dc || any (showSub ss) (dataConFieldLabels dc)
    show_con dc
      | ok_con dc = Just (pprDataConDecl ss gadt dc)
      | otherwise = Nothing

pprDataConDecl :: ShowSub -> Bool -> DataCon -> SDoc
pprDataConDecl ss gadt_style dataCon
  | not gadt_style = ppr_fields tys_w_strs
  | otherwise      = ppr_bndr dataCon <+> dcolon <+>
			sep [ pp_foralls, pprThetaArrowTy theta, pp_tau ]
	-- Printing out the dataCon as a type signature, in GADT style
  where
    (forall_tvs, theta, tau) = tcSplitSigmaTy (dataConUserType dataCon)
    (arg_tys, res_ty)        = tcSplitFunTys tau
    labels     = dataConFieldLabels dataCon
    stricts    = dataConStrictMarks dataCon
    tys_w_strs = zip (map user_ify stricts) arg_tys
    pp_foralls = sdocWithDynFlags $ \dflags ->
                 ppWhen (gopt Opt_PrintExplicitForalls dflags)
                        (pprForAll forall_tvs)

    pp_tau = foldr add (ppr res_ty) tys_w_strs
    add str_ty pp_ty = pprParendBangTy str_ty <+> arrow <+> pp_ty

    pprParendBangTy (bang,ty) = ppr bang <> pprParendType ty
    pprBangTy       (bang,ty) = ppr bang <> ppr ty

    -- See Note [Printing bangs on data constructors]
    user_ify :: HsBang -> HsBang
    user_ify bang | opt_PprStyle_Debug = bang
    user_ify HsStrict                  = HsUserBang Nothing     True
    user_ify (HsUnpack {})             = HsUserBang (Just True) True
    user_ify bang                      = bang

    maybe_show_label (lbl,bty)
	| showSub ss lbl = Just (ppr_bndr lbl <+> dcolon <+> pprBangTy bty)
	| otherwise      = Nothing

    ppr_fields [ty1, ty2]
	| dataConIsInfix dataCon && null labels
	= sep [pprParendBangTy ty1, pprInfixName dataCon, pprParendBangTy ty2]
    ppr_fields fields
	| null labels
	= ppr_bndr dataCon <+> sep (map pprParendBangTy fields)
	| otherwise
	= ppr_bndr dataCon
	  <+> (braces $ sep $ punctuate comma $ ppr_trim $
               map maybe_show_label (zip labels fields))

pprClass :: ShowSub -> Class -> SDoc
pprClass ss cls
  | null methods && null assoc_ts
  = pprClassHdr cls
  | otherwise
  = vcat [ pprClassHdr cls <+> ptext (sLit "where")
         , nest 2 (vcat $ ppr_trim $ 
                   map show_at assoc_ts ++ map show_meth methods)]
  where
    methods  = classMethods cls
    assoc_ts = classATs cls
    show_meth id | showSub ss id  = Just (pprClassMethod id)
	         | otherwise      = Nothing
    show_at tc = case showSub_maybe ss tc of
                      Just ss' -> Just (pprTyCon ss' tc)
                      Nothing  -> Nothing

pprClassMethod :: Id -> SDoc
pprClassMethod id
  = hang (ppr_bndr id <+> dcolon) 2 (pprTypeForUser op_ty)
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

  tidy_sel_ty = tidyTopType (idType id)
  (_sel_tyvars, rho_ty) = splitForAllTys tidy_sel_ty
  op_ty = funResultTy rho_ty

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
ppr_bndr :: NamedThing a => a -> SDoc
ppr_bndr a = parenSymOcc (getOccName a) (ppr (getName a))

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
