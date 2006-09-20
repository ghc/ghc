-----------------------------------------------------------------------------
--
-- Pretty-printing TyThings
--
-- (c) The GHC Team 2005
--
-----------------------------------------------------------------------------

module PprTyThing (
	pprTyThing,
	pprTyThingInContext,
	pprTyThingLoc,
	pprTyThingInContextLoc,
	pprTyThingHdr
  ) where

#include "HsVersions.h"

import qualified GHC

import GHC ( TyThing(..), SrcLoc )
import DataCon ( dataConResTys )
import Outputable

-- -----------------------------------------------------------------------------
-- Pretty-printing entities that we get from the GHC API

-- This should be a good source of sample code for using the GHC API to
-- inspect source code entities.

-- | Pretty-prints a 'TyThing' with its defining location.
pprTyThingLoc :: Bool -> TyThing -> SDoc
pprTyThingLoc exts tyThing 
  = showWithLoc loc (pprTyThing exts tyThing)
  where loc = GHC.nameSrcLoc (GHC.getName tyThing)

-- | Pretty-prints a 'TyThing'.
pprTyThing :: Bool -> TyThing -> SDoc
pprTyThing exts (AnId id)          = pprId         exts id
pprTyThing exts (ADataCon dataCon) = pprDataConSig exts dataCon
pprTyThing exts (ATyCon tyCon)     = pprTyCon      exts tyCon
pprTyThing exts (AClass cls)       = pprClass      exts cls
        
-- | Like 'pprTyThingInContext', but adds the defining location.
pprTyThingInContextLoc :: Bool -> TyThing -> SDoc
pprTyThingInContextLoc exts tyThing 
  = showWithLoc loc (pprTyThingInContext exts tyThing)
  where loc = GHC.nameSrcLoc (GHC.getName tyThing)

-- | Pretty-prints a 'TyThing' in context: that is, if the entity
-- is a data constructor, record selector, or class method, then 
-- the entity's parent declaration is pretty-printed with irrelevant
-- parts omitted.
pprTyThingInContext :: Bool -> TyThing -> SDoc
pprTyThingInContext exts (AnId id)          = pprIdInContext exts id
pprTyThingInContext exts (ADataCon dataCon) = pprDataCon exts dataCon
pprTyThingInContext exts (ATyCon tyCon)     = pprTyCon   exts tyCon
pprTyThingInContext exts (AClass cls)       = pprClass   exts cls

-- | Pretty-prints the 'TyThing' header. For functions and data constructors
-- the function is equivalent to 'pprTyThing' but for type constructors
-- and classes it prints only the header part of the declaration.
pprTyThingHdr :: Bool -> TyThing -> SDoc
pprTyThingHdr exts (AnId id)          = pprId         exts id
pprTyThingHdr exts (ADataCon dataCon) = pprDataConSig exts dataCon
pprTyThingHdr exts (ATyCon tyCon)     = pprTyConHdr   exts tyCon
pprTyThingHdr exts (AClass cls)       = pprClassHdr   exts cls
        
pprTyConHdr exts tyCon =
  addFamily (ptext keyword) <+> ppr_bndr tyCon <+> hsep (map ppr vars)
  where
    vars | GHC.isPrimTyCon tyCon || 
	   GHC.isFunTyCon tyCon = take (GHC.tyConArity tyCon) GHC.alphaTyVars
	 | otherwise = GHC.tyConTyVars tyCon

    keyword | GHC.isSynTyCon tyCon = SLIT("type")
            | GHC.isNewTyCon tyCon = SLIT("newtype")
            | otherwise            = SLIT("data")

    addFamily keytext 
      | GHC.isOpenTyCon tyCon = keytext <> ptext SLIT(" family")
      | otherwise             = keytext

pprDataConSig exts dataCon =
  ppr_bndr dataCon <+> dcolon <+> pprType exts (GHC.dataConType dataCon)

pprClassHdr exts cls =
  let (tyVars, funDeps) = GHC.classTvsFds cls
  in ptext SLIT("class") <+> 
     GHC.pprThetaArrow (GHC.classSCTheta cls) <+>
     ppr_bndr cls <+>
     hsep (map ppr tyVars) <+>
     GHC.pprFundeps funDeps

pprIdInContext exts id
  | GHC.isRecordSelector id  		  = pprRecordSelector exts id
  | Just cls <- GHC.isClassOpId_maybe id  = pprClassOneMethod exts cls id
  | otherwise				  = pprId exts id

pprRecordSelector exts id
  = pprAlgTyCon exts tyCon show_con show_label
  where
	(tyCon,label) = GHC.recordSelectorFieldLabel id
	show_con dataCon  = label `elem` GHC.dataConFieldLabels dataCon
	show_label label' = label == label'

pprId exts id
  = hang (ppr_bndr id <+> dcolon) 2 
	(pprType exts (GHC.idType id))

pprType True  ty = ppr ty
pprType False ty = ppr (GHC.dropForAlls ty)

pprTyCon exts tyCon
  | GHC.isSynTyCon tyCon
  = if GHC.isOpenTyCon tyCon
    then pprTyConHdr exts tyCon <+> dcolon <+> 
	 pprType exts (GHC.synTyConResKind tyCon)
    else 
      let rhs_type = GHC.synTyConType tyCon
      in hang (pprTyConHdr exts tyCon <+> equals) 2 (pprType exts rhs_type)
  | otherwise
  = pprAlgTyCon exts tyCon (const True) (const True)

pprAlgTyCon exts tyCon ok_con ok_label
  | gadt      = pprTyConHdr exts tyCon <+> ptext SLIT("where") $$ 
		   nest 2 (vcat (ppr_trim show_con datacons))
  | otherwise = hang (pprTyConHdr exts tyCon)
    		   2 (add_bars (ppr_trim show_con datacons))
  where
    datacons = GHC.tyConDataCons tyCon
    gadt = any (not . GHC.isVanillaDataCon) datacons

    show_con dataCon
      | ok_con dataCon = Just (pprDataConDecl exts gadt ok_label dataCon)
      | otherwise      = Nothing

pprDataCon exts dataCon = pprAlgTyCon exts tyCon (== dataCon) (const True)
  where tyCon = GHC.dataConTyCon dataCon

pprDataConDecl exts gadt_style show_label dataCon
 = error "kevind stub"
{-
  | not gadt_style = ppr_fields tys_w_strs
  | otherwise      = ppr_bndr dataCon <+> dcolon <+> 
			sep [ ppr_tvs, GHC.pprThetaArrow theta, pp_tau ]
  where
    (tyvars, theta, argTypes, tyCon) = GHC.dataConSig dataCon
    labels = GHC.dataConFieldLabels dataCon
    res_tys = dataConResTys dataCon
    qualVars = filter (flip notElem (GHC.tyConTyVars tyCon)) tyvars
    stricts = GHC.dataConStrictMarks dataCon
    tys_w_strs = zip stricts argTypes

    ppr_tvs 
	| null qualVars = empty
	| otherwise     = ptext SLIT("forall") <+> 
				hsep (map ppr qualVars) <> dot

	-- printing out the dataCon as a type signature, in GADT style
    pp_tau = foldr add pp_res_ty tys_w_strs
    pp_res_ty = ppr_bndr tyCon <+> hsep (map GHC.pprParendType res_tys)
    add (str,ty) pp_ty = pprBangTy str ty <+> arrow <+> pp_ty

    pprParendBangTy (strict,ty)
	| GHC.isMarkedStrict strict = char '!' <> GHC.pprParendType ty
	| otherwise		    = GHC.pprParendType ty

    pprBangTy strict ty
	| GHC.isMarkedStrict strict = char '!' <> ppr ty
	| otherwise		    = ppr ty

    maybe_show_label (lbl,(strict,tp))
	| show_label lbl = Just (ppr lbl <+> dcolon <+> pprBangTy strict tp)
	| otherwise      = Nothing

    ppr_fields [ty1, ty2]
	| GHC.dataConIsInfix dataCon && null labels
	= sep [pprParendBangTy ty1, ppr dataCon, pprParendBangTy ty2]
    ppr_fields fields
	| null labels
	= ppr_bndr dataCon <+> sep (map pprParendBangTy fields)
	| otherwise
	= ppr_bndr dataCon <+> 
		braces (sep (punctuate comma (ppr_trim maybe_show_label 
					(zip labels fields))))
-}
pprClass exts cls
  | null methods = 
	pprClassHdr exts cls
  | otherwise = 
	hang (pprClassHdr exts cls <+> ptext SLIT("where"))
	    2 (vcat (map (pprClassMethod exts) methods))
  where
	methods = GHC.classMethods cls

pprClassOneMethod exts cls this_one = 
  hang (pprClassHdr exts cls <+> ptext SLIT("where"))
	2 (vcat (ppr_trim show_meth methods))
  where
	methods = GHC.classMethods cls
	show_meth id | id == this_one = Just (pprClassMethod exts id)
		     | otherwise      = Nothing

pprClassMethod exts id =
  hang (ppr_bndr id <+> dcolon) 2 (pprType exts (classOpType id))
  where
  -- Here's the magic incantation to strip off the dictionary
  -- from the class op type.  Stolen from IfaceSyn.tyThingToIfaceDecl.
  classOpType id = GHC.funResultTy rho_ty
     where (_sel_tyvars, rho_ty) = GHC.splitForAllTys (GHC.idType id)

ppr_trim :: (a -> Maybe SDoc) -> [a] -> [SDoc]
ppr_trim show xs
  = snd (foldr go (False, []) xs)
  where
    go x (eliding, so_far)
	| Just doc <- show x = (False, doc : so_far)
	| otherwise = if eliding then (True, so_far)
		                 else (True, ptext SLIT("...") : so_far)

add_bars []      = empty
add_bars [c]     = equals <+> c
add_bars (c:cs)  = sep ((equals <+> c) : map (char '|' <+>) cs)

-- Wrap operators in ()
ppr_bndr :: GHC.NamedThing a => a -> SDoc
ppr_bndr a = GHC.pprParenSymName a

showWithLoc :: SrcLoc -> SDoc -> SDoc
showWithLoc loc doc 
    = hang doc 2 (char '\t' <> comment <+> GHC.pprDefnLoc loc)
		-- The tab tries to make them line up a bit
  where
    comment = ptext SLIT("--")

