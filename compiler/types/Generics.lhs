%
% (c) The University of Glasgow 2011
%

\begin{code}

module Generics ( canDoGenerics,
		  mkBindsRep, tc_mkRepTyCon, mkBindsMetaD,
		  MetaTyCons(..), metaTyCons2TyCons
    ) where


import HsSyn
import Type
import TcType
import DataCon

import TyCon
import Name hiding (varName)
import Module (Module, moduleName, moduleNameString)
import IfaceEnv (newGlobalBinder)
import RdrName
import BasicTypes
import TysWiredIn
import PrelNames

-- For generation of representation types
import TcEnv (tcLookupTyCon)
import TcRnMonad
import HscTypes
import BuildTyCl

import SrcLoc
import Bag
import Outputable 
import FastString

#include "HsVersions.h"
\end{code}

%************************************************************************
%*									*
\subsection{Generating representation types}
%*									*
%************************************************************************

\begin{code}
canDoGenerics :: TyCon -> Maybe SDoc
-- Called on source-code data types, to see if we should generate
-- generic functions for them.
-- Nothing == yes
-- Just s  == no, because of `s`

canDoGenerics tycon
  =  mergeErrors (
          -- We do not support datatypes with context
              (if (not (null (tyConStupidTheta tycon)))
                then (Just (ppr tycon <+> text "must not have a datatype context"))
                else Nothing)
          -- We don't like type families
            : (if (isFamilyTyCon tycon)
                then (Just (ppr tycon <+> text "must not be a family instance"))
                else Nothing)
          -- See comment below
            : (map bad_con (tyConDataCons tycon)))
  where
        -- If any of the constructor has an unboxed type as argument,
        -- then we can't build the embedding-projection pair, because
        -- it relies on instantiating *polymorphic* sum and product types
        -- at the argument types of the constructors
    bad_con dc = if (any bad_arg_type (dataConOrigArgTys dc))
                  then (Just (ppr dc <+> text "must not have unlifted or polymorphic arguments"))
                  else (if (not (isVanillaDataCon dc))
                          then (Just (ppr dc <+> text "must be a vanilla data constructor"))
                          else Nothing)

	-- Nor can we do the job if it's an existential data constructor,
	-- Nor if the args are polymorphic types (I don't think)
    bad_arg_type ty = isUnLiftedType ty || not (isTauTy ty)
    
    mergeErrors :: [Maybe SDoc] -> Maybe SDoc
    mergeErrors []           = Nothing
    mergeErrors ((Just s):t) = case mergeErrors t of
                                 Nothing -> Just s
                                 Just s' -> Just (s <> text ", and" $$ s')
    mergeErrors (Nothing :t) = mergeErrors t
\end{code}

%************************************************************************
%*									*
\subsection{Generating the RHS of a generic default method}
%*									*
%************************************************************************

\begin{code}
type US = Int	-- Local unique supply, just a plain Int
type Alt = (LPat RdrName, LHsExpr RdrName)

-- Bindings for the Generic instance
mkBindsRep :: TyCon -> LHsBinds RdrName
mkBindsRep tycon = 
    unitBag (L loc (mkFunBind (L loc from_RDR) from_matches))
  `unionBags`
    unitBag (L loc (mkFunBind (L loc to_RDR) to_matches))
      where
        from_matches = [mkSimpleHsAlt pat rhs | (pat,rhs) <- from_alts]
        to_matches   = [mkSimpleHsAlt pat rhs | (pat,rhs) <- to_alts  ]
        loc           = srcLocSpan (getSrcLoc tycon)
        datacons      = tyConDataCons tycon

        -- Recurse over the sum first
        from_alts, to_alts :: [Alt]
        (from_alts, to_alts) = mkSum (1 :: US) tycon datacons
        
--------------------------------------------------------------------------------
-- The type instance synonym and synonym
--       type instance Rep (D a b) = Rep_D a b
--       type Rep_D a b = ...representation type for D ...
--------------------------------------------------------------------------------

tc_mkRepTyCon :: TyCon            -- The type to generate representation for
               -> MetaTyCons      -- Metadata datatypes to refer to
               -> Module          -- JPM TODO
               -> TcM TyCon       -- Generated representation0 type
tc_mkRepTyCon tycon metaDts mod = 
-- Consider the example input tycon `D`, where data D a b = D_ a
  do { -- `rep0` = GHC.Generics.Rep (type family)
       rep0 <- tcLookupTyCon repTyConName

       -- `rep0Ty` = D1 ... (C1 ... (S1 ... (Rec0 a))) :: * -> *
     ; rep0Ty <- tc_mkRepTy tycon metaDts
    
       -- `rep_name` is a name we generate for the synonym
--     ; rep_name <- newImplicitBinder (tyConName tycon) mkGenR
     ; rep_name <- newGlobalBinder mod (mkGenR (nameOccName (tyConName tycon)))
                     (nameSrcSpan (tyConName tycon))
     ; let -- `tyvars` = [a,b]
           tyvars  = tyConTyVars tycon

           -- rep0Ty has kind * -> *
           rep_kind = liftedTypeKind `mkArrowKind` liftedTypeKind

           -- `appT` = D a b
           appT = [mkTyConApp tycon (mkTyVarTys tyvars)]

     ; buildSynTyCon rep_name tyvars (SynonymTyCon rep0Ty) rep_kind
                     NoParentTyCon (Just (rep0, appT)) }



--------------------------------------------------------------------------------
-- Type representation
--------------------------------------------------------------------------------

tc_mkRepTy :: -- The type to generate representation for
               TyCon 
               -- Metadata datatypes to refer to
            -> MetaTyCons 
               -- Generated representation0 type
            -> TcM Type
tc_mkRepTy tycon metaDts = 
  do
    d1    <- tcLookupTyCon d1TyConName
    c1    <- tcLookupTyCon c1TyConName
    s1    <- tcLookupTyCon s1TyConName
    nS1   <- tcLookupTyCon noSelTyConName
    rec0  <- tcLookupTyCon rec0TyConName
    par0  <- tcLookupTyCon par0TyConName
    u1    <- tcLookupTyCon u1TyConName
    v1    <- tcLookupTyCon v1TyConName
    plus  <- tcLookupTyCon sumTyConName
    times <- tcLookupTyCon prodTyConName
    
    let mkSum' a b = mkTyConApp plus  [a,b]
        mkProd a b = mkTyConApp times [a,b]
        mkRec0 a   = mkTyConApp rec0  [a]
        mkPar0 a   = mkTyConApp par0  [a]
        mkD    a   = mkTyConApp d1    [metaDTyCon, sumP (tyConDataCons a)]
        mkC  i d a = mkTyConApp c1    [d, prod i (dataConOrigArgTys a) 
                                                 (null (dataConFieldLabels a))]
        -- This field has no label
        mkS True  _ a = mkTyConApp s1 [mkTyConTy nS1, a]
        -- This field has a  label
        mkS False d a = mkTyConApp s1 [d, a]
        
        sumP [] = mkTyConTy v1
        sumP l  = ASSERT (length metaCTyCons == length l)
                    foldBal mkSum' [ mkC i d a
                                   | (d,(a,i)) <- zip metaCTyCons (zip l [0..])]
        -- The Bool is True if this constructor has labelled fields
        prod :: Int -> [Type] -> Bool -> Type
        prod i [] _ = ASSERT (length metaSTyCons > i)
                        ASSERT (length (metaSTyCons !! i) == 0)
                          mkTyConTy u1
        prod i l b  = ASSERT (length metaSTyCons > i)
                        ASSERT (length l == length (metaSTyCons !! i))
                          foldBal mkProd [ arg d t b
                                         | (d,t) <- zip (metaSTyCons !! i) l ]
        
        arg :: Type -> Type -> Bool -> Type
        arg d t b = mkS b d (recOrPar t (getTyVar_maybe t))
        -- Argument is not a type variable, use Rec0
        recOrPar t Nothing  = mkRec0 t
        -- Argument is a type variable, use Par0
        recOrPar t (Just _) = mkPar0 t
        
        metaDTyCon  = mkTyConTy (metaD metaDts)
        metaCTyCons = map mkTyConTy (metaC metaDts)
        metaSTyCons = map (map mkTyConTy) (metaS metaDts)
        
    return (mkD tycon)

--------------------------------------------------------------------------------
-- Meta-information
--------------------------------------------------------------------------------

data MetaTyCons = MetaTyCons { -- One meta datatype per dataype
                               metaD :: TyCon
                               -- One meta datatype per constructor
                             , metaC :: [TyCon]
                               -- One meta datatype per selector per constructor
                             , metaS :: [[TyCon]] }
                             
instance Outputable MetaTyCons where
  ppr (MetaTyCons d c s) = ppr d $$ vcat (map ppr c) $$ vcat (map ppr (concat s))
                                   
metaTyCons2TyCons :: MetaTyCons -> Bag TyCon
metaTyCons2TyCons (MetaTyCons d c s) = listToBag (d : c ++ concat s)


-- Bindings for Datatype, Constructor, and Selector instances
mkBindsMetaD :: FixityEnv -> TyCon 
             -> ( LHsBinds RdrName      -- Datatype instance
                , [LHsBinds RdrName]    -- Constructor instances
                , [[LHsBinds RdrName]]) -- Selector instances
mkBindsMetaD fix_env tycon = (dtBinds, allConBinds, allSelBinds)
      where
        mkBag l = foldr1 unionBags 
                    [ unitBag (L loc (mkFunBind (L loc name) matches)) 
                        | (name, matches) <- l ]
        dtBinds       = mkBag [ (datatypeName_RDR, dtName_matches)
                              , (moduleName_RDR, moduleName_matches)]

        allConBinds   = map conBinds datacons
        conBinds c    = mkBag ( [ (conName_RDR, conName_matches c)]
                              ++ ifElseEmpty (dataConIsInfix c)
                                   [ (conFixity_RDR, conFixity_matches c) ]
                              ++ ifElseEmpty (length (dataConFieldLabels c) > 0)
                                   [ (conIsRecord_RDR, conIsRecord_matches c) ]
                              )

        ifElseEmpty p x = if p then x else []
        fixity c      = case lookupFixity fix_env (dataConName c) of
                          Fixity n InfixL -> buildFix n leftAssocDataCon_RDR
                          Fixity n InfixR -> buildFix n rightAssocDataCon_RDR
                          Fixity n InfixN -> buildFix n notAssocDataCon_RDR
        buildFix n assoc = nlHsApps infixDataCon_RDR [nlHsVar assoc
                                                     , nlHsIntLit (toInteger n)]

        allSelBinds   = map (map selBinds) datasels
        selBinds s    = mkBag [(selName_RDR, selName_matches s)]

        loc           = srcLocSpan (getSrcLoc tycon)
        mkStringLHS s = [mkSimpleHsAlt nlWildPat (nlHsLit (mkHsString s))]
        datacons      = tyConDataCons tycon
        datasels      = map dataConFieldLabels datacons

        dtName_matches     = mkStringLHS . showPpr . nameOccName . tyConName 
                           $ tycon
        moduleName_matches = mkStringLHS . moduleNameString . moduleName 
                           . nameModule . tyConName $ tycon

        conName_matches     c = mkStringLHS . showPpr . nameOccName
                              . dataConName $ c
        conFixity_matches   c = [mkSimpleHsAlt nlWildPat (fixity c)]
        conIsRecord_matches _ = [mkSimpleHsAlt nlWildPat (nlHsVar true_RDR)]

        selName_matches     s = mkStringLHS (showPpr (nameOccName s))


--------------------------------------------------------------------------------
-- Dealing with sums
--------------------------------------------------------------------------------

mkSum :: US          -- Base for generating unique names
      -> TyCon       -- The type constructor
      -> [DataCon]   -- The data constructors
      -> ([Alt],     -- Alternatives for the T->Trep "from" function
          [Alt])     -- Alternatives for the Trep->T "to" function

-- Datatype without any constructors
mkSum _us tycon [] = ([from_alt], [to_alt])
  where
    from_alt = (nlWildPat, mkM1_E (makeError errMsgFrom))
    to_alt   = (mkM1_P nlWildPat, makeError errMsgTo)
               -- These M1s are meta-information for the datatype
    makeError s = nlHsApp (nlHsVar error_RDR) (nlHsLit (mkHsString s))
    errMsgFrom = "No generic representation for empty datatype " ++ showPpr tycon
    errMsgTo = "No values for empty datatype " ++ showPpr tycon

-- Datatype with at least one constructor
mkSum us _tycon datacons =
  unzip [ mk1Sum us i (length datacons) d | (d,i) <- zip datacons [1..] ]

-- Build the sum for a particular constructor
mk1Sum :: US        -- Base for generating unique names
       -> Int       -- The index of this constructor
       -> Int       -- Total number of constructors
       -> DataCon   -- The data constructor
       -> (Alt,     -- Alternative for the T->Trep "from" function
           Alt)     -- Alternative for the Trep->T "to" function
mk1Sum us i n datacon = (from_alt, to_alt)
  where
    n_args = dataConSourceArity datacon	-- Existentials already excluded

    datacon_vars = map mkGenericLocal [us .. us+n_args-1]
    us'          = us + n_args

    datacon_rdr  = getRdrName datacon
    app_exp      = nlHsVarApps datacon_rdr datacon_vars
    
    from_alt     = (nlConVarPat datacon_rdr datacon_vars, from_alt_rhs)
    from_alt_rhs = mkM1_E (genLR_E i n (mkProd_E us' datacon_vars))
    
    to_alt     = (mkM1_P (genLR_P i n (mkProd_P us' datacon_vars)), to_alt_rhs)
                 -- These M1s are meta-information for the datatype
    to_alt_rhs = app_exp

-- Generates the L1/R1 sum pattern
genLR_P :: Int -> Int -> LPat RdrName -> LPat RdrName
genLR_P i n p
  | n == 0       = error "impossible"
  | n == 1       = p
  | i <= div n 2 = nlConPat l1DataCon_RDR [genLR_P i     (div n 2) p]
  | otherwise    = nlConPat r1DataCon_RDR [genLR_P (i-m) (n-m)     p]
                     where m = div n 2

-- Generates the L1/R1 sum expression
genLR_E :: Int -> Int -> LHsExpr RdrName -> LHsExpr RdrName
genLR_E i n e
  | n == 0       = error "impossible"
  | n == 1       = e
  | i <= div n 2 = nlHsVar l1DataCon_RDR `nlHsApp` genLR_E i     (div n 2) e
  | otherwise    = nlHsVar r1DataCon_RDR `nlHsApp` genLR_E (i-m) (n-m)     e
                     where m = div n 2

--------------------------------------------------------------------------------
-- Dealing with products
--------------------------------------------------------------------------------

-- Build a product expression
mkProd_E :: US	            -- Base for unique names
	 -> [RdrName]       -- List of variables matched on the lhs
	 -> LHsExpr RdrName -- Resulting product expression
mkProd_E _ []   = mkM1_E (nlHsVar u1DataCon_RDR)
mkProd_E _ vars = mkM1_E (foldBal prod appVars)
                   -- These M1s are meta-information for the constructor
  where
    appVars = map wrapArg_E vars
    prod a b = prodDataCon_RDR `nlHsApps` [a,b]

wrapArg_E :: RdrName -> LHsExpr RdrName
wrapArg_E v = mkM1_E (k1DataCon_RDR `nlHsVarApps` [v])
              -- This M1 is meta-information for the selector

-- Build a product pattern
mkProd_P :: US		        -- Base for unique names
	       -> [RdrName]     -- List of variables to match
	       -> LPat RdrName  -- Resulting product pattern
mkProd_P _ []   = mkM1_P (nlNullaryConPat u1DataCon_RDR)
mkProd_P _ vars = mkM1_P (foldBal prod appVars)
                   -- These M1s are meta-information for the constructor
  where
    appVars = map wrapArg_P vars
    prod a b = prodDataCon_RDR `nlConPat` [a,b]
    
wrapArg_P :: RdrName -> LPat RdrName
wrapArg_P v = mkM1_P (k1DataCon_RDR `nlConVarPat` [v])
              -- This M1 is meta-information for the selector

mkGenericLocal :: US -> RdrName
mkGenericLocal u = mkVarUnqual (mkFastString ("g" ++ show u))

mkM1_E :: LHsExpr RdrName -> LHsExpr RdrName
mkM1_E e = nlHsVar m1DataCon_RDR `nlHsApp` e

mkM1_P :: LPat RdrName -> LPat RdrName
mkM1_P p = m1DataCon_RDR `nlConPat` [p]

-- | Variant of foldr1 for producing balanced lists
foldBal :: (a -> a -> a) -> [a] -> a
foldBal op = foldBal' op (error "foldBal: empty list")

foldBal' :: (a -> a -> a) -> a -> [a] -> a
foldBal' _  x []  = x
foldBal' _  _ [y] = y
foldBal' op x l   = let (a,b) = splitAt (length l `div` 2) l
                    in foldBal' op x a `op` foldBal' op x b

\end{code}
