%
% (c) The University of Glasgow 2011
%

The deriving code for the Generic class
(equivalent to the code in TcGenDeriv, for other classes)

\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details


module TcGenGenerics (canDoGenerics, gen_Generic_binds) where


import DynFlags
import HsSyn
import Type
import TcType
import TcGenDeriv
import DataCon
import TyCon
import FamInstEnv       ( FamInst, mkSynFamInst )
import Module           ( Module, moduleName, moduleNameString )
import IfaceEnv         ( newGlobalBinder )
import Name      hiding ( varName )
import RdrName
import BasicTypes
import TysWiredIn
import PrelNames
import InstEnv
import TcEnv
import MkId
import TcRnMonad
import HscTypes
import BuildTyCl
import SrcLoc
import Bag
import Outputable 
import FastString
import UniqSupply
import Util

#include "HsVersions.h"
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Bindings for the new generic deriving mechanism}
%*                                                                      *
%************************************************************************

For the generic representation we need to generate:
\begin{itemize}
\item A Generic instance
\item A Rep type instance 
\item Many auxiliary datatypes and instances for them (for the meta-information)
\end{itemize}

\begin{code}
gen_Generic_binds :: TyCon -> Module
                 -> TcM (LHsBinds RdrName, BagDerivStuff)
gen_Generic_binds tc mod = do
        { (metaTyCons, rep0TyInst) <- genGenericRepExtras tc mod
        ; metaInsts                <- genDtMeta (tc, metaTyCons)
        ; return ( mkBindsRep tc
                 ,           (DerivFamInst rep0TyInst)
                   `consBag` ((mapBag DerivTyCon (metaTyCons2TyCons metaTyCons))
                   `unionBags` metaInsts)) }

genGenericRepExtras :: TyCon -> Module -> TcM (MetaTyCons, FamInst)
genGenericRepExtras tc mod =
  do  uniqS <- newUniqueSupply
      let
        -- Uniques for everyone
        (uniqD:uniqs) = uniqsFromSupply uniqS
        (uniqsC,us) = splitAt (length tc_cons) uniqs
        uniqsS :: [[Unique]] -- Unique supply for the S datatypes
        uniqsS = mkUniqsS tc_arits us
        mkUniqsS []    _  = []
        mkUniqsS (n:t) us = case splitAt n us of
                              (us1,us2) -> us1 : mkUniqsS t us2

        tc_name   = tyConName tc
        tc_cons   = tyConDataCons tc
        tc_arits  = map dataConSourceArity tc_cons
        
        tc_occ    = nameOccName tc_name
        d_occ     = mkGenD tc_occ
        c_occ m   = mkGenC tc_occ m
        s_occ m n = mkGenS tc_occ m n
        d_name    = mkExternalName uniqD mod d_occ wiredInSrcSpan
        c_names   = [ mkExternalName u mod (c_occ m) wiredInSrcSpan
                      | (u,m) <- zip uniqsC [0..] ]
        s_names   = [ [ mkExternalName u mod (s_occ m n) wiredInSrcSpan 
                        | (u,n) <- zip us [0..] ] | (us,m) <- zip uniqsS [0..] ]
        
        mkTyCon name = ASSERT( isExternalName name )
                       buildAlgTyCon name [] Nothing [] distinctAbstractTyConRhs
                                          NonRecursive False NoParentTyCon

      let metaDTyCon  = mkTyCon d_name
          metaCTyCons = map mkTyCon c_names
          metaSTyCons =  [ [ mkTyCon s_name | s_name <- s_namesC ] 
                         | s_namesC <- s_names ]

          metaDts = MetaTyCons metaDTyCon metaCTyCons metaSTyCons
  
      rep0_tycon <- tc_mkRepTyCon tc metaDts mod
      
      -- pprTrace "rep0" (ppr rep0_tycon) $
      return (metaDts, rep0_tycon)

genDtMeta :: (TyCon, MetaTyCons) -> TcM BagDerivStuff
genDtMeta (tc,metaDts) =
  do  loc <- getSrcSpanM
      dflags <- getDynFlags
      dClas <- tcLookupClass datatypeClassName
      let new_dfun_name clas tycon = newDFunName clas [mkTyConApp tycon []] loc
      d_dfun_name <- new_dfun_name dClas tc
      cClas <- tcLookupClass constructorClassName
      c_dfun_names <- sequence [ new_dfun_name cClas tc | _ <- metaC metaDts ]
      sClas <- tcLookupClass selectorClassName
      s_dfun_names <- sequence (map sequence [ [ new_dfun_name sClas tc 
                                               | _ <- x ] 
                                             | x <- metaS metaDts ])
      fix_env <- getFixityEnv

      let
        safeOverlap = safeLanguageOn dflags
        (dBinds,cBinds,sBinds) = mkBindsMetaD fix_env tc
        
        -- Datatype
        d_metaTycon = metaD metaDts
        d_inst = mkLocalInstance d_dfun $ NoOverlap safeOverlap
        d_binds = VanillaInst dBinds [] False
        d_dfun  = mkDictFunId d_dfun_name (tyConTyVars tc) [] dClas 
                    [ mkTyConTy d_metaTycon ]
        d_mkInst = DerivInst (InstInfo { iSpec = d_inst, iBinds = d_binds })
        
        -- Constructor
        c_metaTycons = metaC metaDts
        c_insts = [ mkLocalInstance (c_dfun c ds) $ NoOverlap safeOverlap
                  | (c, ds) <- myZip1 c_metaTycons c_dfun_names ]
        c_binds = [ VanillaInst c [] False | c <- cBinds ]
        c_dfun c dfun_name = mkDictFunId dfun_name (tyConTyVars tc) [] cClas 
                               [ mkTyConTy c ]
        c_mkInst = [ DerivInst (InstInfo { iSpec = is, iBinds = bs })
                   | (is,bs) <- myZip1 c_insts c_binds ]
        
        -- Selector
        s_metaTycons = metaS metaDts
        s_insts = map (map (\(s,ds) -> mkLocalInstance (s_dfun s ds) $
                                                  NoOverlap safeOverlap))
                    (myZip2 s_metaTycons s_dfun_names)
        s_binds = [ [ VanillaInst s [] False | s <- ss ] | ss <- sBinds ]
        s_dfun s dfun_name = mkDictFunId dfun_name (tyConTyVars tc) [] sClas
                               [ mkTyConTy s ]
        s_mkInst = map (map (\(is,bs) -> DerivInst (InstInfo { iSpec  = is
                                                             , iBinds = bs})))
                       (myZip2 s_insts s_binds)
       
        myZip1 :: [a] -> [b] -> [(a,b)]
        myZip1 l1 l2 = ASSERT (length l1 == length l2) zip l1 l2
        
        myZip2 :: [[a]] -> [[b]] -> [[(a,b)]]
        myZip2 l1 l2 =
          ASSERT (and (zipWith (>=) (map length l1) (map length l2)))
            [ zip x1 x2 | (x1,x2) <- zip l1 l2 ]
        
      return (listToBag (d_mkInst : c_mkInst ++ concat s_mkInst))
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Generating representation types}
%*                                                                      *
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
        from_matches  = [mkSimpleHsAlt pat rhs | (pat,rhs) <- from_alts]
        to_matches    = [mkSimpleHsAlt pat rhs | (pat,rhs) <- to_alts  ]
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
               -> Module          -- Used as the location of the new RepTy
               -> TcM FamInst     -- Generated representation0 coercion
tc_mkRepTyCon tycon metaDts mod = 
-- Consider the example input tycon `D`, where data D a b = D_ a
  do { -- `rep0` = GHC.Generics.Rep (type family)
       rep0 <- tcLookupTyCon repTyConName

     ; let -- `tyvars` = [a,b]
           tyvars     = tyConTyVars tycon
           tyvar_args = mkTyVarTys tyvars

           -- `appT` = D a b
           appT = [mkTyConApp tycon tyvar_args]

       -- `rep0Ty` = D1 ... (C1 ... (S1 ... (Rec0 a))) :: * -> *
     ; rep0Ty <- tc_mkRepTy tycon tyvar_args metaDts
    
       -- `rep_name` is a name we generate for the synonym
     ; rep_name <- newGlobalBinder mod (mkGenR (nameOccName (tyConName tycon)))
                     (nameSrcSpan (tyConName tycon))

     ; return $ mkSynFamInst rep_name tyvars rep0 appT rep0Ty
     }



--------------------------------------------------------------------------------
-- Type representation
--------------------------------------------------------------------------------

tc_mkRepTy :: -- The type to generate representation for, and instantiating types
               TyCon -> [Type]    
               -- Metadata datatypes to refer to
            -> MetaTyCons 
               -- Generated representation0 type
            -> TcM Type
tc_mkRepTy tycon ty_args metaDts = 
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
        mkC  i d a = mkTyConApp c1    [d, prod i (dataConInstOrigArgTys a ty_args) 
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
