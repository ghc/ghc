%
% (c) The University of Glasgow 2006
%

\begin{code}
{-# OPTIONS -fno-warn-incomplete-patterns #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module Generics ( canDoGenerics, mkTyConGenericBinds,
		  mkGenericRhs,
		  validGenericInstanceType, validGenericMethodType,
		  mkBindsRep0, tc_mkRep0TyCon, mkBindsMetaD,
		  MetaTyCons(..), metaTyCons2TyCons
    ) where


import HsSyn
import Type
import TcType
import DataCon

import TyCon
import Name hiding (varName)
import Module (moduleName, moduleNameString)
import RdrName
import BasicTypes
import Var hiding (varName)
import VarSet
import Id
import TysWiredIn
import PrelNames
-- For generation of representation types
import TcEnv (tcLookupTyCon)
import TcRnMonad (TcM, newUnique)
import HscTypes
	
import SrcLoc
import Util
import Bag
import Outputable 
import FastString

#include "HsVersions.h"
\end{code}

Roadmap of what's where in the Generics work.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Parser
No real checks.

RnSource.rnHsType
  Checks that HsNumTy has a "1" in it.

TcInstDcls.mkGenericInstance:
  Checks for invalid type patterns, such as f {| Int |}

TcClassDcl.tcClassSig
  Checks for a method type that is too complicated;
	e.g. has for-alls or lists in it
  We could lift this restriction

TcClassDecl.mkDefMethRhs
  Checks that the instance type is simple, in an instance decl 
  where we let the compiler fill in a generic method.
	e.g.  instance C (T Int)
  	is not valid if C has generic methods.

TcClassDecl.checkGenericClassIsUnary
  Checks that we don't have generic methods in a multi-parameter class

TcClassDecl.checkDefaultBinds
  Checks that all the equations for a method in a class decl
  are generic, or all are non-generic


			
Checking that the type constructors which are present in Generic
patterns (not Unit, this is done differently) is done in mk_inst_info
(TcInstDecls) in a call to tcHsType (TcMonoBinds). This means that
HsOpTy is tied to Generic definitions which is not a very good design
feature, indeed a bug. However, the check is easy to move from
tcHsType back to mk_inst_info and everything will be fine. Also see
bug #5. [I don't think that this is the case anymore after SPJ's latest
changes in that regard.  Delete this comment?  -=chak/7Jun2]

Generics.lhs

Making generic information to put into a tycon. Constructs the
representation type, which, I think, are not used later. Perhaps it is
worth removing them from the GI datatype. Although it does get used in
the construction of conversion functions (internally).

TyCon.lhs

Just stores generic information, accessible by tyConGenInfo or tyConGenIds.

TysWiredIn.lhs

Defines generic and other type and data constructors.

This is sadly incomplete, but will be added to.


Bugs & shortcomings of existing implementation:
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

2. Another pretty big bug I dscovered at the last minute when I was
testing the code is that at the moment the type variable of the class
is scoped over the entire declaration, including the patterns. For
instance, if I have the following code,

class Er a where
 ...
  er {| Plus a b |} (Inl x) (Inl y) = er x y 
  er {| Plus a b |} (Inr x) (Inr y) = er x y 
  er {| Plus a b |} _ _ = False
 
and I print out the types of the generic patterns, I get the
following.  Note that all the variable names for "a" are the same,
while for "b" they are all different.

check_ty
    [std.Generics.Plus{-33u,i-} a{-r6Z-} b{-r7g-},
     std.Generics.Plus{-33u,i-} a{-r6Z-} b{-r7m-},
     std.Generics.Plus{-33u,i-} a{-r6Z-} b{-r7p-}]

This is a bug as if I change the code to

 er {| Plus c b |} (Inl x)  (Inl y) = er x y 

all the names come out to be different.

Thus, all the types (Plus a b) come out to be different, so I cannot
compare them and test whether they are all the same and thus cannot
return an error if the type variables are different.

Temporary fix/hack. I am not checking for this, I just assume they are
the same, see line "check_ty = True" in TcInstDecls. When we resolve
the issue with variables, though - I assume that we will make them to
be the same in all the type patterns, jus uncomment the check and
everything should work smoothly.

Hence, I have also left the rather silly construction of:
* extracting all the type variables from all the types
* putting them *all* into the environment
* typechecking all the types
* selecting one of them and using it as the instance_ty.

(the alternative is to make sure that all the types are the same,
taking one, extracting its variables, putting them into the environment,
type checking it, using it as the instance_ty)
 
6. What happens if we do not supply all of the generic patterns? At
the moment, the compiler crashes with an error message "Non-exhaustive
patterns in a generic declaration" 


What has not been addressed:
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Contexts. In the generated instance declarations for the 3 primitive
type constructors, we need contexts. It is unclear what those should
be. At the moment, we always say eg. (Eq a, Eq b) => Eq (Plus a b)

Type application. We have type application in expressions
(essentially) on the lhs of an equation. Do we want to allow it on the
RHS?

Scoping of type variables in a generic definition. At the moment, (see
TcInstDecls) we extract the type variables inside the type patterns
and add them to the environment. See my bug #2 above. This seems pretty
important.



%************************************************************************
%*									*
\subsection{Getting the representation type out}
%*									*
%************************************************************************

\begin{code}
validGenericInstanceType :: Type -> Bool
  -- Checks for validity of the type pattern in a generic
  -- declaration.  It's ok to have  
  --	f {| a + b |} ...
  -- but it's not OK to have
  --	f {| a + Int |}

validGenericInstanceType inst_ty
  = case tcSplitTyConApp_maybe inst_ty of
	Just (tycon, tys) ->  all isTyVarTy tys && tyConName tycon `elem` genericTyConNames
	Nothing		  ->  False

validGenericMethodType :: Type -> Bool
  -- At the moment we only allow method types built from
  -- 	* type variables
  --	* function arrow
  --	* boxed tuples
  --    * lists
  --	* an arbitrary type not involving the class type variables
  --		e.g. this is ok: 	forall b. Ord b => [b] -> a
  --	             where a is the class variable
validGenericMethodType ty 
  = valid tau
  where
    (local_tvs, _, tau) = tcSplitSigmaTy ty

    valid ty
      | not (isTauTy ty) = False 	-- Note [Higher ramk methods]
      | isTyVarTy ty     = True
      | no_tyvars_in_ty	 = True
      | otherwise	 = case tcSplitTyConApp_maybe ty of
				Just (tc,tys) -> valid_tycon tc && all valid tys
				Nothing	      -> False
      where
	no_tyvars_in_ty = all (`elem` local_tvs) (varSetElems (tyVarsOfType ty))

    valid_tycon tc = tc == funTyCon || tc == listTyCon || isBoxedTupleTyCon tc 
	-- Compare bimapApp, below
\end{code}


%************************************************************************
%*									*
\subsection{Generating representation types}
%*									*
%************************************************************************

\begin{code}
canDoGenerics :: ThetaType -> [DataCon] -> Bool
-- Called on source-code data types, to see if we should generate
-- generic functions for them.  (This info is recorded in the interface file for
-- imported data types.)

canDoGenerics stupid_theta data_cons
  =  not (any bad_con data_cons) 	-- See comment below
  
  -- && not (null data_cons)		-- No values of the type
  -- JPM: we now support empty datatypes
  
     && null stupid_theta -- We do not support datatypes with context (for now)
  where
    bad_con dc = any bad_arg_type (dataConOrigArgTys dc) || not (isVanillaDataCon dc)
  	-- If any of the constructor has an unboxed type as argument,
	-- then we can't build the embedding-projection pair, because
	-- it relies on instantiating *polymorphic* sum and product types
	-- at the argument types of the constructors

	-- Nor can we do the job if it's an existential data constructor,

	-- Nor if the args are polymorphic types (I don't think)
    bad_arg_type ty = isUnLiftedType ty || not (isTauTy ty)
  -- JPM: TODO: I'm not sure I know what isTauTy checks for, so I'm leaving it
	-- like this for now...
\end{code}

%************************************************************************
%*									*
\subsection{Generating the RHS of a generic default method}
%*									*
%************************************************************************

\begin{code}
type US = Int	-- Local unique supply, just a plain Int
type Alt = (LPat RdrName, LHsExpr RdrName)
{-
data GenRep = GenRep {
    genBindsFrom0 :: TyCon -> LHsBinds RdrName
  , genBindsTo0 :: TyCon -> LHsBinds RdrName
  , genBindsFrom1 :: TyCon -> LHsBinds RdrName
  , genBindsTo1 :: TyCon -> LHsBinds RdrName
  , genBindsModuleName :: TyCon -> LHsBinds RdrName
  , genBindsConName :: DataCon -> LHsBinds RdrName
  , genBindsConFixity :: DataCon -> LHsBinds RdrName
  , genBindsConIsRecord :: DataCon -> LHsBinds RdrName
  , genBindsSelName :: DataCon -> Int -> LHsBinds RdrName
  }
-}
-- Bindings for the Representable0 instance
mkBindsRep0 :: TyCon -> LHsBinds RdrName
mkBindsRep0 tycon = 
    unitBag (L loc (mkFunBind (L loc from0_RDR) from0_matches))
  `unionBags`
    unitBag (L loc (mkFunBind (L loc to0_RDR) to0_matches))
      where
        from0_matches = [mkSimpleHsAlt pat rhs | (pat,rhs) <- from0_alts]
        to0_matches   = [mkSimpleHsAlt pat rhs | (pat,rhs) <- to0_alts  ]
        loc           = srcLocSpan (getSrcLoc tycon)
        datacons      = tyConDataCons tycon

        -- Recurse over the sum first
        from0_alts, to0_alts :: [Alt]
        (from0_alts, to0_alts) = mkSum (1 :: US) tycon datacons
        
-- Disabled
mkTyConGenericBinds :: TyCon -> LHsBinds RdrName
mkTyConGenericBinds _tycon = 
  {-
    unitBag (L loc (mkFunBind (L loc from0_RDR) from0_matches))
  `unionBags`
    unitBag (L loc (mkFunBind (L loc to0_RDR) to0_matches))
  `unionBags`
    mkMeta loc tycon
  -}
    emptyBag
{-
  where
    from0_matches = [mkSimpleHsAlt pat rhs | (pat,rhs) <- from0_alts]
    to0_matches   = [mkSimpleHsAlt pat rhs | (pat,rhs) <- to0_alts  ]
    loc           = srcLocSpan (getSrcLoc tycon)
    datacons      = tyConDataCons tycon
    (from0_RDR, to0_RDR) = mkGenericNames tycon

    -- Recurse over the sum first
    from0_alts, to0_alts :: [Alt]
    (from0_alts, to0_alts) = mkSum init_us tycon datacons
    init_us = 1 :: US -- Unique supply
-}

--------------------------------------------------------------------------------
-- Type representation
--------------------------------------------------------------------------------
{-
mkRep0Ty :: TyCon -> LHsType Name
mkRep0Ty tycon = res
  where
    res = d1 `nlHsAppTy` (cons datacons)
    d1 = nlHsTyVar d1TyConName `nlHsAppTy` nlHsTyVar d1TyConName -- TODO
    c1 = nlHsTyVar c1TyConName `nlHsAppTy` nlHsTyVar c1TyConName -- TODO
    s1 = nlHsTyVar s1TyConName `nlHsAppTy` nlHsTyVar noSelTyConName -- TODO
    plus a b = nlHsTyVar sumTyConName `nlHsAppTy` a `nlHsAppTy` b
    times a b = nlHsTyVar prodTyConName `nlHsAppTy` a `nlHsAppTy` b
    k1 x = nlHsTyVar k1TyConName `nlHsAppTy` nlHsTyVar x
    
    datacons = tyConDataCons tycon
    n_args datacon = dataConSourceArity datacon
    datacon_vars datacon = map mkGenericLocal [1 .. n_args datacon]
        
    cons ds = c1 `nlHsAppTy` sum ds
    sum [] = nlHsTyVar v1TyConName
    sum l  = foldBal plus (map sel l)
    sel d = s1 `nlHsAppTy` prod (dataConOrigArgTys d)
    prod [] = nlHsTyVar u1TyConName
    prod l  = foldBal times (map arg l)
    arg :: Type -> LHsType Name
    -- TODO
    arg t = nlHsTyVar k1TyConName `nlHsAppTy` nlHsTyVar v1TyConName -- TODO
-}

tc_mkRep0Ty :: -- The type to generate representation for
               TyCon 
               -- Metadata datatypes to refer to
            -> MetaTyCons 
               -- Generated representation0 type
            -> TcM Type
tc_mkRep0Ty tycon metaDts = 
  do
    d1 <- tcLookupTyCon d1TyConName
    c1 <- tcLookupTyCon c1TyConName
    s1 <- tcLookupTyCon s1TyConName
    rec0 <- tcLookupTyCon rec0TyConName
    u1 <- tcLookupTyCon u1TyConName
    v1 <- tcLookupTyCon v1TyConName
    plus <- tcLookupTyCon sumTyConName
    times <- tcLookupTyCon prodTyConName
    
    let mkSum  a b = mkTyConApp plus  [a,b]
        mkProd a b = mkTyConApp times [a,b]
        mkRec0 a   = mkTyConApp rec0  [a]
        mkD    a   = mkTyConApp d1    [metaDTyCon, sum (tyConDataCons a)]
        mkC  i d a = mkTyConApp c1    [d, prod i (dataConOrigArgTys a)]
        mkS    d a = mkTyConApp s1    [d, a]
        
        sum [] = mkTyConTy v1
        sum l  = ASSERT (length metaCTyCons == length l)
                   foldBal mkSum [ mkC i d a
                                 | (d,(a,i)) <- zip metaCTyCons (zip l [0..]) ]
        prod :: Int -> [Type] -> Type
        prod i [] = ASSERT (length metaSTyCons > i)
                      ASSERT (length (metaSTyCons !! i) == 0)
                        mkTyConTy u1
        prod i l  = ASSERT (length metaSTyCons > i)
                      ASSERT (length l == length (metaSTyCons !! i))
                        foldBal mkProd [ arg d a 
                                       | (d,a) <- zip (metaSTyCons !! i) l ]
        
        arg d t = mkS d (mkRec0 t)
        
        metaDTyCon  = mkTyConTy (metaD metaDts)
        metaCTyCons = map mkTyConTy (metaC metaDts)
        metaSTyCons = map (map mkTyConTy) (metaS metaDts)
        
    return (mkD tycon)

tc_mkRep0TyCon :: TyCon           -- The type to generate representation for
               -> MetaTyCons      -- Metadata datatypes to refer to
               -> TcM TyCon       -- Generated representation0 type
tc_mkRep0TyCon tycon metaDts = 
-- Consider the example input tycon `D`, where data D a b = D_ a
  do
    uniq1   <- newUnique
    uniq2   <- newUnique
    -- `rep0Ty` = D1 ... (C1 ... (S1 ... (Rec0 a))) :: * -> *
    rep0Ty  <- tc_mkRep0Ty tycon metaDts
    -- `rep0` = GHC.Generics.Rep0 (type family)
    rep0    <- tcLookupTyCon rep0TyConName
    
    let mod     = nameModule  (tyConName tycon)
        loc     = nameSrcSpan (tyConName tycon)
        -- `repName` is a name we generate for the synonym
        repName = mkExternalName uniq1 mod (mkGenR0 (nameOccName (tyConName tycon))) loc
        -- `coName` is a name for the coercion
        coName  = mkExternalName uniq2 mod (mkGenR0 (nameOccName (tyConName tycon))) loc
        -- `tyvars` = [a,b]
        tyvars  = tyConTyVars tycon
        -- `appT` = D a b
        appT    = [mkTyConApp tycon (mkTyVarTys tyvars)]
        -- Result
        res = mkSynTyCon repName
                 -- rep0Ty has kind `kind of D` -> *
                 (tyConKind tycon `mkArrowKind` liftedTypeKind)
                 tyvars (SynonymTyCon rep0Ty)
                 (FamInstTyCon rep0 appT
                   (mkCoercionTyCon coName (tyConArity tycon)
                     -- co : forall a b. Rep0 (D a b) ~ `rep0Ty` a b
                     (CoAxiom tyvars (mkTyConApp rep0 appT) rep0Ty)))

    return res

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
  ppr (MetaTyCons d c s) = ppr d <+> ppr c <+> ppr s
                                   
metaTyCons2TyCons :: MetaTyCons -> [TyCon]
metaTyCons2TyCons (MetaTyCons d c s) = d : c ++ concat s


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
                              ++ ifElseEmpty (isTupleCon c)
                                   [(conIsTuple_RDR
                                    ,conIsTuple_matches (dataConTyCon c))]
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
        -- TODO: check that this works
        conIsTuple_matches  c = [mkSimpleHsAlt nlWildPat 
                                  (nlHsApp (nlHsVar arityDataCon_RDR) 
                                           (nlHsIntLit 
                                             (toInteger (tupleTyConArity c))))]

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
mkProd_E :: US			        -- Base for unique names
	       -> [RdrName]       -- List of variables matched on the lhs
	       -> LHsExpr RdrName -- Resulting product expression
mkProd_E _ []   = mkM1_E (nlHsVar u1DataCon_RDR)
mkProd_E _ vars = mkM1_E (foldBal prod appVars)
                   -- These M1s are meta-information for the constructor
  where
    appVars = map wrapArg_E vars
    prod a b = prodDataCon_RDR `nlHsApps` [a,b]

-- TODO: Produce a P0 when v is a parameter
wrapArg_E :: RdrName -> LHsExpr RdrName
wrapArg_E v = mkM1_E (k1DataCon_RDR `nlHsVarApps` [v])
              -- This M1 is meta-information for the selector

-- Build a product pattern
mkProd_P :: US			      -- Base for unique names
	       -> [RdrName]     -- List of variables to match
	       -> LPat RdrName  -- Resulting product pattern
mkProd_P _ []   = mkM1_P (nlNullaryConPat u1DataCon_RDR)
mkProd_P _ vars = mkM1_P (foldBal prod appVars)
                   -- These M1s are meta-information for the constructor
  where
    appVars = map wrapArg_P vars
    prod a b = prodDataCon_RDR `nlConPat` [a,b]
    
-- TODO: Produce a P0 when v is a parameter
wrapArg_P :: RdrName -> LPat RdrName
wrapArg_P v = mkM1_P (k1DataCon_RDR `nlConVarPat` [v])
              -- This M1 is meta-information for the selector


mkGenericLocal :: US -> RdrName
mkGenericLocal u = mkVarUnqual (mkFastString ("g" ++ show u))

mkGenericNames :: TyCon -> (RdrName, RdrName)
mkGenericNames tycon
  = (from_RDR, to_RDR)
  where
    tc_name  = tyConName tycon
    tc_occ   = nameOccName tc_name
    tc_mod   = ASSERT( isExternalName tc_name ) nameModule tc_name
    from_RDR = mkOrig tc_mod (mkGenOcc1 tc_occ)
    to_RDR   = mkOrig tc_mod (mkGenOcc2 tc_occ)
    
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

%************************************************************************
%*									*
\subsection{Generating the RHS of a generic default method}
%*									*
%************************************************************************

Generating the Generic default method.  Uses the bimaps to generate the
actual method. All of this is rather incomplete, but it would be nice
to make even this work.  Example

 	class Foo a where
	  op :: Op a

	instance Foo T

Then we fill in the RHS for op, RenamedHsExpr, by calling mkGenericRhs:

	instance Foo T where
	   op = <mkGenericRhs op a T>

To do this, we generate a pair of RenamedHsExprs (EP toOp fromOp), where

	toOp   :: Op Trep -> Op T
	fromOp :: Op T    -> Op Trep

(the bimap) and then fill in the RHS with

	instance Foo T where
	   op = toOp op

Remember, we're generating a RenamedHsExpr, so the result of all this
will be fed to the type checker.  So the 'op' on the RHS will be 
at the representation type for T, Trep.


Note [Polymorphic methods]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose the class op is polymorphic:

	class Baz a where
	  op :: forall b. Ord b => a -> b -> b

Then we can still generate a bimap with

	toOP :: forall b. (Trep -> b -> b) -> (T -> b -> b)

and fill in the instance decl thus

	instance Foo T where
	   op = toOp op

By the time the type checker has done its stuff we'll get

	instance Foo T where
	   op = \b. \dict::Ord b. toOp b (op Trep b dict)

Note [Higher rank methods]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Higher-rank method types don't work, because we'd generate a bimap that
needs impredicative polymorphism.  In principle that should be possible
(with boxy types and all) but it would take a bit of working out.   Here's
an example:
  class ChurchEncode k where 
    match :: k -> z 
       	  -> (forall a b z. a -> b -> z)  {- product -} 
       	  -> (forall a   z. a -> z)       {- left -} 
       	  -> (forall a   z. a -> z)       {- right -} 
       	  -> z 
  
    match {| Unit    |} Unit      unit prod left right = unit 
    match {| a :*: b |} (x :*: y) unit prod left right = prod x y 
    match {| a :+: b |} (Inl l)   unit prod left right = left l 
    match {| a :+: b |} (Inr r)   unit prod left right = right r 

\begin{code}
mkGenericRhs :: Id -> TyVar -> TyCon -> LHsExpr RdrName
mkGenericRhs sel_id tyvar tycon
  = ASSERT( isSingleton ctxt ) 	-- Checks shape of selector-id context
--    pprTrace "mkGenericRhs" (vcat [ppr sel_id, ppr (idType sel_id), ppr tyvar, ppr tycon, ppr local_tvs, ppr final_ty]) $
    mkHsApp (toEP bimap) (nlHsVar (getRdrName sel_id))
  where 
	-- Initialising the "Environment" with the from/to functions
	-- on the datatype (actually tycon) in question
	(from_RDR, to_RDR) = mkGenericNames tycon 

        -- Instantiate the selector type, and strip off its class context
	(ctxt, op_ty) = tcSplitPhiTy (applyTy (idType sel_id) (mkTyVarTy tyvar))

        -- Do it again!  This deals with the case where the method type 
	-- is polymorphic -- see Note [Polymorphic methods] above
	(local_tvs,_,final_ty) = tcSplitSigmaTy op_ty

	-- Now we probably have a tycon in front
        -- of us, quite probably a FunTyCon.
        ep    = EP (nlHsVar from_RDR) (nlHsVar to_RDR) 
        bimap = generate_bimap (tyvar, ep, local_tvs) final_ty

type EPEnv = (TyVar,			-- The class type variable
	      EP (LHsExpr RdrName),	-- The EP it maps to
	      [TyVar]			-- Other in-scope tyvars; they have an identity EP
	     )

-------------------
generate_bimap :: EPEnv
	       -> Type
	       -> EP (LHsExpr RdrName)
-- Top level case - splitting the TyCon.
generate_bimap env@(tv,ep,local_tvs) ty 
  | all (`elem` local_tvs) (varSetElems (tyVarsOfType ty))
  = idEP	-- A constant type

  | Just tv1 <- getTyVar_maybe ty
  = ASSERT( tv == tv1 ) ep					-- The class tyvar

  | Just (tycon, ty_args) <- tcSplitTyConApp_maybe ty
  = bimapTyCon tycon (map (generate_bimap env) ty_args)

  | otherwise
  = pprPanic "generate_bimap" (ppr ty)

-------------------
bimapTyCon :: TyCon -> [EP  (LHsExpr RdrName)] -> EP (LHsExpr RdrName)
bimapTyCon tycon arg_eps 
  | tycon == funTyCon       = bimapArrow arg_eps
  | tycon == listTyCon      = bimapList arg_eps
  | isBoxedTupleTyCon tycon = bimapTuple arg_eps
  | otherwise		    = pprPanic "bimapTyCon" (ppr tycon)

-------------------
-- bimapArrow :: [EP a a', EP b b'] -> EP (a->b) (a'->b')
bimapArrow :: [EP (LHsExpr RdrName)] -> EP (LHsExpr RdrName)
bimapArrow [ep1, ep2]
  = EP { fromEP = mkHsLam [nlVarPat a_RDR, nlVarPat b_RDR] from_body, 
	 toEP   = mkHsLam [nlVarPat a_RDR, nlVarPat b_RDR] to_body }
  where
    from_body = fromEP ep2 `mkHsApp` (mkHsPar $ nlHsVar a_RDR `mkHsApp` (mkHsPar $ toEP   ep1 `mkHsApp` nlHsVar b_RDR))
    to_body   = toEP   ep2 `mkHsApp` (mkHsPar $ nlHsVar a_RDR `mkHsApp` (mkHsPar $ fromEP ep1 `mkHsApp` nlHsVar b_RDR))

-------------------
-- bimapTuple :: [EP a1 b1, ... EP an bn] -> EP (a1,...an) (b1,..bn)
bimapTuple :: [EP (LHsExpr RdrName)] -> EP (LHsExpr RdrName)
bimapTuple eps 
  = EP { fromEP = mkHsLam [noLoc tuple_pat] from_body,
	 toEP   = mkHsLam [noLoc tuple_pat] to_body }
  where
    names	= takeList eps gs_RDR
    tuple_pat	= TuplePat (map nlVarPat names) Boxed placeHolderType
    eps_w_names = eps `zip` names
    to_body     = mkLHsTupleExpr [toEP   ep `mkHsApp` nlHsVar g | (ep,g) <- eps_w_names]
    from_body   = mkLHsTupleExpr [fromEP ep `mkHsApp` nlHsVar g | (ep,g) <- eps_w_names]

-------------------
-- bimapList :: EP a b -> EP [a] [b]
bimapList :: [EP (LHsExpr RdrName)] -> EP (LHsExpr RdrName)
bimapList [ep]
  = EP { fromEP = nlHsApp (nlHsVar map_RDR) (fromEP ep),
	 toEP   = nlHsApp (nlHsVar map_RDR) (toEP ep) }

-------------------
a_RDR, b_RDR :: RdrName
a_RDR	= mkVarUnqual (fsLit "a")
b_RDR	= mkVarUnqual (fsLit "b")

gs_RDR :: [RdrName]
gs_RDR	= [ mkVarUnqual (mkFastString ("g"++show i)) | i <- [(1::Int) .. ] ]

idEP :: EP (LHsExpr RdrName)
idEP = EP idexpr idexpr
     where
       idexpr = mkHsLam [nlVarPat a_RDR] (nlHsVar a_RDR)
\end{code}
