%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

Monadic type operations

This module contains monadic operations over types that contain
mutable type variables

\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module TcMType (
  TcTyVar, TcKind, TcType, TcTauType, TcThetaType, TcTyVarSet,

  --------------------------------
  -- Creating new mutable type variables
  newFlexiTyVar,
  newFlexiTyVarTy,		-- Kind -> TcM TcType
  newFlexiTyVarTys,		-- Int -> Kind -> TcM [TcType]
  newPolyFlexiTyVarTy,
  newMetaKindVar, newMetaKindVars, mkKindSigVar,
  mkTcTyVarName, cloneMetaTyVar, 

  newMetaTyVar, readMetaTyVar, writeMetaTyVar, writeMetaTyVarRef,
  newMetaDetails, isFilledMetaTyVar, isFlexiMetaTyVar,

  --------------------------------
  -- Creating new evidence variables
  newEvVar, newEvVars, newEq, newDict,
  newWantedEvVar, newWantedEvVars,
  newTcEvBinds, addTcEvBind,
  newFlatWanteds,

  --------------------------------
  -- Instantiation
  tcInstTyVars, tcInstSigTyVars, newSigTyVar,
  tcInstType, 
  tcInstSkolTyVars, tcInstSkolTyVarsLoc, tcInstSuperSkolTyVars,
  tcInstSkolTyVarsX, tcInstSuperSkolTyVarsX,
  tcInstSkolTyVar, tcInstSkolType,
  tcSkolDFunType, tcSuperSkolTyVars,

  --------------------------------
  -- Zonking
  zonkTcPredType, 
  skolemiseSigTv, skolemiseUnboundMetaTyVar,
  zonkTcTyVar, zonkTcTyVars, zonkTyVarsAndFV, 
  zonkQuantifiedTyVar, zonkQuantifiedTyVars,
  zonkTcType, zonkTcTypes, zonkTcThetaType,

  zonkTcKind, defaultKindVarToStar,
  zonkEvVar, zonkWC, zonkId, zonkCt, zonkCts, zonkSkolemInfo,

  tcGetGlobalTyVars, 
  ) where

#include "HsVersions.h"

-- friends:
import TypeRep
import TcType
import TcEvidence
import Type
import Class
import TyCon
import Var

-- others:
import TcRnMonad        -- TcType, amongst others
import Id
import Name
import VarSet
import PrelNames
import DynFlags
import Util
import Outputable
import FastString
import SrcLoc
import Bag

import Control.Monad
import Data.List        ( partition, mapAccumL )
\end{code}


%************************************************************************
%*									*
	Kind variables
%*									*
%************************************************************************

\begin{code}
mkKindName :: Unique -> Name
mkKindName unique = mkSystemName unique kind_var_occ

kind_var_occ :: OccName	-- Just one for all MetaKindVars
			-- They may be jiggled by tidying
kind_var_occ = mkOccName tvName "k"

newMetaKindVar :: TcM TcKind
newMetaKindVar = do { uniq <- newUnique
		    ; details <- newMetaDetails TauTv
                    ; let kv = mkTcTyVar (mkKindName uniq) superKind details
		    ; return (mkTyVarTy kv) }

newMetaKindVars :: Int -> TcM [TcKind]
newMetaKindVars n = mapM (\ _ -> newMetaKindVar) (nOfThem n ())

mkKindSigVar :: Name -> KindVar
-- Use the specified name; don't clone it
mkKindSigVar n = mkTcTyVar n superKind (SkolemTv False)
\end{code}


%************************************************************************
%*									*
     Evidence variables; range over constraints we can abstract over
%*									*
%************************************************************************

\begin{code}
newEvVars :: TcThetaType -> TcM [EvVar]
newEvVars theta = mapM newEvVar theta

newWantedEvVar :: TcPredType -> TcM EvVar 
newWantedEvVar = newEvVar

newWantedEvVars :: TcThetaType -> TcM [EvVar] 
newWantedEvVars theta = mapM newWantedEvVar theta 

--------------

newEvVar :: TcPredType -> TcM EvVar
-- Creates new *rigid* variables for predicates
newEvVar ty = do { name <- newSysName (predTypeOccName ty) 
                 ; return (mkLocalId name ty) }

newEq :: TcType -> TcType -> TcM EvVar
newEq ty1 ty2
  = do { name <- newSysName (mkVarOccFS (fsLit "cobox"))
       ; return (mkLocalId name (mkTcEqPred ty1 ty2)) }

newDict :: Class -> [TcType] -> TcM DictId
newDict cls tys 
  = do { name <- newSysName (mkDictOcc (getOccName cls))
       ; return (mkLocalId name (mkClassPred cls tys)) }

predTypeOccName :: PredType -> OccName
predTypeOccName ty = case classifyPredType ty of
    ClassPred cls _ -> mkDictOcc (getOccName cls)
    EqPred _ _      -> mkVarOccFS (fsLit "cobox")
    TuplePred _     -> mkVarOccFS (fsLit "tup")
    IrredPred _     -> mkVarOccFS (fsLit "irred")
\end{code}

*********************************************************************************
*                                                                               * 
*                   Wanted constraints
*                                                                               *
*********************************************************************************

\begin{code}
newFlatWanteds :: CtOrigin -> ThetaType -> TcM [Ct]
newFlatWanteds orig theta
  = do { loc <- getCtLoc orig
       ; mapM (inst_to_wanted loc) theta }
  where 
    inst_to_wanted loc pty 
          = do { v <- newWantedEvVar pty 
               ; return $ mkNonCanonical loc $
                 CtWanted { ctev_evar = v
                          , ctev_pred = pty } }
\end{code}

%************************************************************************
%*									*
	SkolemTvs (immutable)
%*									*
%************************************************************************

\begin{code}
tcInstType :: ([TyVar] -> TcM (TvSubst, [TcTyVar]))     -- How to instantiate the type variables
	   -> TcType 					-- Type to instantiate
	   -> TcM ([TcTyVar], TcThetaType, TcType)	-- Result
		-- (type vars (excl coercion vars), preds (incl equalities), rho)
tcInstType inst_tyvars ty
  = case tcSplitForAllTys ty of
	([],     rho) -> let	-- There may be overloading despite no type variables;
				-- 	(?x :: Int) => Int -> Int
			   (theta, tau) = tcSplitPhiTy rho
			 in
			 return ([], theta, tau)

	(tyvars, rho) -> do { (subst, tyvars') <- inst_tyvars tyvars
			    ; let (theta, tau) = tcSplitPhiTy (substTy subst rho)
			    ; return (tyvars', theta, tau) }

tcSkolDFunType :: Type -> TcM ([TcTyVar], TcThetaType, TcType)
-- Instantiate a type signature with skolem constants, but 
-- do *not* give them fresh names, because we want the name to
-- be in the type environment: it is lexically scoped.
tcSkolDFunType ty = tcInstType (\tvs -> return (tcSuperSkolTyVars tvs)) ty

tcSuperSkolTyVars :: [TyVar] -> (TvSubst, [TcTyVar])
-- Make skolem constants, but do *not* give them new names, as above
-- Moreover, make them "super skolems"; see comments with superSkolemTv
-- see Note [Kind substitution when instantiating]
-- Precondition: tyvars should be ordered (kind vars first)
tcSuperSkolTyVars = mapAccumL tcSuperSkolTyVar (mkTopTvSubst [])

tcSuperSkolTyVar :: TvSubst -> TyVar -> (TvSubst, TcTyVar)
tcSuperSkolTyVar subst tv
  = (extendTvSubst subst tv (mkTyVarTy new_tv), new_tv)
  where
    kind   = substTy subst (tyVarKind tv)
    new_tv = mkTcTyVar (tyVarName tv) kind superSkolemTv

tcInstSkolTyVar :: SrcSpan -> Bool -> TvSubst -> TyVar
                -> TcRnIf gbl lcl (TvSubst, TcTyVar)
-- Instantiate the tyvar, using 
--      * the occ-name and kind of the supplied tyvar, 
--      * the unique from the monad,
--      * the location either from the tyvar (skol_info = SigSkol)
--                     or from the monad (otherwise)
tcInstSkolTyVar loc overlappable subst tyvar
  = do  { uniq <- newUnique
        ; let new_name = mkInternalName uniq occ loc
              new_tv   = mkTcTyVar new_name kind (SkolemTv overlappable)
        ; return (extendTvSubst subst tyvar (mkTyVarTy new_tv), new_tv) }
  where
    old_name = tyVarName tyvar
    occ      = nameOccName old_name
    kind     = substTy subst (tyVarKind tyvar)

-- Wrappers
-- we need to be able to do this from outside the TcM monad:
tcInstSkolTyVarsLoc :: SrcSpan -> [TyVar] -> TcRnIf gbl lcl (TvSubst, [TcTyVar])
tcInstSkolTyVarsLoc loc = mapAccumLM (tcInstSkolTyVar loc False) (mkTopTvSubst [])

tcInstSkolTyVars :: [TyVar] -> TcM (TvSubst, [TcTyVar])
tcInstSkolTyVars = tcInstSkolTyVarsX (mkTopTvSubst [])

tcInstSuperSkolTyVars :: [TyVar] -> TcM [TcTyVar]
tcInstSuperSkolTyVars = fmap snd . tcInstSkolTyVars' True  (mkTopTvSubst [])

tcInstSkolTyVarsX, tcInstSuperSkolTyVarsX
  :: TvSubst -> [TyVar] -> TcM (TvSubst, [TcTyVar])
tcInstSkolTyVarsX      subst = tcInstSkolTyVars' False subst
tcInstSuperSkolTyVarsX subst = tcInstSkolTyVars' True  subst

tcInstSkolTyVars' :: Bool -> TvSubst -> [TyVar] -> TcM (TvSubst, [TcTyVar])
-- Precondition: tyvars should be ordered (kind vars first)
-- see Note [Kind substitution when instantiating]
tcInstSkolTyVars' isSuperSkol subst tvs
  = do { loc <- getSrcSpanM
       ; mapAccumLM (tcInstSkolTyVar loc isSuperSkol) subst tvs }

tcInstSkolType :: TcType -> TcM ([TcTyVar], TcThetaType, TcType)
-- Instantiate a type with fresh skolem constants
-- Binding location comes from the monad
tcInstSkolType ty = tcInstType tcInstSkolTyVars ty

tcInstSigTyVars :: [TyVar] -> TcM (TvSubst, [TcTyVar])
-- Make meta SigTv type variables for patten-bound scoped type varaibles
-- We use SigTvs for them, so that they can't unify with arbitrary types
-- Precondition: tyvars should be ordered (kind vars first)
-- see Note [Kind substitution when instantiating]
tcInstSigTyVars = mapAccumLM tcInstSigTyVar (mkTopTvSubst [])
	-- The tyvars are freshly made, by tcInstSigTyVar
        -- So mkTopTvSubst [] is ok

tcInstSigTyVar :: TvSubst -> TyVar -> TcM (TvSubst, TcTyVar)
tcInstSigTyVar subst tv
  = do { new_tv <- newSigTyVar (tyVarName tv) (substTy subst (tyVarKind tv))
       ; return (extendTvSubst subst tv (mkTyVarTy new_tv), new_tv) }

newSigTyVar :: Name -> Kind -> TcM TcTyVar
newSigTyVar name kind
  = do { uniq <- newUnique
       ; let name' = setNameUnique name uniq
                      -- Use the same OccName so that the tidy-er
                      -- doesn't gratuitously rename 'a' to 'a0' etc
       ; details <- newMetaDetails SigTv
       ; return (mkTcTyVar name' kind details) }

newMetaDetails :: MetaInfo -> TcM TcTyVarDetails
newMetaDetails info 
  = do { ref <- newMutVar Flexi
       ; untch <- getUntouchables
       ; return (MetaTv { mtv_info = info, mtv_ref = ref, mtv_untch = untch }) }
\end{code}

Note [Kind substitution when instantiating]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we instantiate a bunch of kind and type variables, first we
expect them to be sorted (kind variables first, then type variables).
Then we have to instantiate the kind variables, build a substitution
from old variables to the new variables, then instantiate the type
variables substituting the original kind.

Exemple: If we want to instantiate
  [(k1 :: BOX), (k2 :: BOX), (a :: k1 -> k2), (b :: k1)]
we want
  [(?k1 :: BOX), (?k2 :: BOX), (?a :: ?k1 -> ?k2), (?b :: ?k1)]
instead of the buggous
  [(?k1 :: BOX), (?k2 :: BOX), (?a :: k1 -> k2), (?b :: k1)]


%************************************************************************
%*									*
	MetaTvs (meta type variables; mutable)
%*									*
%************************************************************************

\begin{code}
newMetaTyVar :: MetaInfo -> Kind -> TcM TcTyVar
-- Make a new meta tyvar out of thin air
newMetaTyVar meta_info kind
  = do	{ uniq <- newUnique
        ; let name = mkTcTyVarName uniq s
              s = case meta_info of
                        PolyTv -> fsLit "s"
                        TauTv  -> fsLit "t"
                        SigTv  -> fsLit "a"
        ; details <- newMetaDetails meta_info
	; return (mkTcTyVar name kind details) }

cloneMetaTyVar :: TcTyVar -> TcM TcTyVar
cloneMetaTyVar tv
  = ASSERT( isTcTyVar tv )
    do	{ uniq <- newUnique
        ; ref  <- newMutVar Flexi
        ; let name'    = setNameUnique (tyVarName tv) uniq
              details' = case tcTyVarDetails tv of 
                           details@(MetaTv {}) -> details { mtv_ref = ref }
                           _ -> pprPanic "cloneMetaTyVar" (ppr tv)
        ; return (mkTcTyVar name' (tyVarKind tv) details') }

mkTcTyVarName :: Unique -> FastString -> Name
-- Make sure that fresh TcTyVar names finish with a digit
-- leaving the un-cluttered names free for user names
mkTcTyVarName uniq str = mkSysTvName uniq str

-- Works for both type and kind variables
readMetaTyVar :: TyVar -> TcM MetaDetails
readMetaTyVar tyvar = ASSERT2( isMetaTyVar tyvar, ppr tyvar )
		      readMutVar (metaTvRef tyvar)

isFilledMetaTyVar :: TyVar -> TcM Bool
-- True of a filled-in (Indirect) meta type variable
isFilledMetaTyVar tv
  | not (isTcTyVar tv) = return False
  | MetaTv { mtv_ref = ref } <- tcTyVarDetails tv
  = do 	{ details <- readMutVar ref
	; return (isIndirect details) }
  | otherwise = return False

isFlexiMetaTyVar :: TyVar -> TcM Bool
-- True of a un-filled-in (Flexi) meta type variable
isFlexiMetaTyVar tv
  | not (isTcTyVar tv) = return False
  | MetaTv { mtv_ref = ref } <- tcTyVarDetails tv
  = do 	{ details <- readMutVar ref
	; return (isFlexi details) }
  | otherwise = return False

--------------------
-- Works with both type and kind variables
writeMetaTyVar :: TcTyVar -> TcType -> TcM ()
-- Write into a currently-empty MetaTyVar

writeMetaTyVar tyvar ty
  | not debugIsOn 
  = writeMetaTyVarRef tyvar (metaTvRef tyvar) ty

-- Everything from here on only happens if DEBUG is on
  | not (isTcTyVar tyvar)
  = WARN( True, text "Writing to non-tc tyvar" <+> ppr tyvar )
    return ()

  | MetaTv { mtv_ref = ref } <- tcTyVarDetails tyvar
  = writeMetaTyVarRef tyvar ref ty

  | otherwise
  = WARN( True, text "Writing to non-meta tyvar" <+> ppr tyvar )
    return ()

--------------------
writeMetaTyVarRef :: TcTyVar -> TcRef MetaDetails -> TcType -> TcM ()
-- Here the tyvar is for error checking only; 
-- the ref cell must be for the same tyvar
writeMetaTyVarRef tyvar ref ty
  | not debugIsOn 
  = do { traceTc "writeMetaTyVar" (ppr tyvar <+> text ":=" <+> ppr ty)
       ; writeMutVar ref (Indirect ty) }

-- Everything from here on only happens if DEBUG is on
  | otherwise
  = do { meta_details <- readMutVar ref; 
       -- Zonk kinds to allow the error check to work
       ; zonked_tv_kind <- zonkTcKind tv_kind 
       ; zonked_ty_kind <- zonkTcKind ty_kind

       -- Check for double updates
       ; ASSERT2( isFlexi meta_details, 
                  hang (text "Double update of meta tyvar")
                   2 (ppr tyvar $$ ppr meta_details) )

         traceTc "writeMetaTyVar" (ppr tyvar <+> text ":=" <+> ppr ty)
       ; writeMutVar ref (Indirect ty) 
       ; when (   not (isPredTy tv_kind) 
                    -- Don't check kinds for updates to coercion variables
               && not (zonked_ty_kind `tcIsSubKind` zonked_tv_kind))
       $ WARN( True, hang (text "Ill-kinded update to meta tyvar")
                        2 (    ppr tyvar <+> text "::" <+> ppr tv_kind 
                           <+> text ":=" 
                           <+> ppr ty    <+> text "::" <+> ppr ty_kind) )
         (return ()) }
  where
    tv_kind = tyVarKind tyvar
    ty_kind = typeKind ty
\end{code}


%************************************************************************
%*									*
	MetaTvs: TauTvs
%*									*
%************************************************************************

\begin{code}
newFlexiTyVar :: Kind -> TcM TcTyVar
newFlexiTyVar kind = newMetaTyVar TauTv kind

newFlexiTyVarTy  :: Kind -> TcM TcType
newFlexiTyVarTy kind = do
    tc_tyvar <- newFlexiTyVar kind
    return (TyVarTy tc_tyvar)

newFlexiTyVarTys :: Int -> Kind -> TcM [TcType]
newFlexiTyVarTys n kind = mapM newFlexiTyVarTy (nOfThem n kind)

newPolyFlexiTyVarTy :: TcM TcType
newPolyFlexiTyVarTy = do { tv <- newMetaTyVar PolyTv liftedTypeKind
                         ; return (TyVarTy tv) }

tcInstTyVars :: [TKVar] -> TcM ([TcTyVar], [TcType], TvSubst)
-- Instantiate with META type variables
-- Note that this works for a sequence of kind and type
-- variables.  Eg    [ (k:BOX), (a:k->k) ]
--             Gives [ (k7:BOX), (a8:k7->k7) ]
tcInstTyVars tyvars = tcInstTyVarsX emptyTvSubst tyvars
    -- emptyTvSubst has an empty in-scope set, but that's fine here
    -- Since the tyvars are freshly made, they cannot possibly be
    -- captured by any existing for-alls.

tcInstTyVarsX :: TvSubst -> [TKVar] -> TcM ([TcTyVar], [TcType], TvSubst)
-- The "X" part is because of extending the substitution
tcInstTyVarsX subst tyvars =
  do { (subst', tyvars') <- mapAccumLM tcInstTyVarX subst tyvars
     ; return (tyvars', mkTyVarTys tyvars', subst') }

tcInstTyVarX :: TvSubst -> TKVar -> TcM (TvSubst, TcTyVar)
-- Make a new unification variable tyvar whose Name and Kind come from
-- an existing TyVar. We substitute kind variables in the kind.
tcInstTyVarX subst tyvar
  = do  { uniq <- newUnique
        ; details <- newMetaDetails TauTv
        ; let name   = mkSystemName uniq (getOccName tyvar)
              kind   = substTy subst (tyVarKind tyvar)
              new_tv = mkTcTyVar name kind details 
        ; return (extendTvSubst subst tyvar (mkTyVarTy new_tv), new_tv) }
\end{code}


%************************************************************************
%*									*
\subsection{Zonking -- the exernal interfaces}
%*									*
%************************************************************************

@tcGetGlobalTyVars@ returns a fully-zonked set of tyvars free in the environment.
To improve subsequent calls to the same function it writes the zonked set back into
the environment.

\begin{code}
tcGetGlobalTyVars :: TcM TcTyVarSet
tcGetGlobalTyVars
  = do { (TcLclEnv {tcl_tyvars = gtv_var}) <- getLclEnv
       ; gbl_tvs  <- readMutVar gtv_var
       ; gbl_tvs' <- zonkTyVarsAndFV gbl_tvs
       ; writeMutVar gtv_var gbl_tvs'
       ; return gbl_tvs' }
  where
\end{code}

-----------------  Type variables

\begin{code}
zonkTyVar :: TyVar -> TcM TcType
-- Works on TyVars and TcTyVars
zonkTyVar tv | isTcTyVar tv = zonkTcTyVar tv
             | otherwise    = return (mkTyVarTy tv)
   -- Hackily, when typechecking type and class decls
   -- we have TyVars in scopeadded (only) in 
   -- TcHsType.tcTyClTyVars, but it seems
   -- painful to make them into TcTyVars there

zonkTyVarsAndFV :: TyVarSet -> TcM TyVarSet
zonkTyVarsAndFV tyvars = tyVarsOfTypes <$> mapM zonkTyVar (varSetElems tyvars)

zonkTcTyVars :: [TcTyVar] -> TcM [TcType]
zonkTcTyVars tyvars = mapM zonkTcTyVar tyvars

-----------------  Types
zonkTyVarKind :: TyVar -> TcM TyVar
zonkTyVarKind tv = do { kind' <- zonkTcKind (tyVarKind tv)
                      ; return (setTyVarKind tv kind') }

zonkTcTypes :: [TcType] -> TcM [TcType]
zonkTcTypes tys = mapM zonkTcType tys

zonkTcThetaType :: TcThetaType -> TcM TcThetaType
zonkTcThetaType theta = mapM zonkTcPredType theta

zonkTcPredType :: TcPredType -> TcM TcPredType
zonkTcPredType = zonkTcType
\end{code}

-------------------  These ...ToType, ...ToKind versions
		     are used at the end of type checking

\begin{code}
defaultKindVarToStar :: TcTyVar -> TcM Kind
-- We have a meta-kind: unify it with '*'
defaultKindVarToStar kv 
  = do { ASSERT ( isKindVar kv && isMetaTyVar kv )
         writeMetaTyVar kv liftedTypeKind
       ; return liftedTypeKind }

zonkQuantifiedTyVars :: [TcTyVar] -> TcM [TcTyVar]
-- A kind variable k may occur *after* a tyvar mentioning k in its kind
-- Can be given a mixture of TcTyVars and TyVars, in the case of
-- associated type declarations
zonkQuantifiedTyVars tyvars
  = do { let (kvs, tvs) = partition isKindVar tyvars
             (meta_kvs, skolem_kvs) 
                  = partition (\kv -> isTcTyVar kv && isMetaTyVar kv) kvs

             -- In the non-PolyKinds case, default the kind variables
             -- to *, and zonk the tyvars as usual.  Notice that this
             -- may make zonkQuantifiedTyVars return a shorter list
             -- than it was passed, but that's ok
       ; poly_kinds <- xoptM Opt_PolyKinds
       ; qkvs <- if poly_kinds 
                 then return kvs
                 else WARN ( not (null skolem_kvs), ppr skolem_kvs )
                      do { mapM_ defaultKindVarToStar meta_kvs
                         ; return skolem_kvs }  -- Should be empty

       ; mapM zonk_quant (qkvs ++ tvs) }
           -- Because of the order, any kind variables
           -- mentioned in the kinds of the type variables refer to
           -- the now-quantified versions
  where
    zonk_quant tkv
      | isTcTyVar tkv = zonkQuantifiedTyVar tkv
      | otherwise     = return tkv
      -- For associated types, we have the class variables 
      -- in scope, and they are TyVars not TcTyVars

zonkQuantifiedTyVar :: TcTyVar -> TcM TcTyVar
-- The quantified type variables often include meta type variables
-- we want to freeze them into ordinary type variables, and
-- default their kind (e.g. from OpenTypeKind to TypeKind)
-- 			-- see notes with Kind.defaultKind
-- The meta tyvar is updated to point to the new skolem TyVar.  Now any 
-- bound occurences of the original type variable will get zonked to 
-- the immutable version.
--
-- We leave skolem TyVars alone; they are immutable.
--
-- This function is called on both kind and type variables,
-- but kind variables *only* if PolyKinds is on.
zonkQuantifiedTyVar tv
  = ASSERT2( isTcTyVar tv, ppr tv ) 
    case tcTyVarDetails tv of
      SkolemTv {} -> do { kind <- zonkTcKind (tyVarKind tv)
                        ; return $ setTyVarKind tv kind }
	-- It might be a skolem type variable, 
	-- for example from a user type signature

      MetaTv { mtv_ref = ref } ->
          do when debugIsOn $ do
                 -- [Sept 04] Check for non-empty.
                 -- See note [Silly Type Synonym]
                 cts <- readMutVar ref
                 case cts of
                     Flexi -> return ()
                     Indirect ty -> WARN( True, ppr tv $$ ppr ty )
                                    return ()
             skolemiseUnboundMetaTyVar tv vanillaSkolemTv
      _other -> pprPanic "zonkQuantifiedTyVar" (ppr tv) -- FlatSkol, RuntimeUnk

skolemiseUnboundMetaTyVar :: TcTyVar -> TcTyVarDetails -> TcM TyVar
-- We have a Meta tyvar with a ref-cell inside it
-- Skolemise it, including giving it a new Name, so that
--   we are totally out of Meta-tyvar-land
-- We create a skolem TyVar, not a regular TyVar
--   See Note [Zonking to Skolem]
skolemiseUnboundMetaTyVar tv details
  = ASSERT2( isMetaTyVar tv, ppr tv ) 
    do  { span <- getSrcSpanM    -- Get the location from "here"
                                 -- ie where we are generalising
        ; uniq <- newUnique      -- Remove it from TcMetaTyVar unique land
        ; kind <- zonkTcKind (tyVarKind tv)
        ; let final_kind = defaultKind kind
              final_name = mkInternalName uniq (getOccName tv) span
              final_tv   = mkTcTyVar final_name final_kind details

        ; writeMetaTyVar tv (mkTyVarTy final_tv)
        ; return final_tv }

skolemiseSigTv :: TcTyVar -> TcM TcTyVar
-- In TcBinds we create SigTvs for type signatures
-- but for singleton groups we want them to really be skolems
-- which do not unify with each other
skolemiseSigTv tv  
  = ASSERT2( isSigTyVar tv, ppr tv )
    do { writeMetaTyVarRef tv (metaTvRef tv) (mkTyVarTy skol_tv)
       ; return skol_tv }
  where
    skol_tv = setTcTyVarDetails tv (SkolemTv False)
\end{code}

\begin{code}
zonkImplication :: Implication -> TcM (Bag Implication)
zonkImplication implic@(Implic { ic_untch  = untch
                               , ic_binds  = binds_var
                               , ic_skols  = skols
                               , ic_given  = given
                               , ic_wanted = wanted
                               , ic_info   = info })
  = do { skols'  <- mapM zonkTcTyVarBndr skols  -- Need to zonk their kinds!
                                                -- as Trac #7230 showed
       ; given'  <- mapM zonkEvVar given
       ; info'   <- zonkSkolemInfo info
       ; wanted' <- zonkWCRec binds_var untch wanted
       ; if isEmptyWC wanted' 
         then return emptyBag
         else return $ unitBag $
              implic { ic_fsks   = []  -- Zonking removes all FlatSkol tyvars
                     , ic_skols  = skols'
                     , ic_given  = given'
                     , ic_wanted = wanted'
                     , ic_info   = info' } }

zonkEvVar :: EvVar -> TcM EvVar
zonkEvVar var = do { ty' <- zonkTcType (varType var)
                   ; return (setVarType var ty') }


zonkWC :: EvBindsVar -- May add new bindings for wanted family equalities in here
       -> WantedConstraints -> TcM WantedConstraints
zonkWC binds_var wc
  = do { untch <- getUntouchables
       ; zonkWCRec binds_var untch wc }

zonkWCRec :: EvBindsVar
          -> Untouchables
          -> WantedConstraints -> TcM WantedConstraints
zonkWCRec binds_var untch (WC { wc_flat = flat, wc_impl = implic, wc_insol = insol })
  = do { flat'   <- zonkFlats binds_var untch flat
       ; implic' <- flatMapBagM zonkImplication implic
       ; insol'  <- zonkCts insol -- No need to do the more elaborate zonkFlats thing
       ; return (WC { wc_flat = flat', wc_impl = implic', wc_insol = insol' }) }

zonkFlats :: EvBindsVar -> Untouchables -> Cts -> TcM Cts
-- This zonks and unflattens a bunch of flat constraints
-- See Note [Unflattening while zonking]
zonkFlats binds_var untch cts
  = do { -- See Note [How to unflatten]
         cts <- foldrBagM unflatten_one emptyCts cts
       ; zonkCts cts }
  where
    unflatten_one orig_ct cts
      = do { zct <- zonkCt orig_ct                -- First we need to fully zonk 
           ; mct <- try_zonk_fun_eq orig_ct zct   -- Then try to solve if family equation
           ; return $ maybe cts (`consBag` cts) mct }

    try_zonk_fun_eq orig_ct zct   -- See Note [How to unflatten]
      | EqPred ty_lhs ty_rhs <- classifyPredType (ctPred zct)
          -- NB: zonking de-classifies the constraint,
          --     so we can't look for CFunEqCan
      , Just tv <- getTyVar_maybe ty_rhs
      , ASSERT2( not (isFloatedTouchableMetaTyVar untch tv), ppr tv )
        isTouchableMetaTyVar untch tv
      , not (isSigTyVar tv) || isTyVarTy ty_lhs     -- Never unify a SigTyVar with a non-tyvar
      , typeKind ty_lhs `tcIsSubKind` tyVarKind tv  -- c.f. TcInteract.trySpontaneousEqOneWay
      , not (tv `elemVarSet` tyVarsOfType ty_lhs)
      = ASSERT2( isWantedCt orig_ct, ppr orig_ct )
        ASSERT2( case tcSplitTyConApp_maybe ty_lhs of { Just (tc,_) -> isSynFamilyTyCon tc; _ -> False }, ppr orig_ct )
        do { writeMetaTyVar tv ty_lhs
           ; let evterm = EvCoercion (mkTcReflCo ty_lhs)
                 evvar  = ctev_evar (cc_ev zct)
           ; addTcEvBind binds_var evvar evterm
           ; traceTc "zonkFlats/unflattening" $
             vcat [ text "zct = " <+> ppr zct,
                    text "binds_var = " <+> ppr binds_var ]
           ; return Nothing }
      | otherwise
      = return (Just zct)
\end{code}

Note [Unflattening while zonking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A bunch of wanted constraints could contain wanted equations of the form
(F taus ~ alpha) where alpha is either an ordinary unification variable, or
a flatten unification variable.

These are ordinary wanted constraints and can/should be solved by
ordinary unification alpha := F taus. However the constraint solving
algorithm does not do that, as their 'inert' form is F taus ~ alpha.

Hence, we need an extra step to 'unflatten' these equations by
performing unification. This unification, if it happens at the end of
constraint solving, cannot produce any more interactions in the
constraint solver so it is safe to do it as the very very last step.

We choose therefore to do it during zonking, in the function
zonkFlats. This is in analgoy to the zonking of given flatten skolems
which are eliminated in favor of the underlying type that they are
equal to.

Note that, because we now have to affect *evidence* while zonking
(setting some evidence binds to identities), we have to pass to the
zonkWC function an evidence variable to collect all the extra
variables.

Note [How to unflatten]
~~~~~~~~~~~~~~~~~~~~~~~
How do we unflatten during zonking.  Consider a bunch of flat constraints.
Consider them one by one.  For each such constraint C
  * Zonk C (to apply current substitution)
  * If C is of form F tys ~ alpha, 
       where alpha is touchable
       and   alpha is not mentioned in tys
    then unify alpha := F tys
         and discard C

After processing all the flat constraints, zonk them again to propagate
the inforamtion from later ones to earlier ones.  Eg
  Start:  (F alpha ~ beta, G Int ~ alpha)
  Then we get beta := F alpha
              alpha := G Int
  but we must apply the second unification to the first constraint.


\begin{code}
zonkCts :: Cts -> TcM Cts
zonkCts = mapBagM zonkCt

zonkCt :: Ct -> TcM Ct
zonkCt ct@(CHoleCan { cc_ev = ev })
  = do { ev' <- zonkCtEvidence ev
       ; return $ ct { cc_ev = ev' } }
zonkCt ct
  = do { fl' <- zonkCtEvidence (cc_ev ct)
       ; return (CNonCanonical { cc_ev = fl'
                               , cc_loc = cc_loc ct }) }

zonkCtEvidence :: CtEvidence -> TcM CtEvidence
zonkCtEvidence ctev@(CtGiven { ctev_pred = pred }) 
  = do { pred' <- zonkTcType pred
       ; return (ctev { ctev_pred = pred'}) }
zonkCtEvidence ctev@(CtWanted { ctev_pred = pred })
  = do { pred' <- zonkTcType pred
       ; return (ctev { ctev_pred = pred' }) }
zonkCtEvidence ctev@(CtDerived { ctev_pred = pred })
  = do { pred' <- zonkTcType pred
       ; return (ctev { ctev_pred = pred' }) }

zonkSkolemInfo :: SkolemInfo -> TcM SkolemInfo
zonkSkolemInfo (SigSkol cx ty)  = do { ty' <- zonkTcType ty
                                     ; return (SigSkol cx ty') }
zonkSkolemInfo (InferSkol ntys) = do { ntys' <- mapM do_one ntys
                                     ; return (InferSkol ntys') }
  where
    do_one (n, ty) = do { ty' <- zonkTcType ty; return (n, ty') }
zonkSkolemInfo skol_info = return skol_info
\end{code}

Note [Silly Type Synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this:
	type C u a = u	-- Note 'a' unused

	foo :: (forall a. C u a -> C u a) -> u
	foo x = ...

	bar :: Num u => u
	bar = foo (\t -> t + t)

* From the (\t -> t+t) we get type  {Num d} =>  d -> d
  where d is fresh.

* Now unify with type of foo's arg, and we get:
	{Num (C d a)} =>  C d a -> C d a
  where a is fresh.

* Now abstract over the 'a', but float out the Num (C d a) constraint
  because it does not 'really' mention a.  (see exactTyVarsOfType)
  The arg to foo becomes
	\/\a -> \t -> t+t

* So we get a dict binding for Num (C d a), which is zonked to give
	a = ()
  [Note Sept 04: now that we are zonking quantified type variables
  on construction, the 'a' will be frozen as a regular tyvar on
  quantification, so the floated dict will still have type (C d a).
  Which renders this whole note moot; happily!]

* Then the \/\a abstraction has a zonked 'a' in it.

All very silly.   I think its harmless to ignore the problem.  We'll end up with
a \/\a in the final result but all the occurrences of a will be zonked to ()

Note [Zonking to Skolem]
~~~~~~~~~~~~~~~~~~~~~~~~
We used to zonk quantified type variables to regular TyVars.  However, this
leads to problems.  Consider this program from the regression test suite:

  eval :: Int -> String -> String -> String
  eval 0 root actual = evalRHS 0 root actual

  evalRHS :: Int -> a
  evalRHS 0 root actual = eval 0 root actual

It leads to the deferral of an equality (wrapped in an implication constraint)

  forall a. () => ((String -> String -> String) ~ a)

which is propagated up to the toplevel (see TcSimplify.tcSimplifyInferCheck).
In the meantime `a' is zonked and quantified to form `evalRHS's signature.
This has the *side effect* of also zonking the `a' in the deferred equality
(which at this point is being handed around wrapped in an implication
constraint).

Finally, the equality (with the zonked `a') will be handed back to the
simplifier by TcRnDriver.tcRnSrcDecls calling TcSimplify.tcSimplifyTop.
If we zonk `a' with a regular type variable, we will have this regular type
variable now floating around in the simplifier, which in many places assumes to
only see proper TcTyVars.

We can avoid this problem by zonking with a skolem.  The skolem is rigid
(which we require for a quantified variable), but is still a TcTyVar that the
simplifier knows how to deal with.


%************************************************************************
%*									*
\subsection{Zonking -- the main work-horses: zonkTcType, zonkTcTyVar}
%*									*
%*		For internal use only!					*
%*									*
%************************************************************************

\begin{code}
-- zonkId is used *during* typechecking just to zonk the Id's type
zonkId :: TcId -> TcM TcId
zonkId id
  = do { ty' <- zonkTcType (idType id)
       ; return (Id.setIdType id ty') }

-- For unbound, mutable tyvars, zonkType uses the function given to it
-- For tyvars bound at a for-all, zonkType zonks them to an immutable
--	type variable and zonks the kind too

zonkTcType :: TcType -> TcM TcType
zonkTcType ty
  = go ty
  where
    go (TyConApp tc tys) = do tys' <- mapM go tys
                              return (TyConApp tc tys')

    go (LitTy n)         = return (LitTy n)

    go (FunTy arg res)   = do arg' <- go arg
                              res' <- go res
                              return (FunTy arg' res')

    go (AppTy fun arg)   = do fun' <- go fun
                              arg' <- go arg
                              return (mkAppTy fun' arg')
		-- NB the mkAppTy; we might have instantiated a
		-- type variable to a type constructor, so we need
		-- to pull the TyConApp to the top.

	-- The two interesting cases!
    go (TyVarTy tyvar) | isTcTyVar tyvar = zonkTcTyVar tyvar
		       | otherwise	 = TyVarTy <$> updateTyVarKindM go tyvar
		-- Ordinary (non Tc) tyvars occur inside quantified types

    go (ForAllTy tv ty) = do { tv' <- zonkTcTyVarBndr tv
                             ; ty' <- go ty
                             ; return (ForAllTy tv' ty') }

zonkTcTyVarBndr :: TcTyVar -> TcM TcTyVar
-- A tyvar binder is never a unification variable (MetaTv),
-- rather it is always a skolems.  BUT it may have a kind 
-- that has not yet been zonked, and may include kind
-- unification variables.
zonkTcTyVarBndr tyvar
  = ASSERT2( isImmutableTyVar tyvar, ppr tyvar ) do
    updateTyVarKindM zonkTcType tyvar

zonkTcTyVar :: TcTyVar -> TcM TcType
-- Simply look through all Flexis
zonkTcTyVar tv
  = ASSERT2( isTcTyVar tv, ppr tv ) do
    case tcTyVarDetails tv of
      SkolemTv {}   -> zonk_kind_and_return
      RuntimeUnk {} -> zonk_kind_and_return
      FlatSkol ty   -> zonkTcType ty
      MetaTv { mtv_ref = ref }
         -> do { cts <- readMutVar ref
               ; case cts of
	            Flexi       -> zonk_kind_and_return
	            Indirect ty -> zonkTcType ty }
  where
    zonk_kind_and_return = do { z_tv <- zonkTyVarKind tv
                              ; return (TyVarTy z_tv) }
\end{code}



%************************************************************************
%*									*
			Zonking kinds
%*									*
%************************************************************************

\begin{code}
zonkTcKind :: TcKind -> TcM TcKind
zonkTcKind k = zonkTcType k
\end{code}
    


