%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1995
%
\section[Specialise]{Stamping out overloading, and (optionally) polymorphism}

\begin{code}
#include "HsVersions.h"

module SpecTyFuns (
	specialiseCallTys,
	ConstraintVector(..),
	mkConstraintVector,
	isUnboxedSpecialisation,

	specialiseConstrTys,
	mkSpecialisedCon,

	argTysMatchSpecTys_error,

	pprSpecErrs,

	Maybe(..), Pretty(..), UniType
    ) where

import AbsUniType
import Bag		( Bag, isEmptyBag, bagToList )
import FiniteMap	( FiniteMap, emptyFM, addListToFM_C,
			  keysFM, lookupWithDefaultFM
			)
import Id		( mkSameSpecCon, getIdUniType,
			  isDictFunId, isConstMethodId, Id )
import Maybes	
import Outputable
import Pretty
import Util
\end{code}

%************************************************************************
%*									*
\subsection[@specialiseTys@]{Determine specialising types}
%*									*
%************************************************************************

@specialiseCallTys@ works out which type args don't need to be specialised on,
based on flags, the overloading constraint vector, and the types.

\begin{code}
specialiseCallTys :: Bool 		-- Specialise on all type args
		  -> Bool 		-- Specialise on unboxed type args
		  -> Bool		-- Specialise on overloaded type args
		  -> ConstraintVector	-- Tells which type args are overloaded
		  -> [UniType]		-- Type args
		  -> [Maybe UniType]	-- Nothings replace non-specialised type args

specialiseCallTys True _ _ cvec tys
  = map Just tys
specialiseCallTys False spec_unboxed spec_overloading cvec tys
  = zipWith spec_ty_other cvec tys
  where
    spec_ty_other c ty | (spec_unboxed && isUnboxedDataType ty)
		         || (spec_overloading && c)
		         = Just ty
		       | otherwise
                         = Nothing

type ConstraintVector = [Bool]	-- True for constrained tyvar, false otherwise

mkConstraintVector :: [TyVarTemplate] 
		   -> [(Class,TyVarTemplate)]
	           -> ConstraintVector

mkConstraintVector tyvars class_tyvar_pairs
  = [tyvar `elem` constrained_tyvars | tyvar <- tyvars]
  where
    constrained_tyvars   = map snd class_tyvar_pairs 	-- May contain dups
\end{code}

\begin{code}
isUnboxedSpecialisation :: [Maybe UniType] -> Bool
isUnboxedSpecialisation tys
  = any is_unboxed tys
  where
    is_unboxed (Just ty) = isUnboxedDataType ty
    is_unboxed Nothing   = False
\end{code}

@specialiseConstrTys@ works out which type args don't need to be
specialised on. We only speciailise on unboxed types.

\begin{code}
specialiseConstrTys :: [UniType]
		    -> [Maybe UniType]

specialiseConstrTys tys
  = map maybe_unboxed_ty tys
  where
    maybe_unboxed_ty ty = case isUnboxedDataType ty of
			    True  -> Just ty
			    False -> Nothing
\end{code}

\begin{code}
mkSpecialisedCon :: Id -> [UniType] -> Id
mkSpecialisedCon con tys
  = if spec_reqd
    then mkSameSpecCon spec_tys con
    else con
  where
    spec_tys  = specialiseConstrTys tys
    spec_reqd = maybeToBool (firstJust spec_tys)
\end{code}

@argTysMatchSpecTys@ checks if a list of argument types is consistent
with a list of specialising types. An error message is returned if not.
\begin{code}
argTysMatchSpecTys_error :: [Maybe UniType]
			 -> [UniType] 
			 -> Maybe Pretty
argTysMatchSpecTys_error spec_tys arg_tys
  = if match spec_tys arg_tys
    then Nothing
    else Just (ppSep [ppStr "Spec and Arg Types Inconsistent:",
		      ppStr "spectys=", ppSep [pprMaybeTy PprDebug ty | ty <- spec_tys],
		      ppStr "argtys=", ppSep [pprParendUniType PprDebug ty | ty <- arg_tys]])
  where
    match (Nothing:spec_tys) (arg:arg_tys)
      = not (isUnboxedDataType arg) &&
        match spec_tys arg_tys
    match (Just spec:spec_tys) (arg:arg_tys)
      = case (cmpUniType True{-properly-} spec arg) of
          EQ_   -> match spec_tys arg_tys
          other -> False
    match [] [] = True
    match _  _  = False
\end{code}

@pprSpecErrs@ prints error and warning information
about imported specialisations which do not exist.

\begin{code}
pprSpecErrs :: PprStyle
	    -> (Bag (Id,[Maybe UniType]))	-- errors
	    -> (Bag (Id,[Maybe UniType]))	-- warnings
	    -> (Bag (TyCon,[Maybe UniType]))	-- errors
	    -> Pretty

pprSpecErrs sty spec_errs spec_warn spec_tyerrs
  | not any_errs && not any_warn
  = ppNil

  | otherwise
  = ppAboves [if any_errs then ppAboves [
 		  ppStr "SPECIALISATION ERRORS (Essential):",
		  ppAboves (map pp_module_errs use_modules),
		  ppStr "***"
                  ]
	      else
		  ppNil,
	      if any_warn then ppAboves [
 		  ppStr "SPECIALISATION MESSAGES (Desirable):",
		  ppAboves (map pp_module_warn use_modules),
		  ppStr "***"
                  ]
	      else
		  ppNil
	     ]
  where
    any_errs = not (isEmptyBag spec_errs) || not (isEmptyBag spec_tyerrs)
    any_warn = not (isEmptyBag spec_warn)

    mk_module_fm errs_bag
      = addListToFM_C (++) emptyFM errs_list
      where
        errs_list = map add_name (bagToList errs_bag)

    add_name (id, tys) = (mod, [(name, id, tys)])
		       where
			 (mod,name) = getOrigName id

    tyerrs_fm = mk_module_fm spec_tyerrs
    errs_fm   = mk_module_fm spec_errs
    warn_fm   = mk_module_fm spec_warn

    module_names   = concat [keysFM errs_fm, keysFM warn_fm, keysFM tyerrs_fm]
    sorted_modules = map head (equivClasses _CMP_STRING_ module_names)

	-- Ensure any dfun instance specialisations (module _NIL_) are printed last
	-- ToDo: Print instance specialisations with the instance module
	--       This requires the module which defined the instance to be known:
	--	 add_name could then extract the instance module for a dfun id
	--	 and pp_dfun made a special case of pp_err
    use_modules = if (head sorted_modules == _NIL_)
		  then tail sorted_modules ++ [_NIL_]
		  else sorted_modules


    pp_module_errs :: FAST_STRING -> Pretty
    pp_module_errs mod
      | have_errs && mod == _NIL_ 
	-- A _NIL_ module string corresponds to internal Ids
	-- The only ones for which call instances should arise are
	--   dfuns which correspond to instance specialisations
      = ASSERT (null mod_tyerrs)
        ppAboves [
	    ppStr "*** INSTANCES",
	    ppAboves (map (pp_dfun sty) mod_errs)
            ]

      | have_errs
      = ppAboves [
	    pp_module mod,
	    ppAboves (map (pp_err sty) mod_errs),
	    ppAboves (map (pp_tyerr sty) mod_tyerrs)
	    ]

      | otherwise
      = ppNil

      where
        mod_tyerrs = lookupWithDefaultFM tyerrs_fm [] mod
        mod_errs   = lookupWithDefaultFM errs_fm [] mod
	have_errs  = not (null mod_tyerrs) || not (null mod_errs)


    pp_module_warn :: FAST_STRING -> Pretty
    pp_module_warn mod
      | have_warn && mod == _NIL_
	-- A _NIL_ module string corresponds to internal Ids
	-- The only ones for which call instances should arise are
	--   dfuns which correspond to instance specialisations
      = ppAboves [
	    ppStr "*** INSTANCES",
	    ppAboves (map (pp_dfun sty) mod_warn)
            ]

      | have_warn
      = ppAboves [
	    pp_module mod,
	    ppAboves (map (pp_err sty) mod_warn)
            ]

      | otherwise
      = ppNil

      where
        mod_warn  = lookupWithDefaultFM warn_fm [] mod
        have_warn = not (null mod_warn)


pp_module mod
  = ppCat [ppStr "*** module", ppPStr mod, ppStr "***"]


pp_tyerr :: PprStyle -> (FAST_STRING, TyCon, [Maybe UniType]) -> Pretty

pp_tyerr sty (_, tycon, tys)
  = ppCat [ppStr "{-# SPECIALIZE data",
	   pprNonOp sty tycon, ppCat (map (pprParendUniType sty) spec_tys),
	   ppStr "#-}" ]
  where
    tvs = getTyConTyVarTemplates tycon
    (spec_args, tv_maybes) = unzip (map choose_ty (tvs `zip` tys))
    spec_tys = map (mkForallTy (catMaybes tv_maybes)) spec_args

    choose_ty (tv, Nothing) = (mkTyVarTemplateTy tv, Just tv)
    choose_ty (tv, Just ty) = (ty, Nothing)

pp_err sty (_, id, tys)
  = ppCat [ppStr "{-# SPECIALIZE",
	   pprNonOp sty id, ppStr "::",
	   pprUniType sty spec_ty,
	   ppStr "#-}" ]
  where
    spec_ty = specialiseTy (getIdUniType id) tys 100   -- HACK to drop all dicts!!!

pp_dfun sty (_, id, tys)
  | isDictFunId id
  = ppCat [ppStr "{-# SPECIALIZE instance",
	   pprUniType sty spec_ty,
	   ppStr "#-}" ]
  | isConstMethodId id
  = pp_comment sty "OVERLOADED METHOD" id spec_ty
  | otherwise
  = pp_comment sty "HELP ..." id spec_ty
  where
    spec_ty = specialiseTy (getIdUniType id) tys 100   -- HACK to drop all dicts!!!

pp_comment sty msg id spec_ty
  = ppCat [ppStr "{-", ppStr msg,
	   pprNonOp sty id, ppStr "::",
	   pprUniType sty spec_ty,
	   ppStr "-}" ]
\end{code}
