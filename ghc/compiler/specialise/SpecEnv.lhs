%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1996
%
\section[SpecEnv]{Specialisation info about an @Id@}

\begin{code}
#include "HsVersions.h"

module SpecEnv (
	SpecEnv(..), MatchEnv,
	nullSpecEnv, isNullSpecEnv,
	addOneToSpecEnv, lookupSpecEnv,
	specEnvToList
    ) where

import Ubiq

import MatchEnv
import Type		( matchTys, isTyVarTy )
import Usage		( UVar(..) )
\end{code}


A @SpecEnv@ holds details of an @Id@'s specialisations:

\begin{code}
type CoreExpr = GenCoreExpr Id Id TyVar Unique
type SpecEnv = MatchEnv [Type] CoreExpr
\end{code}

For example, if \tr{f}'s @SpecEnv@ contains the mapping:
\begin{verbatim}
	[List a, b]  ===>  (\d -> f' a b)
\end{verbatim}
then
\begin{verbatim}
	f (List Int) Bool d  ===>  f' Int Bool
\end{verbatim}

\begin{code}
nullSpecEnv :: SpecEnv
nullSpecEnv = nullMEnv

isNullSpecEnv :: SpecEnv -> Bool
isNullSpecEnv env = null (mEnvToList env)

specEnvToList :: SpecEnv -> [([Type],CoreExpr)]
specEnvToList env = mEnvToList env
	
addOneToSpecEnv :: SpecEnv -> [Type] -> CoreExpr -> MaybeErr SpecEnv ([Type], CoreExpr)
addOneToSpecEnv env tys rhs = insertMEnv matchTys env tys rhs

lookupSpecEnv :: SpecEnv -> [Type] -> Maybe (CoreExpr, [(TyVar,Type)])
lookupSpecEnv env tys 
  | all isTyVarTy tys = Nothing	-- Short cut: no specialisation for simple tyvars
  | otherwise	      = lookupMEnv matchTys env tys
\end{code}



=================================================================
	BELOW HERE SCHEDULED FOR DELETION!


The details of one specialisation, held in an @Id@'s
@SpecEnv@ are as follows:
\begin{pseudocode}
data SpecInfo
  = SpecInfo	[Maybe Type] -- Instance types; no free type variables in here
	    	Int		-- No. of dictionaries to eat
		Id		-- Specialised version
\end{pseudocode}

For example, if \tr{f} has this @SpecInfo@:
\begin{verbatim}
	SpecInfo [Just t1, Nothing, Just t3] 2 f'
\end{verbatim}
then
\begin{verbatim}
	f t1 t2 t3 d1 d2  ===>  f t2
\end{verbatim}
The \tr{Nothings} identify type arguments in which the specialised
version is polymorphic.

\begin{pseudocode}
data SpecEnv = SpecEnv [SpecInfo]

mkSpecEnv = SpecEnv
nullSpecEnv = SpecEnv []
addOneToSpecEnv (SpecEnv xs) x = SpecEnv (x : xs)


lookupConstMethodId :: Id -> Type -> Maybe Id
    -- slight variant on "lookupSpecEnv" below

lookupConstMethodId sel_id spec_ty
  = case (getInfo (getIdInfo sel_id)) of
      SpecEnv spec_infos -> firstJust (map try spec_infos)
  where
    try (SpecInfo (Just ty:nothings) _ const_meth_id)
      = ASSERT(all nothing_is_nothing nothings)
	case (cmpType True{-properly-} ty spec_ty) of
	  EQ_ -> Just const_meth_id
	  _   -> Nothing

    nothing_is_nothing Nothing = True  -- debugging only
    nothing_is_nothing _ = panic "nothing_is_nothing!"

lookupSpecId :: Id		-- *un*specialised Id
	     -> [Maybe Type]	-- types to which it is to be specialised
	     -> Id		-- specialised Id

lookupSpecId unspec_id ty_maybes
  = case (getInfo (getIdInfo unspec_id))  of { SpecEnv spec_infos ->

    case (firstJust (map try spec_infos)) of
      Just id -> id
      Nothing -> error ("ERROR: There is some confusion about a value specialised to a type;\ndetails follow (and more info in the User's Guide):\n\t"++(ppShow 80 (ppr PprDebug unspec_id)))
    }
  where
    try (SpecInfo template_maybes _ id)
	| and (zipWith same template_maybes ty_maybes)
	&& length template_maybes == length ty_maybes = Just id
	| otherwise 				      = Nothing

    same Nothing    Nothing    = True
    same (Just ty1) (Just ty2) = ty1 == ty2
    same _	    _	       = False

lookupSpecEnv :: SpecEnv
	      -> [Type]
	      -> Maybe (Id,
			[Type],
			Int)

lookupSpecEnv (SpecEnv []) _ = Nothing 	-- rather common case

lookupSpecEnv spec_env [] = Nothing	-- another common case

	-- This can happen even if there is a non-empty spec_env, because
	-- of eta reduction.  For example, we might have a defn
	--
	--	f = /\a -> \d -> g a d
	-- which gets transformed to
	--	f = g
	--
	-- Now g isn't applied to any arguments

lookupSpecEnv se@(SpecEnv spec_infos) spec_tys
  = select_match spec_infos
  where
    select_match []		-- no matching spec_infos
      = Nothing
    select_match (SpecInfo ty_maybes toss spec_id : rest)
      = case (match ty_maybes spec_tys) of
	  Nothing       -> select_match rest
	  Just tys_left -> select_next [(spec_id,tys_left,toss)] (length tys_left) toss rest

	-- Ambiguity can only arise as a result of specialisations with
	-- an explicit spec_id. The best match is deemed to be the match
	-- with least polymorphism i.e. has the least number of tys left.
	-- This is a non-critical approximation. The only type arguments
	-- where there may be some discretion is for non-overloaded boxed
	-- types. Unboxed types must be matched and we insist that we
	-- always specialise on overloaded types (and discard all the dicts).

    select_next best _ toss []
      =	case best of
	    [match] -> Just match	-- Unique best match
	    ambig   -> pprPanic "Ambiguous Specialisation:\n"
				(ppAboves [ppStr "(check specialisations with explicit spec ids)",
					   ppCat (ppStr "between spec ids:" :
						  map (ppr PprDebug) [id | (id, _, _) <- ambig]),
					   pp_stuff])

    select_next best tnum dnum (SpecInfo ty_maybes toss spec_id : rest)
      = ASSERT(dnum == toss)
	case (match ty_maybes spec_tys) of
	  Nothing       -> select_next best tnum dnum rest
	  Just tys_left ->
	     let tys_len = length tys_left in
	     case _tagCmp tnum tys_len of
	       _LT -> select_next [(spec_id,tys_left,toss)] tys_len dnum rest	-- better match
	       _EQ -> select_next ((spec_id,tys_left,toss):best) tnum dnum rest	-- equivalent match
	       _GT -> select_next best tnum dnum rest				-- worse match


    match [{-out of templates-}] [] = Just []

    match (Nothing:ty_maybes) (spec_ty:spec_tys)
      = case (isUnboxedDataType spec_ty) of
	  True  -> Nothing	-- Can only match boxed type against
				-- type argument which has not been
				-- specialised on
	  False -> case match ty_maybes spec_tys of
		     Nothing  -> Nothing
		     Just tys -> Just (spec_ty:tys)

    match (Just ty:ty_maybes) (spec_ty:spec_tys)
      = case (cmpType True{-properly-} ty spec_ty) of
	  EQ_   -> match ty_maybes spec_tys
	  other -> Nothing

    match [] _ = pprPanic "lookupSpecEnv1\n" pp_stuff
		 -- This is a Real Problem

    match _ [] = pprPanic "lookupSpecEnv2\n" pp_stuff
		 -- Partial eta abstraction might make this happen;
		 -- meanwhile let's leave in the check

    pp_stuff = ppAbove (pp_specs PprDebug True (\x->x) nullIdEnv se) (ppr PprDebug spec_tys)
\end{pseudocode}


\begin{pseudocode}
instance OptIdInfo SpecEnv where
    noInfo = nullSpecEnv

    getInfo (IdInfo _ _ spec _ _ _ _ _ _ _) = spec

    addInfo (IdInfo a b (SpecEnv old_spec) d e f g h i j) (SpecEnv new_spec)
	= IdInfo a b (SpecEnv (new_spec ++ old_spec)) d e f g h i j
	-- We *add* the new specialisation info rather than just replacing it
	-- so that we don't lose old specialisation details.

    ppInfo sty better_id_fn spec_env
      = pp_specs sty True better_id_fn nullIdEnv spec_env

pp_specs sty _ _ _ (SpecEnv [])  = pp_NONE
pp_specs sty print_spec_ids better_id_fn inline_env (SpecEnv specs)
  = ppBeside (ppPStr SLIT("_SPECIALISE_ ")) (pp_the_list [
       ppCat [ppLbrack, ppIntersperse pp'SP{-'-} (map pp_maybe ty_maybes), ppRbrack,
	      ppInt numds,
	      let
		 better_spec_id = better_id_fn spec_id
		 spec_id_info = getIdInfo better_spec_id
	      in
	      if not print_spec_ids || boringIdInfo spec_id_info then
		 ppNil
	      else
		 ppCat [ppChar '{',
			ppIdInfo sty better_spec_id True{-wrkr specs too!-} better_id_fn inline_env spec_id_info,
			ppChar '}']
	     ]
       | (SpecInfo ty_maybes numds spec_id) <- specs ])
  where
    pp_the_list [p]    = p
    pp_the_list (p:ps) = ppBesides [p, pp'SP{-'-}, pp_the_list ps]

    pp_maybe Nothing  = ifPprInterface sty pp_NONE
    pp_maybe (Just t) = pprParendType sty t
\end{pseudocode}

