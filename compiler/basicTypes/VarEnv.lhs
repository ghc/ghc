%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module VarEnv (
        -- * Var, Id and TyVar environments (maps)
	VarEnv, IdEnv, TyVarEnv, CoVarEnv,
	
	-- ** Manipulating these environments
	emptyVarEnv, unitVarEnv, mkVarEnv,
	elemVarEnv, varEnvElts, varEnvKeys,
	extendVarEnv, extendVarEnv_C, extendVarEnv_Acc, extendVarEnvList,
	plusVarEnv, plusVarEnv_C, alterVarEnv,
	delVarEnvList, delVarEnv,
        minusVarEnv, intersectsVarEnv,
	lookupVarEnv, lookupVarEnv_NF, lookupWithDefaultVarEnv,
	mapVarEnv, mapMaybeVarEnv, zipVarEnv,
	modifyVarEnv, modifyVarEnv_Directly,
	isEmptyVarEnv, foldVarEnv, 
	elemVarEnvByKey, lookupVarEnv_Directly,
	filterVarEnv_Directly, restrictVarEnv, 
        partitionVarEnv,

	-- * The InScopeSet type
	InScopeSet, 
	
	-- ** Operations on InScopeSets
	emptyInScopeSet, mkInScopeSet, delInScopeSet,
	extendInScopeSet, extendInScopeSetList, extendInScopeSetSet, 
	getInScopeVars, lookupInScope, lookupInScope_Directly, 
        unionInScope, elemInScopeSet, uniqAway,

	-- * The RnEnv2 type
	RnEnv2, 
	
	-- ** Operations on RnEnv2s
	mkRnEnv2, rnBndr2, rnBndrs2,
	rnOccL, rnOccR, inRnEnvL, inRnEnvR, rnOccL_maybe, rnOccR_maybe,
        rnBndrL, rnBndrR, nukeRnEnvL, nukeRnEnvR,
        delBndrL, delBndrR, delBndrsL, delBndrsR,
        addRnInScopeSet,
        rnEtaL, rnEtaR,
	rnInScope, rnInScopeSet, lookupRnInScope,

	-- * TidyEnv and its operation
	TidyEnv, 
	emptyTidyEnv
    ) where

import OccName
import Var
import VarSet
import UniqFM
import Unique
import Util
import Maybes
import Outputable
import FastTypes
import StaticFlags
import FastString
\end{code}


%************************************************************************
%*									*
		In-scope sets
%*									*
%************************************************************************

\begin{code}
-- | A set of variables that are in scope at some point
data InScopeSet = InScope (VarEnv Var) FastInt
	-- The (VarEnv Var) is just a VarSet.  But we write it like
	-- this to remind ourselves that you can look up a Var in 
	-- the InScopeSet. Typically the InScopeSet contains the
	-- canonical version of the variable (e.g. with an informative
	-- unfolding), so this lookup is useful.
	--
	-- INVARIANT: the VarEnv maps (the Unique of) a variable to 
	--	      a variable with the same Uniqua.  (This was not
	--	      the case in the past, when we had a grevious hack
	--	      mapping var1 to var2.	
	-- 
	-- The FastInt is a kind of hash-value used by uniqAway
	-- For example, it might be the size of the set
	-- INVARIANT: it's not zero; we use it as a multiplier in uniqAway

instance Outputable InScopeSet where
  ppr (InScope s _) = ptext (sLit "InScope") <+> ppr s

emptyInScopeSet :: InScopeSet
emptyInScopeSet = InScope emptyVarSet (_ILIT(1))

getInScopeVars ::  InScopeSet -> VarEnv Var
getInScopeVars (InScope vs _) = vs

mkInScopeSet :: VarEnv Var -> InScopeSet
mkInScopeSet in_scope = InScope in_scope (_ILIT(1))

extendInScopeSet :: InScopeSet -> Var -> InScopeSet
extendInScopeSet (InScope in_scope n) v = InScope (extendVarEnv in_scope v v) (n +# _ILIT(1))

extendInScopeSetList :: InScopeSet -> [Var] -> InScopeSet
extendInScopeSetList (InScope in_scope n) vs
   = InScope (foldl (\s v -> extendVarEnv s v v) in_scope vs)
		    (n +# iUnbox (length vs))

extendInScopeSetSet :: InScopeSet -> VarEnv Var -> InScopeSet
extendInScopeSetSet (InScope in_scope n) vs
   = InScope (in_scope `plusVarEnv` vs) (n +# iUnbox (sizeUFM vs))

delInScopeSet :: InScopeSet -> Var -> InScopeSet
delInScopeSet (InScope in_scope n) v = InScope (in_scope `delVarEnv` v) n

elemInScopeSet :: Var -> InScopeSet -> Bool
elemInScopeSet v (InScope in_scope _) = v `elemVarEnv` in_scope

-- | Look up a variable the 'InScopeSet'.  This lets you map from 
-- the variable's identity (unique) to its full value.
lookupInScope :: InScopeSet -> Var -> Maybe Var
lookupInScope (InScope in_scope _) v  = lookupVarEnv in_scope v

lookupInScope_Directly :: InScopeSet -> Unique -> Maybe Var
lookupInScope_Directly (InScope in_scope _) uniq
  = lookupVarEnv_Directly in_scope uniq

unionInScope :: InScopeSet -> InScopeSet -> InScopeSet
unionInScope (InScope s1 _) (InScope s2 n2)
  = InScope (s1 `plusVarEnv` s2) n2
\end{code}

\begin{code}
-- | @uniqAway in_scope v@ finds a unique that is not used in the
-- in-scope set, and gives that to v. 
uniqAway :: InScopeSet -> Var -> Var
-- It starts with v's current unique, of course, in the hope that it won't
-- have to change, and thereafter uses a combination of that and the hash-code
-- found in the in-scope set
uniqAway in_scope var
  | var `elemInScopeSet` in_scope = uniqAway' in_scope var	-- Make a new one
  | otherwise 			  = var				-- Nothing to do

uniqAway' :: InScopeSet -> Var -> Var
-- This one *always* makes up a new variable
uniqAway' (InScope set n) var
  = try (_ILIT(1))
  where
    orig_unique = getUnique var
    try k 
	  | debugIsOn && (k ># _ILIT(1000))
	  = pprPanic "uniqAway loop:" (ppr (iBox k) <+> text "tries" <+> ppr var <+> int (iBox n)) 
	  | uniq `elemVarSetByKey` set = try (k +# _ILIT(1))
	  | debugIsOn && opt_PprStyle_Debug && (k ># _ILIT(3))
	  = pprTrace "uniqAway:" (ppr (iBox k) <+> text "tries" <+> ppr var <+> int (iBox n)) 
	    setVarUnique var uniq
	  | otherwise = setVarUnique var uniq
	  where
	    uniq = deriveUnique orig_unique (iBox (n *# k))
\end{code}

%************************************************************************
%*									*
		Dual renaming
%*									*
%************************************************************************

\begin{code}
-- | When we are comparing (or matching) types or terms, we are faced with 
-- \"going under\" corresponding binders.  E.g. when comparing:
--
-- > \x. e1	~   \y. e2
--
-- Basically we want to rename [@x@ -> @y@] or [@y@ -> @x@], but there are lots of 
-- things we must be careful of.  In particular, @x@ might be free in @e2@, or
-- y in @e1@.  So the idea is that we come up with a fresh binder that is free
-- in neither, and rename @x@ and @y@ respectively.  That means we must maintain:
--
-- 1. A renaming for the left-hand expression
--
-- 2. A renaming for the right-hand expressions
--
-- 3. An in-scope set
-- 
-- Furthermore, when matching, we want to be able to have an 'occurs check',
-- to prevent:
--
-- > \x. f   ~   \y. y
--
-- matching with [@f@ -> @y@].  So for each expression we want to know that set of
-- locally-bound variables. That is precisely the domain of the mappings 1.
-- and 2., but we must ensure that we always extend the mappings as we go in.
--
-- All of this information is bundled up in the 'RnEnv2'
data RnEnv2
  = RV2 { envL 	   :: VarEnv Var	-- Renaming for Left term
	, envR 	   :: VarEnv Var	-- Renaming for Right term
	, in_scope :: InScopeSet }	-- In scope in left or right terms

-- The renamings envL and envR are *guaranteed* to contain a binding
-- for every variable bound as we go into the term, even if it is not
-- renamed.  That way we can ask what variables are locally bound
-- (inRnEnvL, inRnEnvR)

mkRnEnv2 :: InScopeSet -> RnEnv2
mkRnEnv2 vars = RV2	{ envL 	   = emptyVarEnv 
			, envR 	   = emptyVarEnv
			, in_scope = vars }

addRnInScopeSet :: RnEnv2 -> VarEnv Var -> RnEnv2
addRnInScopeSet env vs
  | isEmptyVarEnv vs = env
  | otherwise        = env { in_scope = extendInScopeSetSet (in_scope env) vs }

rnInScope :: Var -> RnEnv2 -> Bool
rnInScope x env = x `elemInScopeSet` in_scope env

rnInScopeSet :: RnEnv2 -> InScopeSet
rnInScopeSet = in_scope

rnBndrs2 :: RnEnv2 -> [Var] -> [Var] -> RnEnv2
-- ^ Applies 'rnBndr2' to several variables: the two variable lists must be of equal length
rnBndrs2 env bsL bsR = foldl2 rnBndr2 env bsL bsR 

rnBndr2 :: RnEnv2 -> Var -> Var -> RnEnv2
-- ^ @rnBndr2 env bL bR@ goes under a binder @bL@ in the Left term,
-- 		         and binder @bR@ in the Right term.
-- It finds a new binder, @new_b@,
-- and returns an environment mapping @bL -> new_b@ and @bR -> new_b@
rnBndr2 (RV2 { envL = envL, envR = envR, in_scope = in_scope }) bL bR
  = RV2 { envL 	   = extendVarEnv envL bL new_b	  -- See Note
	, envR 	   = extendVarEnv envR bR new_b	  -- [Rebinding]
	, in_scope = extendInScopeSet in_scope new_b }
  where
	-- Find a new binder not in scope in either term
    new_b | not (bL `elemInScopeSet` in_scope) = bL
      	  | not (bR `elemInScopeSet` in_scope) = bR
      	  | otherwise			       = uniqAway' in_scope bL

	-- Note [Rebinding]
	-- If the new var is the same as the old one, note that
	-- the extendVarEnv *deletes* any current renaming
	-- E.g.	  (\x. \x. ...)	 ~  (\y. \z. ...)
	--
	--   Inside \x  \y	{ [x->y], [y->y],       {y} }
	-- 	 \x  \z	  	{ [x->x], [y->y, z->x], {y,x} }

rnBndrL :: RnEnv2 -> Var -> (RnEnv2, Var)
-- ^ Similar to 'rnBndr2' but used when there's a binder on the left
-- side only.
rnBndrL (RV2 { envL = envL, envR = envR, in_scope = in_scope }) bL
  = (RV2 { envL     = extendVarEnv envL bL new_b
         , envR     = envR
	 , in_scope = extendInScopeSet in_scope new_b }, new_b)
  where
    new_b = uniqAway in_scope bL

rnBndrR :: RnEnv2 -> Var -> (RnEnv2, Var)
-- ^ Similar to 'rnBndr2' but used when there's a binder on the right
-- side only.
rnBndrR (RV2 { envL = envL, envR = envR, in_scope = in_scope }) bR
  = (RV2 { envR     = extendVarEnv envR bR new_b
         , envL     = envL
	 , in_scope = extendInScopeSet in_scope new_b }, new_b)
  where
    new_b = uniqAway in_scope bR

rnEtaL :: RnEnv2 -> Var -> (RnEnv2, Var)
-- ^ Similar to 'rnBndrL' but used for eta expansion
-- See Note [Eta expansion]
rnEtaL (RV2 { envL = envL, envR = envR, in_scope = in_scope }) bL
  = (RV2 { envL     = extendVarEnv envL bL new_b
	 , envR     = extendVarEnv envR new_b new_b 	-- Note [Eta expansion]
	 , in_scope = extendInScopeSet in_scope new_b }, new_b)
  where
    new_b = uniqAway in_scope bL

rnEtaR :: RnEnv2 -> Var -> (RnEnv2, Var)
-- ^ Similar to 'rnBndr2' but used for eta expansion
-- See Note [Eta expansion]
rnEtaR (RV2 { envL = envL, envR = envR, in_scope = in_scope }) bR
  = (RV2 { envL     = extendVarEnv envL new_b new_b	-- Note [Eta expansion]
	 , envR     = extendVarEnv envR bR new_b
	 , in_scope = extendInScopeSet in_scope new_b }, new_b)
  where
    new_b = uniqAway in_scope bR

delBndrL, delBndrR :: RnEnv2 -> Var -> RnEnv2
delBndrL rn@(RV2 { envL = env, in_scope = in_scope }) v 
  = rn { envL = env `delVarEnv` v, in_scope = in_scope `extendInScopeSet` v }
delBndrR rn@(RV2 { envR = env, in_scope = in_scope }) v 
  = rn { envR = env `delVarEnv` v, in_scope = in_scope `extendInScopeSet` v }

delBndrsL, delBndrsR :: RnEnv2 -> [Var] -> RnEnv2
delBndrsL rn@(RV2 { envL = env, in_scope = in_scope }) v 
  = rn { envL = env `delVarEnvList` v, in_scope = in_scope `extendInScopeSetList` v }
delBndrsR rn@(RV2 { envR = env, in_scope = in_scope }) v 
  = rn { envR = env `delVarEnvList` v, in_scope = in_scope `extendInScopeSetList` v }

rnOccL, rnOccR :: RnEnv2 -> Var -> Var
-- ^ Look up the renaming of an occurrence in the left or right term
rnOccL (RV2 { envL = env }) v = lookupVarEnv env v `orElse` v
rnOccR (RV2 { envR = env }) v = lookupVarEnv env v `orElse` v

rnOccL_maybe, rnOccR_maybe :: RnEnv2 -> Var -> Maybe Var
-- ^ Look up the renaming of an occurrence in the left or right term
rnOccL_maybe (RV2 { envL = env }) v = lookupVarEnv env v
rnOccR_maybe (RV2 { envR = env }) v = lookupVarEnv env v

inRnEnvL, inRnEnvR :: RnEnv2 -> Var -> Bool
-- ^ Tells whether a variable is locally bound
inRnEnvL (RV2 { envL = env }) v = v `elemVarEnv` env
inRnEnvR (RV2 { envR = env }) v = v `elemVarEnv` env

lookupRnInScope :: RnEnv2 -> Var -> Var
lookupRnInScope env v = lookupInScope (in_scope env) v `orElse` v

nukeRnEnvL, nukeRnEnvR :: RnEnv2 -> RnEnv2
-- ^ Wipe the left or right side renaming
nukeRnEnvL env = env { envL = emptyVarEnv }
nukeRnEnvR env = env { envR = emptyVarEnv }
\end{code}

Note [Eta expansion]
~~~~~~~~~~~~~~~~~~~~
When matching
     (\x.M) ~ N
we rename x to x' with, where x' is not in scope in 
either term.  Then we want to behave as if we'd seen
     (\x'.M) ~ (\x'.N x')
Since x' isn't in scope in N, the form (\x'. N x') doesn't
capture any variables in N.  But we must nevertheless extend
the envR with a binding [x' -> x'], to support the occurs check.
For example, if we don't do this, we can get silly matches like
	forall a.  (\y.a)  ~   v
succeeding with [a -> v y], which is bogus of course.


%************************************************************************
%*									*
		Tidying
%*									*
%************************************************************************

\begin{code}
-- | When tidying up print names, we keep a mapping of in-scope occ-names
-- (the 'TidyOccEnv') and a Var-to-Var of the current renamings
type TidyEnv = (TidyOccEnv, VarEnv Var)

emptyTidyEnv :: TidyEnv
emptyTidyEnv = (emptyTidyOccEnv, emptyVarEnv)
\end{code}


%************************************************************************
%*									*
\subsection{@VarEnv@s}
%*									*
%************************************************************************

\begin{code}
type VarEnv elt   = UniqFM elt
type IdEnv elt    = VarEnv elt
type TyVarEnv elt = VarEnv elt
type CoVarEnv elt = VarEnv elt

emptyVarEnv	  :: VarEnv a
mkVarEnv	  :: [(Var, a)] -> VarEnv a
zipVarEnv	  :: [Var] -> [a] -> VarEnv a
unitVarEnv	  :: Var -> a -> VarEnv a
alterVarEnv	  :: (Maybe a -> Maybe a) -> VarEnv a -> Var -> VarEnv a
extendVarEnv	  :: VarEnv a -> Var -> a -> VarEnv a
extendVarEnv_C	  :: (a->a->a) -> VarEnv a -> Var -> a -> VarEnv a
extendVarEnv_Acc  :: (a->b->b) -> (a->b) -> VarEnv b -> Var -> a -> VarEnv b
plusVarEnv	  :: VarEnv a -> VarEnv a -> VarEnv a
extendVarEnvList  :: VarEnv a -> [(Var, a)] -> VarEnv a
		  
lookupVarEnv_Directly :: VarEnv a -> Unique -> Maybe a
filterVarEnv_Directly :: (Unique -> a -> Bool) -> VarEnv a -> VarEnv a
partitionVarEnv   :: (a -> Bool) -> VarEnv a -> (VarEnv a, VarEnv a)
restrictVarEnv    :: VarEnv a -> VarSet -> VarEnv a
delVarEnvList     :: VarEnv a -> [Var] -> VarEnv a
delVarEnv	  :: VarEnv a -> Var -> VarEnv a
minusVarEnv       :: VarEnv a -> VarEnv b -> VarEnv a
intersectsVarEnv  :: VarEnv a -> VarEnv a -> Bool
plusVarEnv_C	  :: (a -> a -> a) -> VarEnv a -> VarEnv a -> VarEnv a
mapVarEnv	  :: (a -> b) -> VarEnv a -> VarEnv b
mapMaybeVarEnv	  :: (a -> Maybe b) -> VarEnv a -> VarEnv b
modifyVarEnv	  :: (a -> a) -> VarEnv a -> Var -> VarEnv a
varEnvElts	  :: VarEnv a -> [a]
varEnvKeys	  :: VarEnv a -> [Unique]
		  
isEmptyVarEnv	  :: VarEnv a -> Bool
lookupVarEnv	  :: VarEnv a -> Var -> Maybe a
lookupVarEnv_NF   :: VarEnv a -> Var -> a
lookupWithDefaultVarEnv :: VarEnv a -> a -> Var -> a
elemVarEnv	  :: Var -> VarEnv a -> Bool
elemVarEnvByKey   :: Unique -> VarEnv a -> Bool
foldVarEnv	  :: (a -> b -> b) -> b -> VarEnv a -> b
\end{code}

\begin{code}
elemVarEnv       = elemUFM
elemVarEnvByKey  = elemUFM_Directly
alterVarEnv      = alterUFM
extendVarEnv	 = addToUFM
extendVarEnv_C	 = addToUFM_C
extendVarEnv_Acc = addToUFM_Acc
extendVarEnvList = addListToUFM
plusVarEnv_C	 = plusUFM_C
delVarEnvList	 = delListFromUFM
delVarEnv	 = delFromUFM
minusVarEnv      = minusUFM
intersectsVarEnv e1 e2 = not (isEmptyVarEnv (e1 `intersectUFM` e2))
plusVarEnv	 = plusUFM
lookupVarEnv	 = lookupUFM
lookupWithDefaultVarEnv = lookupWithDefaultUFM
mapVarEnv	 = mapUFM
mapMaybeVarEnv	 = mapMaybeUFM
mkVarEnv	 = listToUFM
emptyVarEnv	 = emptyUFM
varEnvElts	 = eltsUFM
varEnvKeys	 = keysUFM
unitVarEnv	 = unitUFM
isEmptyVarEnv	 = isNullUFM
foldVarEnv	 = foldUFM
lookupVarEnv_Directly = lookupUFM_Directly
filterVarEnv_Directly = filterUFM_Directly
partitionVarEnv       = partitionUFM

restrictVarEnv env vs = filterVarEnv_Directly keep env
  where
    keep u _ = u `elemVarSetByKey` vs
    
zipVarEnv tyvars tys   = mkVarEnv (zipEqual "zipVarEnv" tyvars tys)
lookupVarEnv_NF env id = case lookupVarEnv env id of
                         Just xx -> xx
                         Nothing -> panic "lookupVarEnv_NF: Nothing"
\end{code}

@modifyVarEnv@: Look up a thing in the VarEnv, 
then mash it with the modify function, and put it back.

\begin{code}
modifyVarEnv mangle_fn env key
  = case (lookupVarEnv env key) of
      Nothing -> env
      Just xx -> extendVarEnv env key (mangle_fn xx)

modifyVarEnv_Directly :: (a -> a) -> UniqFM a -> Unique -> UniqFM a
modifyVarEnv_Directly mangle_fn env key
  = case (lookupUFM_Directly env key) of
      Nothing -> env
      Just xx -> addToUFM_Directly env key (mangle_fn xx)
\end{code}
