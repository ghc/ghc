%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1995
%
\section[SpecMonad]{Monad for the Specialiser}

\begin{code}
#include "HsVersions.h"

module SpecMonad where

import PlainCore
import SpecTyFuns

IMPORT_Trace
import Outputable	-- ToDo: these may be removable...
import Pretty

import AbsUniType
import Bag
import CmdLineOpts	( GlobalSwitch(..) )
import CoreLift		( mkLiftedId, liftExpr, bindUnlift, applyBindUnlifts )
import IdEnv
import Id
import IdInfo
import InstEnv		( lookupClassInstAtSimpleType )
import Maybes		( catMaybes, firstJust, maybeToBool, Maybe(..) )
import TyVarEnv		-- ( growTyVarEnvList, nullTyVarEnv, TyVarEnv, TypeEnv(..) )
import Util
import UniqSet
import SplitUniq

infixr 9 `thenSM`
\end{code}

%************************************************************************
%*									*
\subsection[cloning-binders]{The Specialising IdEnv and CloneInfo}
%*									*
%************************************************************************

@SpecIdEnv@ maps old Ids to their new "clone". There are three cases:

1) (NoLift CoLitAtom l) : an Id which is bound to a literal

2) (NoLift CoLitAtom l) : an Id bound to a "new" Id	      
   The new Id is a possibly-type-specialised clone of the original

3) Lifted lifted_id unlifted_id :

   This indicates that the original Id has been specialised to an
   unboxed value which must be lifted (see "Unboxed bindings" above)
     @unlifted_id@ is the unboxed clone of the original Id
     @lifted_id@ is a *lifted* version of the original Id

   When you lookup Ids which are Lifted, you have to insert a case
   expression to un-lift the value (done with @bindUnlift@)

   You also have to insert a case to lift the value in the binding
   (done with @liftExpr@)


\begin{code}
type SpecIdEnv = IdEnv CloneInfo

data CloneInfo
 = NoLift PlainCoreAtom	-- refers to cloned id or literal

 | Lifted Id		-- lifted, cloned id
	  Id		-- unlifted, cloned id

\end{code}

%************************************************************************
%*									*
\subsection[monad-Specialise]{Monad used in specialisation}
%*									*
%************************************************************************

Monad has:

 inherited: control flags and
	    recordInst functions with flags cached

	    environment mapping tyvars to types 
	    environment mapping Ids to Atoms
 
 threaded in and out: unique supply

\begin{code}
type SpecM result
  =  (GlobalSwitch -> Bool)
  -> TypeEnv
  -> SpecIdEnv
  -> SplitUniqSupply
  -> result

initSM m sw_chker uniqs
  = m sw_chker nullTyVarEnv nullIdEnv uniqs

returnSM :: a -> SpecM a
thenSM	 :: SpecM a -> (a -> SpecM b) -> SpecM b
fixSM    :: (a -> SpecM a) -> SpecM a

thenSM m k sw_chkr tvenv idenv us
  = case splitUniqSupply us	   of { (s1, s2) ->
    case (m sw_chkr tvenv idenv s1) of { r ->
    k r sw_chkr tvenv idenv s2 }}

returnSM r sw_chkr tvenv idenv us = r

fixSM k sw_chkr tvenv idenv us
 = r
 where
   r = k r sw_chkr tvenv idenv us	-- Recursive in r!
\end{code}


\begin{code}
getSwitchCheckerSM sw_chkr tvenv idenv us = sw_chkr
\end{code}

The only interesting bit is figuring out the type of the SpecId!

\begin{code}
newSpecIds :: [Id]		-- The id of which to make a specialised version
	   -> [Maybe UniType]	-- Specialise to these types
	   -> Int		-- No of dicts to specialise
	   -> SpecM [Id]

newSpecIds new_ids maybe_tys dicts_to_ignore sw_chkr tvenv idenv us
  = [ mkSpecId uniq id maybe_tys (spec_id_ty id) (selectIdInfoForSpecId id)
      | (id,uniq) <- new_ids `zip` uniqs ]
  where
    uniqs = getSUniques (length new_ids) us
    spec_id_ty id = specialiseTy (getIdUniType id) maybe_tys dicts_to_ignore

newTyVars :: Int -> SpecM [TyVar]
newTyVars n sw_chkr tvenv idenv us
 = map mkPolySysTyVar uniqs
 where
   uniqs = getSUniques n us
\end{code}

@cloneLambdaOrCaseBinders@ and @cloneLetBinders@ take a bunch of
binders, and build ``clones'' for them.  The clones differ from the
originals in three ways:

	(a) they have a fresh unique
	(b) they have the current type environment applied to their type
	(c) for Let binders which have been specialised to unboxed values
	    the clone will have a lifted type

As well as returning the list of cloned @Id@s they also return a list of
@CloneInfo@s which the original binders should be bound to.
	    
\begin{code}
cloneLambdaOrCaseBinders :: [Id] 			-- Old binders
			 -> SpecM ([Id], [CloneInfo])	-- New ones

cloneLambdaOrCaseBinders old_ids sw_chkr tvenv idenv us
  = let
	uniqs = getSUniques (length old_ids) us
    in
    unzip (zipWith clone_it old_ids uniqs)
  where
    clone_it old_id uniq
      = (new_id, NoLift (CoVarAtom new_id))
      where
	new_id = applyTypeEnvToId tvenv (mkIdWithNewUniq old_id uniq)

cloneLetBinders :: Bool 			-- Top level ?
		-> Bool 			-- Recursice
		-> [Id] 			-- Old binders
		-> SpecM ([Id], [CloneInfo])	-- New ones

cloneLetBinders top_lev is_rec old_ids sw_chkr tvenv idenv us
  = let
	uniqs = getSUniques (2 * length old_ids) us
    in
    unzip (clone_them old_ids uniqs)
  where
    clone_them [] [] = []

    clone_them (old_id:olds) (u1:u2:uniqs)
      | top_lev
	= (old_id,
	   NoLift (CoVarAtom old_id)) : clone_rest

	 -- Don't clone if it is a top-level thing. Why not?
	 -- (a) we don't want to change the uniques 
	 --     on such things (see TopLevId in Id.lhs)
	 -- (b) we don't have to be paranoid about name capture
	 -- (c) the thing is polymorphic so no need to subst

      | otherwise
	= if (is_rec && isUnboxedDataType new_ty && not (isUnboxedDataType old_ty))
	  then (lifted_id,
		Lifted lifted_id unlifted_id) : clone_rest
	  else (new_id,
		NoLift (CoVarAtom new_id)) : clone_rest

      where 
	clone_rest = clone_them olds uniqs

	new_id = applyTypeEnvToId tvenv (mkIdWithNewUniq old_id u1)
	new_ty = getIdUniType new_id
	old_ty = getIdUniType old_id

	(lifted_id, unlifted_id) = mkLiftedId new_id u2


cloneTyVarSM :: TyVar -> SpecM TyVar

cloneTyVarSM old_tyvar sw_chkr tvenv idenv us
  = let
	uniq = getSUnique us
    in
    cloneTyVar old_tyvar uniq -- new_tyvar

bindId :: Id -> CloneInfo -> SpecM thing -> SpecM thing

bindId id val specm sw_chkr tvenv idenv us
 = specm sw_chkr tvenv (addOneToIdEnv idenv id val) us

bindIds :: [Id] -> [CloneInfo] -> SpecM thing -> SpecM thing

bindIds olds news specm sw_chkr tvenv idenv us
 = specm sw_chkr tvenv (growIdEnvList idenv (zip olds news)) us

bindSpecIds :: [Id]			-- Old
	    -> [(CloneInfo)]		-- New
	    -> [[Maybe SpecInfo]]	-- Corresponding specialisations
					-- Each sub-list corresponds to a different type,
					-- and contains one Maybe spec_info for each id
	    -> SpecM thing 
	    -> SpecM thing

bindSpecIds olds clones spec_infos specm sw_chkr tvenv idenv us
 = specm sw_chkr tvenv (growIdEnvList idenv old_to_clone) us
 where
   old_to_clone = mk_old_to_clone olds clones spec_infos

   -- The important thing here is that we are *lazy* in spec_infos
   mk_old_to_clone [] [] _ = []
   mk_old_to_clone (old:rest_olds) (clone:rest_clones) spec_infos
     = (old, add_spec_info clone) : 
       mk_old_to_clone rest_olds rest_clones spec_infos_rest
     where
       add_spec_info (NoLift (CoVarAtom new))
	 = NoLift (CoVarAtom (new `addIdSpecialisation`
			          (mkSpecEnv spec_infos_this_id)))
       add_spec_info lifted
	 = lifted		-- no specialised instances for unboxed lifted values

       spec_infos_this_id = catMaybes (map head spec_infos)
       spec_infos_rest    = map tail spec_infos


bindTyVar :: TyVar -> UniType -> SpecM thing -> SpecM thing

bindTyVar tyvar ty specm sw_chkr tvenv idenv us
 = specm sw_chkr (growTyVarEnvList tvenv [(tyvar,ty)]) idenv us
\end{code}

\begin{code}
lookupId :: Id -> SpecM CloneInfo

lookupId id sw_chkr tvenv idenv us 
  = case lookupIdEnv idenv id of
      Nothing   -> NoLift (CoVarAtom id)
      Just info -> info
\end{code}

\begin{code}
specTy :: UniType -> SpecM UniType	-- Apply the current type envt to the type

specTy ty sw_chkr tvenv idenv us 
  = applyTypeEnvToTy tvenv ty
\end{code}

\begin{code}
liftId :: Id -> SpecM (Id, Id)
liftId id sw_chkr tvenv idenv us
  = let
	uniq = getSUnique us
    in
    mkLiftedId id uniq
\end{code}

In other monads these @mapSM@ things are usually called @listM@.
I think @mapSM@ is a much better name.  The `2' and `3' variants are
when you want to return two or three results, and get at them
separately.  It saves you having to do an (unzip stuff) right after.

\begin{code}
mapSM  	       :: (a -> SpecM b)	    -> [a] -> SpecM [b]
mapAndUnzipSM  :: (a -> SpecM (b1, b2))	    -> [a] -> SpecM ([b1],[b2])
mapAndUnzip3SM :: (a -> SpecM (b1, b2, b3)) -> [a] -> SpecM ([b1],[b2],[b3])
mapAndUnzip4SM :: (a -> SpecM (b1, b2, b3, b4)) -> [a] -> SpecM ([b1],[b2],[b3],[b4])

mapSM f [] = returnSM []
mapSM f (x:xs) = f x  		`thenSM` \ r ->
		 mapSM f xs	`thenSM` \ rs ->
		 returnSM (r:rs)

mapAndUnzipSM f [] = returnSM ([],[])
mapAndUnzipSM f (x:xs) = f x 			`thenSM` \ (r1, r2) ->
			 mapAndUnzipSM f xs	`thenSM` \ (rs1,rs2) ->
			 returnSM ((r1:rs1),(r2:rs2))

mapAndUnzip3SM f [] = returnSM ([],[],[])
mapAndUnzip3SM f (x:xs) = f x 			`thenSM` \ (r1,r2,r3) ->
			  mapAndUnzip3SM f xs	`thenSM` \ (rs1,rs2,rs3) ->
			  returnSM ((r1:rs1),(r2:rs2),(r3:rs3))

mapAndUnzip4SM f [] = returnSM ([],[],[],[])
mapAndUnzip4SM f (x:xs) = f x 			`thenSM` \ (r1,r2,r3,r4) ->
			  mapAndUnzip4SM f xs	`thenSM` \ (rs1,rs2,rs3,rs4) ->
			  returnSM ((r1:rs1),(r2:rs2),(r3:rs3),(r4:rs4))
\end{code}
