%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[RnBinds]{Renaming and dependency analysis of bindings}

This module does renaming and dependency analysis on value bindings in
the abstract syntax.  It does {\em not} do cycle-checks on class or
type-synonym declarations; those cannot be done at this stage because
they may be affected by renaming (which isn't fully worked out yet).

\begin{code}
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module RnBinds (rnTopBinds, rnTopBindsLHS, rnTopBindsRHS, -- use these for top-level bindings
                rnLocalBindsAndThen, rnValBindsLHS, rnValBindsRHS, -- or these for local bindings
                rnMethodBinds, renameSigs, mkSigTvFn,
                rnMatchGroup, rnGRHSs,
                makeMiniFixityEnv
   ) where

#include "HsVersions.h"

import {-# SOURCE #-} RnExpr( rnLExpr, rnStmts )

import HsSyn
import RdrHsSyn
import RnHsSyn
import TcRnMonad
import RnTypes        ( rnHsSigType, rnLHsType, rnHsTypeFVs,checkPrecMatch)
import RnPat          (rnPatsAndThen_LocalRightwards, rnPat_LocalRec, rnPat_TopRec, 
                       NameMaker, localNameMaker, topNameMaker, applyNameMaker, 
                       patSigErr)
                      
import RnEnv		( lookupLocatedBndrRn, 
                          lookupInstDeclBndr, newIPNameRn,
                          lookupLocatedSigOccRn, bindPatSigTyVarsFV,
                          bindLocalFixities, bindSigTyVarsFV, 
                          warnUnusedLocalBinds, mapFvRn, extendTyVarEnvFVRn,
                          bindLocatedLocalsFV, bindLocalNames, bindLocalNamesFV,
                          bindLocalNamesFV_WithFixities,
                          bindLocatedLocalsRn,
                          checkDupNames, checkShadowing
			)
import DynFlags	( DynFlag(..) )
import HscTypes		(FixItem(..))
import Name
import NameEnv
import UniqFM
import NameSet
import PrelNames	( isUnboundName )
import RdrName		( RdrName, rdrNameOcc )
import SrcLoc		( Located(..), unLoc, noLoc )
import ListSetOps	( findDupsEq )
import BasicTypes	( RecFlag(..) )
import Digraph		( SCC(..), stronglyConnComp )
import Bag
import Outputable
import Maybes		( orElse )
import Util		( filterOut )
import Monad		( foldM )
\end{code}

-- ToDo: Put the annotations into the monad, so that they arrive in the proper
-- place and can be used when complaining.

The code tree received by the function @rnBinds@ contains definitions
in where-clauses which are all apparently mutually recursive, but which may
not really depend upon each other. For example, in the top level program
\begin{verbatim}
f x = y where a = x
	      y = x
\end{verbatim}
the definitions of @a@ and @y@ do not depend on each other at all.
Unfortunately, the typechecker cannot always check such definitions.
\footnote{Mycroft, A. 1984. Polymorphic type schemes and recursive
definitions. In Proceedings of the International Symposium on Programming,
Toulouse, pp. 217-39. LNCS 167. Springer Verlag.}
However, the typechecker usually can check definitions in which only the
strongly connected components have been collected into recursive bindings.
This is precisely what the function @rnBinds@ does.

ToDo: deal with case where a single monobinds binds the same variable
twice.

The vertag tag is a unique @Int@; the tags only need to be unique
within one @MonoBinds@, so that unique-Int plumbing is done explicitly
(heavy monad machinery not needed).


%************************************************************************
%*									*
%* naming conventions							*
%*									*
%************************************************************************

\subsection[name-conventions]{Name conventions}

The basic algorithm involves walking over the tree and returning a tuple
containing the new tree plus its free variables. Some functions, such
as those walking polymorphic bindings (HsBinds) and qualifier lists in
list comprehensions (@Quals@), return the variables bound in local
environments. These are then used to calculate the free variables of the
expression evaluated in these environments.

Conventions for variable names are as follows:
\begin{itemize}
\item
new code is given a prime to distinguish it from the old.

\item
a set of variables defined in @Exp@ is written @dvExp@

\item
a set of variables free in @Exp@ is written @fvExp@
\end{itemize}

%************************************************************************
%*									*
%* analysing polymorphic bindings (HsBindGroup, HsBind)
%*									*
%************************************************************************

\subsubsection[dep-HsBinds]{Polymorphic bindings}

Non-recursive expressions are reconstructed without any changes at top
level, although their component expressions may have to be altered.
However, non-recursive expressions are currently not expected as
\Haskell{} programs, and this code should not be executed.

Monomorphic bindings contain information that is returned in a tuple
(a @FlatMonoBinds@) containing:

\begin{enumerate}
\item
a unique @Int@ that serves as the ``vertex tag'' for this binding.

\item
the name of a function or the names in a pattern. These are a set
referred to as @dvLhs@, the defined variables of the left hand side.

\item
the free variables of the body. These are referred to as @fvBody@.

\item
the definition's actual code. This is referred to as just @code@.
\end{enumerate}

The function @nonRecDvFv@ returns two sets of variables. The first is
the set of variables defined in the set of monomorphic bindings, while the
second is the set of free variables in those bindings.

The set of variables defined in a non-recursive binding is just the
union of all of them, as @union@ removes duplicates. However, the
free variables in each successive set of cumulative bindings is the
union of those in the previous set plus those of the newest binding after
the defined variables of the previous set have been removed.

@rnMethodBinds@ deals only with the declarations in class and
instance declarations.	It expects only to see @FunMonoBind@s, and
it expects the global environment to contain bindings for the binders
(which are all class operations).

%************************************************************************
%*									*
\subsubsection{ Top-level bindings}
%*									*
%************************************************************************

\begin{code}
-- for top-level bindings, we need to make top-level names,
-- so we have a different entry point than for local bindings
rnTopBindsLHS :: UniqFM (Located Fixity) -- mini fixity env for the names we're about to bind
                                         -- these fixities need to be brought into scope with the names
              -> HsValBinds RdrName 
              -> RnM (HsValBindsLR Name RdrName)
rnTopBindsLHS fix_env binds = 
    (uncurry $ rnValBindsLHSFromDoc True) (bindersAndDoc binds) fix_env binds

rnTopBindsRHS :: [Name] -- the names bound by these binds
              -> HsValBindsLR Name RdrName 
              -> RnM (HsValBinds Name, DefUses)
rnTopBindsRHS bound_names binds = 
    do { is_boot <- tcIsHsBoot
       ; if is_boot 
         then rnTopBindsBoot binds
         else rnValBindsRHSGen (\x -> x) -- don't trim free vars
                               bound_names binds }
  

-- wrapper if we don't need to do anything in between the left and right,
-- or anything else in the scope of the left
--
-- never used when there are fixity declarations
rnTopBinds :: HsValBinds RdrName 
           -> RnM (HsValBinds Name, DefUses)
rnTopBinds b = 
  do nl <- rnTopBindsLHS emptyUFM b
     let bound_names = map unLoc (collectHsValBinders nl)
     bindLocalNames bound_names  $ rnTopBindsRHS bound_names nl
       

rnTopBindsBoot :: HsValBindsLR Name RdrName -> RnM (HsValBinds Name, DefUses)
-- A hs-boot file has no bindings. 
-- Return a single HsBindGroup with empty binds and renamed signatures
rnTopBindsBoot (ValBindsIn mbinds sigs)
  = do	{ checkErr (isEmptyLHsBinds mbinds) (bindsInHsBootFile mbinds)
	; sigs' <- renameSigs okHsBootSig sigs
	; return (ValBindsOut [] sigs', usesOnly (hsSigsFVs sigs')) }
\end{code}



%*********************************************************
%*							*
		HsLocalBinds
%*							*
%*********************************************************

\begin{code}
rnLocalBindsAndThen :: HsLocalBinds RdrName
                    -> (HsLocalBinds Name -> RnM (result, FreeVars))
                    -> RnM (result, FreeVars)
-- This version (a) assumes that the binding vars are *not* already in scope
--		 (b) removes the binders from the free vars of the thing inside
-- The parser doesn't produce ThenBinds
rnLocalBindsAndThen EmptyLocalBinds thing_inside
  = thing_inside EmptyLocalBinds

rnLocalBindsAndThen (HsValBinds val_binds) thing_inside
  = rnValBindsAndThen val_binds $ \ val_binds' -> 
      thing_inside (HsValBinds val_binds')

rnLocalBindsAndThen (HsIPBinds binds) thing_inside
  = rnIPBinds binds			`thenM` \ (binds',fv_binds) ->
    thing_inside (HsIPBinds binds')	`thenM` \ (thing, fvs_thing) ->
    returnM (thing, fvs_thing `plusFV` fv_binds)


rnIPBinds (IPBinds ip_binds _no_dict_binds)
  = do	{ (ip_binds', fvs_s) <- mapAndUnzipM (wrapLocFstM rnIPBind) ip_binds
	; return (IPBinds ip_binds' emptyLHsBinds, plusFVs fvs_s) }

rnIPBind (IPBind n expr)
  = newIPNameRn  n		`thenM` \ name ->
    rnLExpr expr		`thenM` \ (expr',fvExpr) ->
    return (IPBind name expr', fvExpr)
\end{code}


%************************************************************************
%*									*
		ValBinds
%*									*
%************************************************************************

\begin{code}
-- wrapper for local binds
-- creates the documentation info and calls the helper below
rnValBindsLHS :: UniqFM (Located Fixity) -- mini fixity env for the names we're about to bind
                                         -- these fixities need to be brought into scope with the names
              -> HsValBinds RdrName
              -> RnM (HsValBindsLR Name RdrName)
rnValBindsLHS fix_env binds = 
    let (boundNames,doc) = bindersAndDoc binds 
    in rnValBindsLHSFromDoc_Local boundNames doc fix_env binds

-- a helper used for local binds that does the duplicates check,
-- just so we don't forget to do it somewhere
rnValBindsLHSFromDoc_Local :: [Located RdrName] -- RdrNames of the LHS (so we don't have to gather them twice)
                           -> SDoc              -- doc string for dup names and shadowing
                           -> UniqFM (Located Fixity) -- mini fixity env for the names we're about to bind
                                                      -- these fixities need to be brought into scope with the names
                           -> HsValBinds RdrName
                           -> RnM (HsValBindsLR Name RdrName)

rnValBindsLHSFromDoc_Local boundNames doc fix_env binds = do
     -- Do error checking: we need to check for dups here because we
     -- don't don't bind all of the variables from the ValBinds at once
     -- with bindLocatedLocals any more.
     --
     checkDupNames doc boundNames
     -- Warn about shadowing, but only in source modules
     ifOptM Opt_WarnNameShadowing (checkShadowing doc boundNames)   

     -- (Note that we don't want to do this at the top level, since
     -- sorting out duplicates and shadowing there happens elsewhere.
     -- The behavior is even different. For example,
     --   import A(f)
     --   f = ...
     -- should not produce a shadowing warning (but it will produce
     -- an ambiguity warning if you use f), but
     --   import A(f)
     --   g = let f = ... in f
     -- should.
     rnValBindsLHSFromDoc False boundNames doc fix_env binds 

bindersAndDoc :: HsValBinds RdrName -> ([Located RdrName], SDoc)
bindersAndDoc binds = 
    let
        -- the unrenamed bndrs for error checking and reporting
        orig = collectHsValBinders binds
        doc = text "In the binding group for:" <+> pprWithCommas ppr (map unLoc orig)
    in
      (orig, doc)

-- renames the left-hand sides
-- generic version used both at the top level and for local binds
-- does some error checking, but not what gets done elsewhere at the top level
rnValBindsLHSFromDoc :: Bool -- top or not
                     -> [Located RdrName] -- RdrNames of the LHS (so we don't have to gather them twice)
                     -> SDoc              -- doc string for dup names and shadowing
                     -> UniqFM (Located Fixity) -- mini fixity env for the names we're about to bind
                                                -- these fixities need to be brought into scope with the names
                     -> HsValBinds RdrName
                     -> RnM (HsValBindsLR Name RdrName)
rnValBindsLHSFromDoc topP original_bndrs doc fix_env binds@(ValBindsIn mbinds sigs)
 = do
     -- rename the LHSes
     mbinds' <- mapBagM (rnBindLHS topP doc fix_env) mbinds
     return $ ValBindsIn mbinds' sigs

-- assumes the LHS vars are in scope
-- general version used both from the top-level and for local things
--
-- does not bind the local fixity declarations
rnValBindsRHSGen :: (FreeVars -> FreeVars)  -- for trimming free var sets
                     -- The trimming function trims the free vars we attach to a
                     -- binding so that it stays reasonably small
                 -> [Name]  -- names bound by the LHSes
                 -> HsValBindsLR Name RdrName
                 -> RnM (HsValBinds Name, DefUses)

rnValBindsRHSGen trim bound_names binds@(ValBindsIn mbinds sigs)
 = do -- rename the sigs
   sigs' <- rename_sigs sigs
   -- rename the RHSes
   binds_w_dus <- mapBagM (rnBind (mkSigTvFn sigs') trim) mbinds
   let (anal_binds, anal_dus) = depAnalBinds binds_w_dus
       (valbind', valbind'_dus) = (ValBindsOut anal_binds sigs',
                                   usesOnly (hsSigsFVs sigs') `plusDU` anal_dus)
   -- We do the check-sigs after renaming the bindings,
   -- so that we have convenient access to the binders
   check_sigs (okBindSig (duDefs anal_dus)) sigs'                       
   returnM (valbind', valbind'_dus)

-- wrapper for local binds
--
-- the *client* of this function is responsible for checking for unused binders;
-- it doesn't (and can't: we don't have the thing inside the binds) happen here
--
-- the client is also responsible for bringing the fixities into scope
rnValBindsRHS :: [Name]  -- names bound by the LHSes
              -> HsValBindsLR Name RdrName
              -> RnM (HsValBinds Name, DefUses)
rnValBindsRHS bound_names binds = 
  rnValBindsRHSGen (\ fvs -> -- only keep the names the names from this group
                    intersectNameSet (mkNameSet bound_names) fvs) bound_names binds


-- for local binds
-- wrapper that does both the left- and right-hand sides 
--
-- here there are no local fixity decls passed in;
-- the local fixity decls come from the ValBinds sigs
rnValBindsAndThen :: HsValBinds RdrName
                  -> (HsValBinds Name -> RnM (result, FreeVars))
                  -> RnM (result, FreeVars)
rnValBindsAndThen binds@(ValBindsIn _ sigs) thing_inside = 
    let 
       (original_bndrs, doc) = bindersAndDoc binds

    in do
      -- (A) create the local fixity environment 
      new_fixities <- makeMiniFixityEnv [L loc sig | L loc (FixSig sig) <- sigs]

      -- (B) rename the LHSes 
      new_lhs <- rnValBindsLHSFromDoc_Local original_bndrs doc new_fixities binds
      let bound_names = map unLoc $ collectHsValBinders new_lhs

      --     and bring them (and their fixities) into scope
      bindLocalNamesFV_WithFixities bound_names new_fixities $ do

      -- (C) do the RHS and thing inside
      (binds', dus) <- rnValBindsRHS bound_names new_lhs 
      (result, result_fvs) <- thing_inside binds'

      let 
            -- the variables used in the val binds are: 
            --   (1) the uses of the binds 
            --   (2) the FVs of the thing-inside
            all_uses = (duUses dus) `plusFV` result_fvs
                -- duUses: It's important to return all the uses.  Otherwise consider:
                --	x = 3
                --	y = let p = x in 'x'	-- NB: p not used
                -- If we don't "see" the dependency of 'y' on 'x', we may put the
                -- bindings in the wrong order, and the type checker will complain
                -- that x isn't in scope

            -- check for unused binders.  note that we only want to do
            -- this for local ValBinds; it gets done elsewhere for
            -- top-level binds (where the scoping is different)
            unused_bndrs = [ b | b <- bound_names, not (b `elemNameSet` all_uses)]

      warnUnusedLocalBinds unused_bndrs

      return (result, 
              -- the bound names are pruned out of all_uses
              -- by the bindLocalNamesFV call above
              all_uses)


-- Process the fixity declarations, making a FastString -> (Located Fixity) map
-- (We keep the location around for reporting duplicate fixity declarations.)
-- 
-- Checks for duplicates, but not that only locally defined things are fixed.
-- Note: for local fixity declarations, duplicates would also be checked in
--       check_sigs below.  But we also use this function at the top level.
makeMiniFixityEnv :: [LFixitySig RdrName]
              -> RnM (UniqFM (Located Fixity)) -- key is the FastString of the OccName
                                               -- of the fixity declaration it came from
                                               
makeMiniFixityEnv decls = foldlM add_one emptyUFM decls
 where
   add_one env (L loc (FixitySig (L name_loc name) fixity)) = do
     { -- this fixity decl is a duplicate iff
       -- the ReaderName's OccName's FastString is already in the env
       -- (we only need to check the local fix_env because
       --  definitions of non-local will be caught elsewhere)
       let {occ = rdrNameOcc name;
            curKey = occNameFS occ;
            fix_item = L loc fixity};

       case lookupUFM env curKey of
         Nothing -> return $ addToUFM env curKey fix_item
         Just (L loc' _) -> do
           { setSrcSpan loc $ 
                        addLocErr (L name_loc name) (dupFixityDecl loc')
           ; return env}
     }

pprFixEnv :: NameEnv FixItem -> SDoc
pprFixEnv env 
  = pprWithCommas (\ (FixItem n f) -> ppr f <+> ppr n)
		  (nameEnvElts env)

dupFixityDecl loc rdr_name
  = vcat [ptext SLIT("Multiple fixity declarations for") <+> quotes (ppr rdr_name),
	  ptext SLIT("also at ") <+> ppr loc]

---------------------

-- renaming a single bind

rnBindLHS :: Bool -- top if true; local if false
          -> SDoc 
          -> UniqFM (Located Fixity) -- mini fixity env for the names we're about to bind
                                     -- these fixities need to be brought into scope with the names
          -> LHsBind RdrName
          -- returns the renamed left-hand side,
          -- and the FreeVars *of the LHS*
          -- (i.e., any free variables of the pattern)
          -> RnM (LHsBindLR Name RdrName)

rnBindLHS topP doc fix_env (L loc (PatBind { pat_lhs = pat, 
                                           pat_rhs = grhss, 
                                           bind_fvs=bind_fvs,
                                           pat_rhs_ty=pat_rhs_ty
                                         })) 
  = setSrcSpan loc $ do
      -- we don't actually use the FV processing of rnPatsAndThen here
      (pat',pat'_fvs) <- (if topP then rnPat_TopRec else rnPat_LocalRec) fix_env pat
      return (L loc (PatBind { pat_lhs = pat', 
                               pat_rhs = grhss, 
                               -- we temporarily store the pat's FVs here;
                               -- gets updated to the FVs of the whole bind
                               -- when doing the RHS below
                               bind_fvs = pat'_fvs,
                               -- these will get ignored in the next pass,
                               -- when we rename the RHS
			       pat_rhs_ty = pat_rhs_ty }))

rnBindLHS topP doc fix_env (L loc (FunBind { fun_id = name@(L nameLoc _), 
                                           fun_infix = inf, 
                                           fun_matches = matches,
                                           fun_co_fn = fun_co_fn, 
                                           bind_fvs = bind_fvs,
                                           fun_tick = fun_tick
                                         }))
  = setSrcSpan loc $ do
      newname <- applyNameMaker (if topP then topNameMaker else localNameMaker) name
      return (L loc (FunBind { fun_id = L nameLoc newname, 
                               fun_infix = inf, 
                               fun_matches = matches,
                               -- we temporatily store the LHS's FVs (empty in this case) here
                               -- gets updated when doing the RHS below
                               bind_fvs = emptyFVs,
                               -- everything else will get ignored in the next pass
                               fun_co_fn = fun_co_fn, 
                               fun_tick = fun_tick
                               }))

-- assumes the left-hands-side vars are in scope
rnBind :: (Name -> [Name])		-- Signature tyvar function
       -> (FreeVars -> FreeVars)	-- Trimming function for rhs free vars
       -> LHsBindLR Name RdrName
       -> RnM (LHsBind Name, [Name], Uses)
rnBind sig_fn trim (L loc (PatBind { pat_lhs = pat, 
                                     pat_rhs = grhss, 
                                     -- pat fvs were stored here while processing the LHS          
                                     bind_fvs=pat_fvs }))
  = setSrcSpan loc $ 
    do	{let bndrs = collectPatBinders pat

	; (grhss', fvs) <- rnGRHSs PatBindRhs grhss
		-- No scoped type variables for pattern bindings

	; return (L loc (PatBind { pat_lhs = pat, 
                                  pat_rhs = grhss', 
				     pat_rhs_ty = placeHolderType, 
                                  bind_fvs = trim fvs }), 
		  bndrs, pat_fvs `plusFV` fvs) }

rnBind sig_fn 
       trim 
       (L loc (FunBind { fun_id = name, 
                         fun_infix = inf, 
                         fun_matches = matches,
                         -- no pattern FVs
                         bind_fvs = _
                       })) 
       -- invariant: no free vars here when it's a FunBind
  = setSrcSpan loc $ 
    do	{ let plain_name = unLoc name

	; (matches', fvs) <- bindSigTyVarsFV (sig_fn plain_name) $
				-- bindSigTyVars tests for Opt_ScopedTyVars
			     rnMatchGroup (FunRhs plain_name inf) matches

	; checkPrecMatch inf plain_name matches'

	; return (L loc (FunBind { fun_id = name, 
                                  fun_infix = inf, 
                                  fun_matches = matches',
				     bind_fvs = trim fvs, 
                                  fun_co_fn = idHsWrapper, 
                                  fun_tick = Nothing }), 
		  [plain_name], fvs)
      }
		
---------------------
depAnalBinds :: Bag (LHsBind Name, [Name], Uses)
	     -> ([(RecFlag, LHsBinds Name)], DefUses)
-- Dependency analysis; this is important so that 
-- unused-binding reporting is accurate
depAnalBinds binds_w_dus
  = (map get_binds sccs, map get_du sccs)
  where
    sccs = stronglyConnComp edges

    keyd_nodes = bagToList binds_w_dus `zip` [0::Int ..]

    edges = [ (node, key, [key | n <- nameSetToList uses,
			         Just key <- [lookupNameEnv key_map n] ])
	    | (node@(_,_,uses), key) <- keyd_nodes ]

    key_map :: NameEnv Int	-- Which binding it comes from
    key_map = mkNameEnv [(bndr, key) | ((_, bndrs, _), key) <- keyd_nodes
				     , bndr <- bndrs ]

    get_binds (AcyclicSCC (bind, _, _)) = (NonRecursive, unitBag bind)
    get_binds (CyclicSCC  binds_w_dus)  = (Recursive, listToBag [b | (b,d,u) <- binds_w_dus])

    get_du (AcyclicSCC (_, bndrs, uses)) = (Just (mkNameSet bndrs), uses)
    get_du (CyclicSCC  binds_w_dus)      = (Just defs, uses)
	where
	  defs = mkNameSet [b | (_,bs,_) <- binds_w_dus, b <- bs]
	  uses = unionManyNameSets [u | (_,_,u) <- binds_w_dus]


---------------------
-- Bind the top-level forall'd type variables in the sigs.
-- E.g 	f :: a -> a
--	f = rhs
--	The 'a' scopes over the rhs
--
-- NB: there'll usually be just one (for a function binding)
--     but if there are many, one may shadow the rest; too bad!
--	e.g  x :: [a] -> [a]
--	     y :: [(a,a)] -> a
--	     (x,y) = e
--      In e, 'a' will be in scope, and it'll be the one from 'y'!

mkSigTvFn :: [LSig Name] -> (Name -> [Name])
-- Return a lookup function that maps an Id Name to the names
-- of the type variables that should scope over its body..
mkSigTvFn sigs
  = \n -> lookupNameEnv env n `orElse` []
  where
    env :: NameEnv [Name]
    env = mkNameEnv [ (name, map hsLTyVarName ltvs)
		    | L _ (TypeSig (L _ name) 
			           (L _ (HsForAllTy Explicit ltvs _ _))) <- sigs]
	-- Note the pattern-match on "Explicit"; we only bind
	-- type variables from signatures with an explicit top-level for-all
\end{code}


@rnMethodBinds@ is used for the method bindings of a class and an instance
declaration.   Like @rnBinds@ but without dependency analysis.

NOTA BENE: we record each {\em binder} of a method-bind group as a free variable.
That's crucial when dealing with an instance decl:
\begin{verbatim}
	instance Foo (T a) where
	   op x = ...
\end{verbatim}
This might be the {\em sole} occurrence of @op@ for an imported class @Foo@,
and unless @op@ occurs we won't treat the type signature of @op@ in the class
decl for @Foo@ as a source of instance-decl gates.  But we should!  Indeed,
in many ways the @op@ in an instance decl is just like an occurrence, not
a binder.

\begin{code}
rnMethodBinds :: Name			-- Class name
	      -> (Name -> [Name])	-- Signature tyvar function
	      -> [Name]			-- Names for generic type variables
	      -> LHsBinds RdrName
	      -> RnM (LHsBinds Name, FreeVars)

rnMethodBinds cls sig_fn gen_tyvars binds
  = foldM do_one (emptyBag,emptyFVs) (bagToList binds)
  where do_one (binds,fvs) bind = do
	   (bind', fvs_bind) <- rnMethodBind cls sig_fn gen_tyvars bind
	   return (bind' `unionBags` binds, fvs_bind `plusFV` fvs)

rnMethodBind cls sig_fn gen_tyvars (L loc (FunBind { fun_id = name, fun_infix = inf, 
					             fun_matches = MatchGroup matches _ }))
  = setSrcSpan loc $ 
    lookupInstDeclBndr cls name			`thenM` \ sel_name -> 
    let plain_name = unLoc sel_name in
	-- We use the selector name as the binder

    bindSigTyVarsFV (sig_fn plain_name)			$
    mapFvRn (rn_match plain_name) matches		`thenM` \ (new_matches, fvs) ->
    let 
	new_group = MatchGroup new_matches placeHolderType
    in
    checkPrecMatch inf plain_name new_group		`thenM_`
    returnM (unitBag (L loc (FunBind { 
				fun_id = sel_name, fun_infix = inf, 
				fun_matches = new_group,
				bind_fvs = fvs, fun_co_fn = idHsWrapper,
				fun_tick = Nothing })), 
	     fvs `addOneFV` plain_name)
	-- The 'fvs' field isn't used for method binds
  where
	-- Truly gruesome; bring into scope the correct members of the generic 
	-- type variables.  See comments in RnSource.rnSourceDecl(ClassDecl)
    rn_match sel_name match@(L _ (Match (L _ (TypePat ty) : _) _ _))
	= extendTyVarEnvFVRn gen_tvs 	$
	  rnMatch (FunRhs sel_name inf) match
	where
	  tvs     = map (rdrNameOcc.unLoc) (extractHsTyRdrTyVars ty)
	  gen_tvs = [tv | tv <- gen_tyvars, nameOccName tv `elem` tvs] 

    rn_match sel_name match = rnMatch (FunRhs sel_name inf) match


-- Can't handle method pattern-bindings which bind multiple methods.
rnMethodBind cls sig_fn gen_tyvars mbind@(L loc (PatBind other_pat _ _ _))
  = addLocErr mbind methodBindErr	`thenM_`
    returnM (emptyBag, emptyFVs) 
\end{code}



%************************************************************************
%*									*
\subsubsection[dep-Sigs]{Signatures (and user-pragmas for values)}
%*									*
%************************************************************************

@renameSigs@ checks for:
\begin{enumerate}
\item more than one sig for one thing;
\item signatures given for things not bound here;
\item with suitably flaggery, that all top-level things have type signatures.
\end{enumerate}
%
At the moment we don't gather free-var info from the types in
signatures.  We'd only need this if we wanted to report unused tyvars.

\begin{code}
renameSigs :: (LSig Name -> Bool) -> [LSig RdrName] -> RnM [LSig Name]
-- Renames the signatures and performs error checks
renameSigs ok_sig sigs 
  = do	{ sigs' <- rename_sigs sigs
	; check_sigs ok_sig sigs'
	; return sigs' }

----------------------
rename_sigs :: [LSig RdrName] -> RnM [LSig Name]
rename_sigs sigs = mappM (wrapLocM renameSig) sigs

----------------------
check_sigs :: (LSig Name -> Bool) -> [LSig Name] -> RnM ()
-- Used for class and instance decls, as well as regular bindings
check_sigs ok_sig sigs 
	-- Check for (a) duplicate signatures
	--	     (b) signatures for things not in this group
  = do	{ 
        traceRn (text "SIGS" <+> ppr sigs)
        ; mappM_ unknownSigErr (filter (not . ok_sig) sigs')
	; mappM_ dupSigDeclErr (findDupsEq eqHsSig sigs') }
  where
	-- Don't complain about an unbound name again
    sigs' = filterOut bad_name sigs
    bad_name sig = case sigName sig of
			Just n -> isUnboundName n
			other  -> False

-- We use lookupLocatedSigOccRn in the signatures, which is a little bit unsatisfactory
-- because this won't work for:
--	instance Foo T where
--	  {-# INLINE op #-}
--	  Baz.op = ...
-- We'll just rename the INLINE prag to refer to whatever other 'op'
-- is in scope.  (I'm assuming that Baz.op isn't in scope unqualified.)
-- Doesn't seem worth much trouble to sort this.

renameSig :: Sig RdrName -> RnM (Sig Name)
-- FixitSig is renamed elsewhere.
renameSig (TypeSig v ty)
  = lookupLocatedSigOccRn v			`thenM` \ new_v ->
    rnHsSigType (quotes (ppr v)) ty		`thenM` \ new_ty ->
    returnM (TypeSig new_v new_ty)

renameSig (SpecInstSig ty)
  = rnLHsType (text "A SPECIALISE instance pragma") ty `thenM` \ new_ty ->
    returnM (SpecInstSig new_ty)

renameSig (SpecSig v ty inl)
  = lookupLocatedSigOccRn v		`thenM` \ new_v ->
    rnHsSigType (quotes (ppr v)) ty	`thenM` \ new_ty ->
    returnM (SpecSig new_v new_ty inl)

renameSig (InlineSig v s)
  = lookupLocatedSigOccRn v		`thenM` \ new_v ->
    returnM (InlineSig new_v s)

renameSig (FixSig (FixitySig v f))
  = lookupLocatedSigOccRn v		`thenM` \ new_v ->
    returnM (FixSig (FixitySig new_v f))
\end{code}


************************************************************************
*									*
\subsection{Match}
*									*
************************************************************************

\begin{code}
rnMatchGroup :: HsMatchContext Name -> MatchGroup RdrName -> RnM (MatchGroup Name, FreeVars)
rnMatchGroup ctxt (MatchGroup ms _)
  = mapFvRn (rnMatch ctxt) ms	`thenM` \ (new_ms, ms_fvs) ->
    returnM (MatchGroup new_ms placeHolderType, ms_fvs)

rnMatch :: HsMatchContext Name -> LMatch RdrName -> RnM (LMatch Name, FreeVars)
rnMatch ctxt  = wrapLocFstM (rnMatch' ctxt)

rnMatch' ctxt match@(Match pats maybe_rhs_sig grhss)
  = 
	-- Deal with the rhs type signature
    bindPatSigTyVarsFV rhs_sig_tys	$ 
    doptM Opt_PatternSignatures `thenM` \ opt_PatternSignatures ->
    (case maybe_rhs_sig of
	Nothing -> returnM (Nothing, emptyFVs)
	Just ty | opt_PatternSignatures -> rnHsTypeFVs doc_sig ty	`thenM` \ (ty', ty_fvs) ->
				     returnM (Just ty', ty_fvs)
		| otherwise	  -> addLocErr ty patSigErr	`thenM_`
				     returnM (Nothing, emptyFVs)
    )					`thenM` \ (maybe_rhs_sig', ty_fvs) ->

	-- Now the main event
       -- note that there are no local ficity decls for matches
    rnPatsAndThen_LocalRightwards ctxt pats	$ \ (pats',_) ->
    rnGRHSs ctxt grhss		`thenM` \ (grhss', grhss_fvs) ->

    returnM (Match pats' maybe_rhs_sig' grhss', grhss_fvs `plusFV` ty_fvs)
	-- The bindPatSigTyVarsFV and rnPatsAndThen will remove the bound FVs
  where
     rhs_sig_tys =  case maybe_rhs_sig of
			Nothing -> []
			Just ty -> [ty]
     doc_sig = text "In a result type-signature"
\end{code}


%************************************************************************
%*									*
\subsubsection{Guarded right-hand sides (GRHSs)}
%*									*
%************************************************************************

\begin{code}
rnGRHSs :: HsMatchContext Name -> GRHSs RdrName -> RnM (GRHSs Name, FreeVars)

rnGRHSs ctxt (GRHSs grhss binds)
  = rnLocalBindsAndThen binds	$ \ binds' ->
    mapFvRn (rnGRHS ctxt) grhss	`thenM` \ (grhss', fvGRHSs) ->
    returnM (GRHSs grhss' binds', fvGRHSs)

rnGRHS :: HsMatchContext Name -> LGRHS RdrName -> RnM (LGRHS Name, FreeVars)
rnGRHS ctxt = wrapLocFstM (rnGRHS' ctxt)

rnGRHS' ctxt (GRHS guards rhs)
  = do	{ pattern_guards_allowed <- doptM Opt_PatternGuards
	; ((guards', rhs'), fvs) <- rnStmts (PatGuard ctxt) guards $
				    rnLExpr rhs

	; checkM (pattern_guards_allowed || is_standard_guard guards')
	  	 (addWarn (nonStdGuardErr guards'))

	; return (GRHS guards' rhs', fvs) }
  where
	-- Standard Haskell 1.4 guards are just a single boolean
	-- expression, rather than a list of qualifiers as in the
	-- Glasgow extension
    is_standard_guard []                     = True
    is_standard_guard [L _ (ExprStmt _ _ _)] = True
    is_standard_guard other	      	     = False
\end{code}

%************************************************************************
%*									*
\subsection{Error messages}
%*									*
%************************************************************************

\begin{code}
dupSigDeclErr sigs@(L loc sig : _)
  = addErrAt loc $
	vcat [ptext SLIT("Duplicate") <+> what_it_is <> colon,
	      nest 2 (vcat (map ppr_sig sigs))]
  where
    what_it_is = hsSigDoc sig
    ppr_sig (L loc sig) = ppr loc <> colon <+> ppr sig

unknownSigErr (L loc sig)
  = do	{ mod <- getModule
	; addErrAt loc $
		vcat [sep [ptext SLIT("Misplaced") <+> what_it_is <> colon, ppr sig],
		      extra_stuff mod sig] }
  where
    what_it_is = hsSigDoc sig
    extra_stuff mod  (TypeSig (L _ n) _)
	| nameIsLocalOrFrom mod n
	= ptext SLIT("The type signature must be given where")
		<+> quotes (ppr n) <+> ptext SLIT("is declared")
	| otherwise
	= ptext SLIT("You cannot give a type signature for an imported value")

    extra_stuff mod other = empty

methodBindErr mbind
 =  hang (ptext SLIT("Pattern bindings (except simple variables) not allowed in instance declarations"))
       2 (ppr mbind)

bindsInHsBootFile mbinds
  = hang (ptext SLIT("Bindings in hs-boot files are not allowed"))
       2 (ppr mbinds)

nonStdGuardErr guards
  = hang (ptext SLIT("accepting non-standard pattern guards (use -XPatternGuards to suppress this message)"))
       4 (interpp'SP guards)
\end{code}
