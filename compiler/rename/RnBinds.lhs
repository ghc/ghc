%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[RnBinds]{Renaming and dependency analysis of bindings}

This module does renaming and dependency analysis on value bindings in
the abstract syntax.  It does {\em not} do cycle-checks on class or
type-synonym declarations; those cannot be done at this stage because
they may be affected by renaming (which isn't fully worked out yet).

\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module RnBinds (
   -- Renaming top-level bindings
   rnTopBindsLHS, rnTopBindsRHS, rnValBindsRHS,

   -- Renaming local bindings
   rnLocalBindsAndThen, rnLocalValBindsLHS, rnLocalValBindsRHS,

   -- Other bindings
   rnMethodBinds, renameSigs, mkSigTvFn,
   rnMatchGroup, rnGRHSs, rnGRHS,
   makeMiniFixityEnv, MiniFixityEnv,
   HsSigCtxt(..)
   ) where

import {-# SOURCE #-} RnExpr( rnLExpr, rnStmts )

import HsSyn
import TcRnMonad
import TcEvidence     ( emptyTcEvBinds )
import RnTypes        ( bindSigTyVarsFV, rnHsSigType, rnLHsType, checkPrecMatch )
import RnPat
import RnEnv
import DynFlags
import Module
import Name
import NameEnv
import NameSet
import RdrName          ( RdrName, rdrNameOcc )
import SrcLoc
import ListSetOps	( findDupsEq )
import BasicTypes	( RecFlag(..) )
import Digraph		( SCC(..) )
import Bag
import Outputable
import FastString
import Data.List	( partition, sort )
import Maybes		( orElse )
import Control.Monad
import Data.Traversable ( traverse )
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
rnTopBindsLHS :: MiniFixityEnv
              -> HsValBinds RdrName 
              -> RnM (HsValBindsLR Name RdrName)
rnTopBindsLHS fix_env binds
  = rnValBindsLHS (topRecNameMaker fix_env) binds

rnTopBindsRHS :: NameSet -> HsValBindsLR Name RdrName 
              -> RnM (HsValBinds Name, DefUses)
rnTopBindsRHS bound_names binds
  = do { is_boot <- tcIsHsBoot
       ; if is_boot 
         then rnTopBindsBoot binds
         else rnValBindsRHS (TopSigCtxt bound_names False) binds }

rnTopBindsBoot :: HsValBindsLR Name RdrName -> RnM (HsValBinds Name, DefUses)
-- A hs-boot file has no bindings. 
-- Return a single HsBindGroup with empty binds and renamed signatures
rnTopBindsBoot (ValBindsIn mbinds sigs)
  = do	{ checkErr (isEmptyLHsBinds mbinds) (bindsInHsBootFile mbinds)
	; (sigs', fvs) <- renameSigs HsBootCtxt sigs
	; return (ValBindsOut [] sigs', usesOnly fvs) }
rnTopBindsBoot b = pprPanic "rnTopBindsBoot" (ppr b)
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
  = rnLocalValBindsAndThen val_binds $ \ val_binds' -> 
      thing_inside (HsValBinds val_binds')

rnLocalBindsAndThen (HsIPBinds binds) thing_inside = do
    (binds',fv_binds) <- rnIPBinds binds
    (thing, fvs_thing) <- thing_inside (HsIPBinds binds')
    return (thing, fvs_thing `plusFV` fv_binds)

rnIPBinds :: HsIPBinds RdrName -> RnM (HsIPBinds Name, FreeVars)
rnIPBinds (IPBinds ip_binds _no_dict_binds) = do
    (ip_binds', fvs_s) <- mapAndUnzipM (wrapLocFstM rnIPBind) ip_binds
    return (IPBinds ip_binds' emptyTcEvBinds, plusFVs fvs_s)

rnIPBind :: IPBind RdrName -> RnM (IPBind Name, FreeVars)
rnIPBind (IPBind ~(Left n) expr) = do
    (expr',fvExpr) <- rnLExpr expr
    return (IPBind (Left n) expr', fvExpr)
\end{code}


%************************************************************************
%*									*
		ValBinds
%*									*
%************************************************************************

\begin{code}
-- Renaming local binding gropus 
-- Does duplicate/shadow check
rnLocalValBindsLHS :: MiniFixityEnv
                   -> HsValBinds RdrName
                   -> RnM ([Name], HsValBindsLR Name RdrName)
rnLocalValBindsLHS fix_env binds 
  = do { binds' <- rnValBindsLHS (localRecNameMaker fix_env) binds 

         -- Check for duplicates and shadowing
	 -- Must do this *after* renaming the patterns
	 -- See Note [Collect binders only after renaming] in HsUtils

         -- We need to check for dups here because we
     	 -- don't don't bind all of the variables from the ValBinds at once
     	 -- with bindLocatedLocals any more.
         -- 
     	 -- Note that we don't want to do this at the top level, since
     	 -- sorting out duplicates and shadowing there happens elsewhere.
     	 -- The behavior is even different. For example,
     	 --   import A(f)
     	 --   f = ...
     	 -- should not produce a shadowing warning (but it will produce
     	 -- an ambiguity warning if you use f), but
     	 --   import A(f)
     	 --   g = let f = ... in f
     	 -- should.
       ; let bound_names = collectHsValBinders binds'
       ; envs <- getRdrEnvs
       ; checkDupAndShadowedNames envs bound_names

       ; return (bound_names, binds') }

-- renames the left-hand sides
-- generic version used both at the top level and for local binds
-- does some error checking, but not what gets done elsewhere at the top level
rnValBindsLHS :: NameMaker 
              -> HsValBinds RdrName
              -> RnM (HsValBindsLR Name RdrName)
rnValBindsLHS topP (ValBindsIn mbinds sigs)
  = do { mbinds' <- mapBagM (rnBindLHS topP doc) mbinds
       ; return $ ValBindsIn mbinds' sigs }
  where
    bndrs = collectHsBindsBinders mbinds
    doc   = text "In the binding group for:" <+> pprWithCommas ppr bndrs

rnValBindsLHS _ b = pprPanic "rnValBindsLHSFromDoc" (ppr b)

-- General version used both from the top-level and for local things
-- Assumes the LHS vars are in scope
--
-- Does not bind the local fixity declarations
rnValBindsRHS :: HsSigCtxt 
              -> HsValBindsLR Name RdrName
              -> RnM (HsValBinds Name, DefUses)

rnValBindsRHS ctxt (ValBindsIn mbinds sigs)
  = do { (sigs', sig_fvs) <- renameSigs ctxt sigs
       ; binds_w_dus <- mapBagM (rnBind (mkSigTvFn sigs')) mbinds
       ; case depAnalBinds binds_w_dus of
           (anal_binds, anal_dus) -> return (valbind', valbind'_dus)
              where
                valbind' = ValBindsOut anal_binds sigs'
                valbind'_dus = anal_dus `plusDU` usesOnly sig_fvs
			       -- Put the sig uses *after* the bindings
			       -- so that the binders are removed from 
			       -- the uses in the sigs
       }

rnValBindsRHS _ b = pprPanic "rnValBindsRHS" (ppr b)

-- Wrapper for local binds
--
-- The *client* of this function is responsible for checking for unused binders;
-- it doesn't (and can't: we don't have the thing inside the binds) happen here
--
-- The client is also responsible for bringing the fixities into scope
rnLocalValBindsRHS :: NameSet  -- names bound by the LHSes
                   -> HsValBindsLR Name RdrName
                   -> RnM (HsValBinds Name, DefUses)
rnLocalValBindsRHS bound_names binds
  = rnValBindsRHS (LocalBindCtxt bound_names) binds

-- for local binds
-- wrapper that does both the left- and right-hand sides 
--
-- here there are no local fixity decls passed in;
-- the local fixity decls come from the ValBinds sigs
rnLocalValBindsAndThen :: HsValBinds RdrName
                       -> (HsValBinds Name -> RnM (result, FreeVars))
                       -> RnM (result, FreeVars)
rnLocalValBindsAndThen binds@(ValBindsIn _ sigs) thing_inside
 = do	{     -- (A) Create the local fixity environment 
	  new_fixities <- makeMiniFixityEnv [L loc sig | L loc (FixSig sig) <- sigs]

	      -- (B) Rename the LHSes 
	; (bound_names, new_lhs) <- rnLocalValBindsLHS new_fixities binds

	      --     ...and bring them (and their fixities) into scope
	; bindLocalNamesFV bound_names              $
          addLocalFixities new_fixities bound_names $ do

	{      -- (C) Do the RHS and thing inside
	  (binds', dus) <- rnLocalValBindsRHS (mkNameSet bound_names) new_lhs 
        ; (result, result_fvs) <- thing_inside binds'

		-- Report unused bindings based on the (accurate) 
		-- findUses.  E.g.
		-- 	let x = x in 3
		-- should report 'x' unused
	; let real_uses = findUses dus result_fvs
	      -- Insert fake uses for variables introduced implicitly by wildcards (#4404)
	      implicit_uses = hsValBindsImplicits binds'
	; warnUnusedLocalBinds bound_names (real_uses `unionNameSets` implicit_uses)

	; let
            -- The variables "used" in the val binds are: 
            --   (1) the uses of the binds (allUses)
            --   (2) the FVs of the thing-inside
            all_uses = allUses dus `plusFV` result_fvs
		-- Note [Unused binding hack]
		-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
		-- Note that *in contrast* to the above reporting of
		-- unused bindings, (1) above uses duUses to return *all* 
		-- the uses, even if the binding is unused.  Otherwise consider:
                --	x = 3
                --	y = let p = x in 'x'	-- NB: p not used
                -- If we don't "see" the dependency of 'y' on 'x', we may put the
                -- bindings in the wrong order, and the type checker will complain
                -- that x isn't in scope
		--
		-- But note that this means we won't report 'x' as unused, 
		-- whereas we would if we had { x = 3; p = x; y = 'x' }

	; return (result, all_uses) }}
         	-- The bound names are pruned out of all_uses
	        -- by the bindLocalNamesFV call above

rnLocalValBindsAndThen bs _ = pprPanic "rnLocalValBindsAndThen" (ppr bs)


-- Process the fixity declarations, making a FastString -> (Located Fixity) map
-- (We keep the location around for reporting duplicate fixity declarations.)
-- 
-- Checks for duplicates, but not that only locally defined things are fixed.
-- Note: for local fixity declarations, duplicates would also be checked in
--       check_sigs below.  But we also use this function at the top level.

makeMiniFixityEnv :: [LFixitySig RdrName] -> RnM MiniFixityEnv

makeMiniFixityEnv decls = foldlM add_one emptyFsEnv decls
 where
   add_one env (L loc (FixitySig (L name_loc name) fixity)) = do
     { -- this fixity decl is a duplicate iff
       -- the ReaderName's OccName's FastString is already in the env
       -- (we only need to check the local fix_env because
       --  definitions of non-local will be caught elsewhere)
       let { fs = occNameFS (rdrNameOcc name)
           ; fix_item = L loc fixity };

       case lookupFsEnv env fs of
         Nothing -> return $ extendFsEnv env fs fix_item
         Just (L loc' _) -> do
           { setSrcSpan loc $ 
             addErrAt name_loc (dupFixityDecl loc' name)
           ; return env}
     }

dupFixityDecl :: SrcSpan -> RdrName -> SDoc
dupFixityDecl loc rdr_name
  = vcat [ptext (sLit "Multiple fixity declarations for") <+> quotes (ppr rdr_name),
	  ptext (sLit "also at ") <+> ppr loc]

---------------------

-- renaming a single bind

rnBindLHS :: NameMaker
          -> SDoc 
          -> LHsBind RdrName
          -- returns the renamed left-hand side,
          -- and the FreeVars *of the LHS*
          -- (i.e., any free variables of the pattern)
          -> RnM (LHsBindLR Name RdrName)

rnBindLHS name_maker _ (L loc bind@(PatBind { pat_lhs = pat }))
  = setSrcSpan loc $ do
      -- we don't actually use the FV processing of rnPatsAndThen here
      (pat',pat'_fvs) <- rnBindPat name_maker pat
      return (L loc (bind { pat_lhs = pat', bind_fvs = pat'_fvs }))
                -- We temporarily store the pat's FVs in bind_fvs;
                -- gets updated to the FVs of the whole bind
                -- when doing the RHS below
                            
rnBindLHS name_maker _ (L loc bind@(FunBind { fun_id = name@(L nameLoc _) }))
  = setSrcSpan loc $ 
    do { newname <- applyNameMaker name_maker name
       ; return (L loc (bind { fun_id = L nameLoc newname })) } 

rnBindLHS _ _ b = pprPanic "rnBindLHS" (ppr b)

-- assumes the left-hands-side vars are in scope
rnBind :: (Name -> [Name])		-- Signature tyvar function
       -> LHsBindLR Name RdrName
       -> RnM (LHsBind Name, [Name], Uses)
rnBind _ (L loc bind@(PatBind { pat_lhs = pat
                              , pat_rhs = grhss 
                                      -- pat fvs were stored in bind_fvs
                                      -- after processing the LHS
                              , bind_fvs = pat_fvs }))
  = setSrcSpan loc $ 
    do	{ mod <- getModule
        ; (grhss', rhs_fvs) <- rnGRHSs PatBindRhs rnLExpr grhss

		-- No scoped type variables for pattern bindings
	; let all_fvs = pat_fvs `plusFV` rhs_fvs
              fvs'    = filterNameSet (nameIsLocalOrFrom mod) all_fvs
	        -- Keep locally-defined Names
		-- As well as dependency analysis, we need these for the
		-- MonoLocalBinds test in TcBinds.decideGeneralisationPlan
              bndrs = collectPatBinders pat
              bind' = bind { pat_rhs  = grhss', bind_fvs = fvs' }
              is_wild_pat = case pat of
                              L _ (WildPat {}) -> True
                              _                -> False

        -- Warn if the pattern binds no variables, except for the
        -- entirely-explicit idiom    _ = rhs
        -- which (a) is not that different from  _v = rhs
        --       (b) is sometimes used to give a type sig for,
        --           or an occurrence of, a variable on the RHS
        ; whenWOptM Opt_WarnUnusedBinds $
          when (null bndrs && not is_wild_pat) $
          addWarn $ unusedPatBindWarn bind'

	; fvs' `seq` -- See Note [Free-variable space leak]
          return (L loc bind', bndrs, all_fvs) }

rnBind sig_fn (L loc bind@(FunBind { fun_id = name 
                            	   , fun_infix = is_infix 
                            	   , fun_matches = matches })) 
       -- invariant: no free vars here when it's a FunBind
  = setSrcSpan loc $
    do	{ let plain_name = unLoc name

	; (matches', rhs_fvs) <- bindSigTyVarsFV (sig_fn plain_name) $
				-- bindSigTyVars tests for Opt_ScopedTyVars
			         rnMatchGroup (FunRhs plain_name is_infix) rnLExpr matches
	; when is_infix $ checkPrecMatch plain_name matches'

        ; mod <- getModule
        ; let fvs' = filterNameSet (nameIsLocalOrFrom mod) rhs_fvs
	        -- Keep locally-defined Names
		-- As well as dependency analysis, we need these for the
		-- MonoLocalBinds test in TcBinds.decideGeneralisationPlan

	; fvs' `seq` -- See Note [Free-variable space leak]
          return (L loc (bind { fun_matches = matches'
			      , bind_fvs   = fvs' }), 
		  [plain_name], rhs_fvs)
      }

rnBind _ b = pprPanic "rnBind" (ppr b)

{-
Note [Free-variable space leak]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We have
    fvs' = trim fvs
and we seq fvs' before turning it as part of a record.

The reason is that trim is sometimes something like
    \xs -> intersectNameSet (mkNameSet bound_names) xs
and we don't want to retain the list bound_names. This showed up in
trac ticket #1136.
-}

---------------------
depAnalBinds :: Bag (LHsBind Name, [Name], Uses)
	     -> ([(RecFlag, LHsBinds Name)], DefUses)
-- Dependency analysis; this is important so that 
-- unused-binding reporting is accurate
depAnalBinds binds_w_dus
  = (map get_binds sccs, map get_du sccs)
  where
    sccs = depAnal (\(_, defs, _) -> defs)
                   (\(_, _, uses) -> nameSetToList uses)
                   (bagToList binds_w_dus)

    get_binds (AcyclicSCC (bind, _, _)) = (NonRecursive, unitBag bind)
    get_binds (CyclicSCC  binds_w_dus)  = (Recursive, listToBag [b | (b,_,_) <- binds_w_dus])

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
    env = mkNameEnv [ (name, hsLKiTyVarNames ltvs)  -- Kind variables and type variables
		    | L _ (TypeSig names
			           (L _ (HsForAllTy Explicit ltvs _ _))) <- sigs
                    , (L _ name) <- names]
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
	      -> LHsBinds RdrName
	      -> RnM (LHsBinds Name, FreeVars)

rnMethodBinds cls sig_fn binds
  = do { checkDupRdrNames meth_names
	     -- Check that the same method is not given twice in the
	     -- same instance decl	instance C T where
	     --			      f x = ...
	     --			      g y = ...
	     --			      f x = ...
	     -- We must use checkDupRdrNames because the Name of the
	     -- method is the Name of the class selector, whose SrcSpan
	     -- points to the class declaration; and we use rnMethodBinds
	     -- for instance decls too

       ; foldlM do_one (emptyBag, emptyFVs) (bagToList binds) }
  where 
    meth_names  = collectMethodBinders binds
    do_one (binds,fvs) bind 
       = do { (bind', fvs_bind) <- rnMethodBind cls sig_fn bind
	    ; return (binds `unionBags` bind', fvs_bind `plusFV` fvs) }

rnMethodBind :: Name
	      -> (Name -> [Name])
	      -> LHsBindLR RdrName RdrName
	      -> RnM (Bag (LHsBindLR Name Name), FreeVars)
rnMethodBind cls sig_fn 
             (L loc bind@(FunBind { fun_id = name, fun_infix = is_infix 
				  , fun_matches = MG { mg_alts = matches } }))
  = setSrcSpan loc $ do
    sel_name <- wrapLocM (lookupInstDeclBndr cls (ptext (sLit "method"))) name
    let plain_name = unLoc sel_name
        -- We use the selector name as the binder

    (new_matches, fvs) <- bindSigTyVarsFV (sig_fn plain_name) $
                          mapFvRn (rnMatch (FunRhs plain_name is_infix) rnLExpr) matches
    let new_group = mkMatchGroup new_matches

    when is_infix $ checkPrecMatch plain_name new_group
    return (unitBag (L loc (bind { fun_id      = sel_name 
                                 , fun_matches = new_group
                                 , bind_fvs    = fvs })),
             fvs `addOneFV` plain_name)
        -- The 'fvs' field isn't used for method binds

-- Can't handle method pattern-bindings which bind multiple methods.
rnMethodBind _ _ (L loc bind@(PatBind {})) = do
    addErrAt loc (methodBindErr bind)
    return (emptyBag, emptyFVs)

rnMethodBind _ _ b = pprPanic "rnMethodBind" (ppr b)
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
\end{enumerate}
%
At the moment we don't gather free-var info from the types in
signatures.  We'd only need this if we wanted to report unused tyvars.

\begin{code}
renameSigs :: HsSigCtxt 
	   -> [LSig RdrName]
	   -> RnM ([LSig Name], FreeVars)
-- Renames the signatures and performs error checks
renameSigs ctxt sigs 
  = do	{ mapM_ dupSigDeclErr (findDupSigs sigs)

	; checkDupMinimalSigs sigs

	; (sigs', sig_fvs) <- mapFvRn (wrapLocFstM (renameSig ctxt)) sigs

	; let (good_sigs, bad_sigs) = partition (okHsSig ctxt) sigs'
	; mapM_ misplacedSigErr bad_sigs		 -- Misplaced

	; return (good_sigs, sig_fvs) } 

----------------------
-- We use lookupSigOccRn in the signatures, which is a little bit unsatisfactory
-- because this won't work for:
--	instance Foo T where
--	  {-# INLINE op #-}
--	  Baz.op = ...
-- We'll just rename the INLINE prag to refer to whatever other 'op'
-- is in scope.  (I'm assuming that Baz.op isn't in scope unqualified.)
-- Doesn't seem worth much trouble to sort this.

renameSig :: HsSigCtxt -> Sig RdrName -> RnM (Sig Name, FreeVars)
-- FixitySig is renamed elsewhere.
renameSig _ (IdSig x)
  = return (IdSig x, emptyFVs)	  -- Actually this never occurs

renameSig ctxt sig@(TypeSig vs ty)
  = do	{ new_vs <- mapM (lookupSigOccRn ctxt sig) vs
	; (new_ty, fvs) <- rnHsSigType (ppr_sig_bndrs vs) ty
	; return (TypeSig new_vs new_ty, fvs) }

renameSig ctxt sig@(GenericSig vs ty)
  = do	{ defaultSigs_on <- xoptM Opt_DefaultSignatures
        ; unless defaultSigs_on (addErr (defaultSigErr sig))
        ; new_v <- mapM (lookupSigOccRn ctxt sig) vs
	; (new_ty, fvs) <- rnHsSigType (ppr_sig_bndrs vs) ty
	; return (GenericSig new_v new_ty, fvs) }

renameSig _ (SpecInstSig ty)
  = do	{ (new_ty, fvs) <- rnLHsType SpecInstSigCtx ty
	; return (SpecInstSig new_ty,fvs) }

-- {-# SPECIALISE #-} pragmas can refer to imported Ids
-- so, in the top-level case (when mb_names is Nothing)
-- we use lookupOccRn.  If there's both an imported and a local 'f'
-- then the SPECIALISE pragma is ambiguous, unlike all other signatures
renameSig ctxt sig@(SpecSig v ty inl)
  = do	{ new_v <- case ctxt of 
                     TopSigCtxt {} -> lookupLocatedOccRn v
                     _             -> lookupSigOccRn ctxt sig v
	; (new_ty, fvs) <- rnHsSigType (quotes (ppr v)) ty
	; return (SpecSig new_v new_ty inl, fvs) }

renameSig ctxt sig@(InlineSig v s)
  = do	{ new_v <- lookupSigOccRn ctxt sig v
	; return (InlineSig new_v s, emptyFVs) }

renameSig ctxt sig@(FixSig (FixitySig v f))
  = do	{ new_v <- lookupSigOccRn ctxt sig v
	; return (FixSig (FixitySig new_v f), emptyFVs) }

renameSig ctxt sig@(MinimalSig bf)
  = do new_bf <- traverse (lookupSigOccRn ctxt sig) bf
       return (MinimalSig new_bf, emptyFVs)

ppr_sig_bndrs :: [Located RdrName] -> SDoc
ppr_sig_bndrs bs = quotes (pprWithCommas ppr bs)

okHsSig :: HsSigCtxt -> LSig a -> Bool
okHsSig ctxt (L _ sig) 
  = case (sig, ctxt) of
     (GenericSig {}, ClsDeclCtxt {}) -> True
     (GenericSig {}, _)              -> False

     (TypeSig {}, _)              -> True

     (FixSig {}, InstDeclCtxt {}) -> False
     (FixSig {}, _)               -> True

     (IdSig {}, TopSigCtxt {})   -> True
     (IdSig {}, InstDeclCtxt {}) -> True
     (IdSig {}, _)               -> False

     (InlineSig {}, HsBootCtxt) -> False
     (InlineSig {}, _)          -> True

     (SpecSig {}, TopSigCtxt {})    -> True
     (SpecSig {}, LocalBindCtxt {}) -> True
     (SpecSig {}, InstDeclCtxt {})  -> True
     (SpecSig {}, _)                -> False

     (SpecInstSig {}, InstDeclCtxt {}) -> True
     (SpecInstSig {}, _)               -> False

     (MinimalSig {}, ClsDeclCtxt {}) -> True
     (MinimalSig {}, _)              -> False

-------------------
findDupSigs :: [LSig RdrName] -> [[(Located RdrName, Sig RdrName)]]
-- Check for duplicates on RdrName version, 
-- because renamed version has unboundName for
-- not-in-scope binders, which gives bogus dup-sig errors
-- NB: in a class decl, a 'generic' sig is not considered 
--     equal to an ordinary sig, so we allow, say
--     	     class C a where
--	       op :: a -> a
--             default op :: Eq a => a -> a
findDupSigs sigs
  = findDupsEq matching_sig (concatMap (expand_sig . unLoc) sigs)
  where
    expand_sig sig@(FixSig (FixitySig n _)) = [(n,sig)]
    expand_sig sig@(InlineSig n _)          = [(n,sig)]
    expand_sig sig@(TypeSig  ns _)   = [(n,sig) | n <- ns]
    expand_sig sig@(GenericSig ns _) = [(n,sig) | n <- ns]
    expand_sig _ = []

    matching_sig (L _ n1,sig1) (L _ n2,sig2) = n1 == n2 && mtch sig1 sig2
    mtch (FixSig {})     (FixSig {})     = True
    mtch (InlineSig {})  (InlineSig {})  = True
    mtch (TypeSig {})    (TypeSig {})    = True
    mtch (GenericSig {}) (GenericSig {}) = True
    mtch _ _ = False

-- Warn about multiple MINIMAL signatures
checkDupMinimalSigs :: [LSig RdrName] -> RnM ()
checkDupMinimalSigs sigs
  = case filter isMinimalLSig sigs of
      minSigs@(_:_:_) -> dupMinimalSigErr minSigs
      _ -> return ()
\end{code}


%************************************************************************
%*									*
\subsection{Match}
%*									*
%************************************************************************

\begin{code}
rnMatchGroup :: Outputable (body RdrName) => HsMatchContext Name
             -> (Located (body RdrName) -> RnM (Located (body Name), FreeVars))
             -> MatchGroup RdrName (Located (body RdrName))
             -> RnM (MatchGroup Name (Located (body Name)), FreeVars)
rnMatchGroup ctxt rnBody (MG { mg_alts = ms }) 
  = do { empty_case_ok <- xoptM Opt_EmptyCase
       ; when (null ms && not empty_case_ok) (addErr (emptyCaseErr ctxt))
       ; (new_ms, ms_fvs) <- mapFvRn (rnMatch ctxt rnBody) ms
       ; return (mkMatchGroup new_ms, ms_fvs) }

rnMatch :: Outputable (body RdrName) => HsMatchContext Name
        -> (Located (body RdrName) -> RnM (Located (body Name), FreeVars))
        -> LMatch RdrName (Located (body RdrName))
        -> RnM (LMatch Name (Located (body Name)), FreeVars)
rnMatch ctxt rnBody = wrapLocFstM (rnMatch' ctxt rnBody)

rnMatch' :: Outputable (body RdrName) => HsMatchContext Name 
         -> (Located (body RdrName) -> RnM (Located (body Name), FreeVars))
         -> Match RdrName (Located (body RdrName))
         -> RnM (Match Name (Located (body Name)), FreeVars)
rnMatch' ctxt rnBody match@(Match pats maybe_rhs_sig grhss)
  = do 	{ 	-- Result type signatures are no longer supported
	  case maybe_rhs_sig of	
	        Nothing -> return ()
	        Just (L loc ty) -> addErrAt loc (resSigErr ctxt match ty)

	       -- Now the main event
	       -- note that there are no local ficity decls for matches
	; rnPats ctxt pats	$ \ pats' -> do
	{ (grhss', grhss_fvs) <- rnGRHSs ctxt rnBody grhss

	; return (Match pats' Nothing grhss', grhss_fvs) }}

emptyCaseErr :: HsMatchContext Name -> SDoc
emptyCaseErr ctxt = hang (ptext (sLit "Empty list of alterantives in") <+> pp_ctxt)
                       2 (ptext (sLit "Use EmptyCase to allow this"))
  where
    pp_ctxt = case ctxt of
                CaseAlt    -> ptext (sLit "case expression")
                LambdaExpr -> ptext (sLit "\\case expression")
                _ -> ptext (sLit "(unexpected)") <+> pprMatchContextNoun ctxt
 

resSigErr :: Outputable body => HsMatchContext Name -> Match RdrName body -> HsType RdrName -> SDoc 
resSigErr ctxt match ty
   = vcat [ ptext (sLit "Illegal result type signature") <+> quotes (ppr ty)
	  , nest 2 $ ptext (sLit "Result signatures are no longer supported in pattern matches")
 	  , pprMatchInCtxt ctxt match ]
\end{code}


%************************************************************************
%*									*
\subsubsection{Guarded right-hand sides (GRHSs)}
%*									*
%************************************************************************

\begin{code}
rnGRHSs :: HsMatchContext Name 
        -> (Located (body RdrName) -> RnM (Located (body Name), FreeVars))
        -> GRHSs RdrName (Located (body RdrName))
        -> RnM (GRHSs Name (Located (body Name)), FreeVars)
rnGRHSs ctxt rnBody (GRHSs grhss binds)
  = rnLocalBindsAndThen binds	$ \ binds' -> do
    (grhss', fvGRHSs) <- mapFvRn (rnGRHS ctxt rnBody) grhss
    return (GRHSs grhss' binds', fvGRHSs)

rnGRHS :: HsMatchContext Name 
       -> (Located (body RdrName) -> RnM (Located (body Name), FreeVars))
       -> LGRHS RdrName (Located (body RdrName))
       -> RnM (LGRHS Name (Located (body Name)), FreeVars)
rnGRHS ctxt rnBody = wrapLocFstM (rnGRHS' ctxt rnBody)

rnGRHS' :: HsMatchContext Name 
        -> (Located (body RdrName) -> RnM (Located (body Name), FreeVars))
        -> GRHS RdrName (Located (body RdrName))
        -> RnM (GRHS Name (Located (body Name)), FreeVars)
rnGRHS' ctxt rnBody (GRHS guards rhs)
  = do	{ pattern_guards_allowed <- xoptM Opt_PatternGuards
        ; ((guards', rhs'), fvs) <- rnStmts (PatGuard ctxt) rnLExpr guards $ \ _ ->
				    rnBody rhs

	; unless (pattern_guards_allowed || is_standard_guard guards')
	  	 (addWarn (nonStdGuardErr guards'))

	; return (GRHS guards' rhs', fvs) }
  where
	-- Standard Haskell 1.4 guards are just a single boolean
	-- expression, rather than a list of qualifiers as in the
	-- Glasgow extension
    is_standard_guard []                       = True
    is_standard_guard [L _ (BodyStmt _ _ _ _)] = True
    is_standard_guard _                        = False
\end{code}

%************************************************************************
%*									*
\subsection{Error messages}
%*									*
%************************************************************************

\begin{code}
dupSigDeclErr :: [(Located RdrName, Sig RdrName)] -> RnM ()
dupSigDeclErr pairs@((L loc name, sig) : _)
  = addErrAt loc $
    vcat [ ptext (sLit "Duplicate") <+> what_it_is 
           <> ptext (sLit "s for") <+> quotes (ppr name)
         , ptext (sLit "at") <+> vcat (map ppr $ sort $ map (getLoc . fst) pairs) ]
  where
    what_it_is = hsSigDoc sig

dupSigDeclErr [] = panic "dupSigDeclErr"

misplacedSigErr :: LSig Name -> RnM ()
misplacedSigErr (L loc sig)
  = addErrAt loc $
    sep [ptext (sLit "Misplaced") <+> hsSigDoc sig <> colon, ppr sig]

defaultSigErr :: Sig RdrName -> SDoc
defaultSigErr sig = vcat [ hang (ptext (sLit "Unexpected default signature:"))
                              2 (ppr sig)
                         , ptext (sLit "Use DefaultSignatures to enable default signatures") ]

methodBindErr :: HsBindLR RdrName RdrName -> SDoc
methodBindErr mbind
 =  hang (ptext (sLit "Pattern bindings (except simple variables) not allowed in instance declarations"))
       2 (ppr mbind)

bindsInHsBootFile :: LHsBindsLR Name RdrName -> SDoc
bindsInHsBootFile mbinds
  = hang (ptext (sLit "Bindings in hs-boot files are not allowed"))
       2 (ppr mbinds)

nonStdGuardErr :: Outputable body => [LStmtLR Name Name body] -> SDoc
nonStdGuardErr guards
  = hang (ptext (sLit "accepting non-standard pattern guards (use PatternGuards to suppress this message)"))
       4 (interpp'SP guards)

unusedPatBindWarn :: HsBind Name -> SDoc
unusedPatBindWarn bind
  = hang (ptext (sLit "This pattern-binding binds no variables:"))
       2 (ppr bind)

dupMinimalSigErr :: [LSig RdrName] -> RnM ()
dupMinimalSigErr sigs@(L loc _ : _)
  = addErrAt loc $
    vcat [ ptext (sLit "Multiple minimal complete definitions")
         , ptext (sLit "at") <+> vcat (map ppr $ sort $ map getLoc sigs)
         , ptext (sLit "Combine alternative minimal complete definitions with `|'") ]
dupMinimalSigErr [] = panic "dupMinimalSigErr"
\end{code}
