%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-2006
%
\section[RnEnv]{Environment manipulation for the renamer monad}

\begin{code}
module RnEnv (
        newTopSrcBinder,
        lookupLocatedTopBndrRn, lookupTopBndrRn,
        lookupLocatedOccRn, lookupOccRn, lookupOccRn_maybe,
        lookupLocalOccRn_maybe,
        lookupLocalOccThLvl_maybe,
        lookupTypeOccRn, lookupKindOccRn,
        lookupGlobalOccRn, lookupGlobalOccRn_maybe,
        reportUnboundName,

        HsSigCtxt(..), lookupLocalTcNames, lookupSigOccRn,

        lookupFixityRn, lookupTyFixityRn,
        lookupInstDeclBndr, lookupSubBndrOcc, lookupFamInstName,
        greRdrName,
        lookupSubBndrGREs, lookupConstructorFields,
        lookupSyntaxName, lookupSyntaxNames, lookupIfThenElse,
        lookupGreRn, lookupGreRn_maybe,
        lookupGreLocalRn_maybe, 
        getLookupOccRn, addUsedRdrNames,

        newLocalBndrRn, newLocalBndrsRn,
        bindLocalNames, bindLocalNamesFV,
        MiniFixityEnv, 
        addLocalFixities,
        bindLocatedLocalsFV, bindLocatedLocalsRn,
        extendTyVarEnvFVRn,

        checkDupRdrNames, checkShadowedRdrNames,
        checkDupNames, checkDupAndShadowedNames, checkTupSize,
        addFvRn, mapFvRn, mapMaybeFvRn, mapFvRnCPS,
        warnUnusedMatches,
        warnUnusedTopBinds, warnUnusedLocalBinds,
        dataTcOccs, kindSigErr, perhapsForallMsg,
        HsDocContext(..), docOfHsDocContext, 

        -- FsEnv
        FastStringEnv, emptyFsEnv, lookupFsEnv, extendFsEnv, mkFsEnv
    ) where

#include "HsVersions.h"

import LoadIface        ( loadInterfaceForName, loadSrcInterface_maybe )
import IfaceEnv
import HsSyn
import RdrName
import HscTypes
import TcEnv            ( tcLookupDataCon, tcLookupField, isBrackStage )
import TcRnMonad
import Id               ( isRecordSelector )
import Name
import NameSet
import NameEnv
import Avail
import Module
import UniqFM
import DataCon          ( dataConFieldLabels, dataConTyCon )
import TyCon            ( isTupleTyCon, tyConArity )
import PrelNames        ( mkUnboundName, isUnboundName, rOOT_MAIN, forall_tv_RDR )
import ErrUtils         ( MsgDoc )
import BasicTypes       ( Fixity(..), FixityDirection(..), minPrecedence, defaultFixity )
import SrcLoc
import Outputable
import Util
import Maybes
import BasicTypes       ( TopLevelFlag(..) )
import ListSetOps       ( removeDups )
import DynFlags
import FastString
import Control.Monad
import Data.List
import qualified Data.Set as Set
import Constants        ( mAX_TUPLE_SIZE )
\end{code}

%*********************************************************
%*                                                      *
                Source-code binders
%*                                                      *
%*********************************************************

\begin{code}
newTopSrcBinder :: Located RdrName -> RnM Name
newTopSrcBinder (L loc rdr_name)
  | Just name <- isExact_maybe rdr_name
  =     -- This is here to catch
        --   (a) Exact-name binders created by Template Haskell
        --   (b) The PrelBase defn of (say) [] and similar, for which
        --       the parser reads the special syntax and returns an Exact RdrName
        -- We are at a binding site for the name, so check first that it
        -- the current module is the correct one; otherwise GHC can get
        -- very confused indeed. This test rejects code like
        --      data T = (,) Int Int
        -- unless we are in GHC.Tup
    if isExternalName name then
      do { this_mod <- getModule
         ; unless (this_mod == nameModule name)
                  (addErrAt loc (badOrigBinding rdr_name))
         ; return name }
    else   -- See Note [Binders in Template Haskell] in Convert.hs
      do { let occ = nameOccName name
         ; occ `seq` return ()  -- c.f. seq in newGlobalBinder
         ; this_mod <- getModule
         ; updNameCache $ \ ns ->
           let name' = mkExternalName (nameUnique name) this_mod occ loc
               ns'   = ns { nsNames = extendNameCache (nsNames ns) this_mod occ name' }
           in (ns', name') }

  | Just (rdr_mod, rdr_occ) <- isOrig_maybe rdr_name
  = do  { this_mod <- getModule
        ; unless (rdr_mod == this_mod || rdr_mod == rOOT_MAIN)
                 (addErrAt loc (badOrigBinding rdr_name))
        -- When reading External Core we get Orig names as binders,
        -- but they should agree with the module gotten from the monad
        --
        -- We can get built-in syntax showing up here too, sadly.  If you type
        --      data T = (,,,)
        -- the constructor is parsed as a type, and then RdrHsSyn.tyConToDataCon
        -- uses setRdrNameSpace to make it into a data constructors.  At that point
        -- the nice Exact name for the TyCon gets swizzled to an Orig name.
        -- Hence the badOrigBinding error message.
        --
        -- Except for the ":Main.main = ..." definition inserted into
        -- the Main module; ugh!

        -- Because of this latter case, we call newGlobalBinder with a module from
        -- the RdrName, not from the environment.  In principle, it'd be fine to
        -- have an arbitrary mixture of external core definitions in a single module,
        -- (apart from module-initialisation issues, perhaps).
        ; newGlobalBinder rdr_mod rdr_occ loc }

  | otherwise
  = do  { unless (not (isQual rdr_name))
                 (addErrAt loc (badQualBndrErr rdr_name))
                -- Binders should not be qualified; if they are, and with a different
                -- module name, we we get a confusing "M.T is not in scope" error later

        ; stage <- getStage
        ; if isBrackStage stage then
                -- We are inside a TH bracket, so make an *Internal* name
                -- See Note [Top-level Names in Template Haskell decl quotes] in RnNames
             do { uniq <- newUnique
                ; return (mkInternalName uniq (rdrNameOcc rdr_name) loc) }
          else
                -- Normal case
             do { this_mod <- getModule
                ; newGlobalBinder this_mod (rdrNameOcc rdr_name) loc } }
\end{code}

%*********************************************************
%*                                                      *
        Source code occurrences
%*                                                      *
%*********************************************************

Looking up a name in the RnEnv.

Note [Type and class operator definitions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want to reject all of these unless we have -XTypeOperators (Trac #3265)
   data a :*: b  = ...
   class a :*: b where ...
   data (:*:) a b  = ....
   class (:*:) a b where ...
The latter two mean that we are not just looking for a
*syntactically-infix* declaration, but one that uses an operator
OccName.  We use OccName.isSymOcc to detect that case, which isn't
terribly efficient, but there seems to be no better way.

\begin{code}
lookupTopBndrRn :: RdrName -> RnM Name
lookupTopBndrRn n = do nopt <- lookupTopBndrRn_maybe n
                       case nopt of
                         Just n' -> return n'
                         Nothing -> do traceRn $ text "lookupTopBndrRn"
                                       unboundName WL_LocalTop n

lookupLocatedTopBndrRn :: Located RdrName -> RnM (Located Name)
lookupLocatedTopBndrRn = wrapLocM lookupTopBndrRn

lookupTopBndrRn_maybe :: RdrName -> RnM (Maybe Name)
-- Look up a top-level source-code binder.   We may be looking up an unqualified 'f',
-- and there may be several imported 'f's too, which must not confuse us.
-- For example, this is OK:
--      import Foo( f )
--      infix 9 f       -- The 'f' here does not need to be qualified
--      f x = x         -- Nor here, of course
-- So we have to filter out the non-local ones.
--
-- A separate function (importsFromLocalDecls) reports duplicate top level
-- decls, so here it's safe just to choose an arbitrary one.
--
-- There should never be a qualified name in a binding position in Haskell,
-- but there can be if we have read in an external-Core file.
-- The Haskell parser checks for the illegal qualified name in Haskell
-- source files, so we don't need to do so here.

lookupTopBndrRn_maybe rdr_name
  | Just name <- isExact_maybe rdr_name
  = do { name' <- lookupExactOcc name; return (Just name') }

  | Just (rdr_mod, rdr_occ) <- isOrig_maybe rdr_name
        -- This deals with the case of derived bindings, where
        -- we don't bother to call newTopSrcBinder first
        -- We assume there is no "parent" name
  = do  { loc <- getSrcSpanM
        ; n <- newGlobalBinder rdr_mod rdr_occ loc
        ; return (Just n)}

  | otherwise
  = do  {  -- Check for operators in type or class declarations
           -- See Note [Type and class operator definitions]
          let occ = rdrNameOcc rdr_name
        ; when (isTcOcc occ && isSymOcc occ)
               (do { op_ok <- xoptM Opt_TypeOperators
                   ; unless op_ok (addErr (opDeclErr rdr_name)) })

        ; mb_gre <- lookupGreLocalRn_maybe rdr_name
        ; case mb_gre of
                Nothing  -> return Nothing
                Just gre -> return (Just $ gre_name gre) }


-----------------------------------------------
lookupExactOcc :: Name -> RnM Name
-- See Note [Looking up Exact RdrNames]
lookupExactOcc name
  | Just thing <- wiredInNameTyThing_maybe name
  , Just tycon <- case thing of
                    ATyCon tc   -> Just tc
                    ADataCon dc -> Just (dataConTyCon dc)
                    _           -> Nothing
  , isTupleTyCon tycon
  = do { checkTupSize (tyConArity tycon)
       ; return name }

  | isExternalName name
  = return name

  | otherwise
  = do { env <- getGlobalRdrEnv
       ; let -- See Note [Splicing Exact names]
             main_occ =  nameOccName name
             demoted_occs = case demoteOccName main_occ of
                              Just occ -> [occ]
                              Nothing  -> []
             gres = [ gre | occ <- main_occ : demoted_occs
                          , gre <- lookupGlobalRdrEnv env occ
                          , gre_name gre == name ]
       ; case gres of
           []    -> -- See Note [Splicing Exact names]
                    do { lcl_env <- getLocalRdrEnv
                       ; unless (name `inLocalRdrEnvScope` lcl_env) $
#ifdef GHCI
                         do { th_topnames_var <- fmap tcg_th_topnames getGblEnv
                            ; th_topnames <- readTcRef th_topnames_var
                            ; unless (name `elemNameSet` th_topnames)
                                     (addErr exact_nm_err)
                            }
#else /* !GHCI */
                         addErr exact_nm_err
#endif /* !GHCI */
                       ; return name
                       }

           [gre] -> return (gre_name gre)
           _     -> pprPanic "lookupExactOcc" (ppr name $$ ppr gres) }

  where
    exact_nm_err = hang (ptext (sLit "The exact Name") <+> quotes (ppr name) <+> ptext (sLit "is not in scope"))
                      2 (vcat [ ptext (sLit "Probable cause: you used a unique Template Haskell name (NameU), ")
                              , ptext (sLit "perhaps via newName, but did not bind it")
                              , ptext (sLit "If that's it, then -ddump-splices might be useful") ])

-----------------------------------------------
lookupInstDeclBndr :: Name -> SDoc -> RdrName -> RnM Name
-- This is called on the method name on the left-hand side of an
-- instance declaration binding. eg.  instance Functor T where
--                                       fmap = ...
--                                       ^^^^ called on this
-- Regardless of how many unqualified fmaps are in scope, we want
-- the one that comes from the Functor class.
--
-- Furthermore, note that we take no account of whether the
-- name is only in scope qualified.  I.e. even if method op is
-- in scope as M.op, we still allow plain 'op' on the LHS of
-- an instance decl
--
-- The "what" parameter says "method" or "associated type",
-- depending on what we are looking up
lookupInstDeclBndr cls what rdr
  = do { when (isQual rdr)
              (addErr (badQualBndrErr rdr))
                -- In an instance decl you aren't allowed
                -- to use a qualified name for the method
                -- (Although it'd make perfect sense.)
       ; lookupSubBndrOcc False -- False => we don't give deprecated
                                -- warnings when a deprecated class
                                -- method is defined. We only warn
                                -- when it's used
                          (ParentIs cls) doc rdr }
  where
    doc = what <+> ptext (sLit "of class") <+> quotes (ppr cls)


-----------------------------------------------
lookupFamInstName :: Maybe Name -> Located RdrName -> RnM (Located Name)
-- Used for TyData and TySynonym family instances only,
-- See Note [Family instance binders]
lookupFamInstName (Just cls) tc_rdr  -- Associated type; c.f RnBinds.rnMethodBind
  = wrapLocM (lookupInstDeclBndr cls (ptext (sLit "associated type"))) tc_rdr
lookupFamInstName Nothing tc_rdr     -- Family instance; tc_rdr is an *occurrence*
  = lookupLocatedOccRn tc_rdr

-----------------------------------------------
lookupConstructorFields :: Name -> RnM [Name]
-- Look up the fields of a given constructor
--   *  For constructors from this module, use the record field env,
--      which is itself gathered from the (as yet un-typechecked)
--      data type decls
--
--    * For constructors from imported modules, use the *type* environment
--      since imported modles are already compiled, the info is conveniently
--      right there

lookupConstructorFields con_name
  = do  { this_mod <- getModule
        ; if nameIsLocalOrFrom this_mod con_name then
          do { RecFields field_env _ <- getRecFieldEnv
             ; return (lookupNameEnv field_env con_name `orElse` []) }
          else
          do { con <- tcLookupDataCon con_name
             ; return (dataConFieldLabels con) } }

-----------------------------------------------
-- Used for record construction and pattern matching
-- When the -XDisambiguateRecordFields flag is on, take account of the
-- constructor name to disambiguate which field to use; it's just the
-- same as for instance decls
--
-- NB: Consider this:
--      module Foo where { data R = R { fld :: Int } }
--      module Odd where { import Foo; fld x = x { fld = 3 } }
-- Arguably this should work, because the reference to 'fld' is
-- unambiguous because there is only one field id 'fld' in scope.
-- But currently it's rejected.

lookupSubBndrOcc :: Bool
                 -> Parent  -- NoParent   => just look it up as usual
                            -- ParentIs p => use p to disambiguate
                 -> SDoc -> RdrName
                 -> RnM Name
lookupSubBndrOcc warnIfDeprec parent doc rdr_name
  | Just n <- isExact_maybe rdr_name   -- This happens in derived code
  = lookupExactOcc n

  | Just (rdr_mod, rdr_occ) <- isOrig_maybe rdr_name
  = lookupOrig rdr_mod rdr_occ

  | otherwise   -- Find all the things the rdr-name maps to
  = do  {       -- and pick the one with the right parent namep
          env <- getGlobalRdrEnv
        ; case lookupSubBndrGREs env parent rdr_name of
                -- NB: lookupGlobalRdrEnv, not lookupGRE_RdrName!
                --     The latter does pickGREs, but we want to allow 'x'
                --     even if only 'M.x' is in scope
            [gre] -> do { addUsedRdrName warnIfDeprec gre (used_rdr_name gre)
                          -- Add a usage; this is an *occurrence* site
                        ; return (gre_name gre) }
            []    -> do { addErr (unknownSubordinateErr doc rdr_name)
                        ; return (mkUnboundName rdr_name) }
            gres  -> do { addNameClashErrRn rdr_name gres
                        ; return (gre_name (head gres)) } }
  where
    -- Note [Usage for sub-bndrs]
    used_rdr_name gre
      | isQual rdr_name = rdr_name
      | otherwise       = greRdrName gre

greRdrName :: GlobalRdrElt -> RdrName
greRdrName gre
  = case gre_prov gre of
      LocalDef    -> unqual_rdr
      Imported is -> used_rdr_name_from_is is

  where
    occ = nameOccName (gre_name gre)
    unqual_rdr = mkRdrUnqual occ

    used_rdr_name_from_is imp_specs     -- rdr_name is unqualified
      | not (all (is_qual . is_decl) imp_specs)
      = unqual_rdr  -- An unqualified import is available
      | otherwise
      =             -- Only qualified imports available, so make up
                    -- a suitable qualifed name from the first imp_spec
        ASSERT( not (null imp_specs) )
        mkRdrQual (is_as (is_decl (head imp_specs))) occ

lookupSubBndrGREs :: GlobalRdrEnv -> Parent -> RdrName -> [GlobalRdrElt]
-- If Parent = NoParent, just do a normal lookup
-- If Parent = Parent p then find all GREs that
--   (a) have parent p
--   (b) for Unqual, are in scope qualified or unqualified
--       for Qual, are in scope with that qualification
lookupSubBndrGREs env parent rdr_name
  = case parent of
      NoParent   -> pickGREs rdr_name gres
      ParentIs p
        | isUnqual rdr_name -> filter (parent_is p) gres
        | otherwise         -> filter (parent_is p) (pickGREs rdr_name gres)

  where
    gres = lookupGlobalRdrEnv env (rdrNameOcc rdr_name)

    parent_is p (GRE { gre_par = ParentIs p' }) = p == p'
    parent_is _ _                               = False
\end{code}

Note [Family instance binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  data family F a
  data instance F T = X1 | X2

The 'data instance' decl has an *occurrence* of F (and T), and *binds*
X1 and X2.  (This is unlike a normal data type declaration which would
bind F too.)  So we want an AvailTC F [X1,X2].

Now consider a similar pair:
  class C a where
    data G a
  instance C S where
    data G S = Y1 | Y2

The 'data G S' *binds* Y1 and Y2, and has an *occurrence* of G.

But there is a small complication: in an instance decl, we don't use
qualified names on the LHS; instead we use the class to disambiguate.
Thus:
  module M where
    import Blib( G )
    class C a where
      data G a
    instance C S where
      data G S = Y1 | Y2
Even though there are two G's in scope (M.G and Blib.G), the occurence
of 'G' in the 'instance C S' decl is unambiguous, because C has only
one associated type called G. This is exactly what happens for methods,
and it is only consistent to do the same thing for types. That's the
role of the function lookupTcdName; the (Maybe Name) give the class of
the encloseing instance decl, if any.

Note [Looking up Exact RdrNames]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Exact RdrNames are generated by Template Haskell.  See Note [Binders
in Template Haskell] in Convert.

For data types and classes have Exact system Names in the binding
positions for constructors, TyCons etc.  For example
    [d| data T = MkT Int |]
when we splice in and Convert to HsSyn RdrName, we'll get
    data (Exact (system Name "T")) = (Exact (system Name "MkT")) ...
These System names are generated by Convert.thRdrName

But, constructors and the like need External Names, not System Names!
So we do the following

 * In RnEnv.newGlobalBinder we spot Exact RdrNames that wrap a
   non-External Name, and make an External name for it. This is
   the name that goes in the GlobalRdrEnv

 * When looking up an occurrence of an Exact name, done in
   RnEnv.lookupExactOcc, we find the Name with the right unique in the
   GlobalRdrEnv, and use the one from the envt -- it will be an
   External Name in the case of the data type/constructor above.

 * Exact names are also use for purely local binders generated
   by TH, such as    \x_33. x_33
   Both binder and occurrence are Exact RdrNames.  The occurrence
   gets looked up in the LocalRdrEnv by RnEnv.lookupOccRn, and
   misses, because lookupLocalRdrEnv always returns Nothing for
   an Exact Name.  Now we fall through to lookupExactOcc, which
   will find the Name is not in the GlobalRdrEnv, so we just use
   the Exact supplied Name.

Note [Splicing Exact names]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the splice $(do { x <- newName "x"; return (VarE x) })
This will generate a (HsExpr RdrName) term that mentions the
Exact RdrName "x_56" (or whatever), but does not bind it.  So
when looking such Exact names we want to check that it's in scope,
otherwise the type checker will get confused.  To do this we need to
keep track of all the Names in scope, and the LocalRdrEnv does just that;
we consult it with RdrName.inLocalRdrEnvScope.

There is another wrinkle.  With TH and -XDataKinds, consider
   $( [d| data Nat = Zero
          data T = MkT (Proxy 'Zero)  |] )
After splicing, but before renaming we get this:
   data Nat_77{tc} = Zero_78{d}
   data T_79{tc} = MkT_80{d} (Proxy 'Zero_78{tc})  |] )
THe occurrence of 'Zero in the data type for T has the right unique,
but it has a TcClsName name-space in its OccName.  (This is set by
the ctxt_ns argument of Convert.thRdrName.)  When we check that is
in scope in the GlobalRdrEnv, we need to look up the DataName namespace
too.  (An alternative would be to make the GlobalRdrEnv also have
a Name -> GRE mapping.)

Note [Usage for sub-bndrs]
~~~~~~~~~~~~~~~~~~~~~~~~~~
If you have this
   import qualified M( C( f ) )
   instance M.C T where
     f x = x
then is the qualified import M.f used?  Obviously yes.
But the RdrName used in the instance decl is unqualified.  In effect,
we fill in the qualification by looking for f's whose class is M.C
But when adding to the UsedRdrNames we must make that qualification
explicit (saying "used  M.f"), otherwise we get "Redundant import of M.f".

So we make up a suitable (fake) RdrName.  But be careful
   import qualifed M
   import M( C(f) )
   instance C T where
     f x = x
Here we want to record a use of 'f', not of 'M.f', otherwise
we'll miss the fact that the qualified import is redundant.

--------------------------------------------------
--              Occurrences
--------------------------------------------------

\begin{code}
getLookupOccRn :: RnM (Name -> Maybe Name)
getLookupOccRn
  = do local_env <- getLocalRdrEnv
       return (lookupLocalRdrOcc local_env . nameOccName)

lookupLocatedOccRn :: Located RdrName -> RnM (Located Name)
lookupLocatedOccRn = wrapLocM lookupOccRn

lookupLocalOccRn_maybe :: RdrName -> RnM (Maybe Name)
-- Just look in the local environment
lookupLocalOccRn_maybe rdr_name
  = do { local_env <- getLocalRdrEnv
       ; return (lookupLocalRdrEnv local_env rdr_name) }

lookupLocalOccThLvl_maybe :: Name -> RnM (Maybe (TopLevelFlag, ThLevel))
-- Just look in the local environment
lookupLocalOccThLvl_maybe name
  = do { lcl_env <- getLclEnv
       ; return (lookupNameEnv (tcl_th_bndrs lcl_env) name) }

-- lookupOccRn looks up an occurrence of a RdrName
lookupOccRn :: RdrName -> RnM Name
lookupOccRn rdr_name
  = do { mb_name <- lookupOccRn_maybe rdr_name
       ; case mb_name of
           Just name -> return name
           Nothing   -> reportUnboundName rdr_name }

lookupKindOccRn :: RdrName -> RnM Name
-- Looking up a name occurring in a kind
lookupKindOccRn rdr_name
  = do { mb_name <- lookupOccRn_maybe rdr_name
       ; case mb_name of
           Just name -> return name
           Nothing   -> reportUnboundName rdr_name  }

-- lookupPromotedOccRn looks up an optionally promoted RdrName.
lookupTypeOccRn :: RdrName -> RnM Name
-- see Note [Demotion]
lookupTypeOccRn rdr_name
  = do { mb_name <- lookupOccRn_maybe rdr_name
       ; case mb_name of {
             Just name -> return name ;
             Nothing   -> lookup_demoted rdr_name } }

lookup_demoted :: RdrName -> RnM Name
lookup_demoted rdr_name
  | Just demoted_rdr <- demoteRdrName rdr_name
    -- Maybe it's the name of a *data* constructor
  = do { data_kinds <- xoptM Opt_DataKinds
       ; mb_demoted_name <- lookupOccRn_maybe demoted_rdr
       ; case mb_demoted_name of
           Nothing -> reportUnboundName rdr_name
           Just demoted_name
             | data_kinds -> return demoted_name
             | otherwise  -> unboundNameX WL_Any rdr_name suggest_dk }

  | otherwise
  = reportUnboundName rdr_name

  where
    suggest_dk = ptext (sLit "A data constructor of that name is in scope; did you mean DataKinds?")
\end{code}

Note [Demotion]
~~~~~~~~~~~~~~~
When the user writes:
  data Nat = Zero | Succ Nat
  foo :: f Zero -> Int

'Zero' in the type signature of 'foo' is parsed as:
  HsTyVar ("Zero", TcClsName)

When the renamer hits this occurence of 'Zero' it's going to realise
that it's not in scope. But because it is renaming a type, it knows
that 'Zero' might be a promoted data constructor, so it will demote
its namespace to DataName and do a second lookup.

The final result (after the renamer) will be:
  HsTyVar ("Zero", DataName)

\begin{code}
--              Use this version to get tracing
--
-- lookupOccRn_maybe, lookupOccRn_maybe' :: RdrName -> RnM (Maybe Name)
-- lookupOccRn_maybe rdr_name
--  = do { mb_res <- lookupOccRn_maybe' rdr_name
--       ; gbl_rdr_env   <- getGlobalRdrEnv
--       ; local_rdr_env <- getLocalRdrEnv
--       ; traceRn $ text "lookupOccRn_maybe" <+>
--           vcat [ ppr rdr_name <+> ppr (getUnique (rdrNameOcc rdr_name))
--                , ppr mb_res
--                , text "Lcl env" <+> ppr local_rdr_env
--                , text "Gbl env" <+> ppr [ (getUnique (nameOccName (gre_name (head gres'))),gres') | gres <- occEnvElts gbl_rdr_env
--                                         , let gres' = filter isLocalGRE gres, not (null gres') ] ]
--       ; return mb_res }

lookupOccRn_maybe :: RdrName -> RnM (Maybe Name)
-- lookupOccRn looks up an occurrence of a RdrName
lookupOccRn_maybe rdr_name
  = do { local_env <- getLocalRdrEnv
       ; case lookupLocalRdrEnv local_env rdr_name of {
          Just name -> return (Just name) ;
          Nothing   -> do
       { mb_name <- lookupGlobalOccRn_maybe rdr_name
       ; case mb_name of {
                Just name  -> return (Just name) ;
                Nothing -> do
       { dflags  <- getDynFlags
       ; is_ghci <- getIsGHCi   -- This test is not expensive,
                                -- and only happens for failed lookups
       ; lookupQualifiedNameGHCi dflags is_ghci rdr_name } } } } }

lookupGlobalOccRn :: RdrName -> RnM Name
-- lookupGlobalOccRn is like lookupOccRn, except that it looks in the global
-- environment.  Adds an error message if the RdrName is not in scope.
lookupGlobalOccRn rdr_name
  = do { mb_name <- lookupGlobalOccRn_maybe rdr_name
       ; case mb_name of
           Just n  -> return n
           Nothing -> do { traceRn (text "lookupGlobalOccRn" <+> ppr rdr_name)
                         ; unboundName WL_Global rdr_name } }

lookupGlobalOccRn_maybe :: RdrName -> RnM (Maybe Name)
-- No filter function; does not report an error on failure

lookupGlobalOccRn_maybe rdr_name
  | Just n <- isExact_maybe rdr_name   -- This happens in derived code
  = do { n' <- lookupExactOcc n; return (Just n') }

  | Just (rdr_mod, rdr_occ) <- isOrig_maybe rdr_name
  = do { n <- lookupOrig rdr_mod rdr_occ
       ; return (Just n) }

  | otherwise
  = do  { mb_gre <- lookupGreRn_maybe rdr_name
        ; case mb_gre of
                Nothing  -> return Nothing
                Just gre -> return (Just (gre_name gre)) }


--------------------------------------------------
--      Lookup in the Global RdrEnv of the module
--------------------------------------------------

lookupGreRn_maybe :: RdrName -> RnM (Maybe GlobalRdrElt)
-- Just look up the RdrName in the GlobalRdrEnv
lookupGreRn_maybe rdr_name
  = lookupGreRn_help rdr_name (lookupGRE_RdrName rdr_name)

lookupGreRn :: RdrName -> RnM GlobalRdrElt
-- If not found, add error message, and return a fake GRE
lookupGreRn rdr_name
  = do  { mb_gre <- lookupGreRn_maybe rdr_name
        ; case mb_gre of {
            Just gre -> return gre ;
            Nothing  -> do
        { traceRn (text "lookupGreRn" <+> ppr rdr_name)
        ; name <- unboundName WL_Global rdr_name
        ; return (GRE { gre_name = name, gre_par = NoParent,
                        gre_prov = LocalDef }) }}}

lookupGreLocalRn_maybe :: RdrName -> RnM (Maybe GlobalRdrElt)
-- Similar, but restricted to locally-defined things
lookupGreLocalRn_maybe rdr_name
  = lookupGreRn_help rdr_name lookup_fn
  where
    lookup_fn env = filter isLocalGRE (lookupGRE_RdrName rdr_name env)

lookupGreRn_help :: RdrName                     -- Only used in error message
                 -> (GlobalRdrEnv -> [GlobalRdrElt])    -- Lookup function
                 -> RnM (Maybe GlobalRdrElt)
-- Checks for exactly one match; reports deprecations
-- Returns Nothing, without error, if too few
lookupGreRn_help rdr_name lookup
  = do  { env <- getGlobalRdrEnv
        ; case lookup env of
            []    -> return Nothing
            [gre] -> do { addUsedRdrName True gre rdr_name
                        ; return (Just gre) }
            gres  -> do { addNameClashErrRn rdr_name gres
                        ; return (Just (head gres)) } }
\end{code}

%*********************************************************
%*                                                      *
                Deprecations
%*                                                      *
%*********************************************************

Note [Handling of deprecations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* We report deprecations at each *occurrence* of the deprecated thing
  (see Trac #5867)

* We do not report deprectations for locally-definded names. For a
  start, we may be exporting a deprecated thing. Also we may use a
  deprecated thing in the defn of another deprecated things.  We may
  even use a deprecated thing in the defn of a non-deprecated thing,
  when changing a module's interface.

* addUsedRdrNames: we do not report deprecations for sub-binders:
     - the ".." completion for records
     - the ".." in an export item 'T(..)'
     - the things exported by a module export 'module M'

\begin{code}
addUsedRdrName :: Bool -> GlobalRdrElt -> RdrName -> RnM ()
-- Record usage of imported RdrNames
addUsedRdrName warnIfDeprec gre rdr
  | isLocalGRE gre = return ()  -- No call to warnIfDeprecated
                                -- See Note [Handling of deprecations]
  | otherwise      = do { env <- getGblEnv
                        ; when warnIfDeprec $ warnIfDeprecated gre
                        ; updMutVar (tcg_used_rdrnames env)
                                    (\s -> Set.insert rdr s) }

addUsedRdrNames :: [RdrName] -> RnM ()
-- Record used sub-binders
-- We don't check for imported-ness here, because it's inconvenient
-- and not stritly necessary.
-- NB: no call to warnIfDeprecated; see Note [Handling of deprecations]
addUsedRdrNames rdrs
  = do { env <- getGblEnv
       ; updMutVar (tcg_used_rdrnames env)
                   (\s -> foldr Set.insert s rdrs) }

warnIfDeprecated :: GlobalRdrElt -> RnM ()
warnIfDeprecated gre@(GRE { gre_name = name, gre_prov = Imported (imp_spec : _) })
  = do { dflags <- getDynFlags
       ; when (wopt Opt_WarnWarningsDeprecations dflags) $
         do { iface <- loadInterfaceForName doc name
            ; case lookupImpDeprec iface gre of
                Just txt -> addWarn (mk_msg txt)
                Nothing  -> return () } }
  where
    mk_msg txt = sep [ sep [ ptext (sLit "In the use of")
                             <+> pprNonVarNameSpace (occNameSpace (nameOccName name))
                             <+> quotes (ppr name)
                           , parens imp_msg <> colon ]
                     , ppr txt ]

    name_mod = ASSERT2( isExternalName name, ppr name ) nameModule name
    imp_mod  = importSpecModule imp_spec
    imp_msg  = ptext (sLit "imported from") <+> ppr imp_mod <> extra
    extra | imp_mod == moduleName name_mod = empty
          | otherwise = ptext (sLit ", but defined in") <+> ppr name_mod

    doc = ptext (sLit "The name") <+> quotes (ppr name) <+> ptext (sLit "is mentioned explicitly")

warnIfDeprecated _ = return ()   -- No deprecations for things defined locally

lookupImpDeprec :: ModIface -> GlobalRdrElt -> Maybe WarningTxt
lookupImpDeprec iface gre
  = mi_warn_fn iface (gre_name gre) `mplus`  -- Bleat if the thing,
    case gre_par gre of                      -- or its parent, is warn'd
       ParentIs p -> mi_warn_fn iface p
       NoParent   -> Nothing
\end{code}

Note [Used names with interface not loaded]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's (just) possible to find a used
Name whose interface hasn't been loaded:

a) It might be a WiredInName; in that case we may not load
   its interface (although we could).

b) It might be GHC.Real.fromRational, or GHC.Num.fromInteger
   These are seen as "used" by the renamer (if -XRebindableSyntax)
   is on), but the typechecker may discard their uses
   if in fact the in-scope fromRational is GHC.Read.fromRational,
   (see tcPat.tcOverloadedLit), and the typechecker sees that the type
   is fixed, say, to GHC.Base.Float (see Inst.lookupSimpleInst).
   In that obscure case it won't force the interface in.

In both cases we simply don't permit deprecations;
this is, after all, wired-in stuff.


%*********************************************************
%*                                                      *
                GHCi support
%*                                                      *
%*********************************************************

A qualified name on the command line can refer to any module at
all: we try to load the interface if we don't already have it, just
as if there was an "import qualified M" declaration for every
module.

If we fail we just return Nothing, rather than bleating
about "attempting to use module ‛D’ (./D.hs) which is not loaded"
which is what loadSrcInterface does.

Note [Safe Haskell and GHCi]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We DONT do this Safe Haskell as we need to check imports. We can
and should instead check the qualified import but at the moment
this requires some refactoring so leave as a TODO

\begin{code}
lookupQualifiedNameGHCi :: DynFlags -> Bool -> RdrName -> RnM (Maybe Name)
lookupQualifiedNameGHCi dflags is_ghci rdr_name
  | Just (mod,occ) <- isQual_maybe rdr_name
  , is_ghci
  , gopt Opt_ImplicitImportQualified dflags   -- Enables this GHCi behaviour
  , not (safeDirectImpsReq dflags)            -- See Note [Safe Haskell and GHCi]
  = -- We want to behave as we would for a source file import here,
    -- and respect hiddenness of modules/packages, hence loadSrcInterface.
    do { res <- loadSrcInterface_maybe doc mod False Nothing
       ; case res of
           Succeeded iface
             | (n:ns) <- [ name
                         | avail <- mi_exports iface
                         , name  <- availNames avail
                         , nameOccName name == occ ]
             -> ASSERT(null ns) return (Just n)

           _ -> -- Either we couldn't load the interface, or
                -- we could but we didn't find the name in it
                do { traceRn (text "lookupQualifiedNameGHCi" <+> ppr rdr_name)
                   ; return Nothing } }

  | otherwise
  = return Nothing
  where
    doc = ptext (sLit "Need to find") <+> ppr rdr_name
\end{code}

Note [Looking up signature names]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lookupSigOccRn is used for type signatures and pragmas
Is this valid?
  module A
        import M( f )
        f :: Int -> Int
        f x = x
It's clear that the 'f' in the signature must refer to A.f
The Haskell98 report does not stipulate this, but it will!
So we must treat the 'f' in the signature in the same way
as the binding occurrence of 'f', using lookupBndrRn

However, consider this case:
        import M( f )
        f :: Int -> Int
        g x = x
We don't want to say 'f' is out of scope; instead, we want to
return the imported 'f', so that later on the reanamer will
correctly report "misplaced type sig".

Note [Signatures for top level things]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data HsSigCtxt = ... | TopSigCtxt NameSet Bool | ....

* The NameSet says what is bound in this group of bindings.
  We can't use isLocalGRE from the GlobalRdrEnv, because of this:
       f x = x
       $( ...some TH splice... )
       f :: Int -> Int
  When we encounter the signature for 'f', the binding for 'f'
  will be in the GlobalRdrEnv, and will be a LocalDef. Yet the
  signature is mis-placed

* The Bool says whether the signature is ok for a class method
  or record selector.  Consider
      infix 3 `f`          -- Yes, ok
      f :: C a => a -> a   -- No, not ok
      class C a where
        f :: a -> a

\begin{code}
data HsSigCtxt
  = TopSigCtxt NameSet Bool  -- At top level, binding these names
                             -- See Note [Signatures for top level things]
                             -- Bool <=> ok to give sig for
                             --          class method or record selctor
  | LocalBindCtxt NameSet    -- In a local binding, binding these names
  | ClsDeclCtxt   Name       -- Class decl for this class
  | InstDeclCtxt  Name       -- Intsance decl for this class
  | HsBootCtxt               -- Top level of a hs-boot file

lookupSigOccRn :: HsSigCtxt
               -> Sig RdrName
               -> Located RdrName -> RnM (Located Name)
lookupSigOccRn ctxt sig
  = wrapLocM $ \ rdr_name ->
    do { mb_name <- lookupBindGroupOcc ctxt (hsSigDoc sig) rdr_name
       ; case mb_name of
           Left err   -> do { addErr err; return (mkUnboundName rdr_name) }
           Right name -> return name }

lookupBindGroupOcc :: HsSigCtxt
                   -> SDoc
                   -> RdrName -> RnM (Either MsgDoc Name)
-- Looks up the RdrName, expecting it to resolve to one of the
-- bound names passed in.  If not, return an appropriate error message
--
-- See Note [Looking up signature names]
lookupBindGroupOcc ctxt what rdr_name
  | Just n <- isExact_maybe rdr_name
  = do { n' <- lookupExactOcc n
       ; return (Right n') }  -- Maybe we should check the side conditions
                              -- but it's a pain, and Exact things only show
                              -- up when you know what you are doing

  | Just (rdr_mod, rdr_occ) <- isOrig_maybe rdr_name
  = do { n' <- lookupOrig rdr_mod rdr_occ
       ; return (Right n') }

  | otherwise
  = case ctxt of
      HsBootCtxt            -> lookup_top (const True)       True
      TopSigCtxt ns meth_ok -> lookup_top (`elemNameSet` ns) meth_ok
      LocalBindCtxt ns      -> lookup_group ns
      ClsDeclCtxt  cls      -> lookup_cls_op cls
      InstDeclCtxt cls      -> lookup_cls_op cls
  where
    lookup_cls_op cls
      = do { env <- getGlobalRdrEnv
           ; let gres = lookupSubBndrGREs env (ParentIs cls) rdr_name
           ; case gres of
               []      -> return (Left (unknownSubordinateErr doc rdr_name))
               (gre:_) -> return (Right (gre_name gre)) }
                        -- If there is more than one local GRE for the
                        -- same OccName 'f', that will be reported separately
                        -- as a duplicate top-level binding for 'f'
      where
        doc = ptext (sLit "method of class") <+> quotes (ppr cls)

    lookup_top keep_me meth_ok
      = do { env <- getGlobalRdrEnv
           ; let all_gres = lookupGlobalRdrEnv env (rdrNameOcc rdr_name)
           ; case filter (keep_me . gre_name) all_gres of
               [] | null all_gres -> bale_out_with empty
                  | otherwise -> bale_out_with local_msg
               (gre:_)
                  | ParentIs {} <- gre_par gre
                  , not meth_ok
                  -> bale_out_with sub_msg
                  | otherwise
                  -> return (Right (gre_name gre)) }

    lookup_group bound_names  -- Look in the local envt (not top level)
      = do { local_env <- getLocalRdrEnv
           ; case lookupLocalRdrEnv local_env rdr_name of
               Just n
                 | n `elemNameSet` bound_names -> return (Right n)
                 | otherwise                   -> bale_out_with local_msg
               Nothing                         -> bale_out_with empty }

    bale_out_with msg
        = return (Left (sep [ ptext (sLit "The") <+> what
                                <+> ptext (sLit "for") <+> quotes (ppr rdr_name)
                           , nest 2 $ ptext (sLit "lacks an accompanying binding")]
                       $$ nest 2 msg))

    local_msg = parens $ ptext (sLit "The")  <+> what <+> ptext (sLit "must be given where")
                           <+> quotes (ppr rdr_name) <+> ptext (sLit "is declared")

    sub_msg = parens $ ptext (sLit "You cannot give a") <+> what
                       <+> ptext (sLit "for a record selector or class method")


---------------
lookupLocalTcNames :: HsSigCtxt -> SDoc -> RdrName -> RnM [Name]
-- GHC extension: look up both the tycon and data con or variable.
-- Used for top-level fixity signatures and deprecations.
-- Complain if neither is in scope.
-- See Note [Fixity signature lookup]
lookupLocalTcNames ctxt what rdr_name
  = do { mb_gres <- mapM lookup (dataTcOccs rdr_name)
       ; let (errs, names) = splitEithers mb_gres
       ; when (null names) $ addErr (head errs) -- Bleat about one only
       ; return names }
  where
    lookup = lookupBindGroupOcc ctxt what

dataTcOccs :: RdrName -> [RdrName]
-- Return both the given name and the same name promoted to the TcClsName
-- namespace.  This is useful when we aren't sure which we are looking at.
dataTcOccs rdr_name
  | Just n <- isExact_maybe rdr_name
  , not (isBuiltInSyntax n)   -- See Note [dataTcOccs and Exact Names]
  = [rdr_name]
  | isDataOcc occ || isVarOcc occ
  = [rdr_name, rdr_name_tc]
  | otherwise
  = [rdr_name]
  where
    occ = rdrNameOcc rdr_name
    rdr_name_tc = setRdrNameSpace rdr_name tcName
\end{code}

Note [dataTcOccs and Exact Names]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Exact RdrNames can occur in code generated by Template Haskell, and generally
those references are, well, exact, so it's wrong to return the TyClsName too.
But there is an awkward exception for built-in syntax. Example in GHCi
   :info []
This parses as the Exact RdrName for nilDataCon, but we also want
the list type constructor.

Note that setRdrNameSpace on an Exact name requires the Name to be External,
which it always is for built in syntax.

%*********************************************************
%*                                                      *
                Fixities
%*                                                      *
%*********************************************************

Note [Fixity signature lookup]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A fixity declaration like

    infixr 2 ?

can refer to a value-level operator, e.g.:

    (?) :: String -> String -> String

or a type-level operator, like:

    data (?) a b = A a | B b

so we extend the lookup of the reader name '?' to the TcClsName namespace, as
well as the original namespace.

The extended lookup is also used in other places, like resolution of
deprecation declarations, and lookup of names in GHCi.

\begin{code}
--------------------------------
type FastStringEnv a = UniqFM a         -- Keyed by FastString


emptyFsEnv  :: FastStringEnv a
lookupFsEnv :: FastStringEnv a -> FastString -> Maybe a
extendFsEnv :: FastStringEnv a -> FastString -> a -> FastStringEnv a
mkFsEnv     :: [(FastString,a)] -> FastStringEnv a

emptyFsEnv  = emptyUFM
lookupFsEnv = lookupUFM
extendFsEnv = addToUFM
mkFsEnv     = listToUFM

--------------------------------
type MiniFixityEnv = FastStringEnv (Located Fixity)
        -- Mini fixity env for the names we're about
        -- to bind, in a single binding group
        --
        -- It is keyed by the *FastString*, not the *OccName*, because
        -- the single fixity decl       infix 3 T
        -- affects both the data constructor T and the type constrctor T
        --
        -- We keep the location so that if we find
        -- a duplicate, we can report it sensibly

--------------------------------
-- Used for nested fixity decls to bind names along with their fixities.
-- the fixities are given as a UFM from an OccName's FastString to a fixity decl

addLocalFixities :: MiniFixityEnv -> [Name] -> RnM a -> RnM a
addLocalFixities mini_fix_env names thing_inside
  = extendFixityEnv (mapCatMaybes find_fixity names) thing_inside
  where
    find_fixity name
      = case lookupFsEnv mini_fix_env (occNameFS occ) of
          Just (L _ fix) -> Just (name, FixItem occ fix)
          Nothing        -> Nothing
      where
        occ = nameOccName name
\end{code}

--------------------------------
lookupFixity is a bit strange.

* Nested local fixity decls are put in the local fixity env, which we
  find with getFixtyEnv

* Imported fixities are found in the HIT or PIT

* Top-level fixity decls in this module may be for Names that are
    either  Global         (constructors, class operations)
    or      Local/Exported (everything else)
  (See notes with RnNames.getLocalDeclBinders for why we have this split.)
  We put them all in the local fixity environment

\begin{code}
lookupFixityRn :: Name -> RnM Fixity
lookupFixityRn name
  | isUnboundName name
  = return (Fixity minPrecedence InfixL) 
    -- Minimise errors from ubound names; eg
    --    a>0 `foo` b>0
    -- where 'foo' is not in scope, should not give an error (Trac #7937)

  | otherwise
  = do { local_fix_env <- getFixityEnv
       ; case lookupNameEnv local_fix_env name of {
           Just (FixItem _ fix) -> return fix ;
           Nothing ->

    do { this_mod <- getModule
       ; if nameIsLocalOrFrom this_mod name || isInteractiveModule (nameModule name)
               -- Interactive modules are all in the fixity env,
               -- and don't have entries in the HPT
         then return defaultFixity
         else lookup_imported } } }
  where
    lookup_imported
      -- For imported names, we have to get their fixities by doing a
      -- loadInterfaceForName, and consulting the Ifaces that comes back
      -- from that, because the interface file for the Name might not
      -- have been loaded yet.  Why not?  Suppose you import module A,
      -- which exports a function 'f', thus;
      --        module CurrentModule where
      --          import A( f )
      --        module A( f ) where
      --          import B( f )
      -- Then B isn't loaded right away (after all, it's possible that
      -- nothing from B will be used).  When we come across a use of
      -- 'f', we need to know its fixity, and it's then, and only
      -- then, that we load B.hi.  That is what's happening here.
      --
      -- loadInterfaceForName will find B.hi even if B is a hidden module,
      -- and that's what we want.
      = do { iface <- loadInterfaceForName doc name
           ; traceRn (text "lookupFixityRn: looking up name in iface cache and found:" <+>
                      vcat [ppr name, ppr $ mi_fix_fn iface (nameOccName name)])
           ; return (mi_fix_fn iface (nameOccName name)) }

    doc = ptext (sLit "Checking fixity for") <+> ppr name

---------------
lookupTyFixityRn :: Located Name -> RnM Fixity
lookupTyFixityRn (L _ n) = lookupFixityRn n

\end{code}

%************************************************************************
%*                                                                      *
                        Rebindable names
        Dealing with rebindable syntax is driven by the
        Opt_RebindableSyntax dynamic flag.

        In "deriving" code we don't want to use rebindable syntax
        so we switch off the flag locally

%*                                                                      *
%************************************************************************

Haskell 98 says that when you say "3" you get the "fromInteger" from the
Standard Prelude, regardless of what is in scope.   However, to experiment
with having a language that is less coupled to the standard prelude, we're
trying a non-standard extension that instead gives you whatever "Prelude.fromInteger"
happens to be in scope.  Then you can
        import Prelude ()
        import MyPrelude as Prelude
to get the desired effect.

At the moment this just happens for
  * fromInteger, fromRational on literals (in expressions and patterns)
  * negate (in expressions)
  * minus  (arising from n+k patterns)
  * "do" notation

We store the relevant Name in the HsSyn tree, in
  * HsIntegral/HsFractional/HsIsString
  * NegApp
  * NPlusKPat
  * HsDo
respectively.  Initially, we just store the "standard" name (PrelNames.fromIntegralName,
fromRationalName etc), but the renamer changes this to the appropriate user
name if Opt_NoImplicitPrelude is on.  That is what lookupSyntaxName does.

We treat the orignal (standard) names as free-vars too, because the type checker
checks the type of the user thing against the type of the standard thing.

\begin{code}
lookupIfThenElse :: RnM (Maybe (SyntaxExpr Name), FreeVars)
-- Different to lookupSyntaxName because in the non-rebindable
-- case we desugar directly rather than calling an existing function
-- Hence the (Maybe (SyntaxExpr Name)) return type
lookupIfThenElse
  = do { rebind <- xoptM Opt_RebindableSyntax
       ; if not rebind
         then return (Nothing, emptyFVs)
         else do { ite <- lookupOccRn (mkVarUnqual (fsLit "ifThenElse"))
                 ; return (Just (HsVar ite), unitFV ite) } }

lookupSyntaxName :: Name                                -- The standard name
                 -> RnM (SyntaxExpr Name, FreeVars)     -- Possibly a non-standard name
lookupSyntaxName std_name
  = do { rebindable_on <- xoptM Opt_RebindableSyntax
       ; if not rebindable_on then 
           return (HsVar std_name, emptyFVs)
         else
            -- Get the similarly named thing from the local environment
           do { usr_name <- lookupOccRn (mkRdrUnqual (nameOccName std_name))
              ; return (HsVar usr_name, unitFV usr_name) } }

lookupSyntaxNames :: [Name]                          -- Standard names
                  -> RnM ([HsExpr Name], FreeVars)   -- See comments with HsExpr.ReboundNames
lookupSyntaxNames std_names
  = do { rebindable_on <- xoptM Opt_RebindableSyntax
       ; if not rebindable_on then 
             return (map HsVar std_names, emptyFVs)
        else
          do { usr_names <- mapM (lookupOccRn . mkRdrUnqual . nameOccName) std_names
             ; return (map HsVar usr_names, mkFVs usr_names) } }
\end{code}


%*********************************************************
%*                                                      *
\subsection{Binding}
%*                                                      *
%*********************************************************

\begin{code}
newLocalBndrRn :: Located RdrName -> RnM Name
-- Used for non-top-level binders.  These should
-- never be qualified.
newLocalBndrRn (L loc rdr_name)
  | Just name <- isExact_maybe rdr_name
  = return name -- This happens in code generated by Template Haskell
                -- See Note [Binders in Template Haskell] in Convert.lhs
  | otherwise
  = do { unless (isUnqual rdr_name)
                (addErrAt loc (badQualBndrErr rdr_name))
       ; uniq <- newUnique
       ; return (mkInternalName uniq (rdrNameOcc rdr_name) loc) }

newLocalBndrsRn :: [Located RdrName] -> RnM [Name]
newLocalBndrsRn = mapM newLocalBndrRn

---------------------
bindLocatedLocalsRn :: [Located RdrName]
                    -> ([Name] -> RnM a)
                    -> RnM a
bindLocatedLocalsRn rdr_names_w_loc enclosed_scope
  = do { checkDupRdrNames rdr_names_w_loc
       ; checkShadowedRdrNames rdr_names_w_loc

        -- Make fresh Names and extend the environment
       ; names <- newLocalBndrsRn rdr_names_w_loc
       ; bindLocalNames names (enclosed_scope names) }

bindLocalNames :: [Name] -> RnM a -> RnM a
bindLocalNames names enclosed_scope
  = do { lcl_env <- getLclEnv
       ; let th_level  = thLevel (tcl_th_ctxt lcl_env)
             th_bndrs' = extendNameEnvList (tcl_th_bndrs lcl_env)
                           [ (n, (NotTopLevel, th_level)) | n <- names ]
             rdr_env'  = extendLocalRdrEnvList (tcl_rdr lcl_env) names
       ; setLclEnv (lcl_env { tcl_th_bndrs = th_bndrs'
                            , tcl_rdr      = rdr_env' })
                    enclosed_scope }

bindLocalNamesFV :: [Name] -> RnM (a, FreeVars) -> RnM (a, FreeVars)
bindLocalNamesFV names enclosed_scope
  = do  { (result, fvs) <- bindLocalNames names enclosed_scope
        ; return (result, delFVs names fvs) }


-------------------------------------
        -- binLocalsFVRn is the same as bindLocalsRn
        -- except that it deals with free vars
bindLocatedLocalsFV :: [Located RdrName]
                    -> ([Name] -> RnM (a,FreeVars)) -> RnM (a, FreeVars)
bindLocatedLocalsFV rdr_names enclosed_scope
  = bindLocatedLocalsRn rdr_names       $ \ names ->
    do (thing, fvs) <- enclosed_scope names
       return (thing, delFVs names fvs)

-------------------------------------

extendTyVarEnvFVRn :: [Name] -> RnM (a, FreeVars) -> RnM (a, FreeVars)
        -- This function is used only in rnSourceDecl on InstDecl
extendTyVarEnvFVRn tyvars thing_inside = bindLocalNamesFV tyvars thing_inside

-------------------------------------
checkDupRdrNames :: [Located RdrName] -> RnM ()
-- Check for duplicated names in a binding group
checkDupRdrNames rdr_names_w_loc
  = mapM_ (dupNamesErr getLoc) dups
  where
    (_, dups) = removeDups (\n1 n2 -> unLoc n1 `compare` unLoc n2) rdr_names_w_loc

checkDupNames :: [Name] -> RnM ()
-- Check for duplicated names in a binding group
checkDupNames names = check_dup_names (filterOut isSystemName names)
                -- See Note [Binders in Template Haskell] in Convert

check_dup_names :: [Name] -> RnM ()
check_dup_names names
  = mapM_ (dupNamesErr nameSrcSpan) dups
  where
    (_, dups) = removeDups (\n1 n2 -> nameOccName n1 `compare` nameOccName n2) names

---------------------
checkShadowedRdrNames :: [Located RdrName] -> RnM ()
checkShadowedRdrNames loc_rdr_names
  = do { envs <- getRdrEnvs
       ; checkShadowedOccs envs get_loc_occ filtered_rdrs }
  where
    filtered_rdrs = filterOut (isExact . unLoc) loc_rdr_names
                -- See Note [Binders in Template Haskell] in Convert
    get_loc_occ (L loc rdr) = (loc,rdrNameOcc rdr)

checkDupAndShadowedNames :: (GlobalRdrEnv, LocalRdrEnv) -> [Name] -> RnM ()
checkDupAndShadowedNames envs names
  = do { check_dup_names filtered_names
       ; checkShadowedOccs envs get_loc_occ filtered_names }
  where
    filtered_names = filterOut isSystemName names
                -- See Note [Binders in Template Haskell] in Convert
    get_loc_occ name = (nameSrcSpan name, nameOccName name)

-------------------------------------
checkShadowedOccs :: (GlobalRdrEnv, LocalRdrEnv)
                  -> (a -> (SrcSpan, OccName))
                  -> [a] -> RnM ()
checkShadowedOccs (global_env,local_env) get_loc_occ ns
  = whenWOptM Opt_WarnNameShadowing $
    do  { traceRn (text "shadow" <+> ppr (map get_loc_occ ns))
        ; mapM_ check_shadow ns }
  where
    check_shadow n
        | startsWithUnderscore occ = return ()  -- Do not report shadowing for "_x"
                                                -- See Trac #3262
        | Just n <- mb_local = complain [ptext (sLit "bound at") <+> ppr (nameSrcLoc n)]
        | otherwise = do { gres' <- filterM is_shadowed_gre gres
                         ; complain (map pprNameProvenance gres') }
        where
          (loc,occ) = get_loc_occ n
          mb_local  = lookupLocalRdrOcc local_env occ
          gres      = lookupGRE_RdrName (mkRdrUnqual occ) global_env
                -- Make an Unqualified RdrName and look that up, so that
                -- we don't find any GREs that are in scope qualified-only

          complain []      = return ()
          complain pp_locs = addWarnAt loc (shadowedNameWarn occ pp_locs)

    is_shadowed_gre :: GlobalRdrElt -> RnM Bool
        -- Returns False for record selectors that are shadowed, when
        -- punning or wild-cards are on (cf Trac #2723)
    is_shadowed_gre gre@(GRE { gre_par = ParentIs _ })
        = do { dflags <- getDynFlags
             ; if (xopt Opt_RecordPuns dflags || xopt Opt_RecordWildCards dflags)
               then do { is_fld <- is_rec_fld gre; return (not is_fld) }
               else return True }
    is_shadowed_gre _other = return True

    is_rec_fld gre      -- Return True for record selector ids
        | isLocalGRE gre = do { RecFields _ fld_set <- getRecFieldEnv
                              ; return (gre_name gre `elemNameSet` fld_set) }
        | otherwise      = do { sel_id <- tcLookupField (gre_name gre)
                              ; return (isRecordSelector sel_id) }
\end{code}


%************************************************************************
%*                                                                      *
               What to do when a lookup fails
%*                                                                      *
%************************************************************************

\begin{code}
data WhereLooking = WL_Any        -- Any binding
                  | WL_Global     -- Any top-level binding (local or imported)
                  | WL_LocalTop   -- Any top-level binding in this module

reportUnboundName :: RdrName -> RnM Name
reportUnboundName rdr = unboundName WL_Any rdr

unboundName :: WhereLooking -> RdrName -> RnM Name
unboundName wl rdr = unboundNameX wl rdr empty

unboundNameX :: WhereLooking -> RdrName -> SDoc -> RnM Name
unboundNameX where_look rdr_name extra
  = do  { show_helpful_errors <- goptM Opt_HelpfulErrors
        ; let what = pprNonVarNameSpace (occNameSpace (rdrNameOcc rdr_name))
              err = unknownNameErr what rdr_name $$ extra
        ; if not show_helpful_errors
          then addErr err
          else do { suggestions <- unknownNameSuggestErr where_look rdr_name
                  ; addErr (err $$ suggestions) }
        ; return (mkUnboundName rdr_name) }

unknownNameErr :: SDoc -> RdrName -> SDoc
unknownNameErr what rdr_name
  = vcat [ hang (ptext (sLit "Not in scope:"))
              2 (what <+> quotes (ppr rdr_name))
         , extra ]
  where
    extra | rdr_name == forall_tv_RDR = perhapsForallMsg
          | otherwise                 = empty

type HowInScope = Either SrcSpan ImpDeclSpec
     -- Left loc    =>  locally bound at loc
     -- Right ispec =>  imported as specified by ispec

unknownNameSuggestErr :: WhereLooking -> RdrName -> RnM SDoc
unknownNameSuggestErr where_look tried_rdr_name
  = do { local_env <- getLocalRdrEnv
       ; global_env <- getGlobalRdrEnv
       ; dflags <- getDynFlags

       ; let all_possibilities :: [(String, (RdrName, HowInScope))]
             all_possibilities
                =  [ (showPpr dflags r, (r, Left loc))
                   | (r,loc) <- local_possibilities local_env ]
                ++ [ (showPpr dflags r, rp) | (r,rp) <- global_possibilities global_env ]

             suggest = fuzzyLookup (showPpr dflags tried_rdr_name) all_possibilities
             perhaps = ptext (sLit "Perhaps you meant")
             extra_err = case suggest of
                           []  -> empty
                           [p] -> perhaps <+> pp_item p
                           ps  -> sep [ perhaps <+> ptext (sLit "one of these:")
                                      , nest 2 (pprWithCommas pp_item ps) ]
       ; return extra_err }
  where
    pp_item :: (RdrName, HowInScope) -> SDoc
    pp_item (rdr, Left loc) = quotes (ppr rdr) <+> loc' -- Locally defined
        where loc' = case loc of
                     UnhelpfulSpan l -> parens (ppr l)
                     RealSrcSpan l -> parens (ptext (sLit "line") <+> int (srcSpanStartLine l))
    pp_item (rdr, Right is) = quotes (ppr rdr) <+>   -- Imported
                              parens (ptext (sLit "imported from") <+> ppr (is_mod is))

    tried_occ     = rdrNameOcc tried_rdr_name
    tried_is_sym  = isSymOcc tried_occ
    tried_ns      = occNameSpace tried_occ
    tried_is_qual = isQual tried_rdr_name

    correct_name_space occ =  occNameSpace occ == tried_ns
                           && isSymOcc occ == tried_is_sym
        -- Treat operator and non-operators as non-matching
        -- This heuristic avoids things like
        --      Not in scope 'f'; perhaps you meant '+' (from Prelude)

    local_ok = case where_look of { WL_Any -> True; _ -> False }
    local_possibilities :: LocalRdrEnv -> [(RdrName, SrcSpan)]
    local_possibilities env
      | tried_is_qual = []
      | not local_ok  = []
      | otherwise     = [ (mkRdrUnqual occ, nameSrcSpan name)
                        | name <- localRdrEnvElts env
                        , let occ = nameOccName name
                        , correct_name_space occ]

    gre_ok :: GlobalRdrElt -> Bool
    gre_ok = case where_look of
                   WL_LocalTop -> isLocalGRE
                   _           -> \_ -> True

    global_possibilities :: GlobalRdrEnv -> [(RdrName, (RdrName, HowInScope))]
    global_possibilities global_env
      | tried_is_qual = [ (rdr_qual, (rdr_qual, how))
                        | gre <- globalRdrEnvElts global_env
                        , gre_ok gre
                        , let name = gre_name gre
                              occ  = nameOccName name
                        , correct_name_space occ
                        , (mod, how) <- quals_in_scope name (gre_prov gre)
                        , let rdr_qual = mkRdrQual mod occ ]

      | otherwise = [ (rdr_unqual, pair)
                    | gre <- globalRdrEnvElts global_env
                    , gre_ok gre
                    , let name = gre_name gre
                          prov = gre_prov gre
                          occ  = nameOccName name
                          rdr_unqual = mkRdrUnqual occ
                    , correct_name_space occ
                    , pair <- case (unquals_in_scope name prov, quals_only occ prov) of
                                (how:_, _)    -> [ (rdr_unqual, how) ]
                                ([],    pr:_) -> [ pr ]  -- See Note [Only-quals]
                                ([],    [])   -> [] ]

              -- Note [Only-quals]
              -- The second alternative returns those names with the same
              -- OccName as the one we tried, but live in *qualified* imports
              -- e.g. if you have:
              --
              -- > import qualified Data.Map as Map
              -- > foo :: Map
              --
              -- then we suggest @Map.Map@.

    --------------------
    unquals_in_scope :: Name -> Provenance -> [HowInScope]
    unquals_in_scope n LocalDef      = [ Left (nameSrcSpan n) ]
    unquals_in_scope _ (Imported is) = [ Right ispec
                                       | i <- is, let ispec = is_decl i
                                       , not (is_qual ispec) ]

    --------------------
    quals_in_scope :: Name -> Provenance -> [(ModuleName, HowInScope)]
    -- Ones for which the qualified version is in scope
    quals_in_scope n LocalDef      = case nameModule_maybe n of
                                       Nothing -> []
                                       Just m  -> [(moduleName m, Left (nameSrcSpan n))]
    quals_in_scope _ (Imported is) = [ (is_as ispec, Right ispec)
                                     | i <- is, let ispec = is_decl i ]

    --------------------
    quals_only :: OccName -> Provenance -> [(RdrName, HowInScope)]
    -- Ones for which *only* the qualified version is in scope
    quals_only _   LocalDef      = []
    quals_only occ (Imported is) = [ (mkRdrQual (is_as ispec) occ, Right ispec)
                                   | i <- is, let ispec = is_decl i, is_qual ispec ]
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Free variable manipulation}
%*                                                                      *
%************************************************************************

\begin{code}
-- A useful utility
addFvRn :: FreeVars -> RnM (thing, FreeVars) -> RnM (thing, FreeVars)
addFvRn fvs1 thing_inside = do { (res, fvs2) <- thing_inside
                               ; return (res, fvs1 `plusFV` fvs2) }

mapFvRn :: (a -> RnM (b, FreeVars)) -> [a] -> RnM ([b], FreeVars)
mapFvRn f xs = do stuff <- mapM f xs
                  case unzip stuff of
                      (ys, fvs_s) -> return (ys, plusFVs fvs_s)

mapMaybeFvRn :: (a -> RnM (b, FreeVars)) -> Maybe a -> RnM (Maybe b, FreeVars)
mapMaybeFvRn _ Nothing = return (Nothing, emptyFVs)
mapMaybeFvRn f (Just x) = do { (y, fvs) <- f x; return (Just y, fvs) }

-- because some of the rename functions are CPSed:
-- maps the function across the list from left to right;
-- collects all the free vars into one set
mapFvRnCPS :: (a  -> (b   -> RnM c) -> RnM c)
           -> [a] -> ([b] -> RnM c) -> RnM c

mapFvRnCPS _ []     cont = cont []
mapFvRnCPS f (x:xs) cont = f x             $ \ x' ->
                           mapFvRnCPS f xs $ \ xs' ->
                           cont (x':xs')
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Envt utility functions}
%*                                                                      *
%************************************************************************

\begin{code}
warnUnusedTopBinds :: [GlobalRdrElt] -> RnM ()
warnUnusedTopBinds gres
    = whenWOptM Opt_WarnUnusedBinds
    $ do isBoot <- tcIsHsBoot
         let noParent gre = case gre_par gre of
                            NoParent -> True
                            ParentIs _ -> False
             -- Don't warn about unused bindings with parents in
             -- .hs-boot files, as you are sometimes required to give
             -- unused bindings (trac #3449).
             gres' = if isBoot then filter noParent gres
                               else                 gres
         warnUnusedGREs gres'

warnUnusedLocalBinds, warnUnusedMatches :: [Name] -> FreeVars -> RnM ()
warnUnusedLocalBinds = check_unused Opt_WarnUnusedBinds
warnUnusedMatches    = check_unused Opt_WarnUnusedMatches

check_unused :: WarningFlag -> [Name] -> FreeVars -> RnM ()
check_unused flag bound_names used_names
 = whenWOptM flag (warnUnusedLocals (filterOut (`elemNameSet` used_names) bound_names))

-------------------------
--      Helpers
warnUnusedGREs :: [GlobalRdrElt] -> RnM ()
warnUnusedGREs gres
 = warnUnusedBinds [(n,p) | GRE {gre_name = n, gre_prov = p} <- gres]

warnUnusedLocals :: [Name] -> RnM ()
warnUnusedLocals names
 = warnUnusedBinds [(n,LocalDef) | n<-names]

warnUnusedBinds :: [(Name,Provenance)] -> RnM ()
warnUnusedBinds names  = mapM_ warnUnusedName (filter reportable names)
 where reportable (name,_)
        | isWiredInName name = False    -- Don't report unused wired-in names
                                        -- Otherwise we get a zillion warnings
                                        -- from Data.Tuple
        | otherwise = not (startsWithUnderscore (nameOccName name))

-------------------------

warnUnusedName :: (Name, Provenance) -> RnM ()
warnUnusedName (name, LocalDef)
  = addUnusedWarning name (nameSrcSpan name)
                     (ptext (sLit "Defined but not used"))

warnUnusedName (name, Imported is)
  = mapM_ warn is
  where
    warn spec = addUnusedWarning name span msg
        where
           span = importSpecLoc spec
           pp_mod = quotes (ppr (importSpecModule spec))
           msg = ptext (sLit "Imported from") <+> pp_mod <+> ptext (sLit "but not used")

addUnusedWarning :: Name -> SrcSpan -> SDoc -> RnM ()
addUnusedWarning name span msg
  = addWarnAt span $
    sep [msg <> colon,
         nest 2 $ pprNonVarNameSpace (occNameSpace (nameOccName name))
                        <+> quotes (ppr name)]
\end{code}

\begin{code}
addNameClashErrRn :: RdrName -> [GlobalRdrElt] -> RnM ()
addNameClashErrRn rdr_name gres
  | all isLocalGRE gres  -- If there are two or more *local* defns, we'll have reported
  = return ()            -- that already, and we don't want an error cascade
  | otherwise
  = addErr (vcat [ptext (sLit "Ambiguous occurrence") <+> quotes (ppr rdr_name),
                  ptext (sLit "It could refer to") <+> vcat (msg1 : msgs)])
  where
    (np1:nps) = gres
    msg1 = ptext  (sLit "either") <+> mk_ref np1
    msgs = [ptext (sLit "    or") <+> mk_ref np | np <- nps]
    mk_ref gre = sep [quotes (ppr (gre_name gre)) <> comma, pprNameProvenance gre]

shadowedNameWarn :: OccName -> [SDoc] -> SDoc
shadowedNameWarn occ shadowed_locs
  = sep [ptext (sLit "This binding for") <+> quotes (ppr occ)
            <+> ptext (sLit "shadows the existing binding") <> plural shadowed_locs,
         nest 2 (vcat shadowed_locs)]

perhapsForallMsg :: SDoc
perhapsForallMsg
  = vcat [ ptext (sLit "Perhaps you intended to use ExplicitForAll or similar flag")
         , ptext (sLit "to enable explicit-forall syntax: forall <tvs>. <type>")]

unknownSubordinateErr :: SDoc -> RdrName -> SDoc
unknownSubordinateErr doc op    -- Doc is "method of class" or
                                -- "field of constructor"
  = quotes (ppr op) <+> ptext (sLit "is not a (visible)") <+> doc

badOrigBinding :: RdrName -> SDoc
badOrigBinding name
  = ptext (sLit "Illegal binding of built-in syntax:") <+> ppr (rdrNameOcc name)
        -- The rdrNameOcc is because we don't want to print Prelude.(,)

dupNamesErr :: Outputable n => (n -> SrcSpan) -> [n] -> RnM ()
dupNamesErr get_loc names
  = addErrAt big_loc $
    vcat [ptext (sLit "Conflicting definitions for") <+> quotes (ppr (head names)),
          locations]
  where
    locs      = map get_loc names
    big_loc   = foldr1 combineSrcSpans locs
    locations = ptext (sLit "Bound at:") <+> vcat (map ppr (sort locs))

kindSigErr :: Outputable a => a -> SDoc
kindSigErr thing
  = hang (ptext (sLit "Illegal kind signature for") <+> quotes (ppr thing))
       2 (ptext (sLit "Perhaps you intended to use KindSignatures"))

badQualBndrErr :: RdrName -> SDoc
badQualBndrErr rdr_name
  = ptext (sLit "Qualified name in binding position:") <+> ppr rdr_name

opDeclErr :: RdrName -> SDoc
opDeclErr n
  = hang (ptext (sLit "Illegal declaration of a type or class operator") <+> quotes (ppr n))
       2 (ptext (sLit "Use TypeOperators to declare operators in type and declarations"))

checkTupSize :: Int -> RnM ()
checkTupSize tup_size
  | tup_size <= mAX_TUPLE_SIZE
  = return ()
  | otherwise
  = addErr (sep [ptext (sLit "A") <+> int tup_size <> ptext (sLit "-tuple is too large for GHC"),
                 nest 2 (parens (ptext (sLit "max size is") <+> int mAX_TUPLE_SIZE)),
                 nest 2 (ptext (sLit "Workaround: use nested tuples or define a data type"))])
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Contexts for renaming errors}
%*                                                                      *
%************************************************************************

\begin{code}

data HsDocContext
  = TypeSigCtx SDoc
  | PatCtx
  | SpecInstSigCtx
  | DefaultDeclCtx
  | ForeignDeclCtx (Located RdrName)
  | DerivDeclCtx
  | RuleCtx FastString
  | TyDataCtx (Located RdrName)
  | TySynCtx (Located RdrName)
  | TyFamilyCtx (Located RdrName)
  | ConDeclCtx (Located RdrName)
  | ClassDeclCtx (Located RdrName)
  | ExprWithTySigCtx
  | TypBrCtx
  | HsTypeCtx
  | GHCiCtx
  | SpliceTypeCtx (LHsType RdrName)
  | ClassInstanceCtx
  | VectDeclCtx (Located RdrName)
  | GenericCtx SDoc   -- Maybe we want to use this more!

docOfHsDocContext :: HsDocContext -> SDoc
docOfHsDocContext (GenericCtx doc) = doc
docOfHsDocContext (TypeSigCtx doc) = text "In the type signature for" <+> doc
docOfHsDocContext PatCtx = text "In a pattern type-signature"
docOfHsDocContext SpecInstSigCtx = text "In a SPECIALISE instance pragma"
docOfHsDocContext DefaultDeclCtx = text "In a `default' declaration"
docOfHsDocContext (ForeignDeclCtx name) = ptext (sLit "In the foreign declaration for") <+> ppr name
docOfHsDocContext DerivDeclCtx = text "In a deriving declaration"
docOfHsDocContext (RuleCtx name) = text "In the transformation rule" <+> ftext name
docOfHsDocContext (TyDataCtx tycon) = text "In the data type declaration for" <+> quotes (ppr tycon)
docOfHsDocContext (TySynCtx name) = text "In the declaration for type synonym" <+> quotes (ppr name)
docOfHsDocContext (TyFamilyCtx name) = text "In the declaration for type family" <+> quotes (ppr name)
docOfHsDocContext (ConDeclCtx name) = text "In the definition of data constructor" <+> quotes (ppr name)
docOfHsDocContext (ClassDeclCtx name) = text "In the declaration for class"     <+> ppr name
docOfHsDocContext ExprWithTySigCtx = text "In an expression type signature"
docOfHsDocContext TypBrCtx = ptext (sLit "In a Template-Haskell quoted type")
docOfHsDocContext HsTypeCtx = text "In a type argument"
docOfHsDocContext GHCiCtx = ptext (sLit "In GHCi input")
docOfHsDocContext (SpliceTypeCtx hs_ty) = ptext (sLit "In the spliced type") <+> ppr hs_ty
docOfHsDocContext ClassInstanceCtx = ptext (sLit "TcSplice.reifyInstances")
docOfHsDocContext (VectDeclCtx tycon) = ptext (sLit "In the VECTORISE pragma for type constructor") <+> quotes (ppr tycon)
\end{code}
