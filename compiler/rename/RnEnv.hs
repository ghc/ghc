{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-2006

\section[RnEnv]{Environment manipulation for the renamer monad}
-}

{-# LANGUAGE CPP, MultiWayIf #-}

module RnEnv (
        newTopSrcBinder,
        lookupLocatedTopBndrRn, lookupTopBndrRn,
        lookupLocatedOccRn, lookupOccRn, lookupOccRn_maybe,
        lookupLocalOccRn_maybe, lookupInfoOccRn,
        lookupLocalOccThLvl_maybe,
        lookupTypeOccRn, lookupKindOccRn,
        lookupGlobalOccRn, lookupGlobalOccRn_maybe,
        lookupOccRn_overloaded, lookupGlobalOccRn_overloaded,
        reportUnboundName, unknownNameSuggestions,
        addNameClashErrRn,

        HsSigCtxt(..), lookupLocalTcNames, lookupSigOccRn,
        lookupSigCtxtOccRn,

        lookupFixityRn, lookupFixityRn_help,
        lookupFieldFixityRn, lookupTyFixityRn,
        lookupInstDeclBndr, lookupRecFieldOcc, lookupFamInstName,
        lookupConstructorFields,
        lookupSyntaxName, lookupSyntaxName', lookupSyntaxNames,
        lookupIfThenElse,
        lookupGreAvailRn,
        getLookupOccRn,mkUnboundName, mkUnboundNameRdr, isUnboundName,
        addUsedGRE, addUsedGREs, addUsedDataCons,

        newLocalBndrRn, newLocalBndrsRn,
        bindLocalNames, bindLocalNamesFV,
        MiniFixityEnv,
        addLocalFixities,
        bindLocatedLocalsFV, bindLocatedLocalsRn,
        extendTyVarEnvFVRn,

        -- Role annotations
        RoleAnnotEnv, emptyRoleAnnotEnv, mkRoleAnnotEnv,
        lookupRoleAnnot, getRoleAnnots,

        checkDupRdrNames, checkShadowedRdrNames,
        checkDupNames, checkDupAndShadowedNames, dupNamesErr,
        checkTupSize,
        addFvRn, mapFvRn, mapMaybeFvRn, mapFvRnCPS,
        warnUnusedMatches, warnUnusedTypePatterns,
        warnUnusedTopBinds, warnUnusedLocalBinds,
        mkFieldEnv,
        dataTcOccs, kindSigErr, perhapsForallMsg, unknownSubordinateErr,
        HsDocContext(..), pprHsDocContext,
        inHsDocContext, withHsDocContext
    ) where

#include "HsVersions.h"

import LoadIface        ( loadInterfaceForName, loadSrcInterface_maybe )
import IfaceEnv
import HsSyn
import RdrName
import HscTypes
import TcEnv
import TcRnMonad
import RdrHsSyn         ( setRdrNameSpace )
import TysWiredIn       ( starKindTyConName, unicodeStarKindTyConName )
import Name
import NameSet
import NameEnv
import Avail
import Module
import ConLike
import DataCon
import TyCon
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
import Data.Function    ( on )
import ListSetOps       ( minusList )
import Constants        ( mAX_TUPLE_SIZE )
import qualified GHC.LanguageExtensions as LangExt

{-
*********************************************************
*                                                      *
                Source-code binders
*                                                      *
*********************************************************

Note [Signature lazy interface loading]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GHC's lazy interface loading can be a bit confusing, so this Note is an
empirical description of what happens in one interesting case. When
compiling a signature module against an its implementation, we do NOT
load interface files associated with its names until after the type
checking phase.  For example:

    module ASig where
        data T
        f :: T -> T

Suppose we compile this with -sig-of "A is ASig":

    module B where
        data T = T
        f T = T

    module A(module B) where
        import B

During type checking, we'll load A.hi because we need to know what the
RdrEnv for the module is, but we DO NOT load the interface for B.hi!
It's wholly unnecessary: our local definition 'data T' in ASig is all
the information we need to finish type checking.  This is contrast to
type checking of ordinary Haskell files, in which we would not have the
local definition "data T" and would need to consult B.hi immediately.
(Also, this situation never occurs for hs-boot files, since you're not
allowed to reexport from another module.)

After type checking, we then check that the types we provided are
consistent with the backing implementation (in checkHiBootOrHsigIface).
At this point, B.hi is loaded, because we need something to compare
against.

I discovered this behavior when trying to figure out why type class
instances for Data.Map weren't in the EPS when I was type checking a
test very much like ASig (sigof02dm): the associated interface hadn't
been loaded yet!  (The larger issue is a moot point, since an instance
declared in a signature can never be a duplicate.)

This behavior might change in the future.  Consider this
alternate module B:

    module B where
        {-# DEPRECATED T, f "Don't use" #-}
        data T = T
        f T = T

One might conceivably want to report deprecation warnings when compiling
ASig with -sig-of B, in which case we need to look at B.hi to find the
deprecation warnings during renaming.  At the moment, you don't get any
warning until you use the identifier further downstream.  This would
require adjusting addUsedGRE so that during signature compilation,
we do not report deprecation warnings for LocalDef.  See also
Note [Handling of deprecations]
-}

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
      do { this_mod <- getModule
         ; externaliseName this_mod name }

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
             do { this_mod <- getModule
                ; traceRn (text "newTopSrcBinder" <+> (ppr this_mod $$ ppr rdr_name $$ ppr loc))
                ; newGlobalBinder this_mod (rdrNameOcc rdr_name) loc }
        }

{-
*********************************************************
*                                                      *
        Source code occurrences
*                                                      *
*********************************************************

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
-}

lookupTopBndrRn :: RdrName -> RnM Name
lookupTopBndrRn n = do nopt <- lookupTopBndrRn_maybe n
                       case nopt of
                         Just n' -> return n'
                         Nothing -> do traceRn $ (text "lookupTopBndrRn fail" <+> ppr n)
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
               (do { op_ok <- xoptM LangExt.TypeOperators
                   ; unless op_ok (addErr (opDeclErr rdr_name)) })

        ; env <- getGlobalRdrEnv
        ; case filter isLocalGRE (lookupGRE_RdrName rdr_name env) of
            [gre] -> return (Just (gre_name gre))
            _     -> return Nothing  -- Ambiguous (can't happen) or unbound
    }

-----------------------------------------------
-- | Lookup an @Exact@ @RdrName@. See Note [Looking up Exact RdrNames].
-- This adds an error if the name cannot be found.
lookupExactOcc :: Name -> RnM Name
lookupExactOcc name
  = do { result <- lookupExactOcc_either name
       ; case result of
           Left err -> do { addErr err
                          ; return name }
           Right name' -> return name' }

-- | Lookup an @Exact@ @RdrName@. See Note [Looking up Exact RdrNames].
-- This never adds an error, but it may return one.
lookupExactOcc_either :: Name -> RnM (Either MsgDoc Name)
-- See Note [Looking up Exact RdrNames]
lookupExactOcc_either name
  | Just thing <- wiredInNameTyThing_maybe name
  , Just tycon <- case thing of
                    ATyCon tc                 -> Just tc
                    AConLike (RealDataCon dc) -> Just (dataConTyCon dc)
                    _                         -> Nothing
  , isTupleTyCon tycon
  = do { checkTupSize (tyConArity tycon)
       ; return (Right name) }

  | isExternalName name
  = return (Right name)

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
           [gre] -> return (Right (gre_name gre))

           []    -> -- See Note [Splicing Exact names]
                    do { lcl_env <- getLocalRdrEnv
                       ; if name `inLocalRdrEnvScope` lcl_env
                         then return (Right name)
                         else
#ifdef GHCI
                         do { th_topnames_var <- fmap tcg_th_topnames getGblEnv
                            ; th_topnames <- readTcRef th_topnames_var
                            ; if name `elemNameSet` th_topnames
                              then return (Right name)
                              else return (Left exact_nm_err)
                            }
#else /* !GHCI */
                         return (Left exact_nm_err)
#endif /* !GHCI */
                       }
           gres -> return (Left (sameNameErr gres))   -- Ugh!  See Note [Template Haskell ambiguity]
       }
  where
    exact_nm_err = hang (text "The exact Name" <+> quotes (ppr name) <+> ptext (sLit "is not in scope"))
                      2 (vcat [ text "Probable cause: you used a unique Template Haskell name (NameU), "
                              , text "perhaps via newName, but did not bind it"
                              , text "If that's it, then -ddump-splices might be useful" ])

sameNameErr :: [GlobalRdrElt] -> MsgDoc
sameNameErr [] = panic "addSameNameErr: empty list"
sameNameErr gres@(_ : _)
  = hang (text "Same exact name in multiple name-spaces:")
       2 (vcat (map pp_one sorted_names) $$ th_hint)
  where
    sorted_names = sortWith nameSrcLoc (map gre_name gres)
    pp_one name
      = hang (pprNameSpace (occNameSpace (getOccName name))
              <+> quotes (ppr name) <> comma)
           2 (text "declared at:" <+> ppr (nameSrcLoc name))

    th_hint = vcat [ text "Probable cause: you bound a unique Template Haskell name (NameU),"
                   , text "perhaps via newName, in different name-spaces."
                   , text "If that's it, then -ddump-splices might be useful" ]


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
       ; mb_name <- lookupSubBndrOcc
                          False -- False => we don't give deprecated
                                -- warnings when a deprecated class
                                -- method is defined. We only warn
                                -- when it's used
                          cls doc rdr
       ; case mb_name of
           Left err -> do { addErr err; return (mkUnboundNameRdr rdr) }
           Right nm -> return nm }
  where
    doc = what <+> text "of class" <+> quotes (ppr cls)


-----------------------------------------------
lookupFamInstName :: Maybe Name -> Located RdrName -> RnM (Located Name)
-- Used for TyData and TySynonym family instances only,
-- See Note [Family instance binders]
lookupFamInstName (Just cls) tc_rdr  -- Associated type; c.f RnBinds.rnMethodBind
  = wrapLocM (lookupInstDeclBndr cls (text "associated type")) tc_rdr
lookupFamInstName Nothing tc_rdr     -- Family instance; tc_rdr is an *occurrence*
  = lookupLocatedOccRn tc_rdr

-----------------------------------------------
lookupConstructorFields :: Name -> RnM [FieldLabel]
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
          do { field_env <- getRecFieldEnv
             ; traceTc "lookupCF" (ppr con_name $$ ppr (lookupNameEnv field_env con_name) $$ ppr field_env)
             ; return (lookupNameEnv field_env con_name `orElse` []) }
          else
          do { con <- tcLookupConLike con_name
             ; traceTc "lookupCF 2" (ppr con)
             ; return (conLikeFieldLabels con) } }

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

lookupRecFieldOcc :: Maybe Name  -- Nothing    => just look it up as usual
                                 -- Just tycon => use tycon to disambiguate
                  -> SDoc -> RdrName
                  -> RnM Name
lookupRecFieldOcc parent doc rdr_name
  | Just tc_name <- parent
  = do { mb_name <- lookupSubBndrOcc True tc_name doc rdr_name
       ; case mb_name of
           Left err -> do { addErr err; return (mkUnboundNameRdr rdr_name) }
           Right n  -> return n }

  | otherwise
  = lookupGlobalOccRn rdr_name

lookupSubBndrOcc :: Bool
                 -> Name     -- Parent
                 -> SDoc
                 -> RdrName
                 -> RnM (Either MsgDoc Name)
-- Find all the things the rdr-name maps to
-- and pick the one with the right parent namep
lookupSubBndrOcc warn_if_deprec the_parent doc rdr_name
  | Just n <- isExact_maybe rdr_name   -- This happens in derived code
  = do { n <- lookupExactOcc n
       ; return (Right n) }

  | Just (rdr_mod, rdr_occ) <- isOrig_maybe rdr_name
  = do { n <- lookupOrig rdr_mod rdr_occ
       ; return (Right n) }

  | isUnboundName the_parent
        -- Avoid an error cascade from malformed decls:
        --   instance Int where { foo = e }
        -- We have already generated an error in rnLHsInstDecl
  = return (Right (mkUnboundNameRdr rdr_name))

  | otherwise
  = do { env <- getGlobalRdrEnv
       ; let gres = lookupGlobalRdrEnv env (rdrNameOcc rdr_name)
                -- NB: lookupGlobalRdrEnv, not lookupGRE_RdrName!
                --     The latter does pickGREs, but we want to allow 'x'
                --     even if only 'M.x' is in scope
       ; traceRn (text "lookupSubBndrOcc" <+> vcat [ppr the_parent, ppr rdr_name, ppr gres, ppr (pick_gres rdr_name gres)])
       ; case pick_gres rdr_name gres of
            (gre:_) -> do { addUsedGRE warn_if_deprec gre
                            -- Add a usage; this is an *occurrence* site
                            -- Note [Usage for sub-bndrs]
                          ; return (Right (gre_name gre)) }
                 -- If there is more than one local GRE for the
                 -- same OccName 'f', that will be reported separately
                 -- as a duplicate top-level binding for 'f'
            [] -> do { ns <- lookupQualifiedNameGHCi rdr_name
                     ; case ns of
                         (n:_) -> return (Right n)  -- Unlikely to be more than one...?
                         [] -> return (Left (unknownSubordinateErr doc rdr_name))
    } }
  where
    -- If Parent = NoParent, just do a normal lookup
    -- If Parent = Parent p then find all GREs that
    --   (a) have parent p
    --   (b) for Unqual, are in scope qualified or unqualified
    --       for Qual, are in scope with that qualification
    pick_gres rdr_name gres
      | isUnqual rdr_name = filter right_parent gres
      | otherwise         = filter right_parent (pickGREs rdr_name gres)

    right_parent (GRE { gre_par = p })
      | ParentIs parent <- p               = parent == the_parent
      | FldParent { par_is = parent } <- p = parent == the_parent
      | otherwise                          = False

{-
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
Even though there are two G's in scope (M.G and Blib.G), the occurrence
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

 * In RnEnv.newTopSrcBinder we spot Exact RdrNames that wrap a
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
The occurrence of 'Zero in the data type for T has the right unique,
but it has a TcClsName name-space in its OccName.  (This is set by
the ctxt_ns argument of Convert.thRdrName.)  When we check that is
in scope in the GlobalRdrEnv, we need to look up the DataName namespace
too.  (An alternative would be to make the GlobalRdrEnv also have
a Name -> GRE mapping.)

Note [Template Haskell ambiguity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The GlobalRdrEnv invariant says that if
  occ -> [gre1, ..., gren]
then the gres have distinct Names (INVARIANT 1 of GlobalRdrEnv).
This is guaranteed by extendGlobalRdrEnvRn (the dups check in add_gre).

So how can we get multiple gres in lookupExactOcc_maybe?  Because in
TH we might use the same TH NameU in two different name spaces.
eg (Trac #7241):
   $(newName "Foo" >>= \o -> return [DataD [] o [] [RecC o []] [''Show]])
Here we generate a type constructor and data constructor with the same
unique, but differnt name spaces.

It'd be nicer to rule this out in extendGlobalRdrEnvRn, but that would
mean looking up the OccName in every name-space, just in case, and that
seems a bit brutal.  So it's just done here on lookup.  But we might
need to revisit that choice.

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
-}

getLookupOccRn :: RnM (Name -> Maybe Name)
getLookupOccRn
  = do local_env <- getLocalRdrEnv
       return (lookupLocalRdrOcc local_env . nameOccName)

mkUnboundNameRdr :: RdrName -> Name
mkUnboundNameRdr rdr = mkUnboundName (rdrNameOcc rdr)

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
  = do { typeintype <- xoptM LangExt.TypeInType
       ; if | typeintype           -> lookupTypeOccRn rdr_name
      -- With -XNoTypeInType, treat any usage of * in kinds as in scope
      -- this is a dirty hack, but then again so was the old * kind.
            | is_star rdr_name     -> return starKindTyConName
            | is_uni_star rdr_name -> return unicodeStarKindTyConName
            | otherwise            -> lookupOccRn rdr_name }

-- lookupPromotedOccRn looks up an optionally promoted RdrName.
lookupTypeOccRn :: RdrName -> RnM Name
-- see Note [Demotion]
lookupTypeOccRn rdr_name
  = do { mb_name <- lookupOccRn_maybe rdr_name
       ; case mb_name of {
             Just name -> return name ;
             Nothing   -> do { dflags <- getDynFlags
                             ; lookup_demoted rdr_name dflags } } }

lookup_demoted :: RdrName -> DynFlags -> RnM Name
lookup_demoted rdr_name dflags
  | Just demoted_rdr <- demoteRdrName rdr_name
    -- Maybe it's the name of a *data* constructor
  = do { data_kinds <- xoptM LangExt.DataKinds
       ; mb_demoted_name <- lookupOccRn_maybe demoted_rdr
       ; case mb_demoted_name of
           Nothing -> unboundNameX WL_Any rdr_name star_info
           Just demoted_name
             | data_kinds ->
             do { whenWOptM Opt_WarnUntickedPromotedConstructors $
                  addWarn (Reason Opt_WarnUntickedPromotedConstructors)
                          (untickedPromConstrWarn demoted_name)
                ; return demoted_name }
             | otherwise  -> unboundNameX WL_Any rdr_name suggest_dk }

  | otherwise
  = reportUnboundName rdr_name

  where
    suggest_dk = text "A data constructor of that name is in scope; did you mean DataKinds?"
    untickedPromConstrWarn name =
      text "Unticked promoted constructor" <> colon <+> quotes (ppr name) <> dot
      $$
      hsep [ text "Use"
           , quotes (char '\'' <> ppr name)
           , text "instead of"
           , quotes (ppr name) <> dot ]

    star_info
      | is_star rdr_name || is_uni_star rdr_name
      = if xopt LangExt.TypeInType dflags
        then text "NB: With TypeInType, you must import" <+>
             ppr rdr_name <+> text "from Data.Kind"
        else empty

      | otherwise
      = empty

is_star, is_uni_star :: RdrName -> Bool
is_star     = (fsLit "*" ==) . occNameFS . rdrNameOcc
is_uni_star = (fsLit "★" ==) . occNameFS . rdrNameOcc

{-
Note [Demotion]
~~~~~~~~~~~~~~~
When the user writes:
  data Nat = Zero | Succ Nat
  foo :: f Zero -> Int

'Zero' in the type signature of 'foo' is parsed as:
  HsTyVar ("Zero", TcClsName)

When the renamer hits this occurrence of 'Zero' it's going to realise
that it's not in scope. But because it is renaming a type, it knows
that 'Zero' might be a promoted data constructor, so it will demote
its namespace to DataName and do a second lookup.

The final result (after the renamer) will be:
  HsTyVar ("Zero", DataName)
-}

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
       ; lookupGlobalOccRn_maybe rdr_name } }

lookupGlobalOccRn_maybe :: RdrName -> RnM (Maybe Name)
-- Looks up a RdrName occurrence in the top-level
--   environment, including using lookupQualifiedNameGHCi
--   for the GHCi case
-- No filter function; does not report an error on failure
-- Uses addUsedRdrName to record use and deprecations
lookupGlobalOccRn_maybe rdr_name
  | Just n <- isExact_maybe rdr_name   -- This happens in derived code
  = do { n' <- lookupExactOcc n; return (Just n') }

  | Just (rdr_mod, rdr_occ) <- isOrig_maybe rdr_name
  = do { n <- lookupOrig rdr_mod rdr_occ
       ; return (Just n) }

  | otherwise
  = do  { mb_gre <- lookupGreRn_maybe rdr_name
        ; case mb_gre of {
            Just gre -> return (Just (gre_name gre)) ;
            Nothing  ->
     do { ns <- lookupQualifiedNameGHCi rdr_name
                      -- This test is not expensive,
                      -- and only happens for failed lookups
       ; case ns of
           (n:_) -> return (Just n)  -- Unlikely to be more than one...?
           []    -> return Nothing } } }

lookupGlobalOccRn :: RdrName -> RnM Name
-- lookupGlobalOccRn is like lookupOccRn, except that it looks in the global
-- environment.  Adds an error message if the RdrName is not in scope.
lookupGlobalOccRn rdr_name
  = do { mb_name <- lookupGlobalOccRn_maybe rdr_name
       ; case mb_name of
           Just n  -> return n
           Nothing -> do { traceRn (text "lookupGlobalOccRn" <+> ppr rdr_name)
                         ; unboundName WL_Global rdr_name } }

lookupInfoOccRn :: RdrName -> RnM [Name]
-- lookupInfoOccRn is intended for use in GHCi's ":info" command
-- It finds all the GREs that RdrName could mean, not complaining
-- about ambiguity, but rather returning them all
-- C.f. Trac #9881
lookupInfoOccRn rdr_name
  | Just n <- isExact_maybe rdr_name   -- e.g. (->)
  = return [n]

  | Just (rdr_mod, rdr_occ) <- isOrig_maybe rdr_name
  = do { n <- lookupOrig rdr_mod rdr_occ
       ; return [n] }

  | otherwise
  = do { rdr_env <- getGlobalRdrEnv
       ; let ns = map gre_name (lookupGRE_RdrName rdr_name rdr_env)
       ; qual_ns <- lookupQualifiedNameGHCi rdr_name
       ; return (ns ++ (qual_ns `minusList` ns)) }

-- | Like 'lookupOccRn_maybe', but with a more informative result if
-- the 'RdrName' happens to be a record selector:
--
--   * Nothing         -> name not in scope (no error reported)
--   * Just (Left x)   -> name uniquely refers to x,
--                        or there is a name clash (reported)
--   * Just (Right xs) -> name refers to one or more record selectors;
--                        if overload_ok was False, this list will be
--                        a singleton.
lookupOccRn_overloaded  :: Bool -> RdrName -> RnM (Maybe (Either Name [FieldOcc Name]))
lookupOccRn_overloaded overload_ok rdr_name
  = do { local_env <- getLocalRdrEnv
       ; case lookupLocalRdrEnv local_env rdr_name of {
          Just name -> return (Just (Left name)) ;
          Nothing   -> do
       { mb_name <- lookupGlobalOccRn_overloaded overload_ok rdr_name
       ; case mb_name of {
           Just name -> return (Just name) ;
           Nothing   -> do
       { ns <- lookupQualifiedNameGHCi rdr_name
                      -- This test is not expensive,
                      -- and only happens for failed lookups
       ; case ns of
           (n:_) -> return $ Just $ Left n  -- Unlikely to be more than one...?
           []    -> return Nothing  } } } } }

lookupGlobalOccRn_overloaded :: Bool -> RdrName -> RnM (Maybe (Either Name [FieldOcc Name]))
lookupGlobalOccRn_overloaded overload_ok rdr_name
  | Just n <- isExact_maybe rdr_name   -- This happens in derived code
  = do { n' <- lookupExactOcc n; return (Just (Left n')) }

  | Just (rdr_mod, rdr_occ) <- isOrig_maybe rdr_name
  = do { n <- lookupOrig rdr_mod rdr_occ
       ; return (Just (Left n)) }

  | otherwise
  = do  { env <- getGlobalRdrEnv
        ; case lookupGRE_RdrName rdr_name env of
                []    -> return Nothing
                [gre] | isRecFldGRE gre
                         -> do { addUsedGRE True gre
                               ; let
                                   fld_occ :: FieldOcc Name
                                   fld_occ
                                     = FieldOcc (noLoc rdr_name) (gre_name gre)
                               ; return (Just (Right [fld_occ])) }
                      | otherwise
                         -> do { addUsedGRE True gre
                               ; return (Just (Left (gre_name gre))) }
                gres  | all isRecFldGRE gres && overload_ok
                            -- Don't record usage for ambiguous selectors
                            -- until we know which is meant
                         -> return
                             (Just (Right
                                     (map (FieldOcc (noLoc rdr_name) . gre_name)
                                           gres)))
                gres     -> do { addNameClashErrRn rdr_name gres
                               ; return (Just (Left (gre_name (head gres)))) } }


--------------------------------------------------
--      Lookup in the Global RdrEnv of the module
--------------------------------------------------

lookupGreRn_maybe :: RdrName -> RnM (Maybe GlobalRdrElt)
-- Look up the RdrName in the GlobalRdrEnv
--   Exactly one binding: records it as "used", return (Just gre)
--   No bindings:         return Nothing
--   Many bindings:       report "ambiguous", return an arbitrary (Just gre)
-- (This API is a bit strange; lookupGRERn2_maybe is simpler.
--  But it works and I don't want to fiddle too much.)
-- Uses addUsedRdrName to record use and deprecations
lookupGreRn_maybe rdr_name
  = do  { env <- getGlobalRdrEnv
        ; case lookupGRE_RdrName rdr_name env of
            []    -> return Nothing
            [gre] -> do { addUsedGRE True gre
                        ; return (Just gre) }
            gres  -> do { addNameClashErrRn rdr_name gres
                        ; traceRn (text "name clash" <+> (ppr rdr_name $$ ppr gres $$ ppr env))
                        ; return (Just (head gres)) } }

lookupGreRn2_maybe :: RdrName -> RnM (Maybe GlobalRdrElt)
-- Look up the RdrName in the GlobalRdrEnv
--   Exactly one binding: record it as "used",   return (Just gre)
--   No bindings:         report "not in scope", return Nothing
--   Many bindings:       report "ambiguous",    return Nothing
-- Uses addUsedRdrName to record use and deprecations
lookupGreRn2_maybe rdr_name
  = do  { env <- getGlobalRdrEnv
        ; case lookupGRE_RdrName rdr_name env of
            []    -> do { _ <- unboundName WL_Global rdr_name
                        ; return Nothing }
            [gre] -> do { addUsedGRE True gre
                        ; return (Just gre) }
            gres  -> do { addNameClashErrRn rdr_name gres
                        ; traceRn (text "name clash" <+> (ppr rdr_name $$ ppr gres $$ ppr env))
                        ; return Nothing } }

lookupGreAvailRn :: RdrName -> RnM (Name, AvailInfo)
-- Used in export lists
-- If not found or ambiguous, add error message, and fake with UnboundName
-- Uses addUsedRdrName to record use and deprecations
lookupGreAvailRn rdr_name
  = do  { mb_gre <- lookupGreRn2_maybe rdr_name
        ; case mb_gre of {
            Just gre -> return (gre_name gre, availFromGRE gre) ;
            Nothing  ->
    do  { traceRn (text "lookupGreRn" <+> ppr rdr_name)
        ; let name = mkUnboundNameRdr rdr_name
        ; return (name, avail name) } } }

{-
*********************************************************
*                                                      *
                Deprecations
*                                                      *
*********************************************************

Note [Handling of deprecations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* We report deprecations at each *occurrence* of the deprecated thing
  (see Trac #5867)

* We do not report deprecations for locally-defined names. For a
  start, we may be exporting a deprecated thing. Also we may use a
  deprecated thing in the defn of another deprecated things.  We may
  even use a deprecated thing in the defn of a non-deprecated thing,
  when changing a module's interface.

* addUsedGREs: we do not report deprecations for sub-binders:
     - the ".." completion for records
     - the ".." in an export item 'T(..)'
     - the things exported by a module export 'module M'
-}

addUsedDataCons :: GlobalRdrEnv -> TyCon -> RnM ()
-- Remember use of in-scope data constructors (Trac #7969)
addUsedDataCons rdr_env tycon
  = addUsedGREs [ gre
                | dc <- tyConDataCons tycon
                , Just gre <- [lookupGRE_Name rdr_env (dataConName dc)] ]

addUsedGRE :: Bool -> GlobalRdrElt -> RnM ()
-- Called for both local and imported things
-- Add usage *and* warn if deprecated
addUsedGRE warn_if_deprec gre
  = do { when warn_if_deprec (warnIfDeprecated gre)
       ; unless (isLocalGRE gre) $
         do { env <- getGblEnv
            ; traceRn (text "addUsedGRE" <+> ppr gre)
            ; updMutVar (tcg_used_gres env) (gre :) } }

addUsedGREs :: [GlobalRdrElt] -> RnM ()
-- Record uses of any *imported* GREs
-- Used for recording used sub-bndrs
-- NB: no call to warnIfDeprecated; see Note [Handling of deprecations]
addUsedGREs gres
  | null imp_gres = return ()
  | otherwise     = do { env <- getGblEnv
                       ; traceRn (text "addUsedGREs" <+> ppr imp_gres)
                       ; updMutVar (tcg_used_gres env) (imp_gres ++) }
  where
    imp_gres = filterOut isLocalGRE gres

warnIfDeprecated :: GlobalRdrElt -> RnM ()
warnIfDeprecated gre@(GRE { gre_name = name, gre_imp = iss })
  | (imp_spec : _) <- iss
  = do { dflags <- getDynFlags
       ; this_mod <- getModule
       ; when (wopt Opt_WarnWarningsDeprecations dflags &&
               not (nameIsLocalOrFrom this_mod name)) $
                   -- See Note [Handling of deprecations]
         do { iface <- loadInterfaceForName doc name
            ; case lookupImpDeprec iface gre of
                Just txt -> addWarn (Reason Opt_WarnWarningsDeprecations)
                                   (mk_msg imp_spec txt)
                Nothing  -> return () } }
  | otherwise
  = return ()
  where
    occ = greOccName gre
    name_mod = ASSERT2( isExternalName name, ppr name ) nameModule name
    doc = text "The name" <+> quotes (ppr occ) <+> ptext (sLit "is mentioned explicitly")

    mk_msg imp_spec txt
      = sep [ sep [ text "In the use of"
                    <+> pprNonVarNameSpace (occNameSpace occ)
                    <+> quotes (ppr occ)
                  , parens imp_msg <> colon ]
            , ppr txt ]
      where
        imp_mod  = importSpecModule imp_spec
        imp_msg  = text "imported from" <+> ppr imp_mod <> extra
        extra | imp_mod == moduleName name_mod = Outputable.empty
              | otherwise = text ", but defined in" <+> ppr name_mod

lookupImpDeprec :: ModIface -> GlobalRdrElt -> Maybe WarningTxt
lookupImpDeprec iface gre
  = mi_warn_fn iface (greOccName gre) `mplus`  -- Bleat if the thing,
    case gre_par gre of                      -- or its parent, is warn'd
       ParentIs  p              -> mi_warn_fn iface (nameOccName p)
       FldParent { par_is = p } -> mi_warn_fn iface (nameOccName p)
       NoParent                 -> Nothing
       PatternSynonym           -> Nothing

{-
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


*********************************************************
*                                                      *
                GHCi support
*                                                      *
*********************************************************

A qualified name on the command line can refer to any module at
all: we try to load the interface if we don't already have it, just
as if there was an "import qualified M" declaration for every
module.

If we fail we just return Nothing, rather than bleating
about "attempting to use module ‘D’ (./D.hs) which is not loaded"
which is what loadSrcInterface does.

Note [Safe Haskell and GHCi]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We DONT do this Safe Haskell as we need to check imports. We can
and should instead check the qualified import but at the moment
this requires some refactoring so leave as a TODO
-}

lookupQualifiedNameGHCi :: RdrName -> RnM [Name]
lookupQualifiedNameGHCi rdr_name
  = -- We want to behave as we would for a source file import here,
    -- and respect hiddenness of modules/packages, hence loadSrcInterface.
    do { dflags  <- getDynFlags
       ; is_ghci <- getIsGHCi
       ; go_for_it dflags is_ghci }

  where
    go_for_it dflags is_ghci
      | Just (mod,occ) <- isQual_maybe rdr_name
      , is_ghci
      , gopt Opt_ImplicitImportQualified dflags   -- Enables this GHCi behaviour
      , not (safeDirectImpsReq dflags)            -- See Note [Safe Haskell and GHCi]
      = do { res <- loadSrcInterface_maybe doc mod False Nothing
           ; case res of
                Succeeded iface
                  -> return [ name
                            | avail <- mi_exports iface
                            , name  <- availNames avail
                            , nameOccName name == occ ]

                _ -> -- Either we couldn't load the interface, or
                     -- we could but we didn't find the name in it
                     do { traceRn (text "lookupQualifiedNameGHCi" <+> ppr rdr_name)
                        ; return [] } }

      | otherwise
      = do { traceRn (text "lookupQualifedNameGHCi: off" <+> ppr rdr_name)
           ; return [] }

    doc = text "Need to find" <+> ppr rdr_name

{-
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
data HsSigCtxt = ... | TopSigCtxt NameSet | ....

* The NameSet says what is bound in this group of bindings.
  We can't use isLocalGRE from the GlobalRdrEnv, because of this:
       f x = x
       $( ...some TH splice... )
       f :: Int -> Int
  When we encounter the signature for 'f', the binding for 'f'
  will be in the GlobalRdrEnv, and will be a LocalDef. Yet the
  signature is mis-placed

* For type signatures the NameSet should be the names bound by the
  value bindings; for fixity declarations, the NameSet should also
  include class sigs and record selectors

      infix 3 `f`          -- Yes, ok
      f :: C a => a -> a   -- No, not ok
      class C a where
        f :: a -> a
-}

data HsSigCtxt
  = TopSigCtxt NameSet       -- At top level, binding these names
                             -- See Note [Signatures for top level things]
  | LocalBindCtxt NameSet    -- In a local binding, binding these names
  | ClsDeclCtxt   Name       -- Class decl for this class
  | InstDeclCtxt  NameSet    -- Instance decl whose user-written method
                             -- bindings are for these methods
  | HsBootCtxt NameSet       -- Top level of a hs-boot file, binding these names
  | RoleAnnotCtxt NameSet    -- A role annotation, with the names of all types
                             -- in the group

instance Outputable HsSigCtxt where
    ppr (TopSigCtxt ns) = text "TopSigCtxt" <+> ppr ns
    ppr (LocalBindCtxt ns) = text "LocalBindCtxt" <+> ppr ns
    ppr (ClsDeclCtxt n) = text "ClsDeclCtxt" <+> ppr n
    ppr (InstDeclCtxt ns) = text "InstDeclCtxt" <+> ppr ns
    ppr (HsBootCtxt ns) = text "HsBootCtxt" <+> ppr ns
    ppr (RoleAnnotCtxt ns) = text "RoleAnnotCtxt" <+> ppr ns

lookupSigOccRn :: HsSigCtxt
               -> Sig RdrName
               -> Located RdrName -> RnM (Located Name)
lookupSigOccRn ctxt sig = lookupSigCtxtOccRn ctxt (hsSigDoc sig)

-- | Lookup a name in relation to the names in a 'HsSigCtxt'
lookupSigCtxtOccRn :: HsSigCtxt
                   -> SDoc         -- ^ description of thing we're looking up,
                                   -- like "type family"
                   -> Located RdrName -> RnM (Located Name)
lookupSigCtxtOccRn ctxt what
  = wrapLocM $ \ rdr_name ->
    do { mb_name <- lookupBindGroupOcc ctxt what rdr_name
       ; case mb_name of
           Left err   -> do { addErr err; return (mkUnboundNameRdr rdr_name) }
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
  = lookupExactOcc_either n   -- allow for the possibility of missing Exacts;
                              -- see Note [dataTcOccs and Exact Names]
      -- Maybe we should check the side conditions
      -- but it's a pain, and Exact things only show
      -- up when you know what you are doing

  | Just (rdr_mod, rdr_occ) <- isOrig_maybe rdr_name
  = do { n' <- lookupOrig rdr_mod rdr_occ
       ; return (Right n') }

  | otherwise
  = case ctxt of
      HsBootCtxt ns    -> lookup_top (`elemNameSet` ns)
      TopSigCtxt ns    -> lookup_top (`elemNameSet` ns)
      RoleAnnotCtxt ns -> lookup_top (`elemNameSet` ns)
      LocalBindCtxt ns -> lookup_group ns
      ClsDeclCtxt  cls -> lookup_cls_op cls
      InstDeclCtxt ns  -> lookup_top (`elemNameSet` ns)
  where
    lookup_cls_op cls
      = lookupSubBndrOcc True cls doc rdr_name
      where
        doc = text "method of class" <+> quotes (ppr cls)

    lookup_top keep_me
      = do { env <- getGlobalRdrEnv
           ; let all_gres = lookupGlobalRdrEnv env (rdrNameOcc rdr_name)
           ; case filter (keep_me . gre_name) all_gres of
               [] | null all_gres -> bale_out_with Outputable.empty
                  | otherwise     -> bale_out_with local_msg
               (gre:_)            -> return (Right (gre_name gre)) }

    lookup_group bound_names  -- Look in the local envt (not top level)
      = do { local_env <- getLocalRdrEnv
           ; case lookupLocalRdrEnv local_env rdr_name of
               Just n
                 | n `elemNameSet` bound_names -> return (Right n)
                 | otherwise                   -> bale_out_with local_msg
               Nothing                         -> bale_out_with Outputable.empty }

    bale_out_with msg
        = return (Left (sep [ text "The" <+> what
                                <+> text "for" <+> quotes (ppr rdr_name)
                           , nest 2 $ text "lacks an accompanying binding"]
                       $$ nest 2 msg))

    local_msg = parens $ text "The"  <+> what <+> ptext (sLit "must be given where")
                           <+> quotes (ppr rdr_name) <+> text "is declared"


---------------
lookupLocalTcNames :: HsSigCtxt -> SDoc -> RdrName -> RnM [(RdrName, Name)]
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
    lookup rdr = do { name <- lookupBindGroupOcc ctxt what rdr
                    ; return (fmap ((,) rdr) name) }

dataTcOccs :: RdrName -> [RdrName]
-- Return both the given name and the same name promoted to the TcClsName
-- namespace.  This is useful when we aren't sure which we are looking at.
-- See also Note [dataTcOccs and Exact Names]
dataTcOccs rdr_name
  | isDataOcc occ || isVarOcc occ
  = [rdr_name, rdr_name_tc]
  | otherwise
  = [rdr_name]
  where
    occ = rdrNameOcc rdr_name
    rdr_name_tc = setRdrNameSpace rdr_name tcName

{-
Note [dataTcOccs and Exact Names]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Exact RdrNames can occur in code generated by Template Haskell, and generally
those references are, well, exact. However, the TH `Name` type isn't expressive
enough to always track the correct namespace information, so we sometimes get
the right Unique but wrong namespace. Thus, we still have to do the double-lookup
for Exact RdrNames.

There is also an awkward situation for built-in syntax. Example in GHCi
   :info []
This parses as the Exact RdrName for nilDataCon, but we also want
the list type constructor.

Note that setRdrNameSpace on an Exact name requires the Name to be External,
which it always is for built in syntax.

*********************************************************
*                                                      *
                Fixities
*                                                      *
*********************************************************

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
-}

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
  = extendFixityEnv (mapMaybe find_fixity names) thing_inside
  where
    find_fixity name
      = case lookupFsEnv mini_fix_env (occNameFS occ) of
          Just (L _ fix) -> Just (name, FixItem occ fix)
          Nothing        -> Nothing
      where
        occ = nameOccName name

{-
--------------------------------
lookupFixity is a bit strange.

* Nested local fixity decls are put in the local fixity env, which we
  find with getFixtyEnv

* Imported fixities are found in the PIT

* Top-level fixity decls in this module may be for Names that are
    either  Global         (constructors, class operations)
    or      Local/Exported (everything else)
  (See notes with RnNames.getLocalDeclBinders for why we have this split.)
  We put them all in the local fixity environment
-}

lookupFixityRn :: Name -> RnM Fixity
lookupFixityRn name = lookupFixityRn' name (nameOccName name)

lookupFixityRn' :: Name -> OccName -> RnM Fixity
lookupFixityRn' name = fmap snd . lookupFixityRn_help' name

-- | 'lookupFixityRn_help' returns @(True, fixity)@ if it finds a 'Fixity'
-- in a local environment or from an interface file. Otherwise, it returns
-- @(False, fixity)@ (e.g., for unbound 'Name's or 'Name's without
-- user-supplied fixity declarations).
lookupFixityRn_help :: Name
                    -> RnM (Bool, Fixity)
lookupFixityRn_help name =
    lookupFixityRn_help' name (nameOccName name)

lookupFixityRn_help' :: Name
                     -> OccName
                     -> RnM (Bool, Fixity)
lookupFixityRn_help' name occ
  | isUnboundName name
  = return (False, Fixity (show minPrecedence) minPrecedence InfixL)
    -- Minimise errors from ubound names; eg
    --    a>0 `foo` b>0
    -- where 'foo' is not in scope, should not give an error (Trac #7937)

  | otherwise
  = do { local_fix_env <- getFixityEnv
       ; case lookupNameEnv local_fix_env name of {
           Just (FixItem _ fix) -> return (True, fix) ;
           Nothing ->

    do { this_mod <- getModule
       ; if nameIsLocalOrFrom this_mod name
               -- Local (and interactive) names are all in the
               -- fixity env, and don't have entries in the HPT
         then return (False, defaultFixity)
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
           ; let mb_fix = mi_fix_fn iface occ
           ; let msg = case mb_fix of
                            Nothing ->
                                  text "looking up name" <+> ppr name
                              <+> text "in iface, but found no fixity for it."
                              <+> text "Using default fixity instead."
                            Just f ->
                                  text "looking up name in iface and found:"
                              <+> vcat [ppr name, ppr f]
           ; traceRn (text "lookupFixityRn_either:" <+> msg)
           ; return (maybe (False, defaultFixity) (\f -> (True, f)) mb_fix)  }

    doc = text "Checking fixity for" <+> ppr name

---------------
lookupTyFixityRn :: Located Name -> RnM Fixity
lookupTyFixityRn (L _ n) = lookupFixityRn n

-- | Look up the fixity of a (possibly ambiguous) occurrence of a record field
-- selector.  We use 'lookupFixityRn'' so that we can specifiy the 'OccName' as
-- the field label, which might be different to the 'OccName' of the selector
-- 'Name' if @DuplicateRecordFields@ is in use (Trac #1173). If there are
-- multiple possible selectors with different fixities, generate an error.
lookupFieldFixityRn :: AmbiguousFieldOcc Name -> RnM Fixity
lookupFieldFixityRn (Unambiguous (L _ rdr) n)
  = lookupFixityRn' n (rdrNameOcc rdr)
lookupFieldFixityRn (Ambiguous   (L _ rdr) _) = get_ambiguous_fixity rdr
  where
    get_ambiguous_fixity :: RdrName -> RnM Fixity
    get_ambiguous_fixity rdr_name = do
      traceRn $ text "get_ambiguous_fixity" <+> ppr rdr_name
      rdr_env <- getGlobalRdrEnv
      let elts =  lookupGRE_RdrName rdr_name rdr_env

      fixities <- groupBy ((==) `on` snd) . zip elts
                  <$> mapM lookup_gre_fixity elts

      case fixities of
        -- There should always be at least one fixity.
        -- Something's very wrong if there are no fixity candidates, so panic
        [] -> panic "get_ambiguous_fixity: no candidates for a given RdrName"
        [ (_, fix):_ ] -> return fix
        ambigs -> addErr (ambiguous_fixity_err rdr_name ambigs)
                  >> return (Fixity(show minPrecedence) minPrecedence InfixL)

    lookup_gre_fixity gre = lookupFixityRn' (gre_name gre) (greOccName gre)

    ambiguous_fixity_err rn ambigs
      = vcat [ text "Ambiguous fixity for record field" <+> quotes (ppr rn)
             , hang (text "Conflicts: ") 2 . vcat .
               map format_ambig $ concat ambigs ]

    format_ambig (elt, fix) = hang (ppr fix)
                                 2 (pprNameProvenance elt)


{- *********************************************************************
*                                                                      *
                        Role annotations
*                                                                      *
********************************************************************* -}

type RoleAnnotEnv = NameEnv (LRoleAnnotDecl Name)

mkRoleAnnotEnv :: [LRoleAnnotDecl Name] -> RoleAnnotEnv
mkRoleAnnotEnv role_annot_decls
 = mkNameEnv [ (name, ra_decl)
             | ra_decl <- role_annot_decls
             , let name = roleAnnotDeclName (unLoc ra_decl)
             , not (isUnboundName name) ]
       -- Some of the role annots will be unbound;
       -- we don't wish to include these

emptyRoleAnnotEnv :: RoleAnnotEnv
emptyRoleAnnotEnv = emptyNameEnv

lookupRoleAnnot :: RoleAnnotEnv -> Name -> Maybe (LRoleAnnotDecl Name)
lookupRoleAnnot = lookupNameEnv

getRoleAnnots :: [Name] -> RoleAnnotEnv -> ([LRoleAnnotDecl Name], RoleAnnotEnv)
getRoleAnnots bndrs role_env
  = ( mapMaybe (lookupRoleAnnot role_env) bndrs
    , delListFromNameEnv role_env bndrs )


{-
************************************************************************
*                                                                      *
                        Rebindable names
        Dealing with rebindable syntax is driven by the
        Opt_RebindableSyntax dynamic flag.

        In "deriving" code we don't want to use rebindable syntax
        so we switch off the flag locally

*                                                                      *
************************************************************************

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
-}

lookupIfThenElse :: RnM (Maybe (SyntaxExpr Name), FreeVars)
-- Different to lookupSyntaxName because in the non-rebindable
-- case we desugar directly rather than calling an existing function
-- Hence the (Maybe (SyntaxExpr Name)) return type
lookupIfThenElse
  = do { rebindable_on <- xoptM LangExt.RebindableSyntax
       ; if not rebindable_on
         then return (Nothing, emptyFVs)
         else do { ite <- lookupOccRn (mkVarUnqual (fsLit "ifThenElse"))
                 ; return ( Just (mkRnSyntaxExpr ite)
                          , unitFV ite ) } }

lookupSyntaxName' :: Name          -- ^ The standard name
                  -> RnM Name      -- ^ Possibly a non-standard name
lookupSyntaxName' std_name
  = do { rebindable_on <- xoptM LangExt.RebindableSyntax
       ; if not rebindable_on then
           return std_name
         else
            -- Get the similarly named thing from the local environment
           lookupOccRn (mkRdrUnqual (nameOccName std_name)) }

lookupSyntaxName :: Name                                -- The standard name
                 -> RnM (SyntaxExpr Name, FreeVars)     -- Possibly a non-standard name
lookupSyntaxName std_name
  = do { rebindable_on <- xoptM LangExt.RebindableSyntax
       ; if not rebindable_on then
           return (mkRnSyntaxExpr std_name, emptyFVs)
         else
            -- Get the similarly named thing from the local environment
           do { usr_name <- lookupOccRn (mkRdrUnqual (nameOccName std_name))
              ; return (mkRnSyntaxExpr usr_name, unitFV usr_name) } }

lookupSyntaxNames :: [Name]                          -- Standard names
                  -> RnM ([HsExpr Name], FreeVars)   -- See comments with HsExpr.ReboundNames
   -- this works with CmdTop, which wants HsExprs, not SyntaxExprs
lookupSyntaxNames std_names
  = do { rebindable_on <- xoptM LangExt.RebindableSyntax
       ; if not rebindable_on then
             return (map (HsVar . noLoc) std_names, emptyFVs)
        else
          do { usr_names <- mapM (lookupOccRn . mkRdrUnqual . nameOccName) std_names
             ; return (map (HsVar . noLoc) usr_names, mkFVs usr_names) } }

{-
*********************************************************
*                                                      *
\subsection{Binding}
*                                                      *
*********************************************************
-}

newLocalBndrRn :: Located RdrName -> RnM Name
-- Used for non-top-level binders.  These should
-- never be qualified.
newLocalBndrRn (L loc rdr_name)
  | Just name <- isExact_maybe rdr_name
  = return name -- This happens in code generated by Template Haskell
                -- See Note [Binders in Template Haskell] in Convert.hs
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
        | Just n <- mb_local = complain [text "bound at" <+> ppr (nameSrcLoc n)]
        | otherwise = do { gres' <- filterM is_shadowed_gre gres
                         ; complain (map pprNameProvenance gres') }
        where
          (loc,occ) = get_loc_occ n
          mb_local  = lookupLocalRdrOcc local_env occ
          gres      = lookupGRE_RdrName (mkRdrUnqual occ) global_env
                -- Make an Unqualified RdrName and look that up, so that
                -- we don't find any GREs that are in scope qualified-only

          complain []      = return ()
          complain pp_locs = addWarnAt (Reason Opt_WarnNameShadowing)
                                       loc
                                       (shadowedNameWarn occ pp_locs)

    is_shadowed_gre :: GlobalRdrElt -> RnM Bool
        -- Returns False for record selectors that are shadowed, when
        -- punning or wild-cards are on (cf Trac #2723)
    is_shadowed_gre gre | isRecFldGRE gre
        = do { dflags <- getDynFlags
             ; return $ not (xopt LangExt.RecordPuns dflags
                             || xopt LangExt.RecordWildCards dflags) }
    is_shadowed_gre _other = return True

{-
************************************************************************
*                                                                      *
               What to do when a lookup fails
*                                                                      *
************************************************************************
-}

data WhereLooking = WL_Any        -- Any binding
                  | WL_Global     -- Any top-level binding (local or imported)
                  | WL_LocalTop   -- Any top-level binding in this module

reportUnboundName :: RdrName -> RnM Name
reportUnboundName rdr = unboundName WL_Any rdr

unboundName :: WhereLooking -> RdrName -> RnM Name
unboundName wl rdr = unboundNameX wl rdr Outputable.empty

unboundNameX :: WhereLooking -> RdrName -> SDoc -> RnM Name
unboundNameX where_look rdr_name extra
  = do  { dflags <- getDynFlags
        ; let show_helpful_errors = gopt Opt_HelpfulErrors dflags
              what = pprNonVarNameSpace (occNameSpace (rdrNameOcc rdr_name))
              err = unknownNameErr what rdr_name $$ extra
        ; if not show_helpful_errors
          then addErr err
          else do { local_env  <- getLocalRdrEnv
                  ; global_env <- getGlobalRdrEnv
                  ; impInfo <- getImports
                  ; let suggestions = unknownNameSuggestions_ where_look
                                        dflags global_env local_env impInfo rdr_name
                  ; addErr (err $$ suggestions) }
        ; return (mkUnboundNameRdr rdr_name) }

unknownNameErr :: SDoc -> RdrName -> SDoc
unknownNameErr what rdr_name
  = vcat [ hang (text "Not in scope:")
              2 (what <+> quotes (ppr rdr_name))
         , extra ]
  where
    extra | rdr_name == forall_tv_RDR = perhapsForallMsg
          | otherwise                 = Outputable.empty

type HowInScope = Either SrcSpan ImpDeclSpec
     -- Left loc    =>  locally bound at loc
     -- Right ispec =>  imported as specified by ispec


-- | Called from the typechecker (TcErrors) when we find an unbound variable
unknownNameSuggestions :: DynFlags
                       -> GlobalRdrEnv -> LocalRdrEnv -> ImportAvails
                       -> RdrName -> SDoc
unknownNameSuggestions = unknownNameSuggestions_ WL_Any

unknownNameSuggestions_ :: WhereLooking -> DynFlags
                       -> GlobalRdrEnv -> LocalRdrEnv -> ImportAvails
                       -> RdrName -> SDoc
unknownNameSuggestions_ where_look dflags global_env local_env imports tried_rdr_name =
    similarNameSuggestions where_look dflags global_env local_env tried_rdr_name $$
    importSuggestions dflags imports tried_rdr_name


similarNameSuggestions :: WhereLooking -> DynFlags
                        -> GlobalRdrEnv -> LocalRdrEnv
                        -> RdrName -> SDoc
similarNameSuggestions where_look dflags global_env
                        local_env tried_rdr_name
  = case suggest of
      []  -> Outputable.empty
      [p] -> perhaps <+> pp_item p
      ps  -> sep [ perhaps <+> text "one of these:"
                 , nest 2 (pprWithCommas pp_item ps) ]
  where
    all_possibilities :: [(String, (RdrName, HowInScope))]
    all_possibilities
       =  [ (showPpr dflags r, (r, Left loc))
          | (r,loc) <- local_possibilities local_env ]
       ++ [ (showPpr dflags r, rp) | (r, rp) <- global_possibilities global_env ]

    suggest = fuzzyLookup (showPpr dflags tried_rdr_name) all_possibilities
    perhaps = text "Perhaps you meant"

    pp_item :: (RdrName, HowInScope) -> SDoc
    pp_item (rdr, Left loc) = pp_ns rdr <+> quotes (ppr rdr) <+> loc' -- Locally defined
        where loc' = case loc of
                     UnhelpfulSpan l -> parens (ppr l)
                     RealSrcSpan l -> parens (text "line" <+> int (srcSpanStartLine l))
    pp_item (rdr, Right is) = pp_ns rdr <+> quotes (ppr rdr) <+>   -- Imported
                              parens (text "imported from" <+> ppr (is_mod is))

    pp_ns :: RdrName -> SDoc
    pp_ns rdr | ns /= tried_ns = pprNameSpace ns
              | otherwise      = Outputable.empty
      where ns = rdrNameSpace rdr

    tried_occ     = rdrNameOcc tried_rdr_name
    tried_is_sym  = isSymOcc tried_occ
    tried_ns      = occNameSpace tried_occ
    tried_is_qual = isQual tried_rdr_name

    correct_name_space occ =  nameSpacesRelated (occNameSpace occ) tried_ns
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
                        , (mod, how) <- quals_in_scope gre
                        , let rdr_qual = mkRdrQual mod occ ]

      | otherwise = [ (rdr_unqual, pair)
                    | gre <- globalRdrEnvElts global_env
                    , gre_ok gre
                    , let name = gre_name gre
                          occ  = nameOccName name
                          rdr_unqual = mkRdrUnqual occ
                    , correct_name_space occ
                    , pair <- case (unquals_in_scope gre, quals_only gre) of
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
    unquals_in_scope :: GlobalRdrElt -> [HowInScope]
    unquals_in_scope (GRE { gre_name = n, gre_lcl = lcl, gre_imp = is })
      | lcl       = [ Left (nameSrcSpan n) ]
      | otherwise = [ Right ispec
                    | i <- is, let ispec = is_decl i
                    , not (is_qual ispec) ]

    --------------------
    quals_in_scope :: GlobalRdrElt -> [(ModuleName, HowInScope)]
    -- Ones for which the qualified version is in scope
    quals_in_scope (GRE { gre_name = n, gre_lcl = lcl, gre_imp = is })
      | lcl = case nameModule_maybe n of
                Nothing -> []
                Just m  -> [(moduleName m, Left (nameSrcSpan n))]
      | otherwise = [ (is_as ispec, Right ispec)
                    | i <- is, let ispec = is_decl i ]

    --------------------
    quals_only :: GlobalRdrElt -> [(RdrName, HowInScope)]
    -- Ones for which *only* the qualified version is in scope
    quals_only (GRE { gre_name = n, gre_imp = is })
      = [ (mkRdrQual (is_as ispec) (nameOccName n), Right ispec)
        | i <- is, let ispec = is_decl i, is_qual ispec ]

-- | Generate helpful suggestions if a qualified name Mod.foo is not in scope.
importSuggestions :: DynFlags -> ImportAvails -> RdrName -> SDoc
importSuggestions _dflags imports rdr_name
  | not (isQual rdr_name || isUnqual rdr_name) = Outputable.empty
  | null interesting_imports
  , Just name <- mod_name
  = hsep
      [ text "No module named"
      , quotes (ppr name)
      , text "is imported."
      ]
  | is_qualified
  , null helpful_imports
  , [(mod,_)] <- interesting_imports
  = hsep
      [ text "Module"
      , quotes (ppr mod)
      , text "does not export"
      , quotes (ppr occ_name) <> dot
      ]
  | is_qualified
  , null helpful_imports
  , mods <- map fst interesting_imports
  = hsep
      [ text "Neither"
      , quotedListWithNor (map ppr mods)
      , text "exports"
      , quotes (ppr occ_name) <> dot
      ]
  | [(mod,imv)] <- helpful_imports_non_hiding
  = fsep
      [ text "Perhaps you want to add"
      , quotes (ppr occ_name)
      , text "to the import list"
      , text "in the import of"
      , quotes (ppr mod)
      , parens (ppr (imv_span imv)) <> dot
      ]
  | not (null helpful_imports_non_hiding)
  = fsep
      [ text "Perhaps you want to add"
      , quotes (ppr occ_name)
      , text "to one of these import lists:"
      ]
    $$
    nest 2 (vcat
        [ quotes (ppr mod) <+> parens (ppr (imv_span imv))
        | (mod,imv) <- helpful_imports_non_hiding
        ])
  | [(mod,imv)] <- helpful_imports_hiding
  = fsep
      [ text "Perhaps you want to remove"
      , quotes (ppr occ_name)
      , text "from the explicit hiding list"
      , text "in the import of"
      , quotes (ppr mod)
      , parens (ppr (imv_span imv)) <> dot
      ]
  | not (null helpful_imports_hiding)
  = fsep
      [ text "Perhaps you want to remove"
      , quotes (ppr occ_name)
      , text "from the hiding clauses"
      , text "in one of these imports:"
      ]
    $$
    nest 2 (vcat
        [ quotes (ppr mod) <+> parens (ppr (imv_span imv))
        | (mod,imv) <- helpful_imports_hiding
        ])
  | otherwise
  = Outputable.empty
 where
  is_qualified = isQual rdr_name
  (mod_name, occ_name) = case rdr_name of
    Unqual occ_name        -> (Nothing, occ_name)
    Qual mod_name occ_name -> (Just mod_name, occ_name)
    _                      -> error "importSuggestions: dead code"


  -- What import statements provide "Mod" at all
  -- or, if this is an unqualified name, are not qualified imports
  interesting_imports = [ (mod, imp)
    | (mod, mod_imports) <- moduleEnvToList (imp_mods imports)
    , Just imp <- return $ pick mod_imports
    ]

  -- We want to keep only one for each original module; preferably one with an
  -- explicit import list (for no particularly good reason)
  pick :: [ImportedModsVal] -> Maybe ImportedModsVal
  pick = listToMaybe . sortBy (compare `on` prefer) . filter select
    where select imv = case mod_name of Just name -> imv_name imv == name
                                        Nothing   -> not (imv_qualified imv)
          prefer imv = (imv_is_hiding imv, imv_span imv)

  -- Which of these would export a 'foo'
  -- (all of these are restricted imports, because if they were not, we
  -- wouldn't have an out-of-scope error in the first place)
  helpful_imports = filter helpful interesting_imports
    where helpful (_,imv)
            = not . null $ lookupGlobalRdrEnv (imv_all_exports imv) occ_name

  -- Which of these do that because of an explicit hiding list resp. an
  -- explicit import list
  (helpful_imports_hiding, helpful_imports_non_hiding)
    = partition (imv_is_hiding . snd) helpful_imports

{-
************************************************************************
*                                                                      *
\subsection{Free variable manipulation}
*                                                                      *
************************************************************************
-}

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

{-
************************************************************************
*                                                                      *
\subsection{Envt utility functions}
*                                                                      *
************************************************************************
-}

warnUnusedTopBinds :: [GlobalRdrElt] -> RnM ()
warnUnusedTopBinds gres
    = whenWOptM Opt_WarnUnusedTopBinds
    $ do env <- getGblEnv
         let isBoot = tcg_src env == HsBootFile
         let noParent gre = case gre_par gre of
                            NoParent -> True
                            PatternSynonym -> True
                            _        -> False
             -- Don't warn about unused bindings with parents in
             -- .hs-boot files, as you are sometimes required to give
             -- unused bindings (trac #3449).
             -- HOWEVER, in a signature file, you are never obligated to put a
             -- definition in the main text.  Thus, if you define something
             -- and forget to export it, we really DO want to warn.
             gres' = if isBoot then filter noParent gres
                               else                 gres
         warnUnusedGREs gres'

warnUnusedLocalBinds, warnUnusedMatches, warnUnusedTypePatterns
  :: [Name] -> FreeVars -> RnM ()
warnUnusedLocalBinds   = check_unused Opt_WarnUnusedLocalBinds
warnUnusedMatches      = check_unused Opt_WarnUnusedMatches
warnUnusedTypePatterns = check_unused Opt_WarnUnusedTypePatterns

check_unused :: WarningFlag -> [Name] -> FreeVars -> RnM ()
check_unused flag bound_names used_names
  = whenWOptM flag (warnUnused flag (filterOut (`elemNameSet` used_names)
                                               bound_names))

-------------------------
--      Helpers
warnUnusedGREs :: [GlobalRdrElt] -> RnM ()
warnUnusedGREs gres = mapM_ warnUnusedGRE gres

warnUnused :: WarningFlag -> [Name] -> RnM ()
warnUnused flag names = do
    fld_env <- mkFieldEnv <$> getGlobalRdrEnv
    mapM_ (warnUnused1 flag fld_env) names

warnUnused1 :: WarningFlag -> NameEnv (FieldLabelString, Name) -> Name -> RnM ()
warnUnused1 flag fld_env name
  = when (reportable name occ) $
    addUnusedWarning flag
                     occ (nameSrcSpan name)
                     (text "Defined but not used")
  where
    occ = case lookupNameEnv fld_env name of
              Just (fl, _) -> mkVarOccFS fl
              Nothing      -> nameOccName name

warnUnusedGRE :: GlobalRdrElt -> RnM ()
warnUnusedGRE gre@(GRE { gre_name = name, gre_lcl = lcl, gre_imp = is })
  | lcl       = do fld_env <- mkFieldEnv <$> getGlobalRdrEnv
                   warnUnused1 Opt_WarnUnusedTopBinds fld_env name
  | otherwise = when (reportable name occ) (mapM_ warn is)
  where
    occ = greOccName gre
    warn spec = addUnusedWarning Opt_WarnUnusedTopBinds occ span msg
        where
           span = importSpecLoc spec
           pp_mod = quotes (ppr (importSpecModule spec))
           msg = text "Imported from" <+> pp_mod <+> ptext (sLit "but not used")

-- | Make a map from selector names to field labels and parent tycon
-- names, to be used when reporting unused record fields.
mkFieldEnv :: GlobalRdrEnv -> NameEnv (FieldLabelString, Name)
mkFieldEnv rdr_env = mkNameEnv [ (gre_name gre, (lbl, par_is (gre_par gre)))
                               | gres <- occEnvElts rdr_env
                               , gre <- gres
                               , Just lbl <- [greLabel gre]
                               ]

-- | Should we report the fact that this 'Name' is unused? The
-- 'OccName' may differ from 'nameOccName' due to
-- DuplicateRecordFields.
reportable :: Name -> OccName -> Bool
reportable name occ
  | isWiredInName name = False    -- Don't report unused wired-in names
                                  -- Otherwise we get a zillion warnings
                                  -- from Data.Tuple
  | otherwise = not (startsWithUnderscore occ)

addUnusedWarning :: WarningFlag -> OccName -> SrcSpan -> SDoc -> RnM ()
addUnusedWarning flag occ span msg
  = addWarnAt (Reason flag) span $
    sep [msg <> colon,
         nest 2 $ pprNonVarNameSpace (occNameSpace occ)
                        <+> quotes (ppr occ)]

addNameClashErrRn :: RdrName -> [GlobalRdrElt] -> RnM ()
addNameClashErrRn rdr_name gres
  | all isLocalGRE gres && not (all isRecFldGRE gres)
               -- If there are two or more *local* defns, we'll have reported
  = return ()  -- that already, and we don't want an error cascade
  | otherwise
  = addErr (vcat [text "Ambiguous occurrence" <+> quotes (ppr rdr_name),
                  text "It could refer to" <+> vcat (msg1 : msgs)])
  where
    (np1:nps) = gres
    msg1 = ptext  (sLit "either") <+> mk_ref np1
    msgs = [text "    or" <+> mk_ref np | np <- nps]
    mk_ref gre = sep [nom <> comma, pprNameProvenance gre]
      where nom = case gre_par gre of
                    FldParent { par_lbl = Just lbl } -> text "the field" <+> quotes (ppr lbl)
                    _                                -> quotes (ppr (gre_name gre))

shadowedNameWarn :: OccName -> [SDoc] -> SDoc
shadowedNameWarn occ shadowed_locs
  = sep [text "This binding for" <+> quotes (ppr occ)
            <+> text "shadows the existing binding" <> plural shadowed_locs,
         nest 2 (vcat shadowed_locs)]

perhapsForallMsg :: SDoc
perhapsForallMsg
  = vcat [ text "Perhaps you intended to use ExplicitForAll or similar flag"
         , text "to enable explicit-forall syntax: forall <tvs>. <type>"]

unknownSubordinateErr :: SDoc -> RdrName -> SDoc
unknownSubordinateErr doc op    -- Doc is "method of class" or
                                -- "field of constructor"
  = quotes (ppr op) <+> text "is not a (visible)" <+> doc

badOrigBinding :: RdrName -> SDoc
badOrigBinding name
  = text "Illegal binding of built-in syntax:" <+> ppr (rdrNameOcc name)
        -- The rdrNameOcc is because we don't want to print Prelude.(,)

dupNamesErr :: Outputable n => (n -> SrcSpan) -> [n] -> RnM ()
dupNamesErr get_loc names
  = addErrAt big_loc $
    vcat [text "Conflicting definitions for" <+> quotes (ppr (head names)),
          locations]
  where
    locs      = map get_loc names
    big_loc   = foldr1 combineSrcSpans locs
    locations = text "Bound at:" <+> vcat (map ppr (sort locs))

kindSigErr :: Outputable a => a -> SDoc
kindSigErr thing
  = hang (text "Illegal kind signature for" <+> quotes (ppr thing))
       2 (text "Perhaps you intended to use KindSignatures")

badQualBndrErr :: RdrName -> SDoc
badQualBndrErr rdr_name
  = text "Qualified name in binding position:" <+> ppr rdr_name

opDeclErr :: RdrName -> SDoc
opDeclErr n
  = hang (text "Illegal declaration of a type or class operator" <+> quotes (ppr n))
       2 (text "Use TypeOperators to declare operators in type and declarations")

checkTupSize :: Int -> RnM ()
checkTupSize tup_size
  | tup_size <= mAX_TUPLE_SIZE
  = return ()
  | otherwise
  = addErr (sep [text "A" <+> int tup_size <> ptext (sLit "-tuple is too large for GHC"),
                 nest 2 (parens (text "max size is" <+> int mAX_TUPLE_SIZE)),
                 nest 2 (text "Workaround: use nested tuples or define a data type")])

{-
************************************************************************
*                                                                      *
\subsection{Contexts for renaming errors}
*                                                                      *
************************************************************************
-}

-- AZ:TODO: Change these all to be Name instead of RdrName.
--          Merge TcType.UserTypeContext in to it.
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
  | FamPatCtx (Located RdrName)    -- The patterns of a type/data family instance
  | ConDeclCtx [Located Name]
  | ClassDeclCtx (Located RdrName)
  | ExprWithTySigCtx
  | TypBrCtx
  | HsTypeCtx
  | GHCiCtx
  | SpliceTypeCtx (LHsType RdrName)
  | ClassInstanceCtx
  | VectDeclCtx (Located RdrName)
  | GenericCtx SDoc   -- Maybe we want to use this more!

withHsDocContext :: HsDocContext -> SDoc -> SDoc
withHsDocContext ctxt doc = doc $$ inHsDocContext ctxt

inHsDocContext :: HsDocContext -> SDoc
inHsDocContext ctxt = text "In" <+> pprHsDocContext ctxt

pprHsDocContext :: HsDocContext -> SDoc
pprHsDocContext (GenericCtx doc)      = doc
pprHsDocContext (TypeSigCtx doc)      = text "the type signature for" <+> doc
pprHsDocContext PatCtx                = text "a pattern type-signature"
pprHsDocContext SpecInstSigCtx        = text "a SPECIALISE instance pragma"
pprHsDocContext DefaultDeclCtx        = text "a `default' declaration"
pprHsDocContext DerivDeclCtx          = text "a deriving declaration"
pprHsDocContext (RuleCtx name)        = text "the transformation rule" <+> ftext name
pprHsDocContext (TyDataCtx tycon)     = text "the data type declaration for" <+> quotes (ppr tycon)
pprHsDocContext (FamPatCtx tycon)     = text "a type pattern of family instance for" <+> quotes (ppr tycon)
pprHsDocContext (TySynCtx name)       = text "the declaration for type synonym" <+> quotes (ppr name)
pprHsDocContext (TyFamilyCtx name)    = text "the declaration for type family" <+> quotes (ppr name)
pprHsDocContext (ClassDeclCtx name)   = text "the declaration for class" <+> quotes (ppr name)
pprHsDocContext ExprWithTySigCtx      = text "an expression type signature"
pprHsDocContext TypBrCtx              = text "a Template-Haskell quoted type"
pprHsDocContext HsTypeCtx             = text "a type argument"
pprHsDocContext GHCiCtx               = text "GHCi input"
pprHsDocContext (SpliceTypeCtx hs_ty) = text "the spliced type" <+> quotes (ppr hs_ty)
pprHsDocContext ClassInstanceCtx      = text "TcSplice.reifyInstances"

pprHsDocContext (ForeignDeclCtx name)
   = text "the foreign declaration for" <+> quotes (ppr name)
pprHsDocContext (ConDeclCtx [name])
   = text "the definition of data constructor" <+> quotes (ppr name)
pprHsDocContext (ConDeclCtx names)
   = text "the definition of data constructors" <+> interpp'SP names
pprHsDocContext (VectDeclCtx tycon)
   = text "the VECTORISE pragma for type constructor" <+> quotes (ppr tycon)
