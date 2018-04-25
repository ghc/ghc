{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-2006

RnEnv contains functions which convert RdrNames into Names.

-}

{-# LANGUAGE CPP, MultiWayIf, NamedFieldPuns #-}

module RnEnv (
        newTopSrcBinder,
        lookupLocatedTopBndrRn, lookupTopBndrRn,
        lookupLocatedOccRn, lookupOccRn, lookupOccRn_maybe,
        lookupLocalOccRn_maybe, lookupInfoOccRn,
        lookupLocalOccThLvl_maybe, lookupLocalOccRn,
        lookupTypeOccRn, lookupKindOccRn,
        lookupGlobalOccRn, lookupGlobalOccRn_maybe,
        lookupOccRn_overloaded, lookupGlobalOccRn_overloaded, lookupExactOcc,

        ChildLookupResult(..),
        lookupSubBndrOcc_helper,
        combineChildLookupResult, -- Called by lookupChildrenExport

        HsSigCtxt(..), lookupLocalTcNames, lookupSigOccRn,
        lookupSigCtxtOccRn,

        lookupInstDeclBndr, lookupRecFieldOcc, lookupFamInstName,
        lookupConstructorFields,

        lookupGreAvailRn,

        -- Rebindable Syntax
        lookupSyntaxName, lookupSyntaxName', lookupSyntaxNames,
        lookupIfThenElse,

        -- Constructing usage information
        addUsedGRE, addUsedGREs, addUsedDataCons,



        dataTcOccs, --TODO: Move this somewhere, into utils?

    ) where

#include "HsVersions.h"

import GhcPrelude

import LoadIface        ( loadInterfaceForName, loadSrcInterface_maybe )
import IfaceEnv
import HsSyn
import RdrName
import HscTypes
import TcEnv
import TcRnMonad
import RdrHsSyn         ( setRdrNameSpace )
import TysWiredIn
import Name
import NameSet
import NameEnv
import Avail
import Module
import ConLike
import DataCon
import TyCon
import ErrUtils         ( MsgDoc )
import PrelNames        ( rOOT_MAIN )
import BasicTypes       ( pprWarningTxtForMsg, TopLevelFlag(..))
import SrcLoc
import Outputable
import Util
import Maybes
import DynFlags
import FastString
import Control.Monad
import ListSetOps       ( minusList )
import qualified GHC.LanguageExtensions as LangExt
import RnUnbound
import RnUtils
import Data.Maybe (isJust)
import qualified Data.Semigroup as Semi

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
  = do  { when (isQual rdr_name)
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
                ; traceRn "newTopSrcBinder" (ppr this_mod $$ ppr rdr_name $$ ppr loc)
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

-- Can be made to not be exposed
-- Only used unwrapped in rnAnnProvenance
lookupTopBndrRn :: RdrName -> RnM Name
lookupTopBndrRn n = do nopt <- lookupTopBndrRn_maybe n
                       case nopt of
                         Just n' -> return n'
                         Nothing -> do traceRn "lookupTopBndrRn fail" (ppr n)
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

lookupTopBndrRn_maybe rdr_name =
  lookupExactOrOrig rdr_name Just $
    do  {  -- Check for operators in type or class declarations
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
                         do { th_topnames_var <- fmap tcg_th_topnames getGblEnv
                            ; th_topnames <- readTcRef th_topnames_var
                            ; if name `elemNameSet` th_topnames
                              then return (Right name)
                              else return (Left exact_nm_err)
                            }
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
lookupFamInstName :: Maybe Name -> Located RdrName
                  -> RnM (Located Name)
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


-- In CPS style as `RnM r` is monadic
lookupExactOrOrig :: RdrName -> (Name -> r) -> RnM r -> RnM r
lookupExactOrOrig rdr_name res k
  | Just n <- isExact_maybe rdr_name   -- This happens in derived code
  = res <$> lookupExactOcc n
  | Just (rdr_mod, rdr_occ) <- isOrig_maybe rdr_name
  = res <$> lookupOrig rdr_mod rdr_occ
  | otherwise = k



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

lookupRecFieldOcc :: Maybe Name -- Nothing    => just look it up as usual
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
  -- This use of Global is right as we are looking up a selector which
  -- can only be defined at the top level.
  = lookupGlobalOccRn rdr_name



-- | Used in export lists to lookup the children.
lookupSubBndrOcc_helper :: Bool -> Bool -> Name -> RdrName
                        -> RnM ChildLookupResult
lookupSubBndrOcc_helper must_have_parent warn_if_deprec parent rdr_name
  | isUnboundName parent
    -- Avoid an error cascade
  = return (FoundName NoParent (mkUnboundNameRdr rdr_name))

  | otherwise = do
  gre_env <- getGlobalRdrEnv

  let original_gres = lookupGlobalRdrEnv gre_env (rdrNameOcc rdr_name)
  -- Disambiguate the lookup based on the parent information.
  -- The remaining GREs are things that we *could* export here, note that
  -- this includes things which have `NoParent`. Those are sorted in
  -- `checkPatSynParent`.
  traceRn "parent" (ppr parent)
  traceRn "lookupExportChild original_gres:" (ppr original_gres)
  traceRn "lookupExportChild picked_gres:" (ppr $ picked_gres original_gres)
  case picked_gres original_gres of
    NoOccurrence ->
      noMatchingParentErr original_gres
    UniqueOccurrence g ->
      if must_have_parent then noMatchingParentErr original_gres
                          else checkFld g
    DisambiguatedOccurrence g ->
      checkFld g
    AmbiguousOccurrence gres ->
      mkNameClashErr gres
    where
        -- Convert into FieldLabel if necessary
        checkFld :: GlobalRdrElt -> RnM ChildLookupResult
        checkFld g@GRE{gre_name, gre_par} = do
          addUsedGRE warn_if_deprec g
          return $ case gre_par of
            FldParent _ mfs ->
              FoundFL  (fldParentToFieldLabel gre_name mfs)
            _ -> FoundName gre_par gre_name

        fldParentToFieldLabel :: Name -> Maybe FastString -> FieldLabel
        fldParentToFieldLabel name mfs =
          case mfs of
            Nothing ->
              let fs = occNameFS (nameOccName name)
              in FieldLabel fs False name
            Just fs -> FieldLabel fs True name

        -- Called when we find no matching GREs after disambiguation but
        -- there are three situations where this happens.
        -- 1. There were none to begin with.
        -- 2. None of the matching ones were the parent but
        --  a. They were from an overloaded record field so we can report
        --     a better error
        --  b. The original lookup was actually ambiguous.
        --     For example, the case where overloading is off and two
        --     record fields are in scope from different record
        --     constructors, neither of which is the parent.
        noMatchingParentErr :: [GlobalRdrElt] -> RnM ChildLookupResult
        noMatchingParentErr original_gres = do
          overload_ok <- xoptM LangExt.DuplicateRecordFields
          case original_gres of
            [] ->  return NameNotFound
            [g] -> return $ IncorrectParent parent
                              (gre_name g) (ppr $ gre_name g)
                              [p | Just p <- [getParent g]]
            gss@(g:_:_) ->
              if all isRecFldGRE gss && overload_ok
                then return $
                      IncorrectParent parent
                        (gre_name g)
                        (ppr $ expectJust "noMatchingParentErr" (greLabel g))
                        [p | x <- gss, Just p <- [getParent x]]
                else mkNameClashErr gss

        mkNameClashErr :: [GlobalRdrElt] -> RnM ChildLookupResult
        mkNameClashErr gres = do
          addNameClashErrRn rdr_name gres
          return (FoundName (gre_par (head gres)) (gre_name (head gres)))

        getParent :: GlobalRdrElt -> Maybe Name
        getParent (GRE { gre_par = p } ) =
          case p of
            ParentIs cur_parent -> Just cur_parent
            FldParent { par_is = cur_parent } -> Just cur_parent
            NoParent -> Nothing

        picked_gres :: [GlobalRdrElt] -> DisambigInfo
        picked_gres gres
          | isUnqual rdr_name
              = mconcat (map right_parent gres)
          | otherwise
              = mconcat (map right_parent (pickGREs rdr_name gres))


        right_parent :: GlobalRdrElt -> DisambigInfo
        right_parent p
          | Just cur_parent <- getParent p
            = if parent == cur_parent
                then DisambiguatedOccurrence p
                else NoOccurrence
          | otherwise
            = UniqueOccurrence p


-- This domain specific datatype is used to record why we decided it was
-- possible that a GRE could be exported with a parent.
data DisambigInfo
       = NoOccurrence
          -- The GRE could never be exported. It has the wrong parent.
       | UniqueOccurrence GlobalRdrElt
          -- The GRE has no parent. It could be a pattern synonym.
       | DisambiguatedOccurrence GlobalRdrElt
          -- The parent of the GRE is the correct parent
       | AmbiguousOccurrence [GlobalRdrElt]
          -- For example, two normal identifiers with the same name are in
          -- scope. They will both be resolved to "UniqueOccurrence" and the
          -- monoid will combine them to this failing case.

instance Outputable DisambigInfo where
  ppr NoOccurrence = text "NoOccurence"
  ppr (UniqueOccurrence gre) = text "UniqueOccurrence:" <+> ppr gre
  ppr (DisambiguatedOccurrence gre) = text "DiambiguatedOccurrence:" <+> ppr gre
  ppr (AmbiguousOccurrence gres)    = text "Ambiguous:" <+> ppr gres

instance Semi.Semigroup DisambigInfo where
  -- This is the key line: We prefer disambiguated occurrences to other
  -- names.
  _ <> DisambiguatedOccurrence g' = DisambiguatedOccurrence g'
  DisambiguatedOccurrence g' <> _ = DisambiguatedOccurrence g'

  NoOccurrence <> m = m
  m <> NoOccurrence = m
  UniqueOccurrence g <> UniqueOccurrence g'
    = AmbiguousOccurrence [g, g']
  UniqueOccurrence g <> AmbiguousOccurrence gs
    = AmbiguousOccurrence (g:gs)
  AmbiguousOccurrence gs <> UniqueOccurrence g'
    = AmbiguousOccurrence (g':gs)
  AmbiguousOccurrence gs <> AmbiguousOccurrence gs'
    = AmbiguousOccurrence (gs ++ gs')

instance Monoid DisambigInfo where
  mempty = NoOccurrence
  mappend = (Semi.<>)

-- Lookup SubBndrOcc can never be ambiguous
--
-- Records the result of looking up a child.
data ChildLookupResult
      = NameNotFound                --  We couldn't find a suitable name
      | IncorrectParent Name        -- Parent
                        Name        -- Name of thing we were looking for
                        SDoc        -- How to print the name
                        [Name]      -- List of possible parents
      | FoundName Parent Name       --  We resolved to a normal name
      | FoundFL FieldLabel          --  We resolved to a FL

-- | Specialised version of msum for RnM ChildLookupResult
combineChildLookupResult :: [RnM ChildLookupResult] -> RnM ChildLookupResult
combineChildLookupResult [] = return NameNotFound
combineChildLookupResult (x:xs) = do
  res <- x
  case res of
    NameNotFound -> combineChildLookupResult xs
    _ -> return res

instance Outputable ChildLookupResult where
  ppr NameNotFound = text "NameNotFound"
  ppr (FoundName p n) = text "Found:" <+> ppr p <+> ppr n
  ppr (FoundFL fls) = text "FoundFL:" <+> ppr fls
  ppr (IncorrectParent p n td ns) = text "IncorrectParent"
                                  <+> hsep [ppr p, ppr n, td, ppr ns]

lookupSubBndrOcc :: Bool
                 -> Name     -- Parent
                 -> SDoc
                 -> RdrName
                 -> RnM (Either MsgDoc Name)
-- Find all the things the rdr-name maps to
-- and pick the one with the right parent namep
lookupSubBndrOcc warn_if_deprec the_parent doc rdr_name = do
  res <-
    lookupExactOrOrig rdr_name (FoundName NoParent) $
      -- This happens for built-in classes, see mod052 for example
      lookupSubBndrOcc_helper True warn_if_deprec the_parent rdr_name
  case res of
    NameNotFound -> return (Left (unknownSubordinateErr doc rdr_name))
    FoundName _p n -> return (Right n)
    FoundFL fl  ->  return (Right (flSelector fl))
    IncorrectParent {} -> return $ Left (unknownSubordinateErr doc rdr_name)


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
unique, but different name spaces.

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
   import qualified M
   import M( C(f) )
   instance C T where
     f x = x
Here we want to record a use of 'f', not of 'M.f', otherwise
we'll miss the fact that the qualified import is redundant.

--------------------------------------------------
--              Occurrences
--------------------------------------------------
-}


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

-- Only used in one place, to rename pattern synonym binders.
-- See Note [Renaming pattern synonym variables] in RnBinds
lookupLocalOccRn :: RdrName -> RnM Name
lookupLocalOccRn rdr_name
  = do { mb_name <- lookupLocalOccRn_maybe rdr_name
       ; case mb_name of
           Just name -> return name
           Nothing   -> unboundName WL_LocalOnly rdr_name }

lookupKindOccRn :: RdrName -> RnM Name
-- Looking up a name occurring in a kind
lookupKindOccRn rdr_name
  | isVarOcc (rdrNameOcc rdr_name)  -- See Note [Promoted variables in types]
  = badVarInType rdr_name
  | otherwise
  = do { typeintype <- xoptM LangExt.TypeInType
       ; if | typeintype           -> lookupTypeOccRn rdr_name
      -- With -XNoTypeInType, treat any usage of * in kinds as in scope
      -- this is a dirty hack, but then again so was the old * kind.
            | isStar rdr_name     -> return starKindTyConName
            | isUniStar rdr_name -> return unicodeStarKindTyConName
            | otherwise            -> lookupOccRn rdr_name }

-- lookupPromotedOccRn looks up an optionally promoted RdrName.
lookupTypeOccRn :: RdrName -> RnM Name
-- see Note [Demotion]
lookupTypeOccRn rdr_name
  | isVarOcc (rdrNameOcc rdr_name)  -- See Note [Promoted variables in types]
  = badVarInType rdr_name
  | otherwise
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
       ; if data_kinds
            then do { mb_demoted_name <- lookupOccRn_maybe demoted_rdr
                    ; case mb_demoted_name of
                        Nothing -> unboundNameX WL_Any rdr_name star_info
                        Just demoted_name ->
                          do { whenWOptM Opt_WarnUntickedPromotedConstructors $
                               addWarn
                                 (Reason Opt_WarnUntickedPromotedConstructors)
                                 (untickedPromConstrWarn demoted_name)
                             ; return demoted_name } }
            else do { -- We need to check if a data constructor of this name is
                      -- in scope to give good error messages. However, we do
                      -- not want to give an additional error if the data
                      -- constructor happens to be out of scope! See #13947.
                      mb_demoted_name <- discardErrs $
                                         lookupOccRn_maybe demoted_rdr
                    ; let suggestion | isJust mb_demoted_name = suggest_dk
                                     | otherwise              = star_info
                    ; unboundNameX WL_Any rdr_name suggestion } }

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
      | isStar rdr_name || isUniStar rdr_name
      = if xopt LangExt.TypeInType dflags
        then text "NB: With TypeInType, you must import" <+>
             ppr rdr_name <+> text "from Data.Kind"
        else empty

      | otherwise
      = empty


badVarInType :: RdrName -> RnM Name
badVarInType rdr_name
  = do { addErr (text "Illegal promoted term variable in a type:"
                 <+> ppr rdr_name)
       ; return (mkUnboundNameRdr rdr_name) }

{- Note [Promoted variables in types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this (Trac #12686):
   x = True
   data Bad = Bad 'x

The parser treats the quote in 'x as saying "use the term
namespace", so we'll get (Bad x{v}), with 'x' in the
VarName namespace.  If we don't test for this, the renamer
will happily rename it to the x bound at top level, and then
the typecheck falls over because it doesn't have 'x' in scope
when kind-checking.

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

lookupOccRnX_maybe :: (RdrName -> RnM (Maybe r)) -> (Name -> r) -> RdrName
                   -> RnM (Maybe r)
lookupOccRnX_maybe globalLookup wrapper rdr_name
  = runMaybeT . msum . map MaybeT $
      [ fmap wrapper <$> lookupLocalOccRn_maybe rdr_name
      , globalLookup rdr_name ]

lookupOccRn_maybe :: RdrName -> RnM (Maybe Name)
lookupOccRn_maybe = lookupOccRnX_maybe lookupGlobalOccRn_maybe id

lookupOccRn_overloaded :: Bool -> RdrName
                       -> RnM (Maybe (Either Name [Name]))
lookupOccRn_overloaded overload_ok
  = lookupOccRnX_maybe global_lookup Left
      where
        global_lookup :: RdrName -> RnM (Maybe (Either Name [Name]))
        global_lookup n =
          runMaybeT . msum . map MaybeT $
            [ lookupGlobalOccRn_overloaded overload_ok n
            , fmap Left . listToMaybe <$> lookupQualifiedNameGHCi n ]



lookupGlobalOccRn_maybe :: RdrName -> RnM (Maybe Name)
-- Looks up a RdrName occurrence in the top-level
--   environment, including using lookupQualifiedNameGHCi
--   for the GHCi case
-- No filter function; does not report an error on failure
-- Uses addUsedRdrName to record use and deprecations
lookupGlobalOccRn_maybe rdr_name =
  lookupExactOrOrig rdr_name Just $
    runMaybeT . msum . map MaybeT $
      [ fmap gre_name <$> lookupGreRn_maybe rdr_name
      , listToMaybe <$> lookupQualifiedNameGHCi rdr_name ]
                      -- This test is not expensive,
                      -- and only happens for failed lookups

lookupGlobalOccRn :: RdrName -> RnM Name
-- lookupGlobalOccRn is like lookupOccRn, except that it looks in the global
-- environment.  Adds an error message if the RdrName is not in scope.
-- You usually want to use "lookupOccRn" which also looks in the local
-- environment.
lookupGlobalOccRn rdr_name
  = do { mb_name <- lookupGlobalOccRn_maybe rdr_name
       ; case mb_name of
           Just n  -> return n
           Nothing -> do { traceRn "lookupGlobalOccRn" (ppr rdr_name)
                         ; unboundName WL_Global rdr_name } }

lookupInfoOccRn :: RdrName -> RnM [Name]
-- lookupInfoOccRn is intended for use in GHCi's ":info" command
-- It finds all the GREs that RdrName could mean, not complaining
-- about ambiguity, but rather returning them all
-- C.f. Trac #9881
lookupInfoOccRn rdr_name =
  lookupExactOrOrig rdr_name (:[]) $
    do { rdr_env <- getGlobalRdrEnv
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

lookupGlobalOccRn_overloaded :: Bool -> RdrName
                             -> RnM (Maybe (Either Name [Name]))
lookupGlobalOccRn_overloaded overload_ok rdr_name =
  lookupExactOrOrig rdr_name (Just . Left) $
     do  { res <- lookupGreRn_helper rdr_name
         ; case res of
                GreNotFound  -> return Nothing
                OneNameMatch gre -> do
                  let wrapper = if isRecFldGRE gre then Right . (:[]) else Left
                  return $ Just (wrapper (gre_name gre))
                MultipleNames gres  | all isRecFldGRE gres && overload_ok ->
                  -- Don't record usage for ambiguous selectors
                  -- until we know which is meant
                  return $ Just (Right (map gre_name gres))
                MultipleNames gres  -> do
                  addNameClashErrRn rdr_name gres
                  return (Just (Left (gre_name (head gres)))) }


--------------------------------------------------
--      Lookup in the Global RdrEnv of the module
--------------------------------------------------

data GreLookupResult = GreNotFound
                     | OneNameMatch GlobalRdrElt
                     | MultipleNames [GlobalRdrElt]

lookupGreRn_maybe :: RdrName -> RnM (Maybe GlobalRdrElt)
-- Look up the RdrName in the GlobalRdrEnv
--   Exactly one binding: records it as "used", return (Just gre)
--   No bindings:         return Nothing
--   Many bindings:       report "ambiguous", return an arbitrary (Just gre)
-- Uses addUsedRdrName to record use and deprecations
lookupGreRn_maybe rdr_name
  = do
      res <- lookupGreRn_helper rdr_name
      case res of
        OneNameMatch gre ->  return $ Just gre
        MultipleNames gres -> do
          traceRn "lookupGreRn_maybe:NameClash" (ppr gres)
          addNameClashErrRn rdr_name gres
          return $ Just (head gres)
        GreNotFound -> return Nothing

{-

Note [ Unbound vs Ambiguous Names ]

lookupGreRn_maybe deals with failures in two different ways. If a name
is unbound then we return a `Nothing` but if the name is ambiguous
then we raise an error and return a dummy name.

The reason for this is that when we call `lookupGreRn_maybe` we are
speculatively looking for whatever we are looking up. If we don't find it,
then we might have been looking for the wrong thing and can keep trying.
On the other hand, if we find a clash then there is no way to recover as
we found the thing we were looking for but can no longer resolve which
the correct one is.

One example of this is in `lookupTypeOccRn` which first looks in the type
constructor namespace before looking in the data constructor namespace to
deal with `DataKinds`.

There is however, as always, one exception to this scheme. If we find
an ambiguous occurence of a record selector and DuplicateRecordFields
is enabled then we defer the selection until the typechecker.

-}




-- Internal Function
lookupGreRn_helper :: RdrName -> RnM GreLookupResult
lookupGreRn_helper rdr_name
  = do  { env <- getGlobalRdrEnv
        ; case lookupGRE_RdrName rdr_name env of
            []    -> return GreNotFound
            [gre] -> do { addUsedGRE True gre
                        ; return (OneNameMatch gre) }
            gres  -> return (MultipleNames gres) }

lookupGreAvailRn :: RdrName -> RnM (Name, AvailInfo)
-- Used in export lists
-- If not found or ambiguous, add error message, and fake with UnboundName
-- Uses addUsedRdrName to record use and deprecations
lookupGreAvailRn rdr_name
  = do
      mb_gre <- lookupGreRn_helper rdr_name
      case mb_gre of
        GreNotFound ->
          do
            traceRn "lookupGreAvailRn" (ppr rdr_name)
            name <- unboundName WL_Global rdr_name
            return (name, avail name)
        MultipleNames gres ->
          do
            addNameClashErrRn rdr_name gres
            let unbound_name = mkUnboundNameRdr rdr_name
            return (unbound_name, avail unbound_name)
                        -- Returning an unbound name here prevents an error
                        -- cascade
        OneNameMatch gre ->
          return (gre_name gre, availFromGRE gre)


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
            ; traceRn "addUsedGRE" (ppr gre)
            ; updMutVar (tcg_used_gres env) (gre :) } }

addUsedGREs :: [GlobalRdrElt] -> RnM ()
-- Record uses of any *imported* GREs
-- Used for recording used sub-bndrs
-- NB: no call to warnIfDeprecated; see Note [Handling of deprecations]
addUsedGREs gres
  | null imp_gres = return ()
  | otherwise     = do { env <- getGblEnv
                       ; traceRn "addUsedGREs" (ppr imp_gres)
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
            , pprWarningTxtForMsg txt ]
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

For example, writing `Data.List.sort` will load the interface file for
`Data.List` as if the user had written `import qualified Data.List`.

If we fail we just return Nothing, rather than bleating
about "attempting to use module ‘D’ (./D.hs) which is not loaded"
which is what loadSrcInterface does.

It is enabled by default and disabled by the flag
`-fno-implicit-import-qualified`.

Note [Safe Haskell and GHCi]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We DON'T do this Safe Haskell as we need to check imports. We can
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
                     do { traceRn "lookupQualifiedNameGHCi" (ppr rdr_name)
                        ; return [] } }

      | otherwise
      = do { traceRn "lookupQualifiedNameGHCi: off" (ppr rdr_name)
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
               -> Sig GhcPs
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
      = do { mname <- lookupLocalOccRn_maybe rdr_name
           ; case mname of
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
-}



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

We treat the original (standard) names as free-vars too, because the type checker
checks the type of the user thing against the type of the standard thing.
-}

lookupIfThenElse :: RnM (Maybe (SyntaxExpr GhcRn), FreeVars)
-- Different to lookupSyntaxName because in the non-rebindable
-- case we desugar directly rather than calling an existing function
-- Hence the (Maybe (SyntaxExpr GhcRn)) return type
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

lookupSyntaxName :: Name                             -- The standard name
                 -> RnM (SyntaxExpr GhcRn, FreeVars) -- Possibly a non-standard
                                                     -- name
lookupSyntaxName std_name
  = do { rebindable_on <- xoptM LangExt.RebindableSyntax
       ; if not rebindable_on then
           return (mkRnSyntaxExpr std_name, emptyFVs)
         else
            -- Get the similarly named thing from the local environment
           do { usr_name <- lookupOccRn (mkRdrUnqual (nameOccName std_name))
              ; return (mkRnSyntaxExpr usr_name, unitFV usr_name) } }

lookupSyntaxNames :: [Name]                         -- Standard names
     -> RnM ([HsExpr GhcRn], FreeVars) -- See comments with HsExpr.ReboundNames
   -- this works with CmdTop, which wants HsExprs, not SyntaxExprs
lookupSyntaxNames std_names
  = do { rebindable_on <- xoptM LangExt.RebindableSyntax
       ; if not rebindable_on then
             return (map (HsVar . noLoc) std_names, emptyFVs)
        else
          do { usr_names <- mapM (lookupOccRn . mkRdrUnqual . nameOccName) std_names
             ; return (map (HsVar . noLoc) usr_names, mkFVs usr_names) } }

-- Error messages


opDeclErr :: RdrName -> SDoc
opDeclErr n
  = hang (text "Illegal declaration of a type or class operator" <+> quotes (ppr n))
       2 (text "Use TypeOperators to declare operators in type and declarations")

badOrigBinding :: RdrName -> SDoc
badOrigBinding name
  | Just _ <- isBuiltInOcc_maybe occ
  = text "Illegal binding of built-in syntax:" <+> ppr occ
    -- Use an OccName here because we don't want to print Prelude.(,)
  | otherwise
  = text "Cannot redefine a Name retrieved by a Template Haskell quote:"
    <+> ppr name
    -- This can happen when one tries to use a Template Haskell splice to
    -- define a top-level identifier with an already existing name, e.g.,
    --
    --   $(pure [ValD (VarP 'succ) (NormalB (ConE 'True)) []])
    --
    -- (See Trac #13968.)
  where
    occ = rdrNameOcc name
