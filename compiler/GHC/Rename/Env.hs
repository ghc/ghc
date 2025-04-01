{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections    #-}

{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-2006

GHC.Rename.Env contains functions which convert RdrNames into Names.

-}

module GHC.Rename.Env (
        newTopSrcBinder,

        lookupLocatedTopBndrRn, lookupLocatedTopBndrRnN, lookupTopBndrRn,
        lookupLocatedTopConstructorRn, lookupLocatedTopConstructorRnN,

        lookupLocatedOccRn, lookupLocatedOccRnConstr, lookupLocatedOccRnRecField,
        lookupLocatedOccRnNone,
        lookupOccRn, lookupOccRn_maybe, lookupSameOccRn_maybe,
        lookupLocalOccRn_maybe, lookupInfoOccRn,
        lookupLocalOccThLvl_maybe, lookupLocalOccRn,
        lookupTypeOccRn,
        lookupGlobalOccRn, lookupGlobalOccRn_maybe,

        lookupExprOccRn,
        lookupRecFieldOcc,
        lookupRecUpdFields,
        getFieldUpdLbl,
        getUpdFieldLbls,

        ChildLookupResult(..),
        lookupSubBndrOcc_helper,

        HsSigCtxt(..), lookupLocalTcNames, lookupSigOccRn,
        lookupSigCtxtOccRn,

        lookupInstDeclBndr, lookupFamInstName,
        lookupConstructorInfo, lookupConstructorFields,
        lookupGREInfo,

        irrefutableConLikeRn, irrefutableConLikeTc,

        lookupGreAvailRn,

        -- Rebindable Syntax
        lookupSyntax, lookupSyntaxExpr, lookupSyntaxNames,
        lookupSyntaxName,
        lookupIfThenElse,

        -- QualifiedDo
        lookupQualifiedDo, lookupQualifiedDoName, lookupNameWithQualifier,

        -- Constructing usage information
        DeprecationWarnings(..),
        addUsedGRE, addUsedGREs, addUsedDataCons,

        dataTcOccs, --TODO: Move this somewhere, into utils?

    ) where

import GHC.Prelude

import Language.Haskell.Syntax.Basic (FieldLabelString(..))

import GHC.Iface.Load
import GHC.Iface.Env
import GHC.Hs
import GHC.Types.Name.Reader
import GHC.Tc.Errors.Types
import GHC.Tc.Errors.Ppr (pprScopeError)
import GHC.Tc.Utils.Env
import GHC.Tc.Types.LclEnv
import GHC.Tc.Utils.Monad
import GHC.Parser.PostProcess ( setRdrNameSpace )
import GHC.Builtin.Types
import GHC.Types.Name
import GHC.Types.Name.Set
import GHC.Types.Name.Env
import GHC.Types.Avail
import GHC.Types.Hint
import GHC.Unit.Module
import GHC.Unit.Module.ModIface
import GHC.Core.ConLike
import GHC.Core.DataCon
import GHC.Core.TyCon
import GHC.Builtin.Names( rOOT_MAIN )
import GHC.Types.Basic  ( TopLevelFlag(..), TupleSort(..), tupleSortBoxity )
import GHC.Types.TyThing ( tyThingGREInfo )
import GHC.Types.SrcLoc as SrcLoc
import GHC.Utils.Outputable as Outputable
import GHC.Types.Unique.FM
import GHC.Types.Unique.DSet
import GHC.Types.Unique.Set
import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Data.Maybe
import GHC.Driver.Env
import GHC.Driver.Session
import GHC.Data.FastString
import GHC.Data.List.SetOps ( minusList )
import qualified GHC.LanguageExtensions as LangExt
import GHC.Rename.Unbound
import GHC.Rename.Utils
import GHC.Data.Bag
import GHC.Types.CompleteMatch
import GHC.Types.PkgQual
import GHC.Types.GREInfo

import Control.Arrow    ( first )
import Control.Monad
import Data.Either      ( partitionEithers )
import Data.Function    ( on )
import Data.List        ( find, partition, groupBy, sortBy )
import qualified Data.List.NonEmpty as NE
import qualified Data.Semigroup as Semi
import System.IO.Unsafe ( unsafePerformIO )

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
Note [Handling of deprecations] in GHC.Rename.Utils
-}

newTopSrcBinder :: LocatedN RdrName -> RnM Name
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
                  (addErrAt (locA loc) (TcRnBindingOfExistingName rdr_name))
         ; return name }
    else   -- See Note [Binders in Template Haskell] in "GHC.ThToHs"
      do { this_mod <- getModule
         ; externaliseName this_mod name }

  | Just (rdr_mod, rdr_occ) <- isOrig_maybe rdr_name
  = do  { this_mod <- getModule
        ; unless (rdr_mod == this_mod || rdr_mod == rOOT_MAIN)
                 (addErrAt (locA loc) (TcRnBindingOfExistingName rdr_name))
        -- When reading External Core we get Orig names as binders,
        -- but they should agree with the module gotten from the monad
        --
        -- We can get built-in syntax showing up here too, sadly.  If you type
        --      data T = (,,,)
        -- the constructor is parsed as a type, and then GHC.Parser.PostProcess.tyConToDataCon
        -- uses setRdrNameSpace to make it into a data constructors.  At that point
        -- the nice Exact name for the TyCon gets swizzled to an Orig name.
        -- Hence the TcRnBindingOfExistingName error message.
        --

        -- MP 2022: I suspect this code path is never called for `rOOT_MAIN` anymore
        -- because External Core has been removed but we instead have some similar logic for
        -- serialising whole programs into interface files in GHC.IfaceToCore.mk_top_id.

        -- Except for the ":Main.main = ..." definition inserted into
        -- the Main module; ugh!

        -- Because of this latter case, we call newGlobalBinder with a module from
        -- the RdrName, not from the environment.  In principle, it'd be fine to
        -- have an arbitrary mixture of external core definitions in a single module,
        -- (apart from module-initialisation issues, perhaps).
        ; newGlobalBinder rdr_mod rdr_occ (locA loc) }

  | otherwise
  = do  { when (isQual rdr_name)
                 (addErrAt (locA loc) (badQualBndrErr rdr_name))
                -- Binders should not be qualified; if they are, and with a different
                -- module name, we get a confusing "M.T is not in scope" error later

        ; stage <- getStage
        ; if isBrackStage stage then
                -- We are inside a TH bracket, so make an *Internal* name
                -- See Note [Top-level Names in Template Haskell decl quotes] in GHC.Rename.Names
             do { uniq <- newUnique
                ; return (mkInternalName uniq (rdrNameOcc rdr_name) (locA loc)) }
          else
             do { this_mod <- getModule
                ; traceRn "newTopSrcBinder" (ppr this_mod $$ ppr rdr_name $$ ppr (locA loc))
                ; newGlobalBinder this_mod (rdrNameOcc rdr_name) (locA loc) }
        }

{-
*********************************************************
*                                                      *
        Source code occurrences
*                                                      *
*********************************************************

Looking up a name in the GHC.Rename.Env.

Note [Type and class operator definitions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want to reject all of these unless we have -XTypeOperators (#3265)
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
lookupTopBndrRn :: WhatLooking -> RdrName -> RnM (WithUserRdr Name)
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
lookupTopBndrRn which_suggest rdr_name =
  lookupExactOrOrig rdr_name (WithUserRdr rdr_name . greName) $
    do  {  -- Check for operators in type or class declarations
           -- See Note [Type and class operator definitions]
          let occ = rdrNameOcc rdr_name
        ; when (isTcOcc occ && isSymOcc occ)
               (do { op_ok <- xoptM LangExt.TypeOperators
                   ; unless op_ok (addErr (TcRnIllegalTypeOperatorDecl rdr_name)) })
        ; env <- getGlobalRdrEnv
        ; WithUserRdr rdr_name <$>
          case filter isLocalGRE (lookupGRE env $ LookupRdrName rdr_name $ RelevantGREsFOS WantNormal) of
            [gre] -> return (greName gre)
            _     -> do -- Ambiguous (can't happen) or unbound
                        traceRn "lookupTopBndrRN fail" (ppr rdr_name)
                        unboundName (LF which_suggest WL_LocalTop) rdr_name
    }

lookupLocatedTopConstructorRn :: Located RdrName -> RnM (Located (WithUserRdr Name))
lookupLocatedTopConstructorRn = wrapLocM (lookupTopBndrRn WL_Constructor)

lookupLocatedTopConstructorRnN :: LocatedN RdrName -> RnM (LocatedN Name)
lookupLocatedTopConstructorRnN = wrapLocMA (fmap getName . lookupTopBndrRn WL_Constructor)

lookupLocatedTopBndrRn :: Located RdrName -> RnM (Located (WithUserRdr Name))
lookupLocatedTopBndrRn = wrapLocM (lookupTopBndrRn WL_Anything)

lookupLocatedTopBndrRnN :: LocatedN RdrName -> RnM (LocatedN Name)
lookupLocatedTopBndrRnN = wrapLocMA (fmap getName . lookupTopBndrRn WL_Anything)

-- | Lookup an @Exact@ @RdrName@. See Note [Looking up Exact RdrNames].
-- This never adds an error, but it may return one, see
-- Note [Errors in lookup functions]
lookupExactOcc_either :: Name -> RnM (Either NotInScopeError GlobalRdrElt)
lookupExactOcc_either name
  | Just thing <- wiredInNameTyThing_maybe name
  , Just (tycon, mkInfo)
      <- case thing of
          ATyCon tc ->
            Just (tc, IAmTyCon . TupleFlavour . tupleSortBoxity)
          AConLike (RealDataCon dc) ->
            let tc = dataConTyCon dc
            in Just (tc, IAmConLike . (\ _ -> mkConInfo (ConIsData $ map dataConName $ tyConDataCons tc) (dataConSourceArity dc) []))
          _ -> Nothing
  , Just tupleSort <- tyConTuple_maybe tycon
  = do { let tupArity = case tupleSort of
               -- Unboxed tuples have twice as many arguments because of the
               -- 'RuntimeRep's (#17837)
               UnboxedTuple -> tyConArity tycon `div` 2
               _ -> tyConArity tycon
       ; let info = mkInfo tupleSort
       ; checkTupSize tupArity
       ; return $ Right $ mkExactGRE name info }

  | isExternalName name
  = do { info <- lookupExternalExactName name
       ; return $ Right $ mkExactGRE name info }

  | otherwise
  = lookupLocalExactGRE name

lookupExternalExactName :: Name -> RnM GREInfo
lookupExternalExactName name
  = do { thing <-
           case wiredInNameTyThing_maybe name of
             Just thing -> return thing
             _          -> tcLookupGlobal name
       ; return $ tyThingGREInfo thing }

lookupLocalExactGRE :: Name -> RnM (Either NotInScopeError GlobalRdrElt)
lookupLocalExactGRE name
  = do { env <- getGlobalRdrEnv
       ; let lk = LookupExactName { lookupExactName = name
                                  , lookInAllNameSpaces = True }
             -- We want to check for clashes where the same Unique
             -- occurs in two different NameSpaces, as per
             -- Note [Template Haskell ambiguity]. So we
             -- check ALL namespaces, not just the NameSpace of the Name.
             -- See test cases T9066, T11809.
       ; case lookupGRE env lk of
           [gre] -> return (Right gre)

           []    -> -- See Note [Splicing Exact names]
                    do { lcl_env <- getLocalRdrEnv
                       ; let gre = mkLocalVanillaGRE NoParent name -- LocalRdrEnv only contains Vanilla things
                       ; if name `inLocalRdrEnvScope` lcl_env
                         then return (Right gre)
                         else
                         do { th_topnames_var <- fmap tcg_th_topnames getGblEnv
                            ; th_topnames <- readTcRef th_topnames_var
                            ; if name `elemNameSet` th_topnames
                              then return (Right gre)
                              else return (Left (NoExactName name))
                            }
                       }

           gres -> return (Left (SameName gres)) }
           -- Ugh!  See Note [Template Haskell ambiguity] }

-----------------------------------------------
lookupInstDeclBndr :: Name -> Subordinate -> RdrName -> RnM Name
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
lookupInstDeclBndr cls what_subordinate rdr
  = do { when (isQual rdr)
              (addErr (badQualBndrErr rdr))
                -- In an instance decl you aren't allowed
                -- to use a qualified name for the method
                -- (Although it'd make perfect sense.)
       ; mb_name <- lookupSubBndrOcc
                          NoDeprecationWarnings
                                -- we don't give deprecated
                                -- warnings when a deprecated class
                                -- method is defined. We only warn
                                -- when it's used
                          (ParentGRE cls (IAmTyCon ClassFlavour)) what_subordinate rdr
       ; case mb_name of
           Left err -> do { addErr $ TcRnNotInScope err rdr
                          ; return (mkUnboundNameRdr rdr) }
           Right nm -> return nm }

-----------------------------------------------
lookupFamInstName :: Maybe Name -> LocatedN RdrName
                  -> RnM (LocatedN Name)
-- Used for TyData and TySynonym family instances only,
-- See Note [Family instance binders]
lookupFamInstName (Just cls) tc_rdr  -- Associated type; c.f GHC.Rename.Bind.rnMethodBind
  = wrapLocMA (lookupInstDeclBndr cls AssociatedTypeOfClass) tc_rdr
lookupFamInstName Nothing tc_rdr     -- Family instance; tc_rdr is an *occurrence*
  = lookupLocatedOccRnConstr tc_rdr

-----------------------------------------------
lookupConstructorFields :: HasDebugCallStack => WithUserRdr Name -> RnM [FieldLabel]
lookupConstructorFields = fmap conInfoFields . lookupConstructorInfo

-- | Look up the arity and record fields of a constructor.
lookupConstructorInfo :: HasDebugCallStack => WithUserRdr Name -> RnM ConInfo
lookupConstructorInfo qcon@(WithUserRdr _ con_name)
  = do { info <- lookupGREInfo_GRE con_name
       ; case info of
            IAmConLike con_info -> return con_info
            UnboundGRE          -> return $ ConInfo (ConIsData []) ConHasPositionalArgs
            IAmTyCon {}         -> failIllegalTyCon WL_ConLike qcon
            _ -> pprPanic "lookupConstructorInfo: not a ConLike" $
                      vcat [ text "name:" <+> ppr con_name ]
       }

-- In CPS style as `RnM r` is monadic
-- Reports an error if the name is an Exact or Orig and it can't find the name
-- Otherwise if it is not an Exact or Orig, returns k
lookupExactOrOrig :: RdrName -> (GlobalRdrElt -> r) -> RnM r -> RnM r
lookupExactOrOrig rdr_name res k
  = do { men <- lookupExactOrOrig_base rdr_name
       ; case men of
          FoundExactOrOrig gre -> return $ res gre
          NotExactOrOrig       -> k
          ExactOrOrigError e   ->
            do { addErr $ TcRnNotInScope e rdr_name
               ; return $ res (mkUnboundGRERdr rdr_name) } }

-- Variant of 'lookupExactOrOrig' that does not report an error
-- See Note [Errors in lookup functions]
-- Calls k if the name is neither an Exact nor Orig
lookupExactOrOrig_maybe :: RdrName -> (Maybe GlobalRdrElt -> r) -> RnM r -> RnM r
lookupExactOrOrig_maybe rdr_name res k
  = do { men <- lookupExactOrOrig_base rdr_name
       ; case men of
           FoundExactOrOrig gre -> return (res (Just gre))
           ExactOrOrigError _   -> return (res Nothing)
           NotExactOrOrig       -> k }

data ExactOrOrigResult
  = FoundExactOrOrig GlobalRdrElt
    -- ^ Found an Exact Or Orig Name
  | ExactOrOrigError NotInScopeError
    -- ^ The RdrName was an Exact
     -- or Orig, but there was an
     -- error looking up the Name
  | NotExactOrOrig
    -- ^ The RdrName is neither an Exact nor Orig

-- Does the actual looking up an Exact or Orig name, see 'ExactOrOrigResult'
lookupExactOrOrig_base :: RdrName -> RnM ExactOrOrigResult
lookupExactOrOrig_base rdr_name
  | Just n <- isExact_maybe rdr_name   -- This happens in derived code
  = cvtEither <$> lookupExactOcc_either n
  | Just (rdr_mod, rdr_occ) <- isOrig_maybe rdr_name
  = do { nm <- lookupOrig rdr_mod rdr_occ

       ; this_mod <- getModule
       ; mb_gre <-
         if nameIsLocalOrFrom this_mod nm
         then lookupLocalExactGRE nm
         else do { info <- lookupExternalExactName nm
                 ; return $ Right $ mkExactGRE nm info }
       ; return $ case mb_gre of
          Left  err -> ExactOrOrigError err
          Right gre -> FoundExactOrOrig gre }
  | otherwise = return NotExactOrOrig
  where
    cvtEither (Left e)    = ExactOrOrigError e
    cvtEither (Right gre) = FoundExactOrOrig gre

{- Note [Errors in lookup functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Many of these lookup functions will attach an error if it can't find the Name it
is trying to lookup. However there are also _maybe and _either variants for many
of these functions.

These variants should *not* attach any errors, as there are
places where we want to attempt looking up a name, but it's not the end of the
world if we don't find it.

For example, see lookupThName_maybe: It calls lookupOccRn_maybe multiple
times for varying names in different namespaces. lookupOccRn_maybe should
therefore never attach an error, instead just return a Nothing.

For these _maybe/_either variant functions then, avoid calling further lookup
functions that can attach errors and instead call their _maybe/_either
counterparts.
-}

-----------------------------------------------
-- | Look up an occurrence of a field in record construction or pattern
-- matching (but not update).
--
-- If -XDisambiguateRecordFields is off, then we will pass 'Nothing' for the
-- 'DataCon' 'Name', i.e. we don't use the data constructor for disambiguation.
-- See Note [DisambiguateRecordFields] and Note [NoFieldSelectors].
lookupRecFieldOcc :: Maybe (WithUserRdr Name)
                       -- Nothing  => just look it up as usual
                       -- Just con => use data con to disambiguate
                  -> RdrName
                  -> RnM Name
lookupRecFieldOcc mb_con rdr_name
  | Just (WithUserRdr _ con) <- mb_con
  , isUnboundName con  -- Avoid error cascade
  = return $ mk_unbound_rec_fld con
  | Just qcon@(WithUserRdr _ con) <- mb_con
  = do { let lbl = FieldLabelString $ occNameFS (rdrNameOcc rdr_name)
       ; mb_nm <- lookupExactOrOrig rdr_name ensure_recfld $  -- See Note [Record field names and Template Haskell]
            do { flds <- lookupConstructorFields qcon
               ; env <- getGlobalRdrEnv
               ; let mb_gre = do fl <- find ((== lbl) . flLabel) flds
                                 -- We have the label, now check it is in scope.  If
                                 -- there is a qualifier, use pickGREs to check that
                                 -- the qualifier is correct, and return the filtered
                                 -- GRE so we get import usage right (see #17853).
                                 gre <- lookupGRE_FieldLabel env fl
                                 if isQual rdr_name
                                 then listToMaybe $ pickGREs rdr_name [gre]
                                 else return gre
               ; traceRn "lookupRecFieldOcc" $
                   vcat [ text "mb_con:" <+> ppr mb_con
                        , text "rdr_name:" <+> ppr rdr_name
                        , text "flds:" <+> ppr flds
                        , text "mb_gre:" <+> ppr mb_gre ]
               ; mapM_ (addUsedGRE AllDeprecationWarnings) mb_gre
               ; return $ flSelector . fieldGRELabel <$> mb_gre }
       ; case mb_nm of
          { Nothing  -> do { addErr (badFieldConErr con lbl)
                           ; return $ mk_unbound_rec_fld con }
          ; Just nm -> return nm } }

  | otherwise  -- Can't use the data constructor to disambiguate
  = lookupGlobalOccRn' WantField rdr_name
    -- This use of Global is right as we are looking up a selector,
    -- which can only be defined at the top level.

  where
    -- When lookup fails, make an unbound name with the right record field
    -- namespace, as that's what we expect to be returned
    -- from 'lookupRecFieldOcc'. See T14307.
    mk_unbound_rec_fld con = mkUnboundName $
      mkRecFieldOccFS (getOccFS con) (occNameFS occ)
    occ = rdrNameOcc rdr_name

    ensure_recfld :: GlobalRdrElt -> Maybe Name
    ensure_recfld gre = do { guard (isRecFldGRE gre)
                           ; return $ greName gre }

{- Note [DisambiguateRecordFields]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we are looking up record fields in record construction or pattern
matching, we can take advantage of the data constructor name to
resolve fields that would otherwise be ambiguous (provided the
-XDisambiguateRecordFields flag is on).

For example, consider:

   data S = MkS { x :: Int }
   data T = MkT { x :: Int }

   e = MkS { x = 3 }

When we are renaming the occurrence of `x` in `e`, instead of looking
`x` up directly (and finding both fields), lookupRecFieldOcc will
search the fields of `MkS` to find the only possible `x` the user can
mean.

Of course, we still have to check the field is in scope, using
lookupGRE_FieldLabel.  The handling of qualified imports is slightly
subtle: the occurrence may be unqualified even if the field is
imported only qualified (but if the occurrence is qualified, the
qualifier must be correct). For example:

   module A where
     data S = MkS { x :: Int }
     data T = MkT { x :: Int }

   module B where
     import qualified A (S(..))
     import A (T(MkT))

     e1 = MkT   { x = 3 }   -- x not in scope, so fail
     e2 = A.MkS { B.x = 3 } -- module qualifier is wrong, so fail
     e3 = A.MkS { x = 3 }   -- x in scope (lack of module qualifier permitted)

In case `e1`, lookupGRE_FieldLabel will return Nothing.  In case `e2`,
lookupGRE_FieldLabel will return the GRE for `A.x`, but then the guard
will fail because the field RdrName `B.x` is qualified and pickGREs
rejects the GRE.  In case `e3`, lookupGRE_FieldLabel will return the
GRE for `A.x` and the guard will succeed because the field RdrName `x`
is unqualified.


Note [DisambiguateRecordFields for updates]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we are looking up record fields in record update, we can take advantage of
the fact that we know we are looking for a field, even though we do not know the
data constructor name (as in Note [DisambiguateRecordFields]), provided the
-XDisambiguateRecordFields flag is on.

For example, consider:

  module N where
    f = ()

  {-# LANGUAGE DisambiguateRecordFields #-}
  module M where
    import N (f)
    data T = MkT { f :: Int }
    t = MkT { f = 1 }  -- unambiguous because MkT determines which field we mean
    u = t { f = 2 }    -- unambiguous because we ignore the non-field 'f'

We filter out non-fields in lookupFieldGREs by using isRecFldGRE, which allows
us to accept the above program.
Of course, if a record update has two fields in scope with the same name,
it is still ambiguous.

We also look up the non-fields with the same textual name

  1. to throw an error if the user hasn't enabled DisambiguateRecordFields,
  2. in order to improve the error message when a user mistakenly tries to use
     a non-field in a record update:

        f = ()
        e x = x { f = () }

Unlike with constructors or pattern-matching, we do not allow the module
qualifier to be omitted from the field names, because we do not have a
data constructor to use to determine the appropriate qualifier.

This is all done in the function lookupFieldGREs, which is called by
GHC.Rename.Pat.rnHsRecUpdFields, which deals with record updates.

Note [Record field names and Template Haskell]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (#12130):

   module Foo where
     import M
     b = $(funny)

   module M(funny) where
     data T = MkT { x :: Int }
     funny :: Q Exp
     funny = [| MkT { x = 3 } |]

When we splice, `MkT` is not lexically in scope, so
lookupGRE_FieldLabel will fail.  But there is no need for
disambiguation anyway, because `x` is an original name, and
lookupGlobalOccRn will find it.
-}

-- | Used in export lists to lookup the children.
lookupSubBndrOcc_helper :: Bool
                        -> DeprecationWarnings
                        -> ParentGRE     -- ^ parent
                        -> RdrName       -- ^ thing we are looking up
                        -> RnM ChildLookupResult
lookupSubBndrOcc_helper must_have_parent warn_if_deprec parent_gre rdr_name
  | isUnboundName (parentGRE_name parent_gre)
    -- Avoid an error cascade
  = return (FoundChild (mkUnboundGRERdr rdr_name))

  | otherwise = do
  gre_env <- getGlobalRdrEnv
  let original_gres = lookupGRE gre_env (LookupChildren parent_gre (rdrNameOcc rdr_name))
      picked_gres = pick_gres original_gres
  -- The remaining GREs are things that we *could* export here.
  -- Note that this includes things which have `NoParent`;
  -- those are sorted in `checkPatSynParent`.
  traceRn "parent" (ppr (parentGRE_name parent_gre))
  traceRn "lookupExportChild original_gres:" (ppr original_gres)
  traceRn "lookupExportChild picked_gres:" (ppr picked_gres $$ ppr must_have_parent)
  case picked_gres of
    NoOccurrence ->
      noMatchingParentErr original_gres
    UniqueOccurrence g ->
      if must_have_parent
      then noMatchingParentErr original_gres
      else checkFld g
    DisambiguatedOccurrence g ->
      checkFld g
    AmbiguousOccurrence gres ->
      mkNameClashErr gres
    where
        checkFld :: GlobalRdrElt -> RnM ChildLookupResult
        checkFld g = do
          addUsedGRE warn_if_deprec g
          return $ FoundChild g

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
          traceRn "npe" (ppr original_gres)
          dup_fields_ok <- xoptM LangExt.DuplicateRecordFields
          case original_gres of
            []  -> return NameNotFound
            [g] -> return $ IncorrectParent parent_gre g
                              [p | ParentIs p <- [greParent g]]
            gss@(g:gss'@(_:_)) ->
              if all isRecFldGRE gss && dup_fields_ok
              then return $
                    IncorrectParent parent_gre g
                      [p | x <- gss, ParentIs p <- [greParent x]]
              else mkNameClashErr $ g NE.:| gss'

        mkNameClashErr :: NE.NonEmpty GlobalRdrElt -> RnM ChildLookupResult
        mkNameClashErr gres = do
          addNameClashErrRn rdr_name gres
          return (FoundChild (NE.head gres))

        pick_gres :: [GlobalRdrElt] -> DisambigInfo
        pick_gres gres
          | isUnqual rdr_name
          -- The child is not qualified: find GREs that are in scope, whether
          -- qualified or unqualified, as per the Haskell 2010 report, 5.2.2:
          --
          --   - In all cases, the parent type constructor T must be in scope.
          --   - A subordinate name is legal if and only if:
          --      (a) it names a constructor or field of T, and
          --      (b) the constructor or field is in scope, regardless of whether
          --          it is in scope under a qualified or unqualified name.
          = mconcat (map right_parent gres)
          | otherwise
          -- The child is qualified: find GREs that are in scope
          -- with that qualification.
          = mconcat (map right_parent (pickGREs rdr_name gres))

        right_parent :: GlobalRdrElt -> DisambigInfo
        right_parent gre
          = case greParent gre of
              ParentIs cur_parent
                 | parentGRE_name parent_gre == cur_parent -> DisambiguatedOccurrence gre
                 | otherwise            -> NoOccurrence
              NoParent                  -> UniqueOccurrence gre

-- | This domain specific datatype is used to record why we decided it was
-- possible that a GRE could be exported with a parent.
data DisambigInfo
       = NoOccurrence
          -- ^ The GRE could not be found, or it has the wrong parent.
       | UniqueOccurrence GlobalRdrElt
          -- ^ The GRE has no parent. It could be a pattern synonym.
       | DisambiguatedOccurrence GlobalRdrElt
          -- ^ The parent of the GRE is the correct parent.
       | AmbiguousOccurrence (NE.NonEmpty GlobalRdrElt)
          -- ^ The GRE is ambiguous.
          --
          -- For example, two normal identifiers with the same name are in
          -- scope. They will both be resolved to "UniqueOccurrence" and the
          -- monoid will combine them to this failing case.

instance Outputable DisambigInfo where
  ppr NoOccurrence = text "NoOccurrence"
  ppr (UniqueOccurrence gre) = text "UniqueOccurrence:" <+> ppr gre
  ppr (DisambiguatedOccurrence gre) = text "DiambiguatedOccurrence:" <+> ppr gre
  ppr (AmbiguousOccurrence gres)    = text "Ambiguous:" <+> ppr gres

instance Semi.Semigroup DisambigInfo where
  -- These are the key lines: we prefer disambiguated occurrences to other
  -- names.
  _ <> DisambiguatedOccurrence g' = DisambiguatedOccurrence g'
  DisambiguatedOccurrence g' <> _ = DisambiguatedOccurrence g'

  NoOccurrence <> m = m
  m <> NoOccurrence = m
  UniqueOccurrence g <> UniqueOccurrence g'
    = AmbiguousOccurrence $ g NE.:| [g']
  UniqueOccurrence g <> AmbiguousOccurrence gs
    = AmbiguousOccurrence (g `NE.cons` gs)
  AmbiguousOccurrence gs <> UniqueOccurrence g'
    = AmbiguousOccurrence (g' `NE.cons` gs)
  AmbiguousOccurrence gs <> AmbiguousOccurrence gs'
    = AmbiguousOccurrence (gs Semi.<> gs')

instance Monoid DisambigInfo where
  mempty = NoOccurrence
  mappend = (Semi.<>)

-- Lookup SubBndrOcc can never be ambiguous
--
-- Records the result of looking up a child.
data ChildLookupResult
      -- | We couldn't find a suitable name
      = NameNotFound
      -- | The child has an incorrect parent
      | IncorrectParent ParentGRE    -- ^ parent
                        GlobalRdrElt -- ^ child we were looking for
                        [Name]       -- ^ list of possible parents
      -- | We resolved to a child
      | FoundChild GlobalRdrElt

instance Outputable ChildLookupResult where
  ppr NameNotFound = text "NameNotFound"
  ppr (FoundChild n) = text "Found:" <+> ppr (greParent n) <+> ppr n
  ppr (IncorrectParent parent g ns)
    = text "IncorrectParent"
      <+> hsep [ppr (parentGRE_name parent), ppr $ greName g, ppr ns]

lookupSubBndrOcc :: DeprecationWarnings
                 -> ParentGRE
                 -> Subordinate
                 -> RdrName
                 -> RnM (Either NotInScopeError Name)
-- ^ Find all the things the 'RdrName' maps to,
-- and pick the one with the right 'Parent' 'Name'.
lookupSubBndrOcc warn_if_deprec the_parent what_subordinate rdr_name =
  lookupExactOrOrig rdr_name (Right . greName) $
    -- This happens for built-in classes, see mod052 for example
    do { child <- lookupSubBndrOcc_helper True warn_if_deprec the_parent rdr_name
       ; return $ case child of
           FoundChild g       -> Right (greName g)
           NameNotFound       -> Left unknown_sub
           IncorrectParent {} -> Left unknown_sub }
       -- See [Mismatched class methods and associated type families]
       -- in TcInstDecls.
  where
    unknown_sub = UnknownSubordinate (parentGRE_name the_parent) what_subordinate

{- Note [Family instance binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
the enclosing instance decl, if any.

Note [Looking up Exact RdrNames]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Exact RdrNames are generated by:

* Template Haskell (See Note [Binders in Template Haskell] in GHC.ThToHs)
* Derived instances (See Note [Auxiliary binders] in GHC.Tc.Deriv.Generate)

For data types and classes have Exact system Names in the binding
positions for constructors, TyCons etc.  For example
    [d| data T = MkT Int |]
when we splice in and convert to HsSyn RdrName, we'll get
    data (Exact (system Name "T")) = (Exact (system Name "MkT")) ...
These System names are generated by GHC.ThToHs.thRdrName

But, constructors and the like need External Names, not System Names!
So we do the following

 * In GHC.Rename.Env.newTopSrcBinder we spot Exact RdrNames that wrap a
   non-External Name, and make an External name for it. This is
   the name that goes in the GlobalRdrEnv

 * When looking up an occurrence of an Exact name, done in
   GHC.Rename.Env.lookupExactOcc, we find the Name with the right unique in the
   GlobalRdrEnv, and use the one from the envt -- it will be an
   External Name in the case of the data type/constructor above.

 * Exact names are also use for purely local binders generated
   by TH, such as    \x_33. x_33
   Both binder and occurrence are Exact RdrNames.  The occurrence
   gets looked up in the LocalRdrEnv by GHC.Rename.Env.lookupOccRn, and
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
eg (#7241):
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


lookupLocatedOccRn :: GenLocated (EpAnn ann) RdrName
                   -> TcRn (GenLocated (EpAnn ann) Name)
lookupLocatedOccRn = wrapLocMA lookupOccRn

lookupLocatedOccRnConstr :: GenLocated (EpAnn ann) RdrName
                         -> TcRn (GenLocated (EpAnn ann) Name)
lookupLocatedOccRnConstr = wrapLocMA lookupOccRnConstr

lookupLocatedOccRnRecField :: GenLocated (EpAnn ann) RdrName
                           -> TcRn (GenLocated (EpAnn ann) Name)
lookupLocatedOccRnRecField = wrapLocMA lookupOccRnRecField

lookupLocatedOccRnNone :: GenLocated (EpAnn ann) RdrName
                       -> TcRn (GenLocated (EpAnn ann) Name)
lookupLocatedOccRnNone = wrapLocMA lookupOccRnNone

lookupLocalOccRn_maybe :: RdrName -> RnM (Maybe Name)
-- Just look in the local environment
lookupLocalOccRn_maybe rdr_name
  = do { local_env <- getLocalRdrEnv
       ; return (lookupLocalRdrEnv local_env rdr_name) }

lookupLocalOccThLvl_maybe :: Name -> RnM (Maybe (TopLevelFlag, ThLevel))
-- Just look in the local environment
lookupLocalOccThLvl_maybe name
  = do { lcl_env <- getLclEnv
       ; return (lookupNameEnv (getLclEnvThBndrs lcl_env) name) }

-- lookupOccRn' looks up an occurrence of a RdrName, and uses its argument to
-- determine what kind of suggestions should be displayed if it is not in scope
lookupOccRn' :: WhatLooking -> RdrName -> RnM Name
lookupOccRn' which_suggest rdr_name
  = do { mb_gre <- lookupOccRn_maybe rdr_name
       ; case mb_gre of
           Just gre  -> return $ greName gre
           Nothing   -> reportUnboundName' which_suggest rdr_name }

-- lookupOccRn looks up an occurrence of a RdrName and displays suggestions if
-- it is not in scope
lookupOccRn :: RdrName -> RnM Name
lookupOccRn = lookupOccRn' WL_Anything

-- | Look up an occurrence of a 'RdrName'.
--
-- Displays constructors and pattern synonyms as suggestions if
-- it is not in scope.
--
-- See Note [lookupOccRnConstr]
lookupOccRnConstr :: RdrName -> RnM Name
lookupOccRnConstr rdr_name
  = do { mb_gre <- lookupOccRn_maybe rdr_name
       ; case mb_gre of
           Just gre  -> return $ greName gre
           Nothing   -> do
            { mb_ty_gre <- lookup_promoted rdr_name
            ; case mb_ty_gre of
              Just gre -> return $ greName gre
              Nothing ->
                reportUnboundName'
                  WL_Constructor
                    -- not WL_ConLike, due to the type-level fallback
                    -- described in Note [lookupOccRnConstr]
                  rdr_name
            } }

{- Note [lookupOccRnConstr]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
lookupOccRnConstr looks up a data constructor or pattern synonym. Simple.

However, there is a fallback to the type level when the lookup fails.
This is required to implement a pat-to-type transformation
(See Note [Pattern to type (P2T) conversion] in GHC.Tc.Gen.Pat)

Consider this example:

  data VisProxy a where VP :: forall a -> VisProxy a

  f :: VisProxy Int -> ()
  f (VP Int) = ()

Here `Int` is actually a type, but it occurs in a position in which we expect
a data constructor.

In all other cases we just use this additional lookup for better
error messaging (See Note [Promotion]).
-}

-- lookupOccRnRecField looks up an occurrence of a RdrName and displays
-- record fields as suggestions if it is not in scope
lookupOccRnRecField :: RdrName -> RnM Name
lookupOccRnRecField = lookupOccRn' WL_RecField

-- lookupOccRnRecField looks up an occurrence of a RdrName and displays
-- no suggestions if it is not in scope
lookupOccRnNone :: RdrName -> RnM Name
lookupOccRnNone = lookupOccRn' WL_None

-- Only used in one place, to rename pattern synonym binders.
-- See Note [Renaming pattern synonym variables] in GHC.Rename.Bind
lookupLocalOccRn :: RdrName -> RnM Name
lookupLocalOccRn rdr_name
  = do { mb_name <- lookupLocalOccRn_maybe rdr_name
       ; case mb_name of
           Just name -> return name
           Nothing   -> unboundName (LF WL_Anything WL_LocalOnly) rdr_name }

-- lookupTypeOccRn looks up an optionally promoted RdrName.
-- Used for looking up type variables.
lookupTypeOccRn :: RdrName -> RnM Name
-- see Note [Demotion]
lookupTypeOccRn rdr_name
  = do { mb_gre <- lookupOccRn_maybe rdr_name
       ; case mb_gre of
             Just gre -> return $ greName gre
             Nothing   ->
               if occName rdr_name == occName eqTyCon_RDR -- See Note [eqTyCon (~) compatibility fallback]
               then eqTyConName <$ addDiagnostic TcRnTypeEqualityOutOfScope
               else lookup_demoted rdr_name }

{- Note [eqTyCon (~) compatibility fallback]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Before GHC Proposal #371, the (~) type operator used in type equality
constraints (a~b) was considered built-in syntax.

This had two implications:

1. Users could use it without importing it from Data.Type.Equality or Prelude.
2. TypeOperators were not required to use it (it was guarded behind TypeFamilies/GADTs instead)

To ease migration and minimize breakage, we continue to support those usages
but emit appropriate warnings.
-}

-- Used when looking up a term name (varName or dataName) in a type
lookup_demoted :: RdrName -> RnM Name
lookup_demoted rdr_name
  | Just demoted_rdr <- demoteRdrNameTcCls rdr_name
    -- Maybe it's the name of a *data* constructor
  = do { data_kinds <- xoptM LangExt.DataKinds
       ; star_is_type <- xoptM LangExt.StarIsType
       ; let is_star_type = if star_is_type then StarIsType else StarIsNotType
             star_is_type_hints = noStarIsTypeHints is_star_type rdr_name
       ; if data_kinds
            then do { mb_demoted_gre <- lookupOccRn_maybe demoted_rdr
                    ; case mb_demoted_gre of
                        Nothing -> unboundNameX looking_for rdr_name star_is_type_hints
                        Just demoted_gre -> return $ greName demoted_gre}
            else do { -- We need to check if a data constructor of this name is
                      -- in scope to give good error messages. However, we do
                      -- not want to give an additional error if the data
                      -- constructor happens to be out of scope! See #13947.
                      mb_demoted_name <- discardErrs $
                                         lookupOccRn_maybe demoted_rdr
                    ; let suggestion | isJust mb_demoted_name
                                     , let additional = text "to refer to the data constructor of that name?"
                                     = [SuggestExtension $ SuggestSingleExtension additional LangExt.DataKinds]
                                     | otherwise
                                     = star_is_type_hints
                    ; unboundNameX looking_for rdr_name suggestion } }

  | isQual rdr_name,
    Just demoted_rdr_name <- demoteRdrNameTv rdr_name
    -- Definitely an illegal term variable, as type variables are never exported.
    -- See Note [Demotion of unqualified variables] (W2)
  = report_qualified_term_in_types rdr_name demoted_rdr_name

  | isUnqual rdr_name,
    Just demoted_rdr_name <- demoteRdrNameTv rdr_name
    -- See Note [Demotion of unqualified variables]
  = do { required_type_arguments <- xoptM LangExt.RequiredTypeArguments
       ; if required_type_arguments
         then do { mb_demoted_gre <- lookupOccRn_maybe demoted_rdr_name
                 ; case mb_demoted_gre of
                     Nothing -> unboundName (LF WL_Anything WL_Anywhere) rdr_name
                     Just demoted_gre -> return $ greName demoted_gre }
         else unboundName looking_for rdr_name }

  | otherwise
  = unboundName looking_for rdr_name

  where
    looking_for = LF WL_Constructor WL_Anywhere

{- Note [Demotion of unqualified variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Under RequiredTypeArguments, a term-level variable name (i.e. a name whose
`occNameSpace` is `varName` as opposed to `tvName`) does not necessarily denote
a term variable. It can actually stand for a type:

  {-# LANGUAGE RequiredTypeArguments #-}
  idv :: forall a -> a -> a     -- Note the "forall a ->" in the type
  idv  t  (x :: t) = id @t x    -- #23739
  --   ^        ^        ^
  --  varName  tvName  tvName   -- NameSpace (GHC.Types.Name.Occurrence)

The variable `t` is an alias for the type variable `a`, so it's valid to use it
in type-level contexts. The only problem is that the namespaces do not match.
Demotion allows us to connect the `tvName` usages to the `varName` binding.

Demotion of an RdrName means that we change its namespace from tvName/tcClsName
to varName/dataName. Suppose we are looking up an occurrence of a variable `a`
in a type (in `lookupTypeOccRn`). The parser gave `a` a `tvName` occurrence,
so we try looking that up first.  If that fails, and RequiredTypeArguments is
on, then "demote" it to the `varName` namespace with `demoteRdrNameTv` and look
that up instead. If that succeeds, use it.

(W1) Wrinkle 1
  As a side effect of demotion, the renamer accepts all these examples:
    t = True         -- Ordinary term-level binding
    x = Proxy @t     -- (1) Bad usage in a HsExpr
    type T = t       -- (2) Bad usage in a TyClDecl
    f :: t -> t      -- (3) Bad usage in a SigDecl

  However, GHC doesn't promote arbitrary terms to types. See the "T2T-Mapping"
  section of GHC Proposal #281: "In the type checking environment, the variable
  must stand for a type variable". Even though the renamer accepts these
  constructs, the type checker has to reject the uses of `t` shown above.

  All three examples are rejected with the `TermVariablePE` promotion error.
  The error is generated by `tcTyVar` (GHC.Tc.Gen.HsType)
      tcTyVar :: Name -> TcM (TcType, TcKind)
  The first thing `tcTyVar` does is call the `tcLookup` helper (GHC.Tc.Utils.Env)
  to find the variable in the type checking environment
      tcLookup :: Name -> TcM TcTyThing
  What happens next depends on the example in question.

  * In the HsExpr example (1), `tcLookup` finds `ATcId` that corresponds to
    the `t = True` binding. The `ATcId` is then then turned into an error by
    the following clause in `tcTyVar`:
       ATcId{} -> promotionErr name TermVariablePE

  * In the TyClDecl example (2) and the SigDecl example (3), we don't have
    `ATcId` in the environment just yet because type declarations and signatures
    are type-checked /before/ term-level bindings.

    This means that `tcLookup` fails to find `t` in the local environment and
    calls `tcLookupGlobal` (GHC.Tc.Utils.Env)
        tcLookupGlobal :: Name -> TcM TyThing

    The global environment does not contain `t` either, so `tcLookupGlobal`
    calls `notFound` (GHC.Tc.Utils.Env)
        notFound :: Name -> TcM TyThing

    At this point GHC would normally generate a panic: if the variable is
    neither in the local nor in the global environment, then it shouldn't have
    passed the renamer. Unfortunately, this expectation is tiresome and
    expensive to maintain, so we add a special case in `notFound` instead.
    If the namespace of the variable is `varName`, the only explanation other
    than a bug in GHC is that the user tried to use a term variable in a type
    context. Hence the following clause in `notFound`:
      _ | isTermVarOrFieldNameSpace (nameNameSpace name) ->
          failWithTc $ TcRnUnpromotableThing name TermVariablePE

(W2) Wrinkle 2
   Only unqualified variable names are demoted, e.g. `f` but not `M.f`.
   The reason is that type variables are never bound to a qualified name:
   they can't be bound at the top level of a module, nor can they be
   exported or imported, so a qualified occurrence `M.f` must refer to a
   term-level definition and is never legal at the type level.
   Demotion of qualified names would not allow us to accept any new programs.
   We use this fact to generate better suggestions in error messages,
   see `report_qualified_term_in_types`.
-}

-- Report a qualified variable name in a type signature:
--   badSig :: Prelude.head
--             ^^^^^^^^^^^
report_qualified_term_in_types :: RdrName -> RdrName -> RnM Name
report_qualified_term_in_types rdr_name demoted_rdr_name =
  do { mName <- lookupGlobalOccRn_maybe (RelevantGREsFOS WantNormal) demoted_rdr_name
     ; case mName of
         (Just _) -> termNameInType looking_for rdr_name demoted_rdr_name []
         Nothing -> unboundTermNameInTypes looking_for rdr_name demoted_rdr_name }
  where
    looking_for = LF WL_Constructor WL_Global

-- If the given RdrName can be promoted to the type level and its promoted variant is in scope,
-- lookup_promoted returns the corresponding type-level Name.
-- Otherwise, the function returns Nothing.
-- See Note [Promotion] below.
lookup_promoted :: RdrName -> RnM (Maybe GlobalRdrElt)
lookup_promoted rdr_name
  | Just promoted_rdr <- promoteRdrName rdr_name
  = lookupOccRn_maybe promoted_rdr
  | otherwise
  = return Nothing

{- Note [Demotion]
~~~~~~~~~~~~~~~~~~
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

Another case of demotion happens when the user tries to
use a qualified term at the type level:

  f :: Prelude.id -> Int

This signature passes the parser to be caught by the renamer.
It allows the compiler to create more informative error messages.

'Prelude.id' in the type signature is parsed as
  HsTyVar ("id", TvName)

To separate the case of a typo from the case of an
intentional attempt to use an imported term's name the compiler demotes
the namespace to VarName (using 'demoteTvNameSpace') and does a lookup.

The same type of demotion happens when the compiler needs to check
if a name of a type variable has already been used for a term that is in scope.
We need to do it to check if a user should change the name
to make his code compatible with the RequiredTypeArguments extension.

Note [Promotion]
~~~~~~~~~~~~~~~
When the user mentions a type constructor or a type variable in a
term-level context, then we report that a value identifier was expected
instead of a type-level one. That makes error messages more precise.
Previously, such errors contained only the info that a given value was out of scope (#18740).
We promote the namespace of RdrName and look up after that
(see the functions promotedRdrName and lookup_promoted).

In particular, we have the following error message
  • Illegal term-level use of the type constructor ‘Int'
      imported from ‘Prelude' (and originally defined in ‘GHC.Types')
  • In the first argument of ‘id'
    In the expression: id Int
    In an equation for ‘x': x = id Int

when the user writes the following declaration

  x = id Int
-}

lookupOccRnX_maybe :: (RdrName -> RnM (Maybe r)) -> (GlobalRdrElt -> RnM r) -> RdrName
                   -> RnM (Maybe r)
lookupOccRnX_maybe globalLookup wrapper rdr_name
  = runMaybeT . msum . map MaybeT $
      [ do { res <- lookupLocalOccRn_maybe rdr_name
           ; case res of
           { Nothing -> return Nothing
           ; Just nm ->
           -- Elements in the LocalRdrEnv are always Vanilla GREs
        do { let gre = mkLocalVanillaGRE NoParent nm
           ; Just <$> wrapper gre } } }
      , globalLookup rdr_name ]

lookupOccRn_maybe :: RdrName -> RnM (Maybe GlobalRdrElt)
lookupOccRn_maybe =
  lookupOccRnX_maybe
    (lookupGlobalOccRn_maybe $ RelevantGREsFOS WantNormal)
    return

-- Used outside this module only by TH name reification (lookupName, lookupThName_maybe)
lookupSameOccRn_maybe :: RdrName -> RnM (Maybe Name)
lookupSameOccRn_maybe =
  lookupOccRnX_maybe
    (get_name <$> lookupGlobalOccRn_maybe SameNameSpace)
    (return . greName)
  where
    get_name :: RnM (Maybe GlobalRdrElt) -> RnM (Maybe Name)
    get_name = fmap (fmap greName)

-- | Look up a 'RdrName' used as a variable in an expression.
--
-- This may be a local variable, global variable, or one or more record selector
-- functions.  It will not return record fields created with the
-- @NoFieldSelectors@ extension (see Note [NoFieldSelectors]).
--
-- If the name is not in scope at the term level, but its promoted equivalent is
-- in scope at the type level, the lookup will succeed (so that the type-checker
-- can report a more informative error later).  See Note [Promotion].
--
lookupExprOccRn :: RdrName -> RnM (Maybe GlobalRdrElt)
lookupExprOccRn rdr_name
  = do { mb_name <- lookupOccRnX_maybe
                      lookupGlobalOccRn_overloaded
                      return
                      rdr_name
       ; case mb_name of
           Nothing   -> lookup_promoted rdr_name
                        -- See Note [Promotion].
                        -- We try looking up the name as a
                        -- type constructor or type variable, if
                        -- we failed to look up the name at the term level.
           p         -> return p }

lookupGlobalOccRn_maybe :: WhichGREs GREInfo -> RdrName -> RnM (Maybe GlobalRdrElt)
-- Looks up a RdrName occurrence in the top-level
-- environment, including using lookupQualifiedNameGHCi
-- for the GHCi case, but first tries to find an Exact or Orig name.
-- No filter function; does not report an error on failure
-- See Note [Errors in lookup functions]
-- Uses addUsedRdrName to record use and deprecations
--
-- Used directly only by getLocalNonValBinders (new_assoc).
lookupGlobalOccRn_maybe which_gres rdr_name =
  lookupExactOrOrig_maybe rdr_name id $
    lookupGlobalOccRn_base which_gres rdr_name

lookupGlobalOccRn :: RdrName -> RnM Name
-- lookupGlobalOccRn is like lookupOccRn, except that it looks in the global
-- environment.  Adds an error message if the RdrName is not in scope.
-- You usually want to use "lookupOccRn" which also looks in the local
-- environment.
--
-- Used by exports_from_avail
lookupGlobalOccRn = lookupGlobalOccRn' WantNormal

lookupGlobalOccRn' :: FieldsOrSelectors -> RdrName -> RnM Name
lookupGlobalOccRn' fos rdr_name
  = lookupExactOrOrig rdr_name greName $
    do { mb_gre <- lookupGlobalOccRn_base which_gres rdr_name
       ; case mb_gre of
           Just gre -> return (greName gre)
           Nothing  -> do { traceRn "lookupGlobalOccRn" (ppr rdr_name)
                          ; unboundName looking_for rdr_name } }
  where
    which_gres   = RelevantGREsFOS fos
    looking_for  = LF { lf_which = what_looking, lf_where =  WL_Global }
    what_looking = case fos of
                      WantBoth   -> WL_RecField
                      WantField  -> WL_RecField
                      WantNormal -> WL_Anything

-- Looks up a RdrName occurrence in the GlobalRdrEnv and with
-- lookupQualifiedNameGHCi. Does not try to find an Exact or Orig name first.
-- lookupQualifiedNameGHCi here is used when we're in GHCi and a name like
-- 'Data.Map.elems' is typed, even if you didn't import Data.Map
lookupGlobalOccRn_base :: WhichGREs GREInfo -> RdrName -> RnM (Maybe GlobalRdrElt)
lookupGlobalOccRn_base which_gres rdr_name =
    runMaybeT . msum . map MaybeT $
    [ lookupGreRn_maybe which_gres rdr_name
    , lookupOneQualifiedNameGHCi fos rdr_name ]
                      -- This test is not expensive,
                      -- and only happens for failed lookups
  where
    fos = case which_gres of
      RelevantGREs { includeFieldSelectors = sel } -> sel
      _ -> if isFieldOcc (rdrNameOcc rdr_name)
           then WantField
           else WantNormal

-- | Lookup a 'Name' in the 'GlobalRdrEnv', falling back to looking up
-- in the type environment it if fails.
lookupGREInfo_GRE :: HasDebugCallStack => Name -> RnM GREInfo
lookupGREInfo_GRE name
  = do { rdr_env <- getGlobalRdrEnv
       ; case lookupGRE_Name rdr_env name of
          Just ( GRE { gre_info = info } )
            -> return info
          _ -> do { hsc_env <- getTopEnv
                  ; return $ lookupGREInfo hsc_env name } }
  -- Just looking in the GlobalRdrEnv is insufficient, as we also
  -- need to handle qualified imports in GHCi; see e.g. T9815ghci.

lookupInfoOccRn :: RdrName -> RnM [Name]
-- ^ lookupInfoOccRn is intended for use in GHCi's ":info" command
-- It finds all the GREs that RdrName could mean, not complaining
-- about ambiguity, but rather returning them all (c.f. #9881).
--
-- lookupInfoOccRn is also used in situations where we check for
-- at least one definition of the RdrName, not complaining about
-- multiple definitions (see #17832).
lookupInfoOccRn rdr_name =
  lookupExactOrOrig rdr_name (\ gre -> [greName gre]) $
    do { rdr_env <- getGlobalRdrEnv
       ; let nms = map greName $ lookupGRE rdr_env (LookupRdrName rdr_name (RelevantGREsFOS WantBoth))
       ; qual_nms <- map greName <$> lookupQualifiedNameGHCi WantBoth rdr_name
       ; return $ nms ++ (qual_nms `minusList` nms) }

-- | Look up all record field names, available in the 'GlobalRdrEnv',
-- that a given 'RdrName' might refer to.
-- (Also includes implicit qualified imports in GHCi).
--
-- Throws an error if no fields are found.
--
-- See Note [DisambiguateRecordFields for updates].
lookupFieldGREs :: GlobalRdrEnv -> LocatedN RdrName -> RnM (NE.NonEmpty FieldGlobalRdrElt)
lookupFieldGREs env (L loc rdr)
  = setSrcSpanA loc
  $ do { res <- lookupExactOrOrig rdr (\ gre -> maybeToList $ fieldGRE_maybe gre) $
           do { let (env_fld_gres, env_var_gres) =
                      partition isRecFldGRE $
                      lookupGRE env (LookupRdrName rdr (RelevantGREsFOS WantBoth))

              -- Handle implicit qualified imports in GHCi. See T10439.
              ; ghci_gres <- lookupQualifiedNameGHCi WantBoth rdr
              ; let (ghci_fld_gres, ghci_var_gres) =
                      partition isRecFldGRE $
                      ghci_gres

              ; let fld_gres = ghci_fld_gres ++ env_fld_gres
                    var_gres = ghci_var_gres ++ env_var_gres

              -- Add an error for ambiguity when -XDisambiguateRecordFields is off.
              --
              -- See Note [DisambiguateRecordFields for updates].
              ; disamb_ok <- xoptM LangExt.DisambiguateRecordFields
              ;  if | not disamb_ok
                    , gre1 : gre2 : others <- fld_gres ++ var_gres
                    -> addErrTc $ TcRnAmbiguousFieldInUpdate (gre1, gre2, others)
                    | otherwise
                    -> return ()
              ; return fld_gres }

       -- Add an error if lookup failed.
       ; case res of
          { gre : gres -> return $ gre NE.:| gres
          ; [] ->
    do { show_helpful_errors <- goptM Opt_HelpfulErrors
       ; (imp_errs, hints) <-
           if show_helpful_errors
           then unknownNameSuggestions emptyLocalRdrEnv WL_RecField rdr
           else return ([], [])
       ; err_msg <- unknownNameSuggestionsMessage (TcRnNotInScope NotARecordField rdr) imp_errs hints
       ; failWithTc err_msg
       } } }

-- | Look up a 'RdrName', which might refer to an overloaded record field.
--
-- Don't allow any ambiguity: emit a name-clash error if there are multiple
-- matching GREs.
lookupGlobalOccRn_overloaded :: RdrName -> RnM (Maybe GlobalRdrElt)
lookupGlobalOccRn_overloaded rdr_name =
  lookupExactOrOrig_maybe rdr_name id $
    do { res <- lookupGreRn_helper (RelevantGREsFOS WantNormal) rdr_name AllDeprecationWarnings
       ; case res of
           GreNotFound        -> lookupOneQualifiedNameGHCi WantNormal rdr_name
           OneNameMatch gre   -> return $ Just gre
           MultipleNames gres@(gre NE.:| _) -> do
              addNameClashErrRn rdr_name gres
              return (Just gre) }

getFieldUpdLbl :: IsPass p => LHsRecUpdField (GhcPass p) q -> LocatedN RdrName
getFieldUpdLbl = fieldOccLRdrName . unLoc . hfbLHS . unLoc

-- | Returns all possible collections of field labels for the given
-- record update.
--
--   Example:
--
--       data D = MkD { fld1 :: Int, fld2 :: Bool }
--       data E = MkE1 { fld1 :: Int, fld2 :: Bool, fld3 :: Char }
--              | MkE2 { fld1 :: Int, fld2 :: Bool }
--       data F = MkF1 { fld1 :: Int } | MkF2 { fld2 :: Bool }
--
--       f r = r { fld1 = a, fld2 = b }
--
--     This function will return:
--
--       [ [ D.fld1, D.fld2 ] -- could be a record update at type D
--       , [ E.fld1, E.fld2 ] -- could be a record update at type E
--       ] -- cannot be a record update at type F: no constructor has both
--         -- of the fields fld1 and fld2
--
-- If there are no valid parents for the record update,
-- throws a 'TcRnBadRecordUpdate' error.
lookupRecUpdFields :: NE.NonEmpty (LHsRecUpdField GhcPs GhcPs)
                   -> RnM (NE.NonEmpty (HsRecUpdParent GhcRn))
lookupRecUpdFields flds
-- See Note [Disambiguating record updates] in GHC.Rename.Pat.
  = do { -- Retrieve the possible GlobalRdrElts that each field could refer to.
       ; gre_env <- getGlobalRdrEnv
       ; fld1_gres NE.:| other_flds_gres <- mapM (lookupFieldGREs gre_env . getFieldUpdLbl) flds
         -- Take an intersection: we are only interested in constructors
         -- which have all of the fields.
       ; let possible_GREs = intersect_by_cons fld1_gres other_flds_gres

       ; traceRn "lookupRecUpdFields" $
           vcat [ text "flds:" <+> ppr (fmap getFieldUpdLbl flds)
                , text "possible_GREs:" <+>
                    ppr (map (fmap greName . rnRecUpdLabels) possible_GREs) ]

       ; case possible_GREs of

          -- There is at least one parent: we can proceed.
          -- The typechecker might be able to finish disambiguating.
          -- See Note [Type-directed record disambiguation] in GHC.Rename.Pat.
       { p1:ps -> return (p1 NE.:| ps)

          -- There are no possible parents for the record update: compute
          -- a minimum set of fields which does not belong to any data constructor,
          -- to report an informative error to the user.
       ; _ ->
          let
            -- The constructors which have the first field.
            fld1_cons :: UniqSet ConLikeName
            fld1_cons = unionManyUniqSets
                      $ NE.toList
                      $ NE.map (recFieldCons . fieldGREInfo) fld1_gres
            -- The field labels of the constructors which have the first field.
            fld1_cons_fields :: UniqFM ConLikeName [FieldLabel]
            fld1_cons_fields
              = fmap (lkp_con_fields gre_env)
              $ getUniqSet fld1_cons
          in failWithTc $ badFieldsUpd (NE.toList flds) fld1_cons_fields } }

  where
    intersect_by_cons :: NE.NonEmpty FieldGlobalRdrElt
                      -> [NE.NonEmpty FieldGlobalRdrElt]
                      -> [HsRecUpdParent GhcRn]
    intersect_by_cons this [] =
      map
        (\ fld -> RnRecUpdParent (fld NE.:| []) (recFieldCons (fieldGREInfo fld)))
        (NE.toList this)
    intersect_by_cons this (new : rest) =
      [ RnRecUpdParent (this_fld NE.<| next_flds) both_cons
      | this_fld <- NE.toList this
      , let this_cons = recFieldCons $ fieldGREInfo this_fld
      , RnRecUpdParent next_flds next_cons <- intersect_by_cons new rest
      , let both_cons = next_cons `intersectUniqSets` this_cons
      , not $ isEmptyUniqSet both_cons
      ]

    lkp_con_fields :: GlobalRdrEnv -> ConLikeName -> [FieldLabel]
    lkp_con_fields gre_env con =
      [ fl
      | let nm = conLikeName_Name con
      , gre      <- maybeToList $ lookupGRE_Name gre_env nm
      , con_info <- maybeToList $ recFieldConLike_maybe gre
      , fl       <- conInfoFields con_info ]

{-**********************************************************************
*                                                                      *
                      Record field errors
*                                                                      *
**********************************************************************-}

getUpdFieldLbls :: forall p q. IsPass p
                => [LHsRecUpdField (GhcPass p) q] -> [RdrName]
getUpdFieldLbls
  = map $ fieldOccRdrName
        . unXRec @(GhcPass p)
        . hfbLHS
        . unXRec @(GhcPass p)

-- | Create an error message when there is no single 'ConLike' which
-- has all of the required fields for a record update.
--
-- This boils down the problem to a smaller set of fields, to avoid
-- the error message containing a lot of uninformative field names that
-- aren't really relevant to the problem.
--
-- NB: this error message should only be triggered when all the field names
-- are in scope (i.e. each individual field name does belong to some
-- constructor in scope).
badFieldsUpd
  :: (OutputableBndrId p)
  => [LHsRecUpdField (GhcPass p) q]
               -- ^ Field names that don't belong to a single datacon
  -> UniqFM ConLikeName [FieldLabel]
      -- ^ The list of field labels for each constructor.
      -- (These are the constructors in which the first field occurs.)
  -> TcRnMessage
badFieldsUpd rbinds fld1_cons_fields
  = TcRnBadRecordUpdate
      (getUpdFieldLbls rbinds)
      (NoConstructorHasAllFields conflictingFields)
          -- See Note [Finding the conflicting fields]
  where
    -- A (preferably small) set of fields such that no constructor contains
    -- all of them.  See Note [Finding the conflicting fields]
    conflictingFields = case nonMembers of
        -- nonMember belongs to a different type.
        (nonMember, _) : _ -> [aMember, nonMember]
        [] -> let
            -- All of rbinds belong to one type. In this case, repeatedly add
            -- a field to the set until no constructor contains the set.

            -- Each field, together with a list indicating which constructors
            -- have all the fields so far.
            growingSets :: [(FieldLabelString, [Bool])]
            growingSets = scanl1 combine membership
            combine (_, setMem) (field, fldMem)
              = (field, zipWith (&&) setMem fldMem)
            in
            -- Fields that don't change the membership status of the set
            -- are redundant and can be dropped.
            map (fst . head) $ groupBy ((==) `on` snd) growingSets

    aMember = assert (not (null members) ) fst (head members)
    (members, nonMembers) = partition (or . snd) membership

    -- For each field, which constructors contain the field?
    membership :: [(FieldLabelString, [Bool])]
    membership
      = sortMembership $
        map
          ( (\fld -> (fld, map (fld `elementOfUniqSet`) fieldLabelSets))
          . FieldLabelString . occNameFS . rdrNameOcc . unLoc . getFieldUpdLbl )
          rbinds

    fieldLabelSets :: [UniqSet FieldLabelString]
    fieldLabelSets = map (mkUniqSet . map flLabel) $ nonDetEltsUFM fld1_cons_fields

    -- Sort in order of increasing number of True, so that a smaller
    -- conflicting set can be found.
    sortMembership =
      map snd .
      sortBy (compare `on` fst) .
      map (\ item@(_, membershipRow) -> (countTrue membershipRow, item))

    countTrue = count id

{-
Note [Finding the conflicting fields]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have
  data A = A {a0, a1 :: Int}
         | B {b0, b1 :: Int}
and we see a record update
  x { a0 = 3, a1 = 2, b0 = 4, b1 = 5 }
Then we'd like to find the smallest subset of fields that no
constructor has all of.  Here, say, {a0,b0}, or {a0,b1}, etc.
We don't really want to report that no constructor has all of
{a0,a1,b0,b1}, because when there are hundreds of fields it's
hard to see what was really wrong.

We may need more than two fields, though; eg
  data T = A { x,y :: Int, v::Int }
         | B { y,z :: Int, v::Int }
         | C { z,x :: Int, v::Int }
with update
   r { x=e1, y=e2, z=e3 }, we

Finding the smallest subset is hard, so the code here makes
a decent stab, no more.  See #7989.
-}

--------------------------------------------------
--      Lookup in the Global RdrEnv of the module
--------------------------------------------------

data GreLookupResult = GreNotFound
                     | OneNameMatch GlobalRdrElt
                     | MultipleNames (NE.NonEmpty GlobalRdrElt)

lookupGreRn_maybe :: WhichGREs GREInfo -> RdrName -> RnM (Maybe GlobalRdrElt)
-- Look up the RdrName in the GlobalRdrEnv
--   Exactly one binding: records it as "used", return (Just gre)
--   No bindings:         return Nothing
--   Many bindings:       report "ambiguous", return an arbitrary (Just gre)
-- Uses addUsedRdrName to record use and deprecations
lookupGreRn_maybe which_gres rdr_name
  = do
      res <- lookupGreRn_helper which_gres rdr_name AllDeprecationWarnings
      case res of
        OneNameMatch gre ->  return $ Just gre
        MultipleNames gres -> do
          traceRn "lookupGreRn_maybe:NameClash" (ppr gres)
          addNameClashErrRn rdr_name gres
          return $ Just (NE.head gres)
        GreNotFound -> return Nothing

{-

Note [ Unbound vs Ambiguous Names ]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
an ambiguous occurrence of a record selector and DuplicateRecordFields
is enabled then we defer the selection until the typechecker.

-}


-- Internal Function
lookupGreRn_helper :: WhichGREs GREInfo -> RdrName -> DeprecationWarnings -> RnM GreLookupResult
lookupGreRn_helper which_gres rdr_name warn_if_deprec
  = do  { env <- getGlobalRdrEnv
        ; case lookupGRE env (LookupRdrName rdr_name which_gres) of
            []    -> return GreNotFound
            [gre] -> do { addUsedGRE warn_if_deprec gre
                        ; return (OneNameMatch gre) }
            -- Don't record usage for ambiguous names
            -- until we know which is meant
            (gre:others) -> return (MultipleNames (gre NE.:| others)) }

lookupGreAvailRn :: WhatLooking -> RdrName -> RnM (Maybe GlobalRdrElt)
-- Used in export lists
-- If not found or ambiguous, add error message, and fake with UnboundName
-- Uses addUsedRdrName to record use and deprecations
lookupGreAvailRn what_looking rdr_name
  = do
      mb_gre <- lookupGreRn_helper (RelevantGREsFOS WantNormal) rdr_name ExportDeprecationWarnings
      case mb_gre of
        GreNotFound ->
          do
            traceRn "lookupGreAvailRn" (ppr rdr_name)
            _ <- unboundName (LF what_looking WL_Global) rdr_name
            return Nothing
        MultipleNames gres ->
          do
            addNameClashErrRn rdr_name gres
            return Nothing
              -- Prevent error cascade
        OneNameMatch gre ->
          return $ Just gre

{-
*********************************************************
*                                                      *
                Deprecations
*                                                      *
*********************************************************

Note [Using isImportedGRE in addUsedGRE]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In addUsedGRE, we want to add any used imported GREs to the tcg_used_gres field,
so that we can emit appropriate warnings (see GHC.Rename.Names.warnUnusedImportDecls).

We want to do this for GREs that were brought into scope through imports. As per
Note [GlobalRdrElt provenance] in GHC.Types.Name.Reader, this means we should
check that gre_imp is non-empty. Checking that gre_lcl is False is INCORRECT,
because we might have obtained the GRE by an Exact or Orig direct reference,
in which case we have both gre_lcl = False and gre_imp = emptyBag.

Geting this wrong can lead to panics in e.g. bestImport, see #23240.
-}

addUsedDataCons :: GlobalRdrEnv -> TyCon -> RnM ()
-- Remember use of in-scope data constructors (#7969)
addUsedDataCons rdr_env tycon
  = addUsedGREs NoDeprecationWarnings
      [ gre
      | dc <- tyConDataCons tycon
      , Just gre <- [lookupGRE_Name rdr_env (dataConName dc)] ]

addUsedGRE :: DeprecationWarnings -> GlobalRdrElt -> RnM ()
-- Called for both local and imported things
-- Add usage *and* warn if deprecated
addUsedGRE warn_if_deprec gre
  = do { warnIfDeprecated warn_if_deprec [gre]
       ; when (isImportedGRE gre) $ -- See Note [Using isImportedGRE in addUsedGRE]
         do { env <- getGblEnv
             -- Do not report the GREInfo (#23424)
            ; traceRn "addUsedGRE" (ppr $ greName gre)
            ; updTcRef (tcg_used_gres env) (gre :) } }

addUsedGREs :: DeprecationWarnings -> [GlobalRdrElt] -> RnM ()
-- Record uses of any *imported* GREs
-- Used for recording used sub-bndrs
-- NB: no call to warnIfDeprecated; see Note [Handling of deprecations] in GHC.Rename.Utils
addUsedGREs warn_if_deprec gres
  = do { warnIfDeprecated warn_if_deprec gres
       ; unless (null imp_gres) $
         do { env <- getGblEnv
              -- Do not report the GREInfo (#23424)
            ; traceRn "addUsedGREs" (ppr $ map greName imp_gres)
            ; updTcRef (tcg_used_gres env) (imp_gres ++) } }
  where
    imp_gres = filter isImportedGRE gres
    -- See Note [Using isImportedGRE in addUsedGRE]

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
about "attempting to use module 'D' (./D.hs) which is not loaded"
which is what loadSrcInterface does.

It is enabled by default and disabled by the flag
`-fno-implicit-import-qualified`.

Note [Safe Haskell and GHCi]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We DON'T do this Safe Haskell as we need to check imports. We can
and should instead check the qualified import but at the moment
this requires some refactoring so leave as a TODO

Note [DuplicateRecordFields and -fimplicit-import-qualified]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When DuplicateRecordFields is used, a single module can export the same OccName
multiple times, for example:

  module M where
    data S = MkS { foo :: Int }
    data T = MkT { foo :: Int }

Now if we refer to M.foo via -fimplicit-import-qualified, we need to report an
ambiguity error.

-}

-- | Like 'lookupQualifiedNameGHCi' but returning at most one name, reporting an
-- ambiguity error if there are more than one.
lookupOneQualifiedNameGHCi :: FieldsOrSelectors -> RdrName -> RnM (Maybe GlobalRdrElt)
lookupOneQualifiedNameGHCi fos rdr_name = do
    all_gres <- lookupQualifiedNameGHCi fos rdr_name
    case all_gres of
      []         -> return Nothing
      [gre]      -> return $ Just $ gre
      (gre:gres) ->
        do addNameClashErrRn rdr_name (gre NE.:| gres)
           return (Just (mkUnboundGRE $ greOccName gre))
             -- (Use mkUnboundGRE to get the correct namespace)

-- | Look up *all* the names to which the 'RdrName' may refer in GHCi (using
-- @-fimplicit-import-qualified@).  This will normally be zero or one, but may
-- be more in the presence of @DuplicateRecordFields@.
lookupQualifiedNameGHCi :: HasDebugCallStack => FieldsOrSelectors -> RdrName -> RnM [GlobalRdrElt]
lookupQualifiedNameGHCi fos rdr_name
  = -- We want to behave as we would for a source file import here,
    -- and respect hiddenness of modules/packages, hence loadSrcInterface.
    do { dflags  <- getDynFlags
       ; is_ghci <- getIsGHCi
       ; go_for_it dflags is_ghci }

  where
    go_for_it dflags is_ghci
      | Just (mod_name,occ) <- isQual_maybe rdr_name
      , let ns = occNameSpace occ
      , is_ghci
      , gopt Opt_ImplicitImportQualified dflags   -- Enables this GHCi behaviour
      , not (safeDirectImpsReq dflags)            -- See Note [Safe Haskell and GHCi]
      = do { res <- loadSrcInterface_maybe doc mod_name NotBoot NoPkgQual
           ; case res of
                Succeeded iface
                  -> do { hsc_env <- getTopEnv
                        ; let gres =
                                [ gre
                                | avail <- mi_exports iface
                                , gname <- availNames avail
                                , let lk_occ = occName gname
                                      lk_ns  = occNameSpace lk_occ
                                , occNameFS occ == occNameFS lk_occ
                                , ns == lk_ns || (ns == varName && isFieldNameSpace lk_ns)
                                , let mod = mi_module iface
                                      gre = lookupGRE_PTE mod hsc_env gname
                                , allowGRE fos gre
                                  -- Include a field if it has a selector or we are looking for all fields;
                                  -- see Note [NoFieldSelectors].
                                ]
                        ; return gres }

                _ -> -- Either we couldn't load the interface, or
                     -- we could but we didn't find the name in it
                     do { traceRn "lookupQualifiedNameGHCi" (ppr rdr_name)
                        ; return [] } }

      | otherwise
      = do { traceRn "lookupQualifiedNameGHCi: off" (ppr rdr_name)
           ; return [] }

    doc = text "Need to find" <+> ppr rdr_name

    -- Lookup a Name for an implicit qualified import in GHCi
    -- in the given PackageTypeEnv.
    lookupGRE_PTE :: Module -> HscEnv -> Name -> GlobalRdrElt
    lookupGRE_PTE mod hsc_env nm =
      -- Fake a GRE so we can report a sensible name clash error if
      -- -fimplicit-import-qualified is used with a module that exports the same
      -- field name multiple times (see
      -- Note [DuplicateRecordFields and -fimplicit-import-qualified]).
      GRE { gre_name = nm
          , gre_par = NoParent
          , gre_lcl = False
          , gre_imp = unitBag is
          , gre_info = info }
        where
          info = lookupGREInfo hsc_env nm
          spec = ImpDeclSpec { is_mod = mod, is_as = moduleName mod, is_pkg_qual = NoPkgQual, is_qual = True, is_isboot = NotBoot, is_dloc = noSrcSpan }
          is = ImpSpec { is_decl = spec, is_item = ImpAll }

-- | Look up the 'GREInfo' associated with the given 'Name'
-- by looking up in the type environment.
lookupGREInfo :: HasDebugCallStack => HscEnv -> Name -> GREInfo
lookupGREInfo hsc_env nm
  | Just ty_thing <- wiredInNameTyThing_maybe nm
  = tyThingGREInfo ty_thing
  | otherwise
  -- Create a thunk which, when forced, loads the interface
  -- and looks up the TyThing in the type environment.
  --
  -- See Note [Retrieving the GREInfo from interfaces] in GHC.Types.GREInfo.
  -- Note: This function is very similar to 'tcIfaceGlobal', it would be better to
  -- use that if possible.
  = case nameModule_maybe nm of
      Nothing  -> UnboundGRE
      Just mod ->
        unsafePerformIO $ do
          _ <- initIfaceLoad hsc_env $
               loadInterface (text "lookupGREInfo" <+> parens (ppr nm))
                 mod ImportBySystem
          mb_ty_thing <- lookupType hsc_env nm
          case mb_ty_thing of
            Nothing -> do
              pprPanic "lookupGREInfo" $
                      vcat [ text "lookup failed:" <+> ppr nm ]
            Just ty_thing -> return $ tyThingGREInfo ty_thing

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
return the imported 'f', so that later on the renamer will
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

Note [Signatures in instance decls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   class C a where
     op :: a -> a
     nop :: a -> a

   instance C ty where
     bop :: [a] -> [a]
     bop x = x

     nop :: [a] -> [a]

When renameing the `bop` binding we'll give it an UnboundName (still with
OccName "bop") because `bop` is not a method of C.  Then

* when doing lookupSigOcc on `bop :: blah` we want to find `bop`, even though it
  is an UnboundName (failing to do this causes #16610, and #25437)

* When doing lookupSigOcc on `nop :: blah` we want to complain that there
  is no accompanying binding, even though `nop` is a class method
-}

data HsSigCtxt
  = TopSigCtxt NameSet       -- At top level, binding these names
                             -- See Note [Signatures for top level things]
  | LocalBindCtxt NameSet    -- In a local binding, binding these names
  | ClsDeclCtxt   Name       -- Class decl for this class
  | InstDeclCtxt  (OccEnv Name)  -- Instance decl whose user-written method
                                 -- bindings are described by this OccEnv
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
               -> GenLocated (EpAnn ann) RdrName
               -> RnM (GenLocated (EpAnn ann) Name)
lookupSigOccRn ctxt sig = lookupSigCtxtOccRn ctxt (SigLikeSig sig)

-- | Lookup a name in relation to the names in a 'HsSigCtxt'
lookupSigCtxtOccRn :: HsSigCtxt
                   -> SigLike -- ^ what are we looking for, e.g. a type family
                   -> GenLocated (EpAnn ann) RdrName
                   -> RnM (GenLocated (EpAnn ann) Name)
lookupSigCtxtOccRn ctxt what
  = wrapLocMA $ \ rdr_name ->
    do { let also_try_tycons = False
       ; mb_names <- lookupBindGroupOcc ctxt what rdr_name also_try_tycons NoNamespaceSpecifier
       ; case mb_names of
           Right name NE.:| rest ->
             do { massertPpr (null rest) $
                    vcat (text "lookupSigCtxtOccRn" <+> ppr name : map (either (pprScopeError rdr_name) ppr) rest)
                ; return name }
           Left err NE.:| _ ->
             do { addErr $ TcRnNotInScope err rdr_name
                ; return (mkUnboundNameRdr rdr_name) }
       }

lookupBindGroupOcc :: HsSigCtxt
                   -> SigLike -- ^ what kind of thing are we looking for?
                   -> RdrName -- ^ what to look up
                   -> Bool -- ^ if the 'RdrName' we are looking up is in
                           -- a value 'NameSpace', should we also look up
                           -- in the type constructor 'NameSpace'?
                   -> NamespaceSpecifier
                   -> RnM (NE.NonEmpty (Either NotInScopeError Name))
-- ^ Looks up the 'RdrName', expecting it to resolve to one of the
-- bound names currently in scope. If not, return an appropriate error message.
--
-- See Note [Looking up signature names].
lookupBindGroupOcc ctxt what rdr_name also_try_tycon_ns ns_spec
  | Just n <- isExact_maybe rdr_name
  = do { mb_gre <- lookupExactOcc_either n
       ; return $ case mb_gre of
          Left err  -> NE.singleton $ Left err
          Right gre -> finish (NoExactName $ greName gre) gre }
      -- Maybe we should check the side conditions
      -- but it's a pain, and Exact things only show
      -- up when you know what you are doing

  | Just (rdr_mod, rdr_occ) <- isOrig_maybe rdr_name
  = do { NE.singleton . Right <$> lookupOrig rdr_mod rdr_occ }

  | otherwise
  = case ctxt of
      HsBootCtxt ns    -> lookup_top (elem_name_set_with_namespace ns)
      TopSigCtxt ns    -> lookup_top (elem_name_set_with_namespace ns)
      RoleAnnotCtxt ns -> lookup_top (elem_name_set_with_namespace ns)
      LocalBindCtxt ns -> lookup_group ns
      ClsDeclCtxt  cls -> lookup_cls_op cls
      InstDeclCtxt occ_env-> lookup_inst occ_env
  where
    elem_name_set_with_namespace ns n = check_namespace n && (n `elemNameSet` ns)

    check_namespace = coveredByNamespaceSpecifier ns_spec . nameNameSpace

    namespace = occNameSpace occ
    occ       = rdrNameOcc rdr_name
    ok_gre    = greIsRelevant relevant_gres namespace
    relevant_gres = RelevantGREs { includeFieldSelectors    = WantBoth
                                 , lookupVariablesForFields = True
                                 , lookupTyConsAsWell = also_try_tycon_ns }

    finish err gre
      | ok_gre gre
      = NE.singleton (Right (greName gre))
      | otherwise
      = NE.singleton (Left err)

    succeed_with n = return $ NE.singleton $ Right n

    lookup_cls_op cls
      = NE.singleton <$>
          lookupSubBndrOcc AllDeprecationWarnings
            (ParentGRE cls (IAmTyCon ClassFlavour))
            MethodOfClass rdr_name

    lookup_inst occ_env  -- See Note [Signatures in instance decls]
      = case lookupOccEnv occ_env occ of
           Nothing -> bale_out_with []
           Just n  -> succeed_with n

    lookup_top keep_me
      = do { env <- getGlobalRdrEnv
           ; let occ = rdrNameOcc rdr_name
                 all_gres = lookupGRE env (LookupOccName occ relevant_gres)
                 names_in_scope = -- If rdr_name lacks a binding, only
                                  -- recommend alternatives from relevant
                                  -- namespaces. See #17593.
                                  map greName
                                $ filter (ok_gre <&&> isLocalGRE)
                                $ globalRdrEnvElts env
                 candidates_msg = candidates names_in_scope
           ; case filter (keep_me . greName) all_gres of
               [] | null all_gres -> bale_out_with candidates_msg
                  | otherwise     -> bale_out_with local_msg
               (gre1:gres)        -> return (fmap (Right . greName) (gre1 NE.:| gres)) }

    lookup_group bound_names  -- Look in the local envt (not top level)
      = do { mname <- lookupLocalOccRn_maybe rdr_name
           ; env <- getLocalRdrEnv
           ; let candidates_msg = candidates $ localRdrEnvElts env
           ; case mname of
               Just n
                 | n `elemNameSet` bound_names -> succeed_with n
                 | otherwise                   -> bale_out_with local_msg
               Nothing                         -> bale_out_with candidates_msg }

    bale_out_with hints = return $ NE.singleton $ Left $ MissingBinding what hints

    local_msg = [SuggestMoveToDeclarationSite what rdr_name]

    -- Identify all similar names and produce a message listing them
    candidates :: [Name] -> [GhcHint]
    candidates names_in_scope
      | (nm : nms) <- map SimilarName similar_names
      = [SuggestSimilarNames rdr_name (nm NE.:| nms)]
      | otherwise
      = []
      where
        similar_names
          = fuzzyLookup (unpackFS $ occNameFS $ rdrNameOcc rdr_name)
          $ map (\x -> ((unpackFS $ occNameFS $ nameOccName x), x))
                names_in_scope


---------------
lookupLocalTcNames :: HsSigCtxt -> SigLike -> NamespaceSpecifier -> RdrName -> RnM [(RdrName, Name)]
-- GHC extension: look up both the tycon and data con or variable.
-- Used for top-level fixity signatures and deprecations.
-- Complain if neither is in scope.
-- See Note [Fixity signature lookup]
lookupLocalTcNames ctxt what ns_spec rdr
  = do { this_mod <- getModule
       ; let also_try_tycon_ns = True
       ; nms_eithers <- fmap (guard_builtin_syntax this_mod rdr) <$>
                        lookupBindGroupOcc ctxt what rdr also_try_tycon_ns ns_spec
       ; let (errs, names) = partitionEithers (NE.toList nms_eithers)
       ; when (null names) $
          addErr (head errs) -- Bleat about one only
       ; return names }
  where
    -- Guard against the built-in syntax (ex: `infixl 6 :`), see #15233
    guard_builtin_syntax this_mod rdr (Right name)
      | isBuiltInOcc (occName rdr)
      , this_mod /= nameModule name
      = Left $ TcRnIllegalBuiltinSyntax what rdr
      | otherwise
      = Right (rdr, name)
    guard_builtin_syntax _ _ (Left err)
      = Left $ TcRnNotInScope err rdr

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

{- Note [dataTcOccs and Exact Names]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
respectively.  Initially, we just store the "standard" name (GHC.Builtin.Names.fromIntegralName,
fromRationalName etc), but the renamer changes this to the appropriate user
name if Opt_NoImplicitPrelude is on.  That is what lookupSyntax does.

We treat the original (standard) names as free-vars too, because the type checker
checks the type of the user thing against the type of the standard thing.
-}

lookupIfThenElse :: RnM (Maybe Name)
-- Looks up "ifThenElse" if rebindable syntax is on
lookupIfThenElse
  = do { rebindable_on <- xoptM LangExt.RebindableSyntax
       ; if not rebindable_on
         then return Nothing
         else do { ite <- lookupOccRnNone (mkVarUnqual (fsLit "ifThenElse"))
                 ; return (Just ite) } }

lookupSyntaxName :: Name                 -- ^ The standard name
                 -> RnM (Name, FreeVars) -- ^ Possibly a non-standard name
-- Lookup a Name that may be subject to Rebindable Syntax (RS).
--
-- - When RS is off, just return the supplied (standard) Name
--
-- - When RS is on, look up the OccName of the supplied Name; return
--   what we find, or the supplied Name if there is nothing in scope
lookupSyntaxName std_name
  = do { rebind <- xoptM LangExt.RebindableSyntax
       ; if not rebind
         then return (std_name, emptyFVs)
         else do { nm <- lookupOccRnNone (mkRdrUnqual (nameOccName std_name))
                 ; return (nm, unitFV nm) } }

lookupSyntaxExpr :: Name                          -- ^ The standard name
                 -> RnM (HsExpr GhcRn, FreeVars)  -- ^ Possibly a non-standard name
lookupSyntaxExpr std_name
  = do { (name, fvs) <- lookupSyntaxName std_name
       ; return (genHsVar name, fvs) }

lookupSyntax :: Name                             -- The standard name
             -> RnM (SyntaxExpr GhcRn, FreeVars) -- Possibly a non-standard
                                                 -- name
lookupSyntax std_name
  = do { (name, fvs) <- lookupSyntaxName std_name
       ; return (mkRnSyntaxExpr name, fvs) }

lookupSyntaxNames :: [Name]                         -- Standard names
     -> RnM ([HsExpr GhcRn], FreeVars) -- See comments with HsExpr.ReboundNames
   -- this works with CmdTop, which wants HsExprs, not SyntaxExprs
lookupSyntaxNames std_names
  = do { rebindable_on <- xoptM LangExt.RebindableSyntax
       ; if not rebindable_on then
             return (map (mkHsVar . noLocA) std_names, emptyFVs)
        else
          do { usr_names <-
                 mapM (lookupOccRnNone . mkRdrUnqual . nameOccName) std_names
             ; return (map (mkHsVar . noLocA) usr_names, mkFVs usr_names) } }


{-
Note [QualifiedDo]
~~~~~~~~~~~~~~~~~~
QualifiedDo is implemented using the same placeholders for operation names in
the AST that were devised for RebindableSyntax. Whenever the renamer checks
which names to use for do syntax, it first checks if the do block is qualified
(e.g. M.do { stmts }), in which case it searches for qualified names. If the
qualified names are not in scope, an error is produced. If the do block is not
qualified, the renamer does the usual search of the names which considers
whether RebindableSyntax is enabled or not. Dealing with QualifiedDo is driven
by the Opt_QualifiedDo dynamic flag.
-}

-- Lookup operations for a qualified do. If the context is not a qualified
-- do, then use lookupSyntaxExpr. See Note [QualifiedDo].
lookupQualifiedDo :: HsStmtContext fn -> Name -> RnM (SyntaxExpr GhcRn, FreeVars)
lookupQualifiedDo ctxt std_name
  = first mkRnSyntaxExpr <$> lookupQualifiedDoName ctxt std_name

lookupNameWithQualifier :: Name -> ModuleName -> RnM (Name, FreeVars)
lookupNameWithQualifier std_name modName
  = do { qname <- lookupOccRnNone (mkRdrQual modName (nameOccName std_name))
       ; return (qname, unitFV qname) }

-- See Note [QualifiedDo].
lookupQualifiedDoName :: HsStmtContext fn -> Name -> RnM (Name, FreeVars)
lookupQualifiedDoName ctxt std_name
  = case qualifiedDoModuleName_maybe ctxt of
      Nothing      -> lookupSyntaxName std_name
      Just modName -> lookupNameWithQualifier std_name modName

--------------------------------------------------------------------------------
-- Helper functions for 'isIrrefutableHsPat'.
--
-- (Defined here to avoid import cycles.)

-- | Check irrefutability of a 'ConLike' in a 'ConPat GhcRn'
-- (the 'Irref-ConLike' condition of Note [Irrefutability of ConPat]).
irrefutableConLikeRn :: HasDebugCallStack
                     => HscEnv
                     -> GlobalRdrEnv
                     -> CompleteMatches -- ^ in-scope COMPLETE pragmas
                     -> WithUserRdr Name -- ^ the 'Name' of the 'ConLike'
                     -> Bool
irrefutableConLikeRn hsc_env rdr_env comps (WithUserRdr _ con_nm)
  | Just gre <- lookupGRE_Name rdr_env con_nm
  = go $ greInfo gre
  | otherwise
  = go $ lookupGREInfo hsc_env con_nm
  where
    go ( IAmConLike conInfo ) =
      case conLikeInfo conInfo of
        ConIsData { conLikeDataCons = tc_cons } ->
          length tc_cons == 1
        ConIsPatSyn ->
          in_single_complete_match con_nm comps
    go _ = False

-- | Check irrefutability of the 'ConLike' in a 'ConPat GhcTc'
-- (the 'Irref-ConLike' condition of Note [Irrefutability of ConPat]),
-- given all in-scope COMPLETE pragmas ('CompleteMatches' in the typechecker,
-- 'DsCompleteMatches' in the desugarer).
irrefutableConLikeTc :: NamedThing con
                     => [CompleteMatchX con]
                         -- ^ in-scope COMPLETE pragmas
                     -> ConLike
                     -> Bool
irrefutableConLikeTc comps con =
  case con of
    RealDataCon dc -> length (tyConDataCons (dataConTyCon dc)) == 1
    PatSynCon {}   -> in_single_complete_match con_nm comps
  where
    con_nm = conLikeName con

-- | Internal helper function: check whether a 'ConLike' is the single member
-- of a COMPLETE set without a result 'TyCon'.
--
-- Why 'without a result TyCon'? See Wrinkle [Irrefutability and COMPLETE pragma result TyCons]
-- in Note [Irrefutability of ConPat].
in_single_complete_match :: NamedThing con => Name -> [CompleteMatchX con] -> Bool
in_single_complete_match con_nm = go
  where
    go [] = False
    go (comp:comps)
      | Nothing <- cmResultTyCon comp
        -- conservative, as we don't have enough info to compute
        -- 'completeMatchAppliesAtType'
      , let comp_nms = mapUniqDSet getName $ cmConLikes comp
      , comp_nms == mkUniqDSet [con_nm]
      = True
      | otherwise
      = go comps

--------------------------------------------------------------------------------
