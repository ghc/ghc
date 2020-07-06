{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

module GHC.Tc.Gen.Export (tcRnExports, exports_from_avail) where

import GHC.Prelude

import GHC.Hs
import GHC.Builtin.Names
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.Env
import GHC.Tc.Utils.TcType
import GHC.Rename.Names
import GHC.Rename.Env
import GHC.Rename.Unbound ( reportUnboundName )
import GHC.Utils.Error
import GHC.Unit.Module
import GHC.Unit.Module.Imported
import GHC.Core.TyCon
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Core.ConLike
import GHC.Core.DataCon
import GHC.Core.PatSyn
import GHC.Data.Maybe
import GHC.Utils.Misc (capitalise)
import GHC.Data.FastString (fsLit)

import GHC.Types.Unique.Set
import GHC.Types.SrcLoc as SrcLoc
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.Name.Set
import GHC.Types.Avail
import GHC.Types.SourceFile
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.TyThing
import GHC.Types.Name.Reader

import Control.Monad
import GHC.Driver.Session
import GHC.Parser.PostProcess ( setRdrNameSpace )
import Data.Either            ( partitionEithers )

{-
************************************************************************
*                                                                      *
\subsection{Export list processing}
*                                                                      *
************************************************************************

Processing the export list.

You might think that we should record things that appear in the export
list as ``occurrences'' (using @addOccurrenceName@), but you'd be
wrong.  We do check (here) that they are in scope, but there is no
need to slurp in their actual declaration (which is what
@addOccurrenceName@ forces).

Indeed, doing so would big trouble when compiling @PrelBase@, because
it re-exports @GHC@, which includes @takeMVar#@, whose type includes
@ConcBase.StateAndSynchVar#@, and so on...

Note [Exports of data families]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose you see (#5306)
        module M where
          import X( F )
          data instance F Int = FInt
What does M export?  AvailTC F [FInt]
                  or AvailTC F [F,FInt]?
The former is strictly right because F isn't defined in this module.
But then you can never do an explicit import of M, thus
    import M( F( FInt ) )
because F isn't exported by M.  Nor can you import FInt alone from here
    import M( FInt )
because we don't have syntax to support that.  (It looks like an import of
the type FInt.)

At one point I implemented a compromise:
  * When constructing exports with no export list, or with module M(
    module M ), we add the parent to the exports as well.
  * But not when you see module M( f ), even if f is a
    class method with a parent.
  * Nor when you see module M( module N ), with N /= M.

But the compromise seemed too much of a hack, so we backed it out.
You just have to use an explicit export list:
    module M( F(..) ) where ...

Note [Avails of associated data families]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose you have (#16077)

    {-# LANGUAGE TypeFamilies #-}
    module A (module A) where

    class    C a  where { data T a }
    instance C () where { data T () = D }

Because @A@ is exported explicitly, GHC tries to produce an export list
from the @GlobalRdrEnv@. In this case, it pulls out the following:

    [ C defined at A.hs:4:1
    , T parent:C defined at A.hs:4:23
    , D parent:T defined at A.hs:5:35 ]

If map these directly into avails, (via 'availFromGRE'), we get
@[C{C;}, C{T;}, T{D;}]@, which eventually gets merged into @[C{C, T;}, T{D;}]@.
That's not right, because @T{D;}@ violates the AvailTC invariant: @T@ is
exported, but it isn't the first entry in the avail!

We work around this issue by expanding GREs where the parent and child
are both type constructors into two GRES.

    T parent:C defined at A.hs:4:23

      =>

    [ T parent:C defined at A.hs:4:23
    , T defined at A.hs:4:23 ]

Then, we get  @[C{C;}, C{T;}, T{T;}, T{D;}]@, which eventually gets merged
into @[C{C, T;}, T{T, D;}]@ (which satsifies the AvailTC invariant).
-}

data ExportAccum        -- The type of the accumulating parameter of
                        -- the main worker function in rnExports
     = ExportAccum
        ExportOccMap           --  Tracks exported occurrence names
        (UniqSet ModuleName)   --  Tracks (re-)exported module names

emptyExportAccum :: ExportAccum
emptyExportAccum = ExportAccum emptyOccEnv emptyUniqSet

accumExports :: (ExportAccum -> x -> TcRn (Maybe (ExportAccum, y)))
             -> [x]
             -> TcRn [y]
accumExports f = fmap (catMaybes . snd) . mapAccumLM f' emptyExportAccum
  where f' acc x = do
          m <- attemptM (f acc x)
          pure $ case m of
            Just (Just (acc', y)) -> (acc', Just y)
            _                     -> (acc, Nothing)

type ExportOccMap = OccEnv (Name, IE GhcPs)
        -- Tracks what a particular exported OccName
        --   in an export list refers to, and which item
        --   it came from.  It's illegal to export two distinct things
        --   that have the same occurrence name

tcRnExports :: Bool       -- False => no 'module M(..) where' header at all
          -> Maybe (LocatedL [LIE GhcPs]) -- Nothing => no explicit export list
          -> TcGblEnv
          -> RnM TcGblEnv

        -- Complains if two distinct exports have same OccName
        -- Warns about identical exports.
        -- Complains about exports items not in scope

tcRnExports explicit_mod exports
          tcg_env@TcGblEnv { tcg_mod     = this_mod,
                              tcg_rdr_env = rdr_env,
                              tcg_imports = imports,
                              tcg_src     = hsc_src }
 = unsetWOptM Opt_WarnWarningsDeprecations $
       -- Do not report deprecations arising from the export
       -- list, to avoid bleating about re-exporting a deprecated
       -- thing (especially via 'module Foo' export item)
   do   {
        ; dflags <- getDynFlags
        ; let is_main_mod = mainModIs dflags == this_mod
        ; let default_main = case mainFunIs dflags of
                 Just main_fun
                     | is_main_mod -> mkUnqual varName (fsLit main_fun)
                 _                 -> main_RDR_Unqual
        ; has_main <- (not . null) <$> lookupInfoOccRn default_main -- #17832
        -- If a module has no explicit header, and it has one or more main
        -- functions in scope, then add a header like
        -- "module Main(main) where ..."                               #13839
        -- See Note [Modules without a module header]
        ; let real_exports
                 | explicit_mod = exports
                 | has_main
                          = Just (noLocA [noLocA (IEVar noExtField
                                     (noLocA (IEName $ noLocA default_main)))])
                        -- ToDo: the 'noLoc' here is unhelpful if 'main'
                        --       turns out to be out of scope
                 | otherwise = Nothing

        ; let do_it = exports_from_avail real_exports rdr_env imports this_mod
        ; (rn_exports, final_avails)
            <- if hsc_src == HsigFile
                then do (mb_r, msgs) <- tryTc do_it
                        case mb_r of
                            Just r  -> return r
                            Nothing -> addMessages msgs >> failM
                else checkNoErrs do_it
        ; let final_ns     = availsToNameSetWithSelectors final_avails

        ; traceRn "rnExports: Exports:" (ppr final_avails)

        ; let new_tcg_env =
                  tcg_env { tcg_exports    = final_avails,
                             tcg_rn_exports = case tcg_rn_exports tcg_env of
                                                Nothing -> Nothing
                                                Just _  -> rn_exports,
                            tcg_dus = tcg_dus tcg_env `plusDU`
                                      usesOnly final_ns }
        ; failIfErrsM
        ; return new_tcg_env }

exports_from_avail :: Maybe (LocatedL [LIE GhcPs])
                         -- ^ 'Nothing' means no explicit export list
                   -> GlobalRdrEnv
                   -> ImportAvails
                         -- ^ Imported modules; this is used to test if a
                         -- @module Foo@ export is valid (it's not valid
                         -- if we didn't import @Foo@!)
                   -> Module
                   -> RnM (Maybe [(LIE GhcRn, Avails)], Avails)
                         -- (Nothing, _) <=> no explicit export list
                         -- if explicit export list is present it contains
                         -- each renamed export item together with its exported
                         -- names.

exports_from_avail Nothing rdr_env _imports _this_mod
   -- The same as (module M) where M is the current module name,
   -- so that's how we handle it, except we also export the data family
   -- when a data instance is exported.
  = do {
    ; warnMissingExportList <- woptM Opt_WarnMissingExportList
    ; warnIfFlag Opt_WarnMissingExportList
        warnMissingExportList
        (missingModuleExportWarn $ moduleName _this_mod)
    ; let avails =
            map fix_faminst . gresToAvailInfo
              . filter isLocalGRE . globalRdrEnvElts $ rdr_env
    ; return (Nothing, avails) }
  where
    -- #11164: when we define a data instance
    -- but not data family, re-export the family
    -- Even though we don't check whether this is actually a data family
    -- only data families can locally define subordinate things (`ns` here)
    -- without locally defining (and instead importing) the parent (`n`)
    fix_faminst (AvailTC n ns flds) =
      let new_ns =
            case ns of
              [] -> [n]
              (p:_) -> if p == n then ns else n:ns
      in AvailTC n new_ns flds

    fix_faminst avail = avail


exports_from_avail (Just (L _ rdr_items)) rdr_env imports this_mod
  = do ie_avails <- accumExports do_litem rdr_items
       let final_exports = nubAvails (concatMap snd ie_avails) -- Combine families
       return (Just ie_avails, final_exports)
  where
    do_litem :: ExportAccum -> LIE GhcPs
             -> RnM (Maybe (ExportAccum, (LIE GhcRn, Avails)))
    do_litem acc lie = setSrcSpan (getLocA lie) (exports_from_item acc lie)

    -- Maps a parent to its in-scope children
    kids_env :: NameEnv [GlobalRdrElt]
    kids_env = mkChildEnv (globalRdrEnvElts rdr_env)

    -- See Note [Avails of associated data families]
    expand_tyty_gre :: GlobalRdrElt -> [GlobalRdrElt]
    expand_tyty_gre (gre@GRE { gre_name = me, gre_par = ParentIs p })
      | isTyConName p, isTyConName me = [gre, gre{ gre_par = NoParent }]
    expand_tyty_gre gre = [gre]

    imported_modules = [ imv_name imv
                       | xs <- moduleEnvElts $ imp_mods imports
                       , imv <- importedByUser xs ]

    exports_from_item :: ExportAccum -> LIE GhcPs
                      -> RnM (Maybe (ExportAccum, (LIE GhcRn, Avails)))
    exports_from_item (ExportAccum occs earlier_mods)
                      (L loc ie@(IEModuleContents _ lmod@(L _ mod)))
        | mod `elementOfUniqSet` earlier_mods    -- Duplicate export of M
        = do { warnIfFlag Opt_WarnDuplicateExports True
                          (dupModuleExport mod) ;
               return Nothing }

        | otherwise
        = do { let { exportValid = (mod `elem` imported_modules)
                                || (moduleName this_mod == mod)
                   ; gre_prs     = pickGREsModExp mod (globalRdrEnvElts rdr_env)
                   ; new_exports = [ availFromGRE gre'
                                   | (gre, _) <- gre_prs
                                   , gre' <- expand_tyty_gre gre ]
                   ; all_gres    = foldr (\(gre1,gre2) gres -> gre1 : gre2 : gres) [] gre_prs
                   ; mods        = addOneToUniqSet earlier_mods mod
                   }

             ; checkErr exportValid (moduleNotImported mod)
             ; warnIfFlag Opt_WarnDodgyExports
                          (exportValid && null gre_prs)
                          (nullModuleExport mod)

             ; traceRn "efa" (ppr mod $$ ppr all_gres)
             ; addUsedGREs all_gres

             ; occs' <- check_occs ie occs new_exports
                      -- This check_occs not only finds conflicts
                      -- between this item and others, but also
                      -- internally within this item.  That is, if
                      -- 'M.x' is in scope in several ways, we'll have
                      -- several members of mod_avails with the same
                      -- OccName.
             ; traceRn "export_mod"
                       (vcat [ ppr mod
                             , ppr new_exports ])

             ; return (Just ( ExportAccum occs' mods
                            , ( L loc (IEModuleContents noExtField lmod)
                              , new_exports))) }

    exports_from_item acc@(ExportAccum occs mods) (L loc ie)
        | Just new_ie <- lookup_doc_ie ie
        = return (Just (acc, (L loc new_ie, [])))

        | otherwise
        = do (new_ie, avail) <- lookup_ie ie
             if isUnboundName (ieName new_ie)
                  then return Nothing    -- Avoid error cascade
                  else do

                    occs' <- check_occs ie occs [avail]

                    return (Just ( ExportAccum occs' mods
                                 , (L loc new_ie, [avail])))

    -------------
    lookup_ie :: IE GhcPs -> RnM (IE GhcRn, AvailInfo)
    lookup_ie (IEVar _ (L l rdr))
        = do (name, avail) <- lookupGreAvailRn $ ieWrappedName rdr
             return (IEVar noExtField (L l (replaceWrappedName rdr name)), avail)

    lookup_ie (IEThingAbs _ (L l rdr))
        = do (name, avail) <- lookupGreAvailRn $ ieWrappedName rdr
             return (IEThingAbs noAnn (L l (replaceWrappedName rdr name))
                    , avail)

    lookup_ie ie@(IEThingAll _ n')
        = do
            (n, avail, flds) <- lookup_ie_all ie n'
            let name = unLoc n
            return (IEThingAll noAnn (replaceLWrappedName n' (unLoc n))
                   , AvailTC name (name:avail) flds)


    lookup_ie ie@(IEThingWith _ l wc sub_rdrs _)
        = do
            (lname, subs, avails, flds)
              <- addExportErrCtxt ie $ lookup_ie_with l sub_rdrs
            (_, all_avail, all_flds) <-
              case wc of
                NoIEWildcard -> return (lname, [], [])
                IEWildcard _ -> lookup_ie_all ie l
            let name = unLoc lname
            return (IEThingWith noAnn (replaceLWrappedName l name) wc subs
                                (flds ++ (map noLoc all_flds)),
                    AvailTC name (name : avails ++ all_avail)
                                 (map unLoc flds ++ all_flds))


    lookup_ie _ = panic "lookup_ie"    -- Other cases covered earlier


    lookup_ie_with :: LIEWrappedName RdrName -> [LIEWrappedName RdrName]
                   -> RnM (Located Name, [LIEWrappedName Name], [Name],
                           [Located FieldLabel])
    lookup_ie_with (L l rdr) sub_rdrs
        = do name <- lookupGlobalOccRn $ ieWrappedName rdr
             (non_flds, flds) <- lookupChildrenExport name sub_rdrs
             if isUnboundName name
                then return (L (locA l) name, [], [name], [])
                else return (L (locA l) name, non_flds
                            , map (ieWrappedName . unLoc) non_flds
                            , flds)

    lookup_ie_all :: IE GhcPs -> LIEWrappedName RdrName
                  -> RnM (Located Name, [Name], [FieldLabel])
    lookup_ie_all ie (L l rdr) =
          do name <- lookupGlobalOccRn $ ieWrappedName rdr
             let gres = findChildren kids_env name
                 (non_flds, flds) = classifyGREs gres
             addUsedKids (ieWrappedName rdr) gres
             warnDodgyExports <- woptM Opt_WarnDodgyExports
             when (null gres) $
                  if isTyConName name
                  then when warnDodgyExports $
                           addWarn (Reason Opt_WarnDodgyExports)
                                   (dodgyExportWarn name)
                  else -- This occurs when you export T(..), but
                       -- only import T abstractly, or T is a synonym.
                       addErr (exportItemErr ie)
             return (L (locA l) name, non_flds, flds)

    -------------
    lookup_doc_ie :: IE GhcPs -> Maybe (IE GhcRn)
    lookup_doc_ie (IEGroup _ lev doc) = Just (IEGroup noExtField lev doc)
    lookup_doc_ie (IEDoc _ doc)       = Just (IEDoc noExtField doc)
    lookup_doc_ie (IEDocNamed _ str)  = Just (IEDocNamed noExtField str)
    lookup_doc_ie _ = Nothing

    -- In an export item M.T(A,B,C), we want to treat the uses of
    -- A,B,C as if they were M.A, M.B, M.C
    -- Happily pickGREs does just the right thing
    addUsedKids :: RdrName -> [GlobalRdrElt] -> RnM ()
    addUsedKids parent_rdr kid_gres = addUsedGREs (pickGREs parent_rdr kid_gres)

classifyGREs :: [GlobalRdrElt] -> ([Name], [FieldLabel])
classifyGREs = partitionEithers . map classifyGRE

classifyGRE :: GlobalRdrElt -> Either Name FieldLabel
classifyGRE gre = case gre_par gre of
  FldParent _ Nothing -> Right (FieldLabel (occNameFS (nameOccName n)) False n)
  FldParent _ (Just lbl) -> Right (FieldLabel lbl True n)
  _                      -> Left  n
  where
    n = gre_name gre

-- Renaming and typechecking of exports happens after everything else has
-- been typechecked.

{-
Note [Modules without a module header]
--------------------------------------------------

The Haskell 2010 report says in section 5.1:

>> An abbreviated form of module, consisting only of the module body, is
>> permitted. If this is used, the header is assumed to be
>> ‘module Main(main) where’.

For modules without a module header, this is implemented the
following way:

If the module has a main function in scope:
   Then create a module header and export the main function,
   as if a module header like ‘module Main(main) where...’ would exist.
   This has the effect to mark the main function and all top level
   functions called directly or indirectly via main as 'used',
   and later on, unused top-level functions can be reported correctly.
   There is no distinction between GHC and GHCi.
If the module has several main functions in scope:
   Then generate a header as above. The ambiguity is reported later in
   module  `GHC.Tc.Module` function `check_main`.
If the module has NO main function:
   Then export all top-level functions. This marks all top level
   functions as 'used'.
   In GHCi this has the effect, that we don't get any 'non-used' warnings.
   In GHC, however, the 'has-main-module' check in GHC.Tc.Module.checkMain
   fires, and we get the error:
      The IO action ‘main’ is not defined in module ‘Main’
-}


-- Renaming exports lists is a minefield. Five different things can appear in
-- children export lists ( T(A, B, C) ).
-- 1. Record selectors
-- 2. Type constructors
-- 3. Data constructors
-- 4. Pattern Synonyms
-- 5. Pattern Synonym Selectors
--
-- However, things get put into weird name spaces.
-- 1. Some type constructors are parsed as variables (-.->) for example.
-- 2. All data constructors are parsed as type constructors
-- 3. When there is ambiguity, we default type constructors to data
-- constructors and require the explicit `type` keyword for type
-- constructors.
--
-- This function first establishes the possible namespaces that an
-- identifier might be in (`choosePossibleNameSpaces`).
--
-- Then for each namespace in turn, tries to find the correct identifier
-- there returning the first positive result or the first terminating
-- error.
--



lookupChildrenExport :: Name -> [LIEWrappedName RdrName]
                     -> RnM ([LIEWrappedName Name], [Located FieldLabel])
lookupChildrenExport spec_parent rdr_items =
  do
    xs <- mapAndReportM doOne rdr_items
    return $ partitionEithers xs
    where
        -- Pick out the possible namespaces in order of priority
        -- This is a consequence of how the parser parses all
        -- data constructors as type constructors.
        choosePossibleNamespaces :: NameSpace -> [NameSpace]
        choosePossibleNamespaces ns
          | ns == varName = [varName, tcName]
          | ns == tcName  = [dataName, tcName]
          | otherwise = [ns]
        -- Process an individual child
        doOne :: LIEWrappedName RdrName
              -> RnM (Either (LIEWrappedName Name) (Located FieldLabel))
        doOne n = do

          let bareName = (ieWrappedName . unLoc) n
              lkup v = lookupSubBndrOcc_helper False True
                        spec_parent (setRdrNameSpace bareName v)

          name <-  combineChildLookupResult $ map lkup $
                   choosePossibleNamespaces (rdrNameSpace bareName)
          traceRn "lookupChildrenExport" (ppr name)
          -- Default to data constructors for slightly better error
          -- messages
          let unboundName :: RdrName
              unboundName = if rdrNameSpace bareName == varName
                                then bareName
                                else setRdrNameSpace bareName dataName

          case name of
            NameNotFound -> do { ub <- reportUnboundName unboundName
                               ; let l = getLoc $ ieLWrappedName n
                               ; return (Left (L (l2l l) (IEName (L l ub))))}
            FoundFL fls -> return $ Right (L (getLocA n) fls)
            FoundName par name -> do { checkPatSynParent spec_parent par name
                                     ; return
                                       $ Left (replaceLWrappedName n name) }
            IncorrectParent p g td gs -> failWithDcErr p g td gs


-- Note: [Typing Pattern Synonym Exports]
-- It proved quite a challenge to precisely specify which pattern synonyms
-- should be allowed to be bundled with which type constructors.
-- In the end it was decided to be quite liberal in what we allow. Below is
-- how Simon described the implementation.
--
-- "Personally I think we should Keep It Simple.  All this talk of
--  satisfiability makes me shiver.  I suggest this: allow T( P ) in all
--   situations except where `P`'s type is ''visibly incompatible'' with
--   `T`.
--
--    What does "visibly incompatible" mean?  `P` is visibly incompatible
--    with
--     `T` if
--       * `P`'s type is of form `... -> S t1 t2`
--       * `S` is a data/newtype constructor distinct from `T`
--
--  Nothing harmful happens if we allow `P` to be exported with
--  a type it can't possibly be useful for, but specifying a tighter
--  relationship is very awkward as you have discovered."
--
-- Note that this allows *any* pattern synonym to be bundled with any
-- datatype type constructor. For example, the following pattern `P` can be
-- bundled with any type.
--
-- ```
-- pattern P :: (A ~ f) => f
-- ```
--
-- So we provide basic type checking in order to help the user out, most
-- pattern synonyms are defined with definite type constructors, but don't
-- actually prevent a library author completely confusing their users if
-- they want to.
--
-- So, we check for exactly four things
-- 1. The name arises from a pattern synonym definition. (Either a pattern
--    synonym constructor or a pattern synonym selector)
-- 2. The pattern synonym is only bundled with a datatype or newtype.
-- 3. Check that the head of the result type constructor is an actual type
--    constructor and not a type variable. (See above example)
-- 4. Is so, check that this type constructor is the same as the parent
--    type constructor.
--
--
-- Note: [Types of TyCon]
--
-- This check appears to be overly complicated, Richard asked why it
-- is not simply just `isAlgTyCon`. The answer for this is that
-- a classTyCon is also an `AlgTyCon` which we explicitly want to disallow.
-- (It is either a newtype or data depending on the number of methods)
--

-- | Given a resolved name in the children export list and a parent. Decide
-- whether we are allowed to export the child with the parent.
-- Invariant: gre_par == NoParent
-- See note [Typing Pattern Synonym Exports]
checkPatSynParent :: Name    -- ^ Alleged parent type constructor
                             -- User wrote T( P, Q )
                  -> Parent  -- The parent of P we discovered
                  -> Name    -- ^ Either a
                             --   a) Pattern Synonym Constructor
                             --   b) A pattern synonym selector
                  -> TcM ()  -- Fails if wrong parent
checkPatSynParent _ (ParentIs {}) _
  = return ()

checkPatSynParent _ (FldParent {}) _
  = return ()

checkPatSynParent parent NoParent mpat_syn
  | isUnboundName parent -- Avoid an error cascade
  = return ()

  | otherwise
  = do { parent_ty_con <- tcLookupTyCon parent
       ; mpat_syn_thing <- tcLookupGlobal mpat_syn

        -- 1. Check that the Id was actually from a thing associated with patsyns
       ; case mpat_syn_thing of
            AnId i | isId i
                   , RecSelId { sel_tycon = RecSelPatSyn p } <- idDetails i
                   -> handle_pat_syn (selErr i) parent_ty_con p

            AConLike (PatSynCon p) -> handle_pat_syn (psErr p) parent_ty_con p

            _ -> failWithDcErr parent mpat_syn (ppr mpat_syn) [] }
  where
    psErr  = exportErrCtxt "pattern synonym"
    selErr = exportErrCtxt "pattern synonym record selector"

    assocClassErr :: SDoc
    assocClassErr = text "Pattern synonyms can be bundled only with datatypes."

    handle_pat_syn :: SDoc
                   -> TyCon      -- ^ Parent TyCon
                   -> PatSyn     -- ^ Corresponding bundled PatSyn
                                 --   and pretty printed origin
                   -> TcM ()
    handle_pat_syn doc ty_con pat_syn

      -- 2. See note [Types of TyCon]
      | not $ isTyConWithSrcDataCons ty_con
      = addErrCtxt doc $ failWithTc assocClassErr

      -- 3. Is the head a type variable?
      | Nothing <- mtycon
      = return ()
      -- 4. Ok. Check they are actually the same type constructor.

      | Just p_ty_con <- mtycon, p_ty_con /= ty_con
      = addErrCtxt doc $ failWithTc typeMismatchError

      -- 5. We passed!
      | otherwise
      = return ()

      where
        expected_res_ty = mkTyConApp ty_con (mkTyVarTys (tyConTyVars ty_con))
        (_, _, _, _, _, res_ty) = patSynSig pat_syn
        mtycon = fst <$> tcSplitTyConApp_maybe res_ty
        typeMismatchError :: SDoc
        typeMismatchError =
          text "Pattern synonyms can only be bundled with matching type constructors"
              $$ text "Couldn't match expected type of"
              <+> quotes (ppr expected_res_ty)
              <+> text "with actual type of"
              <+> quotes (ppr res_ty)


{-===========================================================================-}
check_occs :: IE GhcPs -> ExportOccMap -> [AvailInfo]
           -> RnM ExportOccMap
check_occs ie occs avails
  -- 'names' and 'fls' are the entities specified by 'ie'
  = foldlM check occs names_with_occs
  where
    -- Each Name specified by 'ie', paired with the OccName used to
    -- refer to it in the GlobalRdrEnv
    -- (see Note [Representing fields in AvailInfo] in GHC.Types.Avail).
    --
    -- We check for export clashes using the selector Name, but need
    -- the field label OccName for presenting error messages.
    names_with_occs = availsNamesWithOccs avails

    check occs (name, occ)
      = case lookupOccEnv occs name_occ of
          Nothing -> return (extendOccEnv occs name_occ (name, ie))

          Just (name', ie')
            | name == name'   -- Duplicate export
            -- But we don't want to warn if the same thing is exported
            -- by two different module exports. See ticket #4478.
            -> do { warnIfFlag Opt_WarnDuplicateExports
                               (not (dupExport_ok name ie ie'))
                               (dupExportWarn occ ie ie')
                  ; return occs }

            | otherwise    -- Same occ name but different names: an error
            ->  do { global_env <- getGlobalRdrEnv ;
                     addErr (exportClashErr global_env occ name' name ie' ie) ;
                     return occs }
      where
        name_occ = nameOccName name


dupExport_ok :: Name -> IE GhcPs -> IE GhcPs -> Bool
-- The Name is exported by both IEs. Is that ok?
-- "No"  iff the name is mentioned explicitly in both IEs
--        or one of the IEs mentions the name *alone*
-- "Yes" otherwise
--
-- Examples of "no":  module M( f, f )
--                    module M( fmap, Functor(..) )
--                    module M( module Data.List, head )
--
-- Example of "yes"
--    module M( module A, module B ) where
--        import A( f )
--        import B( f )
--
-- Example of "yes" (#2436)
--    module M( C(..), T(..) ) where
--         class C a where { data T a }
--         instance C Int where { data T Int = TInt }
--
-- Example of "yes" (#2436)
--    module Foo ( T ) where
--      data family T a
--    module Bar ( T(..), module Foo ) where
--        import Foo
--        data instance T Int = TInt

dupExport_ok n ie1 ie2
  = not (  single ie1 || single ie2
        || (explicit_in ie1 && explicit_in ie2) )
  where
    explicit_in (IEModuleContents {}) = False                   -- module M
    explicit_in (IEThingAll _ r)
      = nameOccName n == rdrNameOcc (ieWrappedName $ unLoc r)  -- T(..)
    explicit_in _              = True

    single IEVar {}      = True
    single IEThingAbs {} = True
    single _               = False


dupModuleExport :: ModuleName -> SDoc
dupModuleExport mod
  = hsep [text "Duplicate",
          quotes (text "Module" <+> ppr mod),
          text "in export list"]

moduleNotImported :: ModuleName -> SDoc
moduleNotImported mod
  = hsep [text "The export item",
          quotes (text "module" <+> ppr mod),
          text "is not imported"]

nullModuleExport :: ModuleName -> SDoc
nullModuleExport mod
  = hsep [text "The export item",
          quotes (text "module" <+> ppr mod),
          text "exports nothing"]

missingModuleExportWarn :: ModuleName -> SDoc
missingModuleExportWarn mod
  = hsep [text "The export item",
          quotes (text "module" <+> ppr mod),
          text "is missing an export list"]


dodgyExportWarn :: Name -> SDoc
dodgyExportWarn item
  = dodgyMsg (text "export") item (dodgyMsgInsert item :: IE GhcRn)

exportErrCtxt :: Outputable o => String -> o -> SDoc
exportErrCtxt herald exp =
  text "In the" <+> text (herald ++ ":") <+> ppr exp


addExportErrCtxt :: (OutputableBndrId p)
                 => IE (GhcPass p) -> TcM a -> TcM a
addExportErrCtxt ie = addErrCtxt exportCtxt
  where
    exportCtxt = text "In the export:" <+> ppr ie

exportItemErr :: IE GhcPs -> SDoc
exportItemErr export_item
  = sep [ text "The export item" <+> quotes (ppr export_item),
          text "attempts to export constructors or class methods that are not visible here" ]


dupExportWarn :: OccName -> IE GhcPs -> IE GhcPs -> SDoc
dupExportWarn occ_name ie1 ie2
  = hsep [quotes (ppr occ_name),
          text "is exported by", quotes (ppr ie1),
          text "and",            quotes (ppr ie2)]

dcErrMsg :: Name -> String -> SDoc -> [SDoc] -> SDoc
dcErrMsg ty_con what_is thing parents =
          text "The type constructor" <+> quotes (ppr ty_con)
                <+> text "is not the parent of the" <+> text what_is
                <+> quotes thing <> char '.'
                $$ text (capitalise what_is)
                <> text "s can only be exported with their parent type constructor."
                $$ (case parents of
                      [] -> empty
                      [_] -> text "Parent:"
                      _  -> text "Parents:") <+> fsep (punctuate comma parents)

failWithDcErr :: Name -> Name -> SDoc -> [Name] -> TcM a
failWithDcErr parent thing thing_doc parents = do
  ty_thing <- tcLookupGlobal thing
  failWithTc $ dcErrMsg parent (tyThingCategory' ty_thing)
                        thing_doc (map ppr parents)
  where
    tyThingCategory' :: TyThing -> String
    tyThingCategory' (AnId i)
      | isRecordSelector i = "record selector"
    tyThingCategory' i = tyThingCategory i


exportClashErr :: GlobalRdrEnv -> OccName
               -> Name -> Name
               -> IE GhcPs -> IE GhcPs
               -> MsgDoc
exportClashErr global_env occ name1 name2 ie1 ie2
  = vcat [ text "Conflicting exports for" <+> quotes (ppr occ) <> colon
         , ppr_export ie1' name1'
         , ppr_export ie2' name2' ]
  where
    ppr_export ie name = nest 3 (hang (quotes (ppr ie) <+> text "exports" <+>
                                       quotes (ppr_name name))
                                    2 (pprNameProvenance (get_gre name)))

    -- DuplicateRecordFields means that nameOccName might be a mangled
    -- $sel-prefixed thing, in which case show the correct OccName alone
    ppr_name name
      | nameOccName name == occ = ppr name
      | otherwise               = ppr occ

    -- get_gre finds a GRE for the Name, so that we can show its provenance
    get_gre name
        = fromMaybe (pprPanic "exportClashErr" (ppr name))
                    (lookupGRE_Name_OccName global_env name occ)
    get_loc name = greSrcSpan (get_gre name)
    (name1', ie1', name2', ie2') =
      case SrcLoc.leftmost_smallest (get_loc name1) (get_loc name2) of
        LT -> (name1, ie1, name2, ie2)
        GT -> (name2, ie2, name1, ie1)
        EQ -> panic "exportClashErr: clashing exports have idential location"
