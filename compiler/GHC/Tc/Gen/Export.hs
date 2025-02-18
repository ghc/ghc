{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TupleSections     #-}

module GHC.Tc.Gen.Export (rnExports, exports_from_avail, classifyGREs) where

import GHC.Prelude

import GHC.Hs
import GHC.Builtin.Names
import GHC.Tc.Errors.Types
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.Env
    ( TyThing(AConLike, AnId), tcLookupGlobal, tcLookupTyCon )
import GHC.Tc.Utils.TcType
import GHC.Rename.Doc
import GHC.Rename.Module
import GHC.Rename.Names
import GHC.Rename.Env
import GHC.Rename.Unbound ( reportUnboundName )
import GHC.Unit.Module
import GHC.Unit.Module.Imported
import GHC.Unit.Module.Warnings
import GHC.Core.TyCon
import GHC.Utils.Misc (sndOf3, thdOf3)
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Core.ConLike
import GHC.Core.PatSyn
import GHC.Data.Maybe
import GHC.Data.FastString (fsLit)
import GHC.Driver.Env
import GHC.Driver.DynFlags
import GHC.Parser.PostProcess ( setRdrNameSpace )
import qualified GHC.LanguageExtensions as LangExt

import GHC.Types.Unique.Map
import GHC.Types.SrcLoc as SrcLoc
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.Name.Set
import GHC.Types.DefaultEnv (ClassDefaults (cd_class), DefaultEnv,
                             emptyDefaultEnv, filterDefaultEnv, isEmptyDefaultEnv)
import GHC.Types.Avail
import GHC.Types.SourceFile
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.Name.Reader

import Control.Arrow ( first )
import Control.Monad ( when )
import qualified Data.List.NonEmpty as NE
import Data.Traversable   ( for )
import Data.List ( sortBy )
import qualified Data.Map as Map

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
into @[C{C, T;}, T{T, D;}]@ (which satisfies the AvailTC invariant).
-}

data ExportAccum        -- The type of the accumulating parameter of
                        -- the main worker function in rnExports
     = ExportAccum {
         expacc_exp_occs :: ExportOccMap,
           -- ^ Tracks exported occurrence names
         expacc_mods :: UniqMap ModuleName [Name],
           -- ^ Tracks (re-)exported module names
           --   and the names they re-export
         expacc_warn_spans :: ExportWarnSpanNames,
           -- ^ Information about warnings for names
         expacc_dont_warn :: DontWarnExportNames
           -- ^ What names not to export warnings for
           --   (because they are exported without a warning)
     }


emptyExportAccum :: ExportAccum
emptyExportAccum = ExportAccum emptyOccEnv emptyUniqMap [] emptyNameEnv

accumExports :: (ExportAccum -> x -> TcRn (ExportAccum, Maybe y))
             -> [x]
             -> TcRn ([y], ExportWarnSpanNames, DontWarnExportNames)
accumExports f xs = do
  (ExportAccum _ _ export_warn_spans dont_warn_export, ys)
    <- mapAccumLM f' emptyExportAccum xs
  return ( catMaybes ys
         , export_warn_spans
         , dont_warn_export )
  where f' acc x
          = fromMaybe (acc, Nothing) <$> attemptM (f acc x)

type ExportOccMap = OccEnv (Name, IE GhcPs)
        -- Tracks what a particular exported OccName
        --   in an export list refers to, and which item
        --   it came from.  It's illegal to export two distinct things
        --   that have the same occurrence name

rnExports :: Bool       -- False => no 'module M(..) where' header at all
          -> Maybe (LocatedLI [LIE GhcPs]) -- Nothing => no explicit export list
          -> RnM TcGblEnv

        -- Complains if two distinct exports have same OccName
        -- Warns about identical exports.
        -- Complains about exports items not in scope

rnExports explicit_mod exports
 = checkNoErrs $   -- Fail if anything in rnExports finds
                   -- an error fails, to avoid error cascade
   do   { hsc_env <- getTopEnv
        ; tcg_env <- getGblEnv
        ; let dflags = hsc_dflags hsc_env
              TcGblEnv { tcg_mod     = this_mod
                       , tcg_rdr_env = rdr_env
                       , tcg_imports = imports
                       , tcg_warns   = warns
                       , tcg_src     = hsc_src } = tcg_env
              default_main | mainModIs (hsc_HUE hsc_env) == this_mod
                           , Just main_fun <- mainFunIs dflags
                           = mkUnqual varName (fsLit main_fun)
                           | otherwise
                           = main_RDR_Unqual
        ; has_main <- (not . null) <$> lookupInfoOccRn default_main -- #17832

        -- If a module has no explicit header, and it has one or more main
        -- functions in scope, then add a header like
        -- "module Main(main) where ..."                               #13839
        -- See Note [Modules without a module header]
        ; let real_exports
                 | explicit_mod = exports
                 | has_main
                          = Just (noLocA [noLocA (IEVar Nothing
                                     (noLocA (IEName noExtField $ noLocA default_main)) Nothing)])
                        -- ToDo: the 'noLoc' here is unhelpful if 'main'
                        --       turns out to be out of scope
                 | otherwise = Nothing

        -- Rename the export list
        ; let do_it = exports_from_avail real_exports rdr_env imports this_mod
        ; (rn_exports, final_avails, new_export_warns)
            <- if hsc_src == HsigFile
                then do (mb_r, msgs) <- tryTc do_it
                        case mb_r of
                            Just r  -> return r
                            Nothing -> addMessages msgs >> failM
                else checkNoErrs do_it

        -- Final processing
        ; let final_ns = availsToNameSet final_avails
              drop_defaults (spans, _defaults, avails) = (spans, avails)

        ; traceRn "rnExports: Exports:" (ppr final_avails)

        ; return (tcg_env { tcg_exports    = final_avails
                          , tcg_rn_exports = case tcg_rn_exports tcg_env of
                                                Nothing -> Nothing
                                                Just _  -> map drop_defaults <$> rn_exports
                          , tcg_default_exports = case exports of
                              Nothing -> emptyDefaultEnv
                              _ -> foldMap (foldMap sndOf3) rn_exports
                          , tcg_dus = tcg_dus tcg_env `plusDU`
                                      usesOnly final_ns
                          , tcg_warns = insertWarnExports
                                        warns new_export_warns}) }

-- | List of names and the information about their warnings
--   (warning, export list item span)
type ExportWarnSpanNames = [(Name, WarningTxt GhcRn, SrcSpan)]

-- | Map from names that should not have export warnings to
--   the spans of export list items that are missing those warnings
type DontWarnExportNames = NameEnv (NE.NonEmpty SrcSpan)


{- Note [Default exports]
~~~~~~~~~~~~~~~~~~~~~~~~~
Named default declarations (see Note [Named default declarations] in
GHC.Tc.Gen.Default) can be exported. A named default declaration is
exported only when it's specified in the export list, using the `default`
keyword and the class name.  For example:

    module TextWrap (Text, default IsString) where
      import Data.String (IsString)
      import Data.Text (Text)
      default IsString (Text, String)

A module with no explicit export list does not export any default
declarations, and neither does the re-export of a whole imported module.

The export item `default IsString` is parsed into the `IE` item

    IEThingAbs ext (L loc (IEDefault ext "IsString")) doc

If exported, a default is imported automatically much like a class instance. For
example,

    import TextWrap ()

would import the above `default IsString (Text, String)` declaration into the
importing module.

The `cd_module` field of `ClassDefaults` tracks the module whence the default was
imported from, for the purpose of warning reports. The said warning report may be
triggered by `-Wtype-defaults` or by a user-defined `WARNING` pragma attached to
the default export. In the latter case the warning text is stored in the
`cd_warn` field. See test `testsuite/tests/default/ExportWarn.hs` for an example
of a user-defined warning on default.
-}

exports_from_avail :: Maybe (LocatedLI [LIE GhcPs])
                         -- ^ 'Nothing' means no explicit export list
                   -> GlobalRdrEnv
                   -> ImportAvails
                         -- ^ Imported modules; this is used to test if a
                         -- @module Foo@ export is valid (it's not valid
                         -- if we didn't import @Foo@!)
                   -> Module
                   -> RnM (Maybe [(LIE GhcRn, DefaultEnv, Avails)], Avails, ExportWarnNames GhcRn)
                         -- (Nothing, _, _) <=> no explicit export list
                         -- if explicit export list is present it contains
                         -- each renamed export item together with its exported
                         -- names.

exports_from_avail Nothing rdr_env _imports _this_mod
   -- The same as (module M) where M is the current module name,
   -- so that's how we handle it, except we also export the data family
   -- when a data instance is exported.
  = do {
    ; addDiagnostic
        (TcRnMissingExportList $ moduleName _this_mod)
    ; let avails =
            map fix_faminst . gresToAvailInfo
              . filter isLocalGRE . globalRdrEnvElts $ rdr_env
    ; return (Nothing, avails, []) }
  where
    -- #11164: when we define a data instance
    -- but not data family, re-export the family
    -- Even though we don't check whether this is actually a data family
    -- only data families can locally define subordinate things (`ns` here)
    -- without locally defining (and instead importing) the parent (`n`)
    fix_faminst avail@(AvailTC n ns)
      | availExportsDecl avail
      = avail
      | otherwise
      = AvailTC n (n:ns)
    fix_faminst avail = avail


exports_from_avail (Just (L _ rdr_items)) rdr_env imports this_mod
  = do (ie_avails, export_warn_spans, dont_warn_export)
         <- accumExports do_litem rdr_items
       let final_exports = nubAvails (concatMap thdOf3 ie_avails) -- Combine families
       export_warn_names <- aggregate_warnings export_warn_spans dont_warn_export
       return (Just ie_avails, final_exports, export_warn_names)
  where
    do_litem :: ExportAccum -> LIE GhcPs
             -> RnM (ExportAccum, Maybe (LIE GhcRn, DefaultEnv, Avails))
    do_litem acc lie = setSrcSpan (getLocA lie) (exports_from_item acc lie)

    -- Maps a parent to its in-scope children
    kids_env :: NameEnv [GlobalRdrElt]
    kids_env = mkChildEnv (globalRdrEnvElts rdr_env)

    -- See Note [Avails of associated data families]
    expand_tyty_gre :: GlobalRdrElt -> [GlobalRdrElt]
    expand_tyty_gre (gre@GRE { gre_par = ParentIs p })
      | isTyConName p
      , isTyConName (greName gre)
      = [gre, gre{ gre_par = NoParent }]
    expand_tyty_gre gre
      = [gre]

    imported_modules = [ imv_name imv
                       | xs <- Map.elems $ imp_mods imports
                       , imv <- importedByUser xs ]

    exports_from_item :: ExportAccum -> LIE GhcPs
                      -> RnM (ExportAccum, Maybe (LIE GhcRn, DefaultEnv, Avails))
    exports_from_item expacc@ExportAccum{
                        expacc_exp_occs   = occs,
                        expacc_mods       = earlier_mods,
                        expacc_warn_spans = export_warn_spans,
                        expacc_dont_warn  = dont_warn_export
                      } (L loc ie@(IEModuleContents (warn_txt_ps, _) lmod@(L _ mod)))
      | Just exported_names <- lookupUniqMap earlier_mods mod  -- Duplicate export of M
      = do { addDiagnostic (TcRnDupeModuleExport mod)
           ; (export_warn_spans', dont_warn_export', _) <-
                process_warning export_warn_spans
                                dont_warn_export
                                exported_names
                                warn_txt_ps
                                (locA loc)
                   -- Checks if all the names are exported with the same warning message
                   -- or if they should not be warned about
           ; return ( expacc{ expacc_warn_spans = export_warn_spans'
                            , expacc_dont_warn  = dont_warn_export' }
                    , Nothing ) }

      | otherwise
      = do { let { exportValid    = (mod `elem` imported_modules)
                                  || (moduleName this_mod == mod)
                 ; gre_prs        = pickGREsModExp mod (globalRdrEnvElts rdr_env)
                 ; new_gres       = [ gre'
                                    | (gre, _) <- gre_prs
                                    , gre' <- expand_tyty_gre gre ]
                 ; new_exports    = map availFromGRE new_gres
                 ; all_gres       = foldr (\(gre1,gre2) gres -> gre1 : gre2 : gres) [] gre_prs
                 ; exported_names = map greName new_gres
                 ; mods           = addToUniqMap earlier_mods mod exported_names
                 }

            ; checkErr exportValid (TcRnExportedModNotImported mod)
            ; warnIf (exportValid && null gre_prs) (TcRnNullExportedModule mod)

            ; traceRn "efa" (ppr mod $$ ppr all_gres)
            ; addUsedGREs ExportDeprecationWarnings all_gres

            ; occs' <- check_occs occs ie new_gres
                          -- This check_occs not only finds conflicts
                          -- between this item and others, but also
                          -- internally within this item.  That is, if
                          -- 'M.x' is in scope in several ways, we'll have
                          -- several members of mod_avails with the same
                          -- OccName.
            ; (export_warn_spans', dont_warn_export', warn_txt_rn) <-
                process_warning export_warn_spans
                                dont_warn_export
                                exported_names
                                warn_txt_ps
                                (locA loc)

            ; traceRn "export_mod"
                      (vcat [ ppr mod
                            , ppr new_exports ])
            ; return ( ExportAccum { expacc_exp_occs   = occs'
                                   , expacc_mods       = mods
                                   , expacc_warn_spans = export_warn_spans'
                                   , expacc_dont_warn  = dont_warn_export' }
                     , Just (L loc (IEModuleContents warn_txt_rn lmod), emptyDefaultEnv, new_exports) ) }

    exports_from_item acc lie = do
        m_doc_ie <- lookup_doc_ie lie
        case m_doc_ie of
          Just new_ie -> return (acc, Just (new_ie, emptyDefaultEnv, []))
          Nothing -> do
            m_ie <- lookup_ie acc lie
            case m_ie of
              Nothing -> return (acc, Nothing)
              Just (acc', new_ie, Left cls) -> do
                defaults <- tcg_default <$> getGblEnv
                let exported_default = filterDefaultEnv ((cls ==) . nameOccName . tyConName . cd_class) defaults
                return (acc', Just (new_ie, exported_default, []))
              Just (acc', new_ie, Right avail)
                -> return (acc', Just (new_ie, emptyDefaultEnv, [avail]))

    -------------
    lookup_ie :: ExportAccum -> LIE GhcPs -> RnM (Maybe (ExportAccum, LIE GhcRn, Either OccName AvailInfo))
    lookup_ie expacc@ExportAccum{
            expacc_exp_occs   = occs,
            expacc_warn_spans = export_warn_spans,
            expacc_dont_warn  = dont_warn_export
          } (L loc ie@(IEVar warn_txt_ps l doc))
        = do mb_gre <- lookupGreAvailRn $ lieWrappedName l
             for mb_gre $ \ gre -> do
               let avail = availFromGRE gre
                   name = greName gre

               occs' <- check_occs occs ie [gre]
               (export_warn_spans', dont_warn_export', warn_txt_rn)
                 <- process_warning export_warn_spans
                                    dont_warn_export
                                    [name]
                                    warn_txt_ps
                                    (locA loc)

               doc' <- traverse rnLHsDoc doc
               return ( expacc{ expacc_exp_occs   = occs'
                              , expacc_warn_spans = export_warn_spans'
                              , expacc_dont_warn  = dont_warn_export' }
                      , L loc (IEVar warn_txt_rn (replaceLWrappedName l name) doc')
                      , Right avail )

    lookup_ie expacc@ExportAccum{
            expacc_exp_occs   = occs,
            expacc_warn_spans = export_warn_spans,
            expacc_dont_warn  = dont_warn_export
          } (L loc ie@(IEThingAbs warn_txt_ps l doc))
        = do mb_gre <- lookupGreAvailRn $ lieWrappedName l
             for mb_gre $ \ gre -> do
               let avail = availFromGRE gre
                   name = greName gre

               occs' <- check_occs occs ie [gre]
               (export_warn_spans', dont_warn_export', warn_txt_rn)
                 <- process_warning export_warn_spans
                                    dont_warn_export
                                    [name]
                                    warn_txt_ps
                                    (locA loc)

               doc' <- traverse rnLHsDoc doc
               avail' <- case unLoc l of
                 -- see Note [Default exports]
                 IEDefault _ cls -> do
                   let defaultOccName = nameOccName . tyConName . cd_class
                       occName = rdrNameOcc (unLoc cls)
                   defaults <- tcg_default <$> getGblEnv
                   when (isEmptyDefaultEnv $ filterDefaultEnv ((occName ==) . defaultOccName) defaults)
                        (addErr $ TcRnExportHiddenDefault ie)
                   pure (Left occName)
                 _ -> pure (Right avail)
               return ( expacc{ expacc_exp_occs   = occs'
                              , expacc_warn_spans = export_warn_spans'
                              , expacc_dont_warn  = dont_warn_export' }
                      , L loc (IEThingAbs warn_txt_rn (replaceLWrappedName l name) doc')
                      , avail' )

    lookup_ie expacc@ExportAccum{
            expacc_exp_occs   = occs,
            expacc_warn_spans = export_warn_spans,
            expacc_dont_warn  = dont_warn_export
          } (L loc ie@(IEThingAll (warn_txt_ps, ann) l doc))
        = do mb_gre <- lookupGreAvailRn $ lieWrappedName l
             for mb_gre $ \ par -> do
               all_kids <- lookup_ie_kids_all ie l par
               let name = greName par
                   all_gres = par : all_kids
                   all_names = map greName all_gres

               occs' <- check_occs occs ie all_gres
               (export_warn_spans', dont_warn_export', warn_txt_rn)
                 <- process_warning export_warn_spans
                                    dont_warn_export
                                    all_names
                                    warn_txt_ps
                                    (locA loc)

               doc' <- traverse rnLHsDoc doc
               return ( expacc{ expacc_exp_occs   = occs'
                              , expacc_warn_spans = export_warn_spans'
                              , expacc_dont_warn  = dont_warn_export' }
                      , L loc (IEThingAll (warn_txt_rn, ann) (replaceLWrappedName l name) doc')
                      , Right (AvailTC name all_names) )

    lookup_ie expacc@ExportAccum{
            expacc_exp_occs   = occs,
            expacc_warn_spans = export_warn_spans,
            expacc_dont_warn  = dont_warn_export
          } (L loc ie@(IEThingWith (warn_txt_ps, ann) l wc sub_rdrs doc))
        = do mb_gre <- addErrCtxt (ExportCtxt ie)
                     $ lookupGreAvailRn $ lieWrappedName l
             for mb_gre $ \ par -> do
               (subs, with_kids)
                 <- addErrCtxt (ExportCtxt ie)
                  $ lookup_ie_kids_with par sub_rdrs

               wc_kids <-
                 case wc of
                   NoIEWildcard -> return []
                   IEWildcard _ -> lookup_ie_kids_all ie l par

               let name = greName par
                   all_kids = with_kids ++ wc_kids
                   all_gres = par : all_kids
                   all_names = map greName all_gres

               occs' <- check_occs occs ie all_gres
               (export_warn_spans', dont_warn_export', warn_txt_rn)
                 <- process_warning export_warn_spans
                                    dont_warn_export
                                    all_names
                                    warn_txt_ps
                                    (locA loc)

               doc' <- traverse rnLHsDoc doc
               return ( expacc{ expacc_exp_occs   = occs'
                              , expacc_warn_spans = export_warn_spans'
                              , expacc_dont_warn  = dont_warn_export' }
                      , L loc (IEThingWith (warn_txt_rn, ann) (replaceLWrappedName l name) wc subs doc')
                      , Right (AvailTC name all_names) )

    lookup_ie _ _ = panic "lookup_ie"    -- Other cases covered earlier


    lookup_ie_kids_with :: GlobalRdrElt -> [LIEWrappedName GhcPs]
                   -> RnM ([LIEWrappedName GhcRn], [GlobalRdrElt])
    lookup_ie_kids_with gre sub_rdrs =
      do { let name = greName gre
         ; kids <- lookupChildrenExport name sub_rdrs
         ; return (map fst kids, map snd kids) }

    lookup_ie_kids_all :: IE GhcPs -> LIEWrappedName GhcPs -> GlobalRdrElt
                  -> RnM [GlobalRdrElt]
    lookup_ie_kids_all ie (L _ rdr) gre =
      do { let name = greName gre
               gres = findChildren kids_env name
         ; addUsedKids (ieWrappedName rdr) gres
         ; when (null gres) $
            if isTyConName name
            then addTcRnDiagnostic (TcRnDodgyExports gre)
            else -- This occurs when you export T(..), but
                 -- only import T abstractly, or T is a synonym.
                 addErr (TcRnExportHiddenComponents ie)
         ; return gres }

    -------------

    -- Runs for every Name
    -- - If there is no new warning, flags that the old warning should not be
    --     included (since a warning should only be emitted if all
    --     of the export statements have a warning)
    -- - If the Name already has a warning, adds it
    process_warning :: ExportWarnSpanNames       -- Old aggregate data about warnins
                    -> DontWarnExportNames       -- Old names not to warn about
                    -> [Name]                              -- Names to warn about
                    -> Maybe (LWarningTxt GhcPs) -- Warning
                    -> SrcSpan                             -- Span of the export list item
                    -> RnM (ExportWarnSpanNames, -- Aggregate data about the warnings
                            DontWarnExportNames, -- Names not to warn about in the end
                                                 -- (when there was a non-warned export)
                            Maybe (LWarningTxt GhcRn)) -- Renamed warning
    process_warning export_warn_spans
                    dont_warn_export
                    names Nothing loc
      = return ( export_warn_spans
               , foldr update_dont_warn_export
                       dont_warn_export names
               , Nothing )
      where
        update_dont_warn_export :: Name -> DontWarnExportNames -> DontWarnExportNames
        update_dont_warn_export name dont_warn_export'
          = extendNameEnv_Acc (NE.<|)
                              NE.singleton
                              dont_warn_export'
                              name
                              loc

    process_warning export_warn_spans
                    dont_warn_export
                    names (Just warn_txt_ps) loc
      = do
          warn_txt_rn <- rnLWarningTxt warn_txt_ps
          let new_export_warn_spans = map (, unLoc warn_txt_rn, loc) names
          return ( new_export_warn_spans ++ export_warn_spans
                 , dont_warn_export
                 , Just warn_txt_rn )

    -- For each name exported with any warnings throws an error
    --   if there are any exports of that name with a different warning
    aggregate_warnings :: ExportWarnSpanNames
                       -> DontWarnExportNames
                       -> RnM (ExportWarnNames GhcRn)
    aggregate_warnings export_warn_spans dont_warn_export
      = fmap catMaybes
      $ mapM (aggregate_single . extract_name)
      $ NE.groupBy (\(n1, _, _) (n2, _, _) -> n1 == n2)
      $ sortBy (\(n1, _, _) (n2, _, _) -> n1 `compare` n2) export_warn_spans
      where
        extract_name :: NE.NonEmpty (Name, WarningTxt GhcRn, SrcSpan)
                     -> (Name, NE.NonEmpty (WarningTxt GhcRn, SrcSpan))
        extract_name l@((name, _, _) NE.:| _)
          = (name, NE.map (\(_, warn_txt, span) -> (warn_txt, span)) l)

        aggregate_single :: (Name, NE.NonEmpty (WarningTxt GhcRn, SrcSpan))
                         -> RnM (Maybe (Name, WarningTxt GhcRn))
        aggregate_single (name, (warn_txt_rn, loc) NE.:| warn_spans)
          = do
              -- Emit an error if the warnings differ
              case NE.nonEmpty spans_different of
                Nothing -> return ()
                Just spans_different
                  -> addErrAt loc (TcRnDifferentExportWarnings name spans_different)
              -- Emit a warning if some export list items do not have a warning
              case lookupNameEnv dont_warn_export name of
                Nothing -> return $ Just (name, warn_txt_rn)
                Just not_warned_spans -> do
                  addDiagnosticAt loc (TcRnIncompleteExportWarnings name not_warned_spans)
                  return Nothing
          where
            spans_different = map snd $ filter (not . warningTxtSame warn_txt_rn . fst) warn_spans

    -------------
    lookup_doc_ie :: LIE GhcPs -> RnM (Maybe (LIE GhcRn))
    lookup_doc_ie (L loc (IEGroup _ lev doc)) = do
      doc' <- rnLHsDoc doc
      pure $ Just (L loc (IEGroup noExtField lev doc'))
    lookup_doc_ie (L loc (IEDoc _ doc))       = do
      doc' <- rnLHsDoc doc
      pure $ Just (L loc (IEDoc noExtField doc'))
    lookup_doc_ie (L loc (IEDocNamed _ str))
      = pure $ Just (L loc (IEDocNamed noExtField str))
    lookup_doc_ie _ = pure Nothing

    -- In an export item M.T(A,B,C), we want to treat the uses of
    -- A,B,C as if they were M.A, M.B, M.C
    -- Happily pickGREs does just the right thing
    addUsedKids :: RdrName -> [GlobalRdrElt] -> RnM ()
    addUsedKids parent_rdr kid_gres
      = addUsedGREs ExportDeprecationWarnings (pickGREs parent_rdr kid_gres)

-- Renaming and typechecking of exports happens after everything else has
-- been typechecked.

{-
Note [Modules without a module header]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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



lookupChildrenExport :: Name -> [LIEWrappedName GhcPs]
                     -> RnM ([(LIEWrappedName GhcRn, GlobalRdrElt)])
lookupChildrenExport spec_parent rdr_items = mapAndReportM doOne rdr_items
    where
        -- Process an individual child
        doOne :: LIEWrappedName GhcPs
              -> RnM (LIEWrappedName GhcRn, GlobalRdrElt)
        doOne n = do

          let bareName = (ieWrappedName . unLoc) n
              what_lkup :: LookupChild
              what_lkup =
                LookupChild
                  { wantedParent       = spec_parent
                  , lookupDataConFirst = True
                  , prioritiseParent   = False -- See T11970.
                  }

                -- Do not report export list declaration deprecations
          name <-  lookupSubBndrOcc_helper False ExportDeprecationWarnings
                        spec_parent bareName what_lkup
          traceRn "lookupChildrenExport" (ppr name)
          -- Default to data constructors for slightly better error
          -- messages
          let unboundName :: RdrName
              unboundName = if rdrNameSpace bareName == varName
                            then bareName
                            else setRdrNameSpace bareName dataName

          case name of
            NameNotFound ->
              do { ub <- reportUnboundName unboundName
                 ; let l = getLoc n
                       gre = mkLocalGRE UnboundGRE NoParent ub
                 ; return (L l (IEName noExtField (L (l2l l) ub)), gre)}
            FoundChild child@(GRE { gre_name = child_nm, gre_par = par }) ->
              do { checkPatSynParent spec_parent par child_nm
                 ; return (replaceLWrappedName n child_nm, child)
                 }
            IncorrectParent p c gs -> failWithDcErr p (greName c) gs


-- Note [Typing Pattern Synonym Exports]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
-- Note [Types of TyCon]
-- ~~~~~~~~~~~~~~~~~~~~~
-- This check appears to be overly complicated, Richard asked why it
-- is not simply just `isAlgTyCon`. The answer for this is that
-- a classTyCon is also an `AlgTyCon` which we explicitly want to disallow.
-- (It is either a newtype or data depending on the number of methods)
--

-- | Given a resolved name in the children export list and a parent. Decide
-- whether we are allowed to export the child with the parent.
-- Invariant: gre_par == NoParent
-- See Note [Typing Pattern Synonym Exports]
checkPatSynParent :: Name    -- ^ Alleged parent type constructor
                             -- User wrote T( P, Q )
                  -> Parent  -- The parent of P we discovered
                  -> Name
                       -- ^ Either a
                       --   a) Pattern Synonym Constructor
                       --   b) A pattern synonym selector
                  -> TcM ()  -- Fails if wrong parent
checkPatSynParent _ (ParentIs {}) _
  = return ()

checkPatSynParent parent NoParent nm
  | isUnboundName parent -- Avoid an error cascade
  = return ()

  | otherwise
  = do { parent_ty_con  <- tcLookupTyCon  parent
       ; mpat_syn_thing <- tcLookupGlobal nm

        -- 1. Check that the Id was actually from a thing associated with patsyns
       ; case mpat_syn_thing of
            AnId i | isId i
                   , RecSelId { sel_tycon = RecSelPatSyn p } <- idDetails i
                   -> handle_pat_syn (PatSynRecSelExportCtxt p nm) parent_ty_con p

            AConLike (PatSynCon p) -> handle_pat_syn (PatSynExportCtxt p) parent_ty_con p

            _ -> failWithDcErr parent nm [] }
  where
    handle_pat_syn :: ErrCtxtMsg
                   -> TyCon      -- Parent TyCon
                   -> PatSyn     -- Corresponding bundled PatSyn
                                 -- and pretty printed origin
                   -> TcM ()
    handle_pat_syn doc ty_con pat_syn

      -- 2. See Note [Types of TyCon]
      | not $ isTyConWithSrcDataCons ty_con
      = addErrCtxt doc $ failWithTc TcRnPatSynBundledWithNonDataCon

      -- 3. Is the head a type variable?
      | Nothing <- mtycon
      = return ()
      -- 4. Ok. Check they are actually the same type constructor.

      | Just p_ty_con <- mtycon, p_ty_con /= ty_con
      = addErrCtxt doc $ failWithTc
          (TcRnPatSynBundledWithWrongType expected_res_ty res_ty)

      -- 5. We passed!
      | otherwise
      = return ()

      where
        expected_res_ty = mkTyConApp ty_con (mkTyVarTys (tyConTyVars ty_con))
        (_, _, _, _, _, res_ty) = patSynSig pat_syn
        mtycon = fst <$> tcSplitTyConApp_maybe res_ty


{-===========================================================================-}

-- | Insert the given 'GlobalRdrElt's into the 'ExportOccMap', checking that
-- each of the given 'GlobalRdrElt's does not appear multiple times in
-- the 'ExportOccMap', as per Note [Exporting duplicate declarations].
check_occs :: ExportOccMap -> IE GhcPs -> [GlobalRdrElt] -> RnM ExportOccMap
check_occs occs ie gres
  -- 'gres' are the entities specified by 'ie'
  = do { drf <- xoptM LangExt.DuplicateRecordFields
       ; foldlM (check drf) occs gres }
  where

    -- Check for distinct children exported with the same OccName (an error) or
    -- for duplicate exports of the same child (a warning).
    --
    -- See Note [Exporting duplicate declarations].
    check :: Bool -> ExportOccMap -> GlobalRdrElt -> RnM ExportOccMap
    check drf_enabled occs gre
      = case try_insert occs gre of
          Right occs'
            -- If DuplicateRecordFields is not enabled, also make sure
            -- that we are not exporting two fields with the same occNameFS
            -- under different namespaces.
            --
            -- See Note [Exporting duplicate record fields].
            | drf_enabled || not (isFieldOcc child_occ)
            -> return occs'
            | otherwise
            -> do { let flds = filter (\(_,ie') -> not $ dupFieldExport_ok ie ie')
                             $ lookupFieldsOccEnv occs (occNameFS child_occ)
                  ; case flds of { [] -> return occs'; clash1:clashes ->
               do { addDuplicateFieldExportErr (gre,ie) (clash1 NE.:| clashes)
                  ; return occs } } }

          Left (child', ie')
            | child == child' -- Duplicate export of a single Name: a warning.
            -> do { warnIf (not (dupExport_ok child ie ie')) (TcRnDuplicateExport gre ie ie')
                  ; return occs }

            | otherwise       -- Same OccName but different Name: an error.
            ->  do { global_env <- getGlobalRdrEnv
                   ; addErr (exportClashErr global_env child' child ie' ie)
                   ; return occs }
      where
        child = greName gre
        child_occ = occName child

    -- Try to insert a child into the map, returning Left if there is something
    -- already exported with the same OccName.
    try_insert :: ExportOccMap -> GlobalRdrElt -> Either (Name, IE GhcPs) ExportOccMap
    try_insert occs child
      = case lookupOccEnv occs occ of
          Nothing -> Right (extendOccEnv occs occ (greName child, ie))
          Just x  -> Left x
      where
        occ = greOccName child

-- | Is it OK for the given name to be exported by both export items?
--
-- See Note [Exporting duplicate declarations].
dupExport_ok :: Name -> IE GhcPs -> IE GhcPs -> Bool
dupExport_ok child ie1 ie2
  = not (  single ie1 || single ie2
        || (explicit_in ie1 && explicit_in ie2) )
  where
    explicit_in (IEModuleContents {}) = False                   -- module M
    explicit_in (IEThingAll _ r _)
      = occName child == rdrNameOcc (ieWrappedName $ unLoc r)  -- T(..)
    explicit_in _              = True

    single IEVar {}      = True
    single IEThingAbs {} = True
    single _             = False

failWithDcErr :: Name -> Name -> [Name] -> TcM a
failWithDcErr parent child parents = do
  ty_thing <- tcLookupGlobal child
  failWithTc $ TcRnExportedParentChildMismatch parent ty_thing child parents


exportClashErr :: GlobalRdrEnv
               -> Name -> Name
               -> IE GhcPs -> IE GhcPs
               -> TcRnMessage
exportClashErr global_env child1 child2 ie1 ie2
  = TcRnConflictingExports occ gre1' ie1' gre2' ie2'
  where
    occ = occName child1
    -- get_gre finds a GRE for the Name, so that we can show its provenance
    gre1 = get_gre child1
    gre2 = get_gre child2
    get_gre child
        = fromMaybe (pprPanic "exportClashErr" (ppr child))
                    (lookupGRE_Name global_env child)
    (gre1', ie1', gre2', ie2') =
      case SrcLoc.leftmost_smallest (greSrcSpan gre1) (greSrcSpan gre2) of
        LT -> (gre1, ie1, gre2, ie2)
        GT -> (gre2, ie2, gre1, ie1)
        EQ -> panic "exportClashErr: clashing exports have identical location"

addDuplicateFieldExportErr :: (GlobalRdrElt, IE GhcPs)
                           -> NE.NonEmpty (Name, IE GhcPs)
                           -> RnM ()
addDuplicateFieldExportErr gre others
  = do { rdr_env <- getGlobalRdrEnv
       ; let lkup = expectJust . lookupGRE_Name rdr_env
             other_gres = fmap (first lkup) others
       ; addErr (TcRnDuplicateFieldExport gre other_gres) }

-- | Is it OK to export two clashing duplicate record fields coming from the
-- given export items, with @-XDisambiguateRecordFields@ disabled?
--
-- See Note [Exporting duplicate record fields].
dupFieldExport_ok :: IE GhcPs -> IE GhcPs -> Bool
dupFieldExport_ok ie1 ie2
  | IEModuleContents {} <- ie1
  , ie2 == ie1
  = True
  | otherwise
  = False

{- Note [Exporting duplicate declarations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want to check that two different export items don't have both attempt to export
the same thing. What do we mean precisely? There are three main situations to consider:

  1. We export two distinct Names with identical OccNames. This is an error.
  2. We export the same Name in two different export items. This is usually
     a warning, but see below.
  3. We export a duplicate record field, and DuplicateRecordFields is not enabled.
     See Note [Exporting duplicate record fields].

Concerning (2), we sometimes want to allow a duplicate export of a given Name,
as #4478 points out. The logic, as implemented in dupExport_ok, is that we
do not allow a given Name to be exported by two IEs iff either:

  - the Name is mentioned explicitly in both IEs, or
  - one of the IEs mentions the name *alone*.

Examples:

  NOT OK: module M( f, f )

    f is mentioned explicitly in both

  NOT OK: module M( fmap, Functor(..) )
  NOT OK: module M( module Data.Functor, fmap )

    One of the import items mentions fmap alone, which is also
    exported by the other export item.

  OK:
    module M( module A, module B ) where
      import A( f )
      import B( f )

  OK: (#2436)
    module M( C(..), T(..) ) where
      class C a where { data T a }
      instance C Int where { data T Int = TInt }

  OK: (#2436)
    module Foo ( T ) where
      data family T a
    module Bar ( T(..), module Foo ) where
      import Foo
      data instance T Int = TInt

Note [Exporting duplicate record fields]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Record fields belonging to different datatypes belong to different namespaces,
as explained in Note [Record field namespacing] in GHC.Types.Name.Occurrence.
However, when the DuplicateRecordFields extension is NOT enabled, we want to
prevent users from exporting record fields that share the same underlying occNameFS.

To enforce this, in check_occs, when inserting a new record field into the ExportOccMap
and DuplicateRecordFields is not enabled, we also look up any clashing record fields,
and report an error.

Note however that the clash check has an extra wrinkle, similar to dupExport_ok,
as we want to allow the following:

  {-# LANGUAGE DuplicateRecordFields #-}
  module M1 where
    data D1 = MkD1 { foo :: Int }
    data D2 = MkD2 { foo :: Bool }

  ---------------------------------------------

   module M2 ( module M1 ) where
     import M1

That is, we should be allowed to re-export the whole module M1, without reporting
any nameclashes, even though M1 exports duplicate record fields and we have not
enabled -XDuplicateRecordFields in M2. This logic is implemented in
dupFieldExport_ok. See test case NoDRFModuleExport.

Note that this logic only applies to whole-module imports, as we don't want
to allow the following:

  module N0 where
    data family D a
  module N1 where
    import N0
    data instance D Int = MkDInt { foo :: Int }
  module N2 where
    import N0
    data instance D Bool = MkDBool { foo :: Int }

  module N (D(..)) where
    import N1
    import N2

Here, the single export item D(..) of N exports both record fields,
`$fld:MkDInt:foo` and `$fld:MkDBool:foo`, so we have to reject the program.
See test overloadedrecfldsfail10.
-}
