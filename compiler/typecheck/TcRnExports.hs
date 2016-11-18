{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module TcRnExports (tcRnExports) where

import HsSyn
import PrelNames
import RdrName
import TcRnMonad
import TcEnv
import TcMType
import TcType
import RnNames
import RnEnv
import ErrUtils
import Id
import IdInfo
import Module
import Name
import NameEnv
import NameSet
import Avail
import TyCon
import SrcLoc
import HscTypes
import Outputable
import ConLike
import DataCon
import PatSyn
import FastString
import Maybes
import qualified GHC.LanguageExtensions as LangExt
import Util (capitalise)


import Control.Monad
import DynFlags
import RnHsDoc          ( rnHsDoc )
import RdrHsSyn        ( setRdrNameSpace )
import Data.Either      ( partitionEithers )

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
Suppose you see (Trac #5306)
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
-}

data ExportAccum        -- The type of the accumulating parameter of
                        -- the main worker function in rnExports
     = ExportAccum
        [LIE Name]             --  Export items with Names
        ExportOccMap           --  Tracks exported occurrence names
        [AvailInfo]            --  The accumulated exported stuff
                                --   Not nub'd!

emptyExportAccum :: ExportAccum
emptyExportAccum = ExportAccum [] emptyOccEnv []

type ExportOccMap = OccEnv (Name, IE RdrName)
        -- Tracks what a particular exported OccName
        --   in an export list refers to, and which item
        --   it came from.  It's illegal to export two distinct things
        --   that have the same occurrence name

tcRnExports :: Bool       -- False => no 'module M(..) where' header at all
          -> Maybe (Located [LIE RdrName]) -- Nothing => no explicit export list
          -> TcGblEnv
          -> RnM TcGblEnv

        -- Complains if two distinct exports have same OccName
        -- Warns about identical exports.
        -- Complains about exports items not in scope

tcRnExports explicit_mod exports
          tcg_env@TcGblEnv { tcg_mod     = this_mod,
                              tcg_rdr_env = rdr_env,
                              tcg_imports = imports }
 = unsetWOptM Opt_WarnWarningsDeprecations $
       -- Do not report deprecations arising from the export
       -- list, to avoid bleating about re-exporting a deprecated
       -- thing (especially via 'module Foo' export item)
   do   {
        -- If the module header is omitted altogether, then behave
        -- as if the user had written "module Main(main) where..."
        -- EXCEPT in interactive mode, when we behave as if he had
        -- written "module Main where ..."
        -- Reason: don't want to complain about 'main' not in scope
        --         in interactive mode
        ; dflags <- getDynFlags
        ; let real_exports
                 | explicit_mod = exports
                 | ghcLink dflags == LinkInMemory = Nothing
                 | otherwise
                          = Just (noLoc [noLoc (IEVar (noLoc main_RDR_Unqual))])
                        -- ToDo: the 'noLoc' here is unhelpful if 'main'
                        --       turns out to be out of scope

        ; (rn_exports, final_avails)
            <- exports_from_avail real_exports rdr_env imports this_mod
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

exports_from_avail :: Maybe (Located [LIE RdrName])
                         -- Nothing => no explicit export list
                   -> GlobalRdrEnv
                   -> ImportAvails
                   -> Module
                   -> RnM (Maybe [LIE Name], [AvailInfo])

exports_from_avail Nothing rdr_env _imports _this_mod
   -- The same as (module M) where M is the current module name,
   -- so that's how we handle it, except we also export the data family
   -- when a data instance is exported.
  = let avails =
          map fix_faminst . gresToAvailInfo
            . filter isLocalGRE . globalRdrEnvElts $ rdr_env
    in return (Nothing, avails)
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
  = do ExportAccum ie_names _ exports
        <-  checkNoErrs $ foldAndRecoverM do_litem emptyExportAccum rdr_items
       let final_exports = nubAvails exports -- Combine families
       return (Just ie_names, final_exports)
  where
    do_litem :: ExportAccum -> LIE RdrName -> RnM ExportAccum
    do_litem acc lie = setSrcSpan (getLoc lie) (exports_from_item acc lie)

    -- Maps a parent to its in-scope children
    kids_env :: NameEnv [GlobalRdrElt]
    kids_env = mkChildEnv (globalRdrEnvElts rdr_env)


    imported_modules = [ imv_name imv
                       | xs <- moduleEnvElts $ imp_mods imports, imv <- xs ]

    exports_from_item :: ExportAccum -> LIE RdrName -> RnM ExportAccum
    exports_from_item acc@(ExportAccum ie_names occs exports)
                      (L loc (IEModuleContents (L lm mod)))
        | let earlier_mods = [ mod
                             | (L _ (IEModuleContents (L _ mod))) <- ie_names ]
        , mod `elem` earlier_mods    -- Duplicate export of M
        = do { warnIf (Reason Opt_WarnDuplicateExports) True
                      (dupModuleExport mod) ;
               return acc }

        | otherwise
        = do { let { exportValid = (mod `elem` imported_modules)
                                || (moduleName this_mod == mod)
                   ; gre_prs     = pickGREsModExp mod (globalRdrEnvElts rdr_env)
                   ; new_exports = map (availFromGRE . fst) gre_prs
                   ; names       = map (gre_name     . fst) gre_prs
                   ; all_gres    = foldr (\(gre1,gre2) gres -> gre1 : gre2 : gres) [] gre_prs
               }

             ; checkErr exportValid (moduleNotImported mod)
             ; warnIf (Reason Opt_WarnDodgyExports)
                      (exportValid && null gre_prs)
                      (nullModuleExport mod)

             ; traceRn "efa" (ppr mod $$ ppr all_gres)
             ; addUsedGREs all_gres

             ; occs' <- check_occs (IEModuleContents (noLoc mod)) occs names
                      -- This check_occs not only finds conflicts
                      -- between this item and others, but also
                      -- internally within this item.  That is, if
                      -- 'M.x' is in scope in several ways, we'll have
                      -- several members of mod_avails with the same
                      -- OccName.
             ; traceRn "export_mod"
                       (vcat [ ppr mod
                             , ppr new_exports ])
             ; return (ExportAccum (L loc (IEModuleContents (L lm mod)) : ie_names)
                                   occs'
                                   (new_exports ++ exports)) }

    exports_from_item acc@(ExportAccum lie_names occs exports) (L loc ie)
        | isDoc ie
        = do new_ie <- lookup_doc_ie ie
             return (ExportAccum (L loc new_ie : lie_names) occs exports)

        | otherwise
        = do (new_ie, avail) <-
              setSrcSpan loc $ lookup_ie ie
             if isUnboundName (ieName new_ie)
                  then return acc    -- Avoid error cascade
                  else do

                    occs' <- check_occs ie occs (availNames avail)

                    return (ExportAccum (L loc new_ie : lie_names) occs' (avail : exports))

    -------------
    lookup_ie :: IE RdrName -> RnM (IE Name, AvailInfo)
    lookup_ie (IEVar (L l rdr))
        = do (name, avail) <- lookupGreAvailRn rdr
             return (IEVar (L l name), avail)

    lookup_ie (IEThingAbs (L l rdr))
        = do (name, avail) <- lookupGreAvailRn rdr
             return (IEThingAbs (L l name), avail)

    lookup_ie ie@(IEThingAll n)
        = do
            (n, avail, flds) <- lookup_ie_all ie n
            let name = unLoc n
            return (IEThingAll n, AvailTC name (name:avail) flds)


    lookup_ie ie@(IEThingWith l wc sub_rdrs _)
        = do
            (lname, subs, avails, flds)
              <- addExportErrCtxt ie $ lookup_ie_with l sub_rdrs
            (_, all_avail, all_flds) <-
              case wc of
                NoIEWildcard -> return (lname, [], [])
                IEWildcard _ -> lookup_ie_all ie l
            let name = unLoc lname
            return (IEThingWith lname wc subs (map noLoc (flds ++ all_flds)),
                    AvailTC name (name : avails ++ all_avail)
                                 (flds ++ all_flds))




    lookup_ie _ = panic "lookup_ie"    -- Other cases covered earlier

    lookup_ie_with :: Located RdrName -> [Located RdrName]
                   -> RnM (Located Name, [Located Name], [Name], [FieldLabel])
    lookup_ie_with (L l rdr) sub_rdrs
        = do name <- lookupGlobalOccRn rdr
             (non_flds, flds) <- lookupChildrenExport name sub_rdrs
             if isUnboundName name
                then return (L l name, [], [name], [])
                else return (L l name, non_flds
                            , map unLoc non_flds
                            , map unLoc flds)
    lookup_ie_all :: IE RdrName -> Located RdrName
                  -> RnM (Located Name, [Name], [FieldLabel])
    lookup_ie_all ie (L l rdr) =
          do name <- lookupGlobalOccRn rdr
             let gres = findChildren kids_env name
                 (non_flds, flds) = classifyGREs gres
             addUsedKids rdr gres
             warnDodgyExports <- woptM Opt_WarnDodgyExports
             when (null gres) $
                  if isTyConName name
                  then when warnDodgyExports $
                           addWarn (Reason Opt_WarnDodgyExports)
                                   (dodgyExportWarn name)
                  else -- This occurs when you export T(..), but
                       -- only import T abstractly, or T is a synonym.
                       addErr (exportItemErr ie)
             return (L l name, non_flds, flds)

    -------------
    lookup_doc_ie :: IE RdrName -> RnM (IE Name)
    lookup_doc_ie (IEGroup lev doc) = do rn_doc <- rnHsDoc doc
                                         return (IEGroup lev rn_doc)
    lookup_doc_ie (IEDoc doc)       = do rn_doc <- rnHsDoc doc
                                         return (IEDoc rn_doc)
    lookup_doc_ie (IEDocNamed str)  = return (IEDocNamed str)
    lookup_doc_ie _ = panic "lookup_doc_ie"    -- Other cases covered earlier

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

isDoc :: IE RdrName -> Bool
isDoc (IEDoc _)      = True
isDoc (IEDocNamed _) = True
isDoc (IEGroup _ _)  = True
isDoc _ = False

-- Renaming and typechecking of exports happens after everything else has
-- been typechecked.



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


-- Records the result of looking up a child.
data ChildLookupResult
      = NameNotFound                --  We couldn't find a suitable name
      | NameErr ErrMsg              --  We found an unambiguous name
                                    --  but there's another error
                                    --  we should abort from
      | FoundName Name              --  We resolved to a normal name
      | FoundFL FieldLabel       --  We resolved to a FL

instance Outputable ChildLookupResult where
  ppr NameNotFound = text "NameNotFound"
  ppr (FoundName n) = text "Found:" <+> ppr n
  ppr (FoundFL fls) = text "FoundFL:" <+> ppr fls
  ppr (NameErr _) = text "Error"

-- Left biased accumulation monoid. Chooses the left-most positive occurence.
instance Monoid ChildLookupResult where
  mempty = NameNotFound
  NameNotFound `mappend` m2 = m2
  NameErr m `mappend` _ = NameErr m -- Abort from the first error
  FoundName n1 `mappend` _ = FoundName n1
  FoundFL fls `mappend` _ = FoundFL fls

lookupChildrenExport :: Name -> [Located RdrName]
                     -> RnM ([Located Name], [Located FieldLabel])
lookupChildrenExport parent rdr_items =
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
        doOne :: Located RdrName
              -> RnM (Either (Located Name) (Located FieldLabel))
        doOne n = do

          let bareName = unLoc n
              lkup v = lookupExportChild parent (setRdrNameSpace bareName v)

          name <-  fmap mconcat . mapM lkup $
                    (choosePossibleNamespaces (rdrNameSpace bareName))

          -- Default to data constructors for slightly better error
          -- messages
          let unboundName :: RdrName
              unboundName = if rdrNameSpace bareName == varName
                                then bareName
                                else setRdrNameSpace bareName dataName

          case name of
            NameNotFound -> Left . L (getLoc n) <$> reportUnboundName unboundName
            FoundFL fls -> return $ Right (L (getLoc n) fls)
            FoundName name -> return $ Left (L (getLoc n) name)
            NameErr err_msg -> reportError err_msg >> failM



-- | Also captures the current context
mkNameErr :: SDoc -> TcM ChildLookupResult
mkNameErr errMsg = do
  tcinit <- tcInitTidyEnv
  NameErr <$> mkErrTcM (tcinit, errMsg)


-- | Used in export lists to lookup the children.
lookupExportChild :: Name -> RdrName -> RnM ChildLookupResult
lookupExportChild parent rdr_name
  | isUnboundName parent
    -- Avoid an error cascade
  = return (FoundName (mkUnboundNameRdr rdr_name))

  | otherwise = do
  gre_env <- getGlobalRdrEnv

  let original_gres = lookupGRE_RdrName rdr_name gre_env
  -- Disambiguate the lookup based on the parent information.
  -- The remaining GREs are things that we *could* export here, note that
  -- this includes things which have `NoParent`. Those are sorted in
  -- `checkPatSynParent`.
  traceRn "lookupExportChild original_gres:" (ppr original_gres)
  case picked_gres original_gres of
    NoOccurence ->
      noMatchingParentErr original_gres
    UniqueOccurence g ->
      checkPatSynParent parent (gre_name g)
    DisambiguatedOccurence g ->
      checkFld g
    AmbiguousOccurence gres ->
      mkNameClashErr gres
    where
        -- Convert into FieldLabel if necessary
        checkFld :: GlobalRdrElt -> RnM ChildLookupResult
        checkFld g@GRE{gre_name, gre_par} = do
          addUsedGRE True g
          return $ case gre_par of
            FldParent _ mfs ->  do
              FoundFL  (fldParentToFieldLabel gre_name mfs)
            _ -> FoundName gre_name

        fldParentToFieldLabel :: Name -> Maybe FastString -> FieldLabel
        fldParentToFieldLabel name mfs =
          case mfs of
            Nothing ->
              let fs = occNameFS (nameOccName name)
              in FieldLabel fs False name
            Just fs -> FieldLabel fs True name

        -- Called when we fine no matching GREs after disambiguation but
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
            [g] -> mkDcErrMsg parent (gre_name g) [p | Just p <- [getParent g]]
            gss@(g:_:_) ->
              if all isRecFldGRE gss && overload_ok
                then mkNameErr (dcErrMsg parent "record selector"
                                  (expectJust "noMatchingParentErr" (greLabel g))
                                  [ppr p | x <- gss, Just p <- [getParent x]])
                else mkNameClashErr gss

        mkNameClashErr :: [GlobalRdrElt] -> RnM ChildLookupResult
        mkNameClashErr gres = do
          addNameClashErrRn rdr_name gres
          return (FoundName (gre_name (head gres)))

        getParent :: GlobalRdrElt -> Maybe Name
        getParent (GRE { gre_par = p } ) =
          case p of
            ParentIs cur_parent -> Just cur_parent
            FldParent { par_is = cur_parent } -> Just cur_parent
            NoParent -> Nothing

        picked_gres :: [GlobalRdrElt] -> DisambigInfo
        picked_gres gres
          | isUnqual rdr_name = mconcat (map right_parent gres)
          | otherwise         = mconcat (map right_parent (pickGREs rdr_name gres))


        right_parent :: GlobalRdrElt -> DisambigInfo
        right_parent p
          | Just cur_parent <- getParent p
            = if parent == cur_parent
                then DisambiguatedOccurence p
                else NoOccurence
          | otherwise
            = UniqueOccurence p

-- This domain specific datatype is used to record why we decided it was
-- possible that a GRE could be exported with a parent.
data DisambigInfo
       = NoOccurence
          -- The GRE could never be exported. It has the wrong parent.
       | UniqueOccurence GlobalRdrElt
          -- The GRE has no parent. It could be a pattern synonym.
       | DisambiguatedOccurence GlobalRdrElt
          -- The parent of the GRE is the correct parent
       | AmbiguousOccurence [GlobalRdrElt]
          -- For example, two normal identifiers with the same name are in
          -- scope. They will both be resolved to "UniqueOccurence" and the
          -- monoid will combine them to this failing case.

instance Monoid DisambigInfo where
  mempty = NoOccurence
  -- This is the key line: We prefer disambiguated occurences to other
  -- names.
  UniqueOccurence _ `mappend` DisambiguatedOccurence g' = DisambiguatedOccurence g'
  DisambiguatedOccurence g' `mappend` UniqueOccurence _ = DisambiguatedOccurence g'


  NoOccurence `mappend` m = m
  m `mappend` NoOccurence = m
  UniqueOccurence g `mappend` UniqueOccurence g' = AmbiguousOccurence [g, g']
  UniqueOccurence g `mappend` AmbiguousOccurence gs = AmbiguousOccurence (g:gs)
  DisambiguatedOccurence g `mappend` DisambiguatedOccurence g'  = AmbiguousOccurence [g, g']
  DisambiguatedOccurence g `mappend` AmbiguousOccurence gs = AmbiguousOccurence (g:gs)
  AmbiguousOccurence gs `mappend` UniqueOccurence g' = AmbiguousOccurence (g':gs)
  AmbiguousOccurence gs `mappend` DisambiguatedOccurence g' = AmbiguousOccurence (g':gs)
  AmbiguousOccurence gs `mappend` AmbiguousOccurence gs' = AmbiguousOccurence (gs ++ gs')




--
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
-- This check appears to be overlly complicated, Richard asked why it
-- is not simply just `isAlgTyCon`. The answer for this is that
-- a classTyCon is also an `AlgTyCon` which we explicitly want to disallow.
-- (It is either a newtype or data depending on the number of methods)
--

-- | Given a resolved name in the children export list and a parent. Decide
-- whether we are allowed to export the child with the parent.
-- Invariant: gre_par == NoParent
-- See note [Typing Pattern Synonym Exports]
checkPatSynParent    :: Name   -- ^ Type constructor
                     -> Name   -- ^ Either a
                               --   a) Pattern Synonym Constructor
                               --   b) A pattern synonym selector
               -> TcM ChildLookupResult
checkPatSynParent parent mpat_syn = do
  parent_ty_con <- tcLookupTyCon parent
  mpat_syn_thing <- tcLookupGlobal mpat_syn
  let expected_res_ty =
          mkTyConApp parent_ty_con (mkTyVarTys (tyConTyVars parent_ty_con))

      handlePatSyn errCtxt =
        addErrCtxt errCtxt
        . tc_one_ps_export_with expected_res_ty parent_ty_con
  -- 1. Check that the Id was actually from a thing associated with patsyns
  case mpat_syn_thing of
      AnId i
        | isId i               ->
        case idDetails i of
          RecSelId { sel_tycon = RecSelPatSyn p } -> handlePatSyn (selErr i) p
          _ -> mkDcErrMsg parent mpat_syn []
      AConLike (PatSynCon p)    ->  handlePatSyn (psErr p) p
      _ -> mkDcErrMsg parent mpat_syn []
  where

    psErr = exportErrCtxt "pattern synonym"
    selErr = exportErrCtxt "pattern synonym record selector"

    assocClassErr :: SDoc
    assocClassErr =
      text "Pattern synonyms can be bundled only with datatypes."

    tc_one_ps_export_with :: TcTauType -- ^ TyCon type
                       -> TyCon       -- ^ Parent TyCon
                       -> PatSyn   -- ^ Corresponding bundled PatSyn
                                           -- and pretty printed origin
                       -> TcM ChildLookupResult
    tc_one_ps_export_with expected_res_ty ty_con pat_syn

      -- 2. See note [Types of TyCon]
      | not $ isTyConWithSrcDataCons ty_con = mkNameErr assocClassErr
      -- 3. Is the head a type variable?
      | Nothing <- mtycon = return (FoundName mpat_syn)
      -- 4. Ok. Check they are actually the same type constructor.
      | Just p_ty_con <- mtycon, p_ty_con /= ty_con = mkNameErr typeMismatchError
      -- 5. We passed!
      | otherwise = return (FoundName mpat_syn)

      where
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


check_occs :: IE RdrName -> ExportOccMap -> [Name] -> RnM ExportOccMap
check_occs ie occs names  -- 'names' are the entities specifed by 'ie'
  = foldlM check occs names
  where
    check occs name
      = case lookupOccEnv occs name_occ of
          Nothing -> return (extendOccEnv occs name_occ (name, ie))

          Just (name', ie')
            | name == name'   -- Duplicate export
            -- But we don't want to warn if the same thing is exported
            -- by two different module exports. See ticket #4478.
            -> do { warnIf (Reason Opt_WarnDuplicateExports)
                           (not (dupExport_ok name ie ie'))
                           (dupExportWarn name_occ ie ie')
                  ; return occs }

            | otherwise    -- Same occ name but different names: an error
            ->  do { global_env <- getGlobalRdrEnv ;
                     addErr (exportClashErr global_env name' name ie' ie) ;
                     return occs }
      where
        name_occ = nameOccName name


dupExport_ok :: Name -> IE RdrName -> IE RdrName -> Bool
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
-- Example of "yes" (Trac #2436)
--    module M( C(..), T(..) ) where
--         class C a where { data T a }
--         instance C Int where { data T Int = TInt }
--
-- Example of "yes" (Trac #2436)
--    module Foo ( T ) where
--      data family T a
--    module Bar ( T(..), module Foo ) where
--        import Foo
--        data instance T Int = TInt

dupExport_ok n ie1 ie2
  = not (  single ie1 || single ie2
        || (explicit_in ie1 && explicit_in ie2) )
  where
    explicit_in (IEModuleContents _) = False                -- module M
    explicit_in (IEThingAll r) = nameOccName n == rdrNameOcc (unLoc r)  -- T(..)
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
  = text "The export item `module" <+> ppr mod <>
    text "' is not imported"

nullModuleExport :: ModuleName -> SDoc
nullModuleExport mod
  = text "The export item `module" <+> ppr mod <> ptext (sLit "' exports nothing")


dodgyExportWarn :: Name -> SDoc
dodgyExportWarn item = dodgyMsg (text "export") item

exportErrCtxt :: Outputable o => String -> o -> SDoc
exportErrCtxt herald exp =
  text "In the" <+> text (herald ++ ":") <+> ppr exp


addExportErrCtxt :: (HasOccName s, OutputableBndr s) => IE s -> TcM a -> TcM a
addExportErrCtxt ie = addErrCtxt exportCtxt
  where
    exportCtxt = text "In the export:" <+> ppr ie

exportItemErr :: IE RdrName -> SDoc
exportItemErr export_item
  = sep [ text "The export item" <+> quotes (ppr export_item),
          text "attempts to export constructors or class methods that are not visible here" ]


dupExportWarn :: OccName -> IE RdrName -> IE RdrName -> SDoc
dupExportWarn occ_name ie1 ie2
  = hsep [quotes (ppr occ_name),
          text "is exported by", quotes (ppr ie1),
          text "and",            quotes (ppr ie2)]

dcErrMsg :: Outputable a => Name -> String -> a -> [SDoc] -> SDoc
dcErrMsg ty_con what_is thing parents =
          text "The type constructor" <+> quotes (ppr ty_con)
                <+> text "is not the parent of the" <+> text what_is
                <+> quotes (ppr thing) <> char '.'
                $$ text (capitalise what_is)
                <> text "s can only be exported with their parent type constructor."
                $$ (case parents of
                      [] -> empty
                      [_] -> text "Parent:"
                      _  -> text "Parents:") <+> fsep (punctuate comma parents)

mkDcErrMsg :: Name -> Name -> [Name] -> TcM ChildLookupResult
mkDcErrMsg parent thing parents = do
  ty_thing <- tcLookupGlobal thing
  mkNameErr (dcErrMsg parent (tyThingCategory' ty_thing) thing (map ppr parents))
  where
    tyThingCategory' :: TyThing -> String
    tyThingCategory' (AnId i)
      | isRecordSelector i = "record selector"
    tyThingCategory' i = tyThingCategory i


exportClashErr :: GlobalRdrEnv -> Name -> Name -> IE RdrName -> IE RdrName
               -> MsgDoc
exportClashErr global_env name1 name2 ie1 ie2
  = vcat [ text "Conflicting exports for" <+> quotes (ppr occ) <> colon
         , ppr_export ie1' name1'
         , ppr_export ie2' name2' ]
  where
    occ = nameOccName name1
    ppr_export ie name = nest 3 (hang (quotes (ppr ie) <+> text "exports" <+>
                                       quotes (ppr name))
                                    2 (pprNameProvenance (get_gre name)))

    -- get_gre finds a GRE for the Name, so that we can show its provenance
    get_gre name
        = fromMaybe (pprPanic "exportClashErr" (ppr name)) (lookupGRE_Name global_env name)
    get_loc name = greSrcSpan (get_gre name)
    (name1', ie1', name2', ie2') = if get_loc name1 < get_loc name2
                                   then (name1, ie1, name2, ie2)
                                   else (name2, ie2, name1, ie1)
