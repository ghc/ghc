{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ApplicativeDo #-}

module HaddockUtils
  ( addFieldDoc,
    addFieldDocs,
    addConDoc,
    addConDocs,
    addConDocFirst,

    addModuleHaddock,
  ) where

import GhcPrelude

import GHC.Hs
import SrcLoc
import GHC.Driver.Session ( WarningFlag(..) )
import Outputable hiding ( (<>) )

import Data.Semigroup
import Data.Foldable
import Data.Traversable
import Control.Monad
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader
import Data.Functor.Identity
import Data.Coerce

import Lexer

addModuleHaddock :: Located HsModule -> P (Located HsModule)
addModuleHaddock lmod = runAddHaddock (addHaddockModule lmod)

newtype HdkM a = HdkM (ReaderT LocRange (State [PsLocated HdkComment]) a)
  deriving (Functor, Applicative, Monad)
  -- The state of HdkM is a list of pending (unassociated with an AST node)
  -- Haddock comments, sorted by location, in ascending order.
  --
  -- We go over the AST, looking up these comments using 'takeHdkComments'.
  -- The remaining ones are ignored with a warning (-Wignored-haddock).

mkHdkM :: (LocRange -> [PsLocated HdkComment] -> (a, [PsLocated HdkComment])) -> HdkM a
unHdkM :: HdkM a -> (LocRange -> [PsLocated HdkComment] -> (a, [PsLocated HdkComment]))
mkHdkM = coerce
unHdkM = coerce

data HdkA a = HdkA (Maybe BufSpan) (HdkM a)

instance Functor HdkA where
  fmap f (HdkA l m) = HdkA l (fmap f m)

instance Applicative HdkA where
  pure a = HdkA mempty (pure a)
  HdkA l1 m1 <*> HdkA l2 m2 =
    HdkA (l1 <> l2) (delim1 m1 <*> delim2 m2)
    where
      delim1 = inLocRange (locRangeTo (fmap @Maybe bufSpanStart l2))
      delim2 = inLocRange (locRangeFrom (fmap @Maybe bufSpanEnd l1))

registerHdkA :: Located a -> HdkA ()
registerHdkA a = HdkA (getBufSpan (getLoc a)) (pure ())

delimitHdkA :: SrcSpan -> HdkA a -> HdkA a
delimitHdkA l' (HdkA l m) = HdkA (getBufSpan l' <> l) m

withLayoutInfoHdkA :: LayoutInfo -> HdkA a -> HdkA a
withLayoutInfoHdkA NoLayoutInfo m = m
withLayoutInfoHdkA ExplicitBraces m = m
withLayoutInfoHdkA (VirtualBraces n) (HdkA l m) =
    HdkA l (inLocRange loc_range m)
  where
    loc_range = LocRange mempty mempty (ColumnFrom (n+1))

runAddHaddock :: HdkA a -> P a
runAddHaddock (HdkA _ m) = do
  pState <- getPState
  let (a, other_hdk_comments) = unHdkM m mempty (reverse (hdk_comments pState))
  mapM_ reportHdkComment other_hdk_comments
  return a
  where
    reportHdkComment :: PsLocated HdkComment -> P ()
    reportHdkComment (L l _) =
      addWarning Opt_WarnIgnoredHaddock (mkSrcSpanPs l) $
        text "A Haddock comment cannot appear in this position and will be ignored."

concatLHsDocString :: [LHsDocString] -> Maybe LHsDocString
concatLHsDocString xs = L l <$> concatDocs docs
  where l = foldr combineSrcSpans noSrcSpan locs
        locs = map getLoc xs
        docs = map unLoc xs

addHaddockModule :: Located HsModule -> HdkA (Located HsModule)
addHaddockModule (L l_mod mod) = do
  headerDocs <-
    for @Maybe (hsmodName mod) $ \(L l_name _) ->
    HdkA (getBufSpan l_name) $ do
      docs <-
        inLocRange (locRangeTo (getBufPos (srcSpanStart l_name))) $
        takeHdkComments getDocNext
      pure $ concatLHsDocString docs
  hsmodExports' <- traverse @Maybe addHaddockExports (hsmodExports mod)
  traverse_ registerHdkA (hsmodImports mod)
  hsmodDecls' <- addHaddockInterleaveItems (VirtualBraces 1) getDocDecl addHaddockDecl (hsmodDecls mod)
                                        --- ^^^^^^^^^ TODO (int-index): actual module indent level
  pure $ L l_mod $
    mod { hsmodExports = hsmodExports'
        , hsmodDecls = hsmodDecls'
        , hsmodHaddockModHeader = join @Maybe headerDocs }

addHaddockExports
  :: Located [LIE GhcPs]
  -> HdkA (Located [LIE GhcPs])
addHaddockExports (L l_exports exports) =
  delimitHdkA l_exports $ do
    exports' <- addHaddockInterleaveItems NoLayoutInfo getDocIE add_export exports
    registerHdkA (L (srcLocSpan (srcSpanEnd l_exports)) ()) -- Do not conume comments after the closing parenthesis
    pure $ L l_exports exports'
  where
    add_export e@(L l_export _) = HdkA (getBufSpan l_export) (pure e)

-- Add Haddock items to a list of non-Haddock items.
-- Used to process export lists (with getDocIE) and declarations (with getDocDecl).
addHaddockInterleaveItems
  :: forall a.
     LayoutInfo
  -> (PsLocated HdkComment -> Maybe a) -- Get a documentation item
  -> (a -> HdkA a) -- Process a non-documentation item
  -> [a]           -- Unprocessed (non-documentation) items
  -> HdkA [a]      -- Documentation items & processed non-documentation items
addHaddockInterleaveItems layoutInfo getDocItem processItem = go
  where
    go :: [a] -> HdkA [a]
    go [] = HdkA mempty (takeHdkComments getDocItem)
    go (item : items) = do
      docItems <- HdkA mempty (takeHdkComments getDocItem)
      item' <- withLayoutInfoHdkA layoutInfo $ processItem item
      other_items <- go items
      pure $ docItems ++ item':other_items

getDocDecl :: PsLocated HdkComment -> Maybe (LHsDecl GhcPs)
getDocDecl a = mapLoc (DocD noExtField) <$> getDocDecl' a

getDocDecl' :: PsLocated HdkComment -> Maybe LDocDecl
getDocDecl' (L l_comment hdk_comment) =
  Just $ L (mkSrcSpanPs l_comment) $
    case hdk_comment of
      HdkCommentNext doc -> DocCommentNext doc
      HdkCommentPrev doc -> DocCommentPrev doc
      HdkCommentNamed s doc -> DocCommentNamed s doc
      HdkCommentSection n doc -> DocGroup n doc

getDocIE :: PsLocated HdkComment -> Maybe (LIE GhcPs)
getDocIE (L l_comment hdk_comment) =
  case hdk_comment of
    HdkCommentSection n doc -> Just $ L l (IEGroup noExtField n doc)
    HdkCommentNamed s _doc -> Just $ L l (IEDocNamed noExtField s)
    HdkCommentNext doc -> Just $ L l (IEDoc noExtField doc)
    _ -> Nothing
  where l = mkSrcSpanPs l_comment

getDocNext :: PsLocated HdkComment -> Maybe LHsDocString
getDocNext (L l (HdkCommentNext doc)) = Just $ L (mkSrcSpanPs l) doc
getDocNext _ = Nothing

getDocPrev :: PsLocated HdkComment -> Maybe LHsDocString
getDocPrev (L l (HdkCommentPrev doc)) = Just $ L (mkSrcSpanPs l) doc
getDocPrev _ = Nothing

addHaddockDecl :: LHsDecl GhcPs -> HdkA (LHsDecl GhcPs)
addHaddockDecl (L l_decl (SigD _ (TypeSig _ names t))) =
  delimitHdkA l_decl $ do
    traverse_ registerHdkA names
    t' <- addHaddockSigWcType t
    pure (L l_decl (SigD noExtField (TypeSig noExtField names t')))
addHaddockDecl (L l_decl (SigD _ (PatSynSig _ names t))) =
  delimitHdkA l_decl $ do
    traverse_ registerHdkA names
    t' <- addHaddockSigType t
    pure (L l_decl (SigD noExtField (PatSynSig noExtField names t')))
addHaddockDecl (L l_decl (SigD _ (ClassOpSig _ is_dflt names t))) =
  delimitHdkA l_decl $ do
    traverse_ registerHdkA names
    t' <- addHaddockSigType t
    pure (L l_decl (SigD noExtField (ClassOpSig noExtField is_dflt names t')))
addHaddockDecl (L l_decl (TyClD _ decl))
  | DataDecl { tcdLName, tcdTyVars, tcdFixity, tcdDataDefn = defn } <- decl
  , HsDataDefn { dd_ND, dd_ctxt, dd_cType, dd_kindSig, dd_cons, dd_derivs } <- defn
  = delimitHdkA l_decl $ do
      registerHdkA tcdLName
      traverse_ registerHdkA dd_kindSig
      dd_cons' <- traverse addHaddockConDecl dd_cons
      dd_derivs' <- addHaddockDeriving dd_derivs
      pure $
        let defn' = HsDataDefn
                      { dd_ext = noExtField
                      , dd_ND, dd_ctxt, dd_cType, dd_kindSig
                      , dd_derivs = dd_derivs'
                      , dd_cons = dd_cons' }
            decl' = DataDecl
                      { tcdDExt = noExtField
                      , tcdLName, tcdTyVars, tcdFixity
                      , tcdDataDefn = defn' }
        in L l_decl (TyClD noExtField decl')
  | ClassDecl { tcdCExt = tcdLayout,
                tcdCtxt, tcdLName, tcdTyVars, tcdFixity, tcdFDs,
                tcdSigs, tcdMeths, tcdATs, tcdATDefs } <- decl
  = delimitHdkA l_decl $ do
      where_cls' <-
        addHaddockInterleaveItems tcdLayout getDocDecl addHaddockDecl $
        flattenBindsAndSigs (tcdMeths, tcdSigs, tcdATs, tcdATDefs, [], [])
      pure $
        let (tcdMeths', tcdSigs', tcdATs', tcdATDefs', _, tcdDocs) = partitionBindsAndSigs id where_cls'
            decl' = ClassDecl { tcdCExt = tcdLayout
                              , tcdCtxt, tcdLName, tcdTyVars, tcdFixity, tcdFDs
                              , tcdSigs = tcdSigs'
                              , tcdMeths = tcdMeths'
                              , tcdATs = tcdATs'
                              , tcdATDefs = tcdATDefs'
                              , tcdDocs }
        in L l_decl (TyClD noExtField decl')
addHaddockDecl (L l_decl (InstD _ decl))
  | DataFamInstD { dfid_inst } <- decl
  , DataFamInstDecl { dfid_eqn } <- dfid_inst
  = delimitHdkA l_decl $ do
    dfid_eqn' <- addHaddockImplicitBndrs (\fam_eqn -> case fam_eqn of
      FamEqn { feqn_tycon, feqn_bndrs, feqn_pats, feqn_fixity, feqn_rhs }
        | HsDataDefn { dd_ND, dd_ctxt, dd_cType, dd_kindSig, dd_cons, dd_derivs } <- feqn_rhs
        -> do
          registerHdkA feqn_tycon
          traverse_ registerHdkA dd_kindSig
          dd_cons' <- traverse addHaddockConDecl dd_cons
          dd_derivs' <- addHaddockDeriving dd_derivs
          pure $
            let defn' = HsDataDefn
                          { dd_ext = noExtField
                          , dd_ND, dd_ctxt, dd_cType, dd_kindSig
                          , dd_derivs = dd_derivs'
                          , dd_cons = dd_cons' }
            in FamEqn { feqn_ext = noExtField,
                        feqn_tycon, feqn_bndrs, feqn_pats, feqn_fixity,
                        feqn_rhs = defn' }
      FamEqn { feqn_rhs = XHsDataDefn x } -> noExtCon x
      XFamEqn x -> noExtCon x
      ) dfid_eqn
    pure $ L l_decl (InstD noExtField (DataFamInstD {
      dfid_ext = noExtField,
      dfid_inst = DataFamInstDecl { dfid_eqn = dfid_eqn' } }))
addHaddockDecl (L l_decl (ForD _ decl))
  = delimitHdkA l_decl $ do
    decl' <-
      case decl of
        ForeignImport { fd_name, fd_sig_ty, fd_fi } -> do
          registerHdkA fd_name
          fd_sig_ty' <- addHaddockSigType fd_sig_ty
          pure ForeignImport { fd_i_ext = noExtField,
                               fd_sig_ty = fd_sig_ty',
                               fd_name, fd_fi }
        ForeignExport { fd_name, fd_sig_ty, fd_fe } -> do
          registerHdkA fd_name
          fd_sig_ty' <- addHaddockSigType fd_sig_ty
          pure ForeignExport { fd_e_ext = noExtField,
                               fd_sig_ty = fd_sig_ty',
                               fd_name, fd_fe }
        XForeignDecl x -> noExtCon x
    pure $ L l_decl (ForD noExtField decl')
addHaddockDecl d = delimitHdkA (getLoc d) (pure d)

addHaddockDeriving :: HsDeriving GhcPs -> HdkA (HsDeriving GhcPs)
addHaddockDeriving lderivs =
  delimitHdkA (getLoc lderivs) $
  for @Located lderivs $ \derivs ->
    traverse addHaddockDerivingClause derivs

addHaddockDerivingClause :: LHsDerivingClause GhcPs -> HdkA (LHsDerivingClause GhcPs)
addHaddockDerivingClause lderiv =
  delimitHdkA (getLoc lderiv) $
  for @Located lderiv $ \deriv ->
  case deriv of
    HsDerivingClause { deriv_clause_strategy, deriv_clause_tys } -> do
      traverse_ @Maybe registerHdkA deriv_clause_strategy
      deriv_clause_tys' <-
        delimitHdkA (getLoc deriv_clause_tys) $
        for @Located (deriv_clause_tys) $ \tys ->
          traverse addHaddockSigType tys
      pure HsDerivingClause
        { deriv_clause_ext = noExtField,
          deriv_clause_strategy,
          deriv_clause_tys = deriv_clause_tys' }
    XHsDerivingClause x -> noExtCon x

addHaddockConDecl :: LConDecl GhcPs -> HdkA (LConDecl GhcPs)
addHaddockConDecl (L l_con con) = HdkA (getBufSpan l_con) $ do
  trailingConDocs <- do
    nextDocs <-
      inLocRange (locRangeTo (getBufPos (srcSpanStart l_con))) $
      peekHdkComments getDocNext
    -- See Note [Trailing comment on constructor declaration]
    let inner_docs_range = locRangeFrom (getBufPos (srcSpanStart l_con)) <>
                           locRangeTo (getBufPos (srcSpanEnd l_con))
    innerDocs <- inLocRange inner_docs_range (peekHdkComments Just)
    if null innerDocs && null nextDocs
      then inLocRange (locRangeFrom (getBufPos (srcSpanEnd l_con))) $
           takeHdkComments getDocPrev
      else return []
  let getConDoc (L l _) = HdkA (getBufSpan l) $ do
        nextDocs <-
          inLocRange (locRangeTo (getBufPos (srcSpanStart l))) $
          takeHdkComments getDocNext
        prevDocs <-
          inLocRange (locRangeFrom (getBufPos (srcSpanEnd l))) $
          takeHdkComments getDocPrev
        return $ concatLHsDocString (nextDocs ++ prevDocs ++ trailingConDocs)
      hdk_a_m (HdkA _ m) = m
  hdk_a_m $ case con of
    ConDeclGADT { con_g_ext, con_names, con_forall, con_qvars, con_mb_cxt, con_args, con_res_ty } -> do
      con_doc' <- getConDoc (head con_names)
      con_args' <-
        case con_args of
          PrefixCon ts -> do
            ts' <- traverse addHaddockType ts
            pure $ PrefixCon ts'
          RecCon (L l_rec flds) -> do
            flds' <- traverse addHaddockConDeclField flds
            pure $ RecCon (L l_rec flds')
          InfixCon _ _ -> panic "ConDeclGADT InfixCon"
      con_res_ty' <- addHaddockType con_res_ty
      pure $ L l_con $
        ConDeclGADT { con_g_ext, con_names, con_forall, con_qvars, con_mb_cxt,
                      con_doc = con_doc',
                      con_args = con_args',
                      con_res_ty = con_res_ty' }
    ConDeclH98 { con_ext, con_name, con_forall, con_ex_tvs, con_mb_cxt, con_args } -> do
      case con_args of
        PrefixCon ts -> do
          con_doc' <- getConDoc con_name
          ts' <- traverse addHaddockType ts
          pure $ L l_con $
            ConDeclH98 { con_ext, con_name, con_forall, con_ex_tvs, con_mb_cxt,
                         con_doc = con_doc',
                         con_args = PrefixCon ts' }
        InfixCon t1 t2 -> do
          t1' <- addHaddockType t1
          con_doc' <- getConDoc con_name
          t2' <- addHaddockType t2
          pure $ L l_con $
            ConDeclH98 { con_ext, con_name, con_forall, con_ex_tvs, con_mb_cxt,
                         con_doc = con_doc',
                         con_args = InfixCon t1' t2' }
        RecCon (L l_rec flds) -> do
          con_doc' <- getConDoc con_name
          flds' <- traverse addHaddockConDeclField flds
          pure $ L l_con $
            ConDeclH98 { con_ext, con_name, con_forall, con_ex_tvs, con_mb_cxt,
                         con_doc = con_doc',
                         con_args = RecCon (L l_rec flds') }
    XConDecl x -> noExtCon x

{- Note [Trailing comment on constructor declaration]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The trailing comment after a constructor declaration is associated with the
constructor itself when there are no other comments inside the declaration:

   data T = MkT A B        -- ^ Comment on MkT
   data T = MkT { x :: A } -- ^ Comment on MkT

When there are other comments, the trailing comment applies to the last field:

   data T = MkT -- ^ Comment on MkT
            A   -- ^ Comment on A
            B   -- ^ Comment on B

   data T =
     MkT { a :: A   -- ^ Comment on a
         , b :: B   -- ^ Comment on b
         , c :: C } -- ^ Comment on c
-}

addHaddockConDeclField :: LConDeclField GhcPs -> HdkA (LConDeclField GhcPs)
addHaddockConDeclField (L l_fld fld) = HdkA (getBufSpan l_fld) $ do
  nextDocs <-
    inLocRange (locRangeTo (getBufPos (srcSpanStart l_fld))) $
    takeHdkComments getDocNext
  prevDocs <-
    inLocRange (locRangeFrom (getBufPos (srcSpanEnd l_fld))) $
    takeHdkComments getDocPrev
  let cd_fld_doc = concatLHsDocString (nextDocs ++ prevDocs)
  return $ L l_fld $ case fld of
    ConDeclField { cd_fld_ext, cd_fld_names, cd_fld_type } ->
      ConDeclField { cd_fld_ext, cd_fld_names, cd_fld_type, cd_fld_doc }
    XConDeclField x -> noExtCon x

addHaddockWildCardBndrs
  :: (a -> HdkA a)
  -> HsWildCardBndrs GhcPs a
  -> HdkA (HsWildCardBndrs GhcPs a)
addHaddockWildCardBndrs f (HsWC _ t) = HsWC noExtField <$> f t
addHaddockWildCardBndrs _ (XHsWildCardBndrs x) = noExtCon x

addHaddockImplicitBndrs
  :: (a -> HdkA a)
  -> HsImplicitBndrs GhcPs a
  -> HdkA (HsImplicitBndrs GhcPs a)
addHaddockImplicitBndrs f (HsIB _ t) = HsIB noExtField <$> f t
addHaddockImplicitBndrs _ (XHsImplicitBndrs x) = noExtCon x

addHaddockSigType :: LHsSigType GhcPs -> HdkA (LHsSigType GhcPs)
addHaddockSigType = addHaddockImplicitBndrs addHaddockType

addHaddockSigWcType :: LHsSigWcType GhcPs -> HdkA (LHsSigWcType GhcPs)
addHaddockSigWcType = addHaddockWildCardBndrs addHaddockSigType

addHaddockType :: LHsType GhcPs -> HdkA (LHsType GhcPs)
addHaddockType (L l_t (HsForAllTy _ fvf bndrs body)) =
  delimitHdkA l_t $ do
    body' <- addHaddockType body
    pure (L l_t (HsForAllTy noExtField fvf bndrs body'))
addHaddockType (L l_t (HsQualTy _ lhs rhs)) =
  delimitHdkA l_t $ do
    rhs' <- addHaddockType rhs
    pure (L l_t (HsQualTy noExtField lhs rhs'))
addHaddockType (L l_t (HsFunTy _ lhs rhs)) =
  delimitHdkA l_t $ do
    lhs' <- addHaddockType lhs
    rhs' <- addHaddockType rhs
    pure (L l_t (HsFunTy noExtField lhs' rhs'))
addHaddockType t@(L l_t _) =
  HdkA (getBufSpan l_t) $ do
    nextDocs <-
      inLocRange (locRangeTo (getBufPos (srcSpanStart l_t))) $
      takeHdkComments getDocNext
    prevDocs <-
      inLocRange (locRangeFrom (getBufPos (srcSpanEnd l_t))) $
      takeHdkComments getDocPrev
    let mDoc = concatLHsDocString (nextDocs ++ prevDocs)
    return $ mkLHsDocTyMaybe t mDoc

data LowerLocBound = StartOfFile | StartLoc BufPos

instance Semigroup LowerLocBound where
  StartOfFile <> l = l
  l <> StartOfFile = l
  StartLoc l1 <> StartLoc l2 = StartLoc (max l1 l2)

instance Monoid LowerLocBound where
  mempty = StartOfFile

data UpperLocBound = EndOfFile | EndLoc BufPos

instance Semigroup UpperLocBound where
  EndOfFile <> l = l
  l <> EndOfFile = l
  EndLoc l1 <> EndLoc l2 = EndLoc (min l1 l2)

instance Monoid UpperLocBound where
  mempty = EndOfFile

newtype ColumnBound = ColumnFrom Int -- n >= 1

instance Semigroup ColumnBound where
  ColumnFrom n <> ColumnFrom m = ColumnFrom (max n m)

instance Monoid ColumnBound where
  mempty = ColumnFrom 1

-- | A location range for extracting documentation comments.
data LocRange =
  LocRange
    LowerLocBound  -- from
    UpperLocBound  -- to
    ColumnBound

instance Semigroup LocRange where
  LocRange from1 to1 col1 <> LocRange from2 to2 col2 =
    LocRange (from1 <> from2) (to1 <> to2) (col1 <> col2)

instance Monoid LocRange where
  mempty = LocRange mempty mempty mempty

locRangeFrom :: Maybe BufPos -> LocRange
locRangeFrom (Just l) = LocRange (StartLoc l) EndOfFile mempty
locRangeFrom Nothing = mempty

locRangeTo :: Maybe BufPos -> LocRange
locRangeTo (Just l) = LocRange StartOfFile (EndLoc l) mempty
locRangeTo Nothing = mempty

inLocRange :: LocRange -> HdkM a -> HdkM a
inLocRange r m = mkHdkM $ \range -> unHdkM m (r <> range)

-- | The state monad but without newtype wrapping/unwrapping.
type InlineState s a = s -> (a, s)

-- Take the Haddock comments that satisfy the matching function,
-- leaving the rest pending.
takeHdkComments :: forall a. (PsLocated HdkComment -> Maybe a) -> HdkM [a]
takeHdkComments f =
  mkHdkM $ \range ->
  case range of
    LocRange hdk_from hdk_to hdk_col ->
      zoom_column hdk_col $
      zoom_after hdk_from $
      zoom_before hdk_to $
      foldr add_comment ([], [])
  where
    add_comment
      :: PsLocated HdkComment
      -> ([a], [PsLocated HdkComment])
      -> ([a], [PsLocated HdkComment])
    add_comment hdk_comment (items, other_hdk_comments) =
      case f hdk_comment of
        Just item -> (item : items, other_hdk_comments)
        Nothing -> (items, hdk_comment : other_hdk_comments)

    zoom_after
      :: LowerLocBound
      -> InlineState [PsLocated e] x
      -> InlineState [PsLocated e] x
    zoom_after StartOfFile m = m
    zoom_after (StartLoc l) m =
      \comments ->
        let
          is_after (L l_comment _) = bufSpanStart (psBufSpan l_comment) >= l
          (comments_before, comments_after) = break is_after comments
          (result, other_comments) = m comments_after
        in
          -- 'comments_before' will typically include only incorrectly
          -- positioned comments, so the concatenation cost is small.
          (result, comments_before ++ other_comments)

    zoom_before
      :: UpperLocBound
      -> InlineState [PsLocated e] x
      -> InlineState [PsLocated e] x
    zoom_before EndOfFile m = m
    zoom_before (EndLoc l) m =
      \comments ->
        let
          is_before (L l_comment _) = bufSpanStart (psBufSpan l_comment) <= l
          (comments_before, comments_after) = span is_before comments
          (result, other_comments) = m comments_before
        in
          -- 'other_comments' will typically include only incorrectly
          -- positioned comments, so the concatenation cost is small.
          (result, other_comments ++ comments_after)

    zoom_column
      :: ColumnBound
      -> InlineState [PsLocated e] x
      -> InlineState [PsLocated e] x
    zoom_column (ColumnFrom n) m =
      \comments ->
        let
          is_indented (L l_comment _) = srcSpanStartCol (psRealSpan l_comment) >= n
          (comments_indented, comments_not_indented) = span is_indented comments
          (result, other_comments) = m comments_indented
        in
          -- 'other_comments' will typically include only incorrectly
          -- positioned comments, so the concatenation cost is small.
          (result, other_comments ++ comments_not_indented)

-- | Peek at the Haddock comments that satisfy the matching function. Unlike
-- 'takeHdkComments', leave them pending.
peekHdkComments :: (PsLocated HdkComment -> Maybe a) -> HdkM [a]
peekHdkComments f =
  mkHdkM $ \range comments ->
    let (r, _) = unHdkM (takeHdkComments f) range comments
    in (r, comments)

mkLHsDocTy :: LHsType GhcPs -> LHsDocString -> LHsType GhcPs
mkLHsDocTy t doc =
  let loc = getLoc t `combineSrcSpans` getLoc doc
  in L loc (HsDocTy noExtField t doc)

mkLHsDocTyMaybe :: LHsType GhcPs -> Maybe LHsDocString -> LHsType GhcPs
mkLHsDocTyMaybe t = maybe t (mkLHsDocTy t)

-- -----------------------------------------------------------------------------
-- Adding documentation to record fields (used in parsing).

addFieldDoc :: LConDeclField a -> Maybe LHsDocString -> LConDeclField a
addFieldDoc (L l fld) doc
  = L l (fld { cd_fld_doc = cd_fld_doc fld `mplus` doc })

addFieldDocs :: [LConDeclField a] -> Maybe LHsDocString -> [LConDeclField a]
addFieldDocs [] _ = []
addFieldDocs (x:xs) doc = addFieldDoc x doc : xs


addConDoc :: LConDecl a -> Maybe LHsDocString -> LConDecl a
addConDoc decl    Nothing = decl
addConDoc (L p c) doc     = L p ( c { con_doc = con_doc c `mplus` doc } )

addConDocs :: [LConDecl a] -> Maybe LHsDocString -> [LConDecl a]
addConDocs [] _ = []
addConDocs [x] doc = [addConDoc x doc]
addConDocs (x:xs) doc = x : addConDocs xs doc

addConDocFirst :: [LConDecl a] -> Maybe LHsDocString -> [LConDecl a]
addConDocFirst [] _ = []
addConDocFirst (x:xs) doc = addConDoc x doc : xs
