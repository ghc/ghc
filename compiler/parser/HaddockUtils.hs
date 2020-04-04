{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{- | This module implements 'addHaddockToModule', which inserts Haddock
    comments accumulated during parsing into the AST (#17544).

We process Haddock comments in two phases:

1. Parse the program (via the Happy parser in `Parser.y`), generating
   an AST, and (quite separately) a list of all the Haddock comments
   found in the file. More precisely, the Haddock comments are
   accumulated in the `hdk_comments` field of the `PState`, the parser
   state (see Lexer.x):

     data PState = PState { ...
                          ,  hdk_comments :: [PsLocated HdkComment] }

   Each of these Haddock comments has a `PsSpan`, which gives the `BufPos` of
   the beginning and end of the Haddock comment.

2. Walk over the AST, attaching the Haddock comments to the correct
   parts of the tree. This step is called `addHaddockToModule`, and is
   implemented in this module.

   See Note [Adding Haddock comments to the syntax tree].

This apporach codifies an important principle:

  The presence or absence of a Haddock comment should never change the parsing
  of a program.

Alternative approaches that did not work properly:

1. Using 'RealSrcLoc' instead of 'BufPos'. This led to failures in presence
   of {-# LANGUAGE CPP #-} and other sources of line pragmas. See documentation
   on 'BufPos' (in basicTypes/SrcLoc.hs) for the details.

2. In earlier versions of GHC, the Haddock comments were incorporated into the
   Parser.y grammar. The parser constructed the AST and attached comments to it in
   a single pass. See Note [Old solution: Haddock in the grammar] for the details.
-}
module HaddockUtils (addHaddockToModule) where

import GhcPrelude

import GHC.Hs
import GHC.Types.SrcLoc
import GHC.Driver.Session ( WarningFlag(..) )
import Outputable hiding ( (<>) )
import Bag

import Data.Semigroup
import Data.Foldable
import Data.Traversable
import Control.Monad
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader
import Data.Functor.Identity
import Data.Coerce

import Lexer
import Util (mergeListsBy)

{- Note [Adding Haddock comments to the syntax tree]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'addHaddock' traverses the AST in concrete syntax order, building a computation
(represented by HdkA) that reconstructs the AST but with Haddock comments
inserted in appropriate positions:

  addHaddock :: HasHaddock a => a -> HdkA a

Consider this code example:

  f :: Int  -- ^ comment on argument
    -> Bool -- ^ comment on result

In the AST, the "Int" part of this snippet is represented like this
(pseudo-code):

  L (BufSpan 6 8) (HsTyVar "Int") :: LHsType GhcPs

And the comments are represented like this (pseudo-code):

  L (BufSpan 11 35) (HdkCommentPrev "comment on argument")
  L (BufSpan 46 69) (HdkCommentPrev "comment on result")

So when we are traversing the AST and 'addHaddock' is applied to HsTyVar "Int",
how does it know to associate it with "comment on argument" but not with
"comment on result"?

The trick is to look in the space between syntactic elements. In the example above,
the location range in which we search for HdkCommentPrev is as follows:

  f :: Int████████████████████████
   ████Bool -- ^ comment on result

We search for comments after  HsTyVar "Int"  and until the next syntactic
element, in this case  HsTyVar "Bool".

Ignoring the "->" allows us to accomodate alternative coding styles:

  f :: Int ->   -- ^ comment on argument
       Bool     -- ^ comment on result

Sometimes we also need to take indentation information into account.
Compare the following examples:

    class C a where
      f :: a -> Int
      -- ^ comment on f

    class C a where
      f :: a -> Int
    -- ^ comment on C

Notice how "comment on f" and "comment on C" differ only by indentation level.

Therefore, in order to know the location range in which the comments are applicable
to a syntactic elements, we need three nuggets of information:
  1. lower bound on the BufPos of a comment
  2. upper bound on the BufPos of a comment
  3. minimum indentation level of a comment

This information is represented by the 'LocRange' type.

In order to propagate this information, we have the 'HdkA' applicative.
'HdkA' is defined as follows:

  data HdkA a = HdkA (Maybe BufSpan) (HdkM a)

The first field contains a 'BufSpan', which represents the location
span taken by a syntactic element:

  addHaddock (L bufSpan ...) = HdkA (Just bufSpan) ...

The second field, 'HdkM', is a stateful computation that looks up Haddock
comments in the specified location range:

  HdkM a =
       LocRange                  -- The allowed location range
    -> [PsLocated HdkComment]    -- Unallocated comments
    -> (a,                       -- AST with comments inserted into it
        [PsLocated HdkComment])  -- Leftover comments

The 'Applicative' instance for 'HdkA' is defined in such a way that the
location range of every computation is defined by its neighbours:

  addHaddock aaa <*> addHaddock bbb <*> addHaddock ccc

Here, the 'LocRange' passed to the 'HdkM' computation of  addHaddock bbb
is determined by the BufSpan recorded in  addHaddock aaa  and  addHaddock ccc.

This is why it's important to traverse the AST in the order of the concrete
syntax. In the example above we assume that  aaa, bbb, ccc  are ordered by location:

  * getBufSpan (getLoc aaa) < getBufSpan (getLoc bbb)
  * getBufSpan (getLoc bbb) < getBufSpan (getLoc ccc)

Violation of this assumption would lead to bugs, and care must be taken to
traverse the AST correctly. For example, when dealing with class declarations,
we have to use 'flattenBindsAndSign' to traverse it in the correct order.
-}

-- | Add Haddock documentation accumulated in the parser state
-- to a parsed HsModule.
--
-- Reports badly positioned comments when -Winvalid-haddock is enabled.
addHaddockToModule :: Located HsModule -> P (Located HsModule)
addHaddockToModule lmod = do
  pState <- getPState
  let all_comments = reverse (hdk_comments pState)
      (lmod', ignored_comments) = runHdkA (addHaddock lmod) all_comments
        -- lmod':            module with Haddock comments inserted into the AST
        -- ignored_comments: leftover Haddock comments not inserted into the AST
  mapM_ reportHdkComment ignored_comments
  return lmod'
  where
    reportHdkComment :: PsLocated HdkComment -> P ()
    reportHdkComment (L l _) =
      addWarning Opt_WarnInvalidHaddock (mkSrcSpanPs l) $
        text "A Haddock comment cannot appear in this position and will be ignored."

-- | The state monad but without newtype wrapping/unwrapping.
type InlineState s a = s -> (a, s)

-- | 'HdkM' without newtype wrapping/unwrapping.
type InlineHdkM a = LocRange -> [PsLocated HdkComment] -> (a, [PsLocated HdkComment])

-- | The state of 'HdkM' is a list of pending (unassociated with an AST node)
-- Haddock comments, sorted by location: in ascending order of the starting 'BufPos'.
--
-- We go over the AST, looking up these comments using 'takeHdkComments' and
-- removing them from the state. The remaining, un-removed ones are ignored
-- with a warning (-Winvalid-haddock). Also, using a state means we never use
-- the same Haddock twice.
--
-- See Note [Adding Haddock comments to the syntax tree].
newtype HdkM a = HdkM (ReaderT LocRange (State [PsLocated HdkComment]) a)
  deriving (Functor, Applicative, Monad)

mkHdkM :: InlineHdkM a -> HdkM a
unHdkM :: HdkM a -> InlineHdkM a
mkHdkM = coerce
unHdkM = coerce

-- See Note [Adding Haddock comments to the syntax tree].
data HdkA a = HdkA (Maybe BufSpan) (HdkM a)
  deriving (Functor)

instance Applicative HdkA where
  pure a = HdkA mempty (pure a)
  HdkA l1 m1 <*> HdkA l2 m2 =
    HdkA (l1 <> l2) (delim1 m1 <*> delim2 m2)
    where
      delim1 = inLocRange (locRangeTo (fmap @Maybe bufSpanStart l2))
      delim2 = inLocRange (locRangeFrom (fmap @Maybe bufSpanEnd l1))

runHdkA :: HdkA a -> [PsLocated HdkComment] -> (a, [PsLocated HdkComment])
runHdkA (HdkA _ m) = unHdkM m mempty

-- | Let the neighbours know about an item at this location.
-- See Note [Adding Haddock comments to the syntax tree].
registerHdkA :: Located a -> HdkA ()
registerHdkA a = HdkA (getBufSpan (getLoc a)) (pure ())

delimitHdkA :: SrcSpan -> HdkA a -> HdkA a
delimitHdkA l' (HdkA l m) = HdkA (getBufSpan l' <> l) m

concatLHsDocString :: [LHsDocString] -> Maybe LHsDocString
concatLHsDocString xs = L l <$> concatDocs docs
  where l = foldr combineSrcSpans noSrcSpan locs
        locs = map getLoc xs
        docs = map unLoc xs

-- See Note [Adding Haddock comments to the syntax tree].
class HasHaddock a where
  addHaddock :: a -> HdkA a

instance HasHaddock a => HasHaddock [a] where
  addHaddock = traverse addHaddock

instance HasHaddock (Located HsModule) where
  addHaddock (L l_mod mod) = do
    headerDocs <-
      for @Maybe (hsmodName mod) $ \(L l_name _) ->
      HdkA (getBufSpan l_name) $ do
        docs <-
          inLocRange (locRangeTo (getBufPos (srcSpanStart l_name))) $
          takeHdkComments getDocNext
        pure $ concatLHsDocString docs
    hsmodExports' <- traverse @Maybe addHaddock (hsmodExports mod)
    traverse_ registerHdkA (hsmodImports mod)
    let layout_info = hsmodLayout mod
    hsmodDecls' <- addHaddockInterleaveItems layout_info (getDocDecl layout_info) (hsmodDecls mod)
    pure $ L l_mod $
      mod { hsmodExports = hsmodExports'
          , hsmodDecls = hsmodDecls'
          , hsmodHaddockModHeader = join @Maybe headerDocs }

instance HasHaddock (Located [LIE GhcPs]) where
  addHaddock (L l_exports exports) =
    delimitHdkA l_exports $ do
      exports' <- addHaddockInterleaveItems NoLayoutInfo getDocIE exports
      registerHdkA (L (srcLocSpan (srcSpanEnd l_exports)) ()) -- Do not conume comments after the closing parenthesis
      pure $ L l_exports exports'

instance HasHaddock (LIE GhcPs) where
  addHaddock a = a <$ registerHdkA a

-- Add Haddock items to a list of non-Haddock items.
-- Used to process export lists (with getDocIE) and declarations (with getDocDecl).
addHaddockInterleaveItems
  :: forall a.
     HasHaddock a
  => LayoutInfo
  -> (PsLocated HdkComment -> Maybe a) -- Get a documentation item
  -> [a]           -- Unprocessed (non-documentation) items
  -> HdkA [a]      -- Documentation items & processed non-documentation items
addHaddockInterleaveItems layout_info get_doc_item = go
  where
    go :: [a] -> HdkA [a]
    go [] = HdkA mempty (takeHdkComments get_doc_item)
    go (item : items) = do
      docItems <- HdkA mempty (takeHdkComments get_doc_item)
      item' <- with_layout_info (addHaddock item)
      other_items <- go items
      pure $ docItems ++ item':other_items

    with_layout_info :: HdkA a -> HdkA a
    with_layout_info = case layout_info of
      NoLayoutInfo -> id
      ExplicitBraces -> id
      VirtualBraces n ->
        \(HdkA l m) ->
          let loc_range = LocRange mempty mempty (ColumnFrom (n+1))
          in HdkA l (inLocRange loc_range m)

getDocDecl :: LayoutInfo -> PsLocated HdkComment -> Maybe (LHsDecl GhcPs)
getDocDecl layout_info a = mapLoc (DocD noExtField) <$> getDocDecl' layout_info a

getDocDecl' :: LayoutInfo -> PsLocated HdkComment -> Maybe LDocDecl
getDocDecl' layout_info (L l_comment hdk_comment)
  | indent_mismatch = Nothing
  | otherwise =
    Just $ L (mkSrcSpanPs l_comment) $
      case hdk_comment of
        HdkCommentNext doc -> DocCommentNext doc
        HdkCommentPrev doc -> DocCommentPrev doc
        HdkCommentNamed s doc -> DocCommentNamed s doc
        HdkCommentSection n doc -> DocGroup n doc
  where
    indent_mismatch = case layout_info of
      NoLayoutInfo -> False
      ExplicitBraces -> False
      VirtualBraces n -> n /= srcSpanStartCol (psRealSpan l_comment)

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

instance HasHaddock (LHsDecl GhcPs) where
  addHaddock ldecl =
    delimitHdkA (getLoc ldecl) $
    for @Located ldecl addHaddock

instance HasHaddock (HsDecl GhcPs) where
  -- Type signatures
  addHaddock (SigD _ (TypeSig _ names t)) = do
      traverse_ registerHdkA names
      t' <- addHaddock t
      pure (SigD noExtField (TypeSig noExtField names t'))

  -- Pattern synonym type signatures
  addHaddock (SigD _ (PatSynSig _ names t)) = do
    traverse_ registerHdkA names
    t' <- addHaddock t
    pure (SigD noExtField (PatSynSig noExtField names t'))

  -- Class method signatures
  addHaddock (SigD _ (ClassOpSig _ is_dflt names t)) = do
    traverse_ registerHdkA names
    t' <- addHaddock t
    pure (SigD noExtField (ClassOpSig noExtField is_dflt names t'))

  -- Data declarations
  addHaddock (TyClD _ decl)
    | DataDecl { tcdLName, tcdTyVars, tcdFixity, tcdDataDefn = defn } <- decl
    , HsDataDefn { dd_ND, dd_ctxt, dd_cType, dd_kindSig, dd_cons, dd_derivs } <- defn
    = do
        registerHdkA tcdLName
        traverse_ registerHdkA dd_kindSig
        dd_cons' <- addHaddock dd_cons
        dd_derivs' <- addHaddock dd_derivs
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
          in TyClD noExtField decl'

  -- Class declarations
  addHaddock (TyClD _ decl)
    | ClassDecl { tcdCExt = tcdLayout,
                  tcdCtxt, tcdLName, tcdTyVars, tcdFixity, tcdFDs,
                  tcdSigs, tcdMeths, tcdATs, tcdATDefs } <- decl
    = do
        registerHdkA tcdLName
        where_cls' <-
          addHaddockInterleaveItems tcdLayout (getDocDecl tcdLayout) $
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
          in TyClD noExtField decl'

  -- Data family instances
  addHaddock (InstD _ decl)
    | DataFamInstD { dfid_inst } <- decl
    , DataFamInstDecl { dfid_eqn } <- dfid_inst
    = do
      dfid_eqn' <- addHaddockImplicitBndrs (\fam_eqn -> case fam_eqn of
        FamEqn { feqn_tycon, feqn_bndrs, feqn_pats, feqn_fixity, feqn_rhs }
          | HsDataDefn { dd_ND, dd_ctxt, dd_cType, dd_kindSig, dd_cons, dd_derivs } <- feqn_rhs
          -> do
            registerHdkA feqn_tycon
            traverse_ registerHdkA dd_kindSig
            dd_cons' <- addHaddock dd_cons
            dd_derivs' <- addHaddock dd_derivs
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
      pure $ InstD noExtField (DataFamInstD {
        dfid_ext = noExtField,
        dfid_inst = DataFamInstDecl { dfid_eqn = dfid_eqn' } })

  -- Foreign imports
  addHaddock (ForD _ decl) = do
    decl' <-
      case decl of
        ForeignImport { fd_name, fd_sig_ty, fd_fi } -> do
          registerHdkA fd_name
          fd_sig_ty' <- addHaddock fd_sig_ty
          pure ForeignImport { fd_i_ext = noExtField,
                               fd_sig_ty = fd_sig_ty',
                               fd_name, fd_fi }
        ForeignExport { fd_name, fd_sig_ty, fd_fe } -> do
          registerHdkA fd_name
          fd_sig_ty' <- addHaddock fd_sig_ty
          pure ForeignExport { fd_e_ext = noExtField,
                               fd_sig_ty = fd_sig_ty',
                               fd_name, fd_fe }
        XForeignDecl x -> noExtCon x
    pure $ ForD noExtField decl'

  -- Other declarations
  addHaddock d = pure d

instance HasHaddock (HsDeriving GhcPs) where
  addHaddock lderivs =
    delimitHdkA (getLoc lderivs) $
    for @Located lderivs addHaddock

instance HasHaddock (LHsDerivingClause GhcPs) where
  addHaddock lderiv =
    delimitHdkA (getLoc lderiv) $
    for @Located lderiv $ \deriv ->
    case deriv of
      HsDerivingClause { deriv_clause_strategy, deriv_clause_tys } -> do
        traverse_ @Maybe registerHdkA deriv_clause_strategy
        deriv_clause_tys' <-
          delimitHdkA (getLoc deriv_clause_tys) $
          for @Located deriv_clause_tys addHaddock
        pure HsDerivingClause
          { deriv_clause_ext = noExtField,
            deriv_clause_strategy,
            deriv_clause_tys = deriv_clause_tys' }
      XHsDerivingClause x -> noExtCon x

instance HasHaddock (LConDecl GhcPs) where
  addHaddock (L l_con con) = HdkA (getBufSpan l_con) $ do
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
            PrefixCon ts -> PrefixCon <$> addHaddock ts
            RecCon (L l_rec flds) -> do
              flds' <- addHaddock flds
              pure $ RecCon (L l_rec flds')
            InfixCon _ _ -> panic "ConDeclGADT InfixCon"
        con_res_ty' <- addHaddock con_res_ty
        pure $ L l_con $
          ConDeclGADT { con_g_ext, con_names, con_forall, con_qvars, con_mb_cxt,
                        con_doc = con_doc',
                        con_args = con_args',
                        con_res_ty = con_res_ty' }
      ConDeclH98 { con_ext, con_name, con_forall, con_ex_tvs, con_mb_cxt, con_args } -> do
        case con_args of
          PrefixCon ts -> do
            con_doc' <- getConDoc con_name
            ts' <- addHaddock ts
            pure $ L l_con $
              ConDeclH98 { con_ext, con_name, con_forall, con_ex_tvs, con_mb_cxt,
                           con_doc = con_doc',
                           con_args = PrefixCon ts' }
          InfixCon t1 t2 -> do
            t1' <- addHaddock t1
            con_doc' <- getConDoc con_name
            t2' <- addHaddock t2
            pure $ L l_con $
              ConDeclH98 { con_ext, con_name, con_forall, con_ex_tvs, con_mb_cxt,
                           con_doc = con_doc',
                           con_args = InfixCon t1' t2' }
          RecCon (L l_rec flds) -> do
            con_doc' <- getConDoc con_name
            flds' <- addHaddock flds
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

instance HasHaddock (LConDeclField GhcPs) where
  addHaddock (L l_fld fld) =
    HdkA (getBufSpan l_fld) $ do
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

instance HasHaddock (LHsSigWcType GhcPs) where
  addHaddock = addHaddockWildCardBndrs addHaddock

instance HasHaddock (LHsSigType GhcPs) where
  addHaddock = addHaddockImplicitBndrs addHaddock

instance HasHaddock (LHsType GhcPs) where
  addHaddock ltype =
    delimitHdkA (getLoc ltype) $
    for @Located ltype $ \t ->
    case t of
      HsForAllTy _ fvf bndrs body -> do
        body' <- addHaddock body
        pure (HsForAllTy noExtField fvf bndrs body')
      HsQualTy _ lhs rhs -> do
        rhs' <- addHaddock rhs
        pure (HsQualTy noExtField lhs rhs')
      HsFunTy _ lhs rhs -> do
        lhs' <- addHaddock lhs
        rhs' <- addHaddock rhs
        pure (HsFunTy noExtField lhs' rhs')
      _ -> HdkA mempty $ do
        let l = getLoc ltype
            (l_start, l_end) = (srcSpanStart l, srcSpanEnd l)
            before_t = locRangeTo (getBufPos l_start)
            after_t = locRangeFrom (getBufPos l_end)
        nextDocs <- inLocRange before_t $ takeHdkComments getDocNext
        prevDocs <- inLocRange after_t $ takeHdkComments getDocPrev
        let mDoc = concatLHsDocString (nextDocs ++ prevDocs)
        return $ case mDoc of
          Nothing -> t
          Just doc -> HsDocTy noExtField ltype doc

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

-- Take the Haddock comments that satisfy the matching function,
-- leaving the rest pending.
takeHdkComments :: forall a. (PsLocated HdkComment -> Maybe a) -> HdkM [a]
takeHdkComments f =
  mkHdkM $ \range ->
  case range of
    LocRange hdk_from hdk_to hdk_col ->
      zoom_after hdk_from $
      zoom_before hdk_to $
      zoom_column hdk_col $
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

{- Note [Old solution: Haddock in the grammar]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the past, Haddock comments were incorporated into the grammar (Parser.y).
This led to excessive complexity and duplication.

For example, here's the grammar production for types without documentation:

  type : btype
       | btype '->' ctype

To support Haddock, we had to also maintain an additional grammar production
for types with documentation on function arguments and function result:

  typedoc : btype
          | btype docprev
          | docnext btype
          | btype '->'     ctypedoc
          | btype docprev '->' ctypedoc
          | docnext btype '->' ctypedoc

Sometimes handling documentation comments during parsing led to bugs (#17561),
and sometimes it simply made it hard to modify and extend the grammar.

Another issue was that sometimes Haddock would fail to parse code
that GHC could parse succesfully:

  class BadIndent where
    f :: a -> Int
  -- ^ comment
    g :: a -> Int

This declaration was accepted by ghc but rejected by ghc -haddock.
-}

-- | The inverse of 'partitionBindsAndSigs' that merges partitioned items back
-- into a flat list. Elements are put back into the order in which they
-- appeared in the original program before partitioning, using BufPos to order
-- them.
--
-- Precondition (unchecked): the input lists are already sorted.
flattenBindsAndSigs
  :: (LHsBinds GhcPs, [LSig GhcPs], [LFamilyDecl GhcPs],
      [LTyFamInstDecl GhcPs], [LDataFamInstDecl GhcPs], [LDocDecl])
  -> [LHsDecl GhcPs]
flattenBindsAndSigs (all_bs, all_ss, all_ts, all_tfis, all_dfis, all_docs) =
  mergeListsBy cmp [
    map_l (\b -> ValD noExtField b) (bagToList all_bs),
    map_l (\s -> SigD noExtField s) all_ss,
    map_l (\t -> TyClD noExtField (FamDecl noExtField t)) all_ts,
    map_l (\tfi -> InstD noExtField (TyFamInstD noExtField tfi)) all_tfis,
    map_l (\dfi -> InstD noExtField (DataFamInstD noExtField dfi)) all_dfis,
    map_l (\d -> DocD noExtField d) all_docs
  ]
  where
    cmp :: LHsDecl GhcPs -> LHsDecl GhcPs -> Ordering
    cmp (L (getBufSpan -> Just a) _) (L (getBufSpan -> Just b) _) =
      compare a b
    cmp _ _ = panic "flattenBindsAndSigs: HsDecl without BufSpan"

    map_l :: (a -> b) -> [Located a] -> [Located b]
    map_l f = map (mapLoc f)
