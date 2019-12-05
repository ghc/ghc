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
   on 'BufPos' (in GHC.Types.SrcLoc) for the details.

2. In earlier versions of GHC, the Haddock comments were incorporated into the
   Parser.y grammar. The parser constructed the AST and attached comments to it in
   a single pass. See Note [Old solution: Haddock in the grammar] for the details.
-}
module GHC.Parser.PostProcess.Haddock (addHaddockToModule) where

import GHC.Prelude hiding (mod)

import GHC.Hs
import GHC.Types.SrcLoc
import GHC.Driver.Session ( WarningFlag(..) )
import GHC.Utils.Outputable hiding ( (<>) )
import GHC.Data.Bag

import Data.Semigroup
import Data.Foldable
import Data.Traversable
import Data.Maybe
import Control.Monad
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Data.Functor.Identity
import Data.Coerce

import GHC.Parser.Lexer
import GHC.Utils.Misc (mergeListsBy, filterOut, mapLastM)

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
      initial_hdk_st = HdkSt all_comments []
      (lmod', final_hdk_st) = runHdkA (addHaddock lmod) initial_hdk_st
      hdk_errors = collectHdkErrors final_hdk_st
        -- lmod':      module with Haddock comments inserted into the AST
        -- hdk_errors: errors accumulated during AST/comment processing
  mapM_ reportHdkError hdk_errors
  return lmod'
  where
    reportHdkError :: HdkErr -> P ()
    reportHdkError (HdkErrInvalidComment (L l _)) =
      addWarning Opt_WarnInvalidHaddock (mkSrcSpanPs l) $
        text "A Haddock comment cannot appear in this position and will be ignored."
    reportHdkError (HdkErrExtraComment (L l _)) =
      addWarning Opt_WarnInvalidHaddock l $
        text "Multiple Haddock comments for a single entity are not allowed." $$
        text "The extraneous comment will be ignored."

collectHdkErrors :: HdkSt -> [HdkErr]
collectHdkErrors HdkSt{ hdk_st_pending, hdk_st_errors } =
  map HdkErrInvalidComment hdk_st_pending -- leftover Haddock comments not inserted into the AST
  ++ reverse hdk_st_errors

-- | The state monad but without newtype wrapping/unwrapping.
type InlineState s a = s -> (a, s)

-- | The state of HdkM.
data HdkSt =
  HdkSt
    { hdk_st_pending :: [PsLocated HdkComment]
        -- a list of pending (unassociated with an AST node)
        -- Haddock comments, sorted by location: in ascending order of the starting 'BufPos'
    , hdk_st_errors :: [HdkErr]
        -- errors accumulated in reverse order
    }

-- | Errors accumulated in HdkM.
data HdkErr
  = HdkErrInvalidComment (PsLocated HdkComment)
  | HdkErrExtraComment LHsDocString

-- | 'HdkM' without newtype wrapping/unwrapping.
type InlineHdkM a = LocRange -> HdkSt -> (a, HdkSt)

-- | The state of 'HdkM' contains a list of pending Haddock comments. We go
-- over the AST, looking up these comments using 'takeHdkComments' and removing
-- them from the state. The remaining, un-removed ones are ignored with a
-- warning (-Winvalid-haddock). Also, using a state means we never use the same
-- Haddock twice.
--
-- See Note [Adding Haddock comments to the syntax tree].
newtype HdkM a = HdkM (ReaderT LocRange (State HdkSt) a)
  deriving (Functor, Applicative, Monad)

mkHdkM :: InlineHdkM a -> HdkM a
unHdkM :: HdkM a -> InlineHdkM a
mkHdkM = coerce
unHdkM = coerce

-- See Note [Adding Haddock comments to the syntax tree].
--
-- 'HdkA' provides a way to propagate location information from surrounding
-- computations:
--
--   left_neighbour <*> HdkA inner_span inner_m <*> right_neighbour
--
-- Here, the following holds:
--
-- * the 'left_neighbour' will only see Haddock comments until 'bufSpanStart' of 'inner_span'
-- * the 'right_neighbour' will only see Haddock comments after 'bufSpanEnd' of 'inner_span'
-- * the 'inner_m' will only see Haddock comments between its 'left_neighbour' and its 'right_neighbour'
--
-- In other words, every computation:
--
--  * delimits the surrounding computations
--  * is delimited by the surrounding computation
--
--  Therefore, a 'HdkA' computation must be always considered in the context in
--  which it is used.
data HdkA a = HdkA (Maybe BufSpan) (HdkM a)
  deriving (Functor)

instance Applicative HdkA where
  pure a = liftHdkA (pure a)
  HdkA l1 m1 <*> HdkA l2 m2 =
    HdkA (l1 <> l2) (delim1 m1 <*> delim2 m2)
    where
      -- Delimit the LHS by the location information from the RHS
      delim1 = inLocRange (locRangeTo (fmap @Maybe bufSpanStart l2))
      -- Delimit the RHS by the location information from the LHS
      delim2 = inLocRange (locRangeFrom (fmap @Maybe bufSpanEnd l1))

runHdkA :: HdkA a -> HdkSt -> (a, HdkSt)
runHdkA (HdkA _ m) = unHdkM m mempty

-- Let the neighbours know about an item at this location.
-- See Note [Adding Haddock comments to the syntax tree].
registerLocHdkA :: SrcSpan -> HdkA ()
registerLocHdkA l = HdkA (getBufSpan l) (pure ())

-- Let the neighbours know about an item at this location.
-- See Note [Adding Haddock comments to the syntax tree].
registerHdkA :: Located a -> HdkA ()
registerHdkA a = registerLocHdkA (getLoc a)

-- Modify the action of a HdkA computation.
hoistHdkA :: (HdkM a -> HdkM b) -> HdkA a -> HdkA b
hoistHdkA f (HdkA l m) = HdkA l (f m)

-- Lift a HdkM computation to HdkA.
liftHdkA :: HdkM a -> HdkA a
liftHdkA = HdkA mempty

-- Extend the declared location span of a 'HdkA' computation:
--
--    left_neighbour <*> extendHdkA l x <*> right_neighbour
--
-- The declared location of 'x' now includes 'l', so that the surrounding
-- computations 'left_neighbour' and 'right_neighbour' will not look for
-- Haddock comments inside the 'l' location span.
extendHdkA :: SrcSpan -> HdkA a -> HdkA a
extendHdkA l' (HdkA l m) = HdkA (getBufSpan l' <> l) m

appendHdkError :: HdkErr -> HdkM ()
appendHdkError e = HdkM (ReaderT (\_ -> modify append_err))
  where
    append_err hdk_st = hdk_st { hdk_st_errors = e : hdk_st_errors hdk_st }

selectDocString :: [LHsDocString] -> HdkM (Maybe LHsDocString)
selectDocString = select . filterOut (isEmptyDocString . unLoc)
  where
    select [] = return Nothing
    select [doc] = return (Just doc)
    select (doc : extra_docs) = do
      reportExtraDocs extra_docs
      return (Just doc)

reportExtraDocs :: [LHsDocString] -> HdkM ()
reportExtraDocs =
  traverse_ (\extra_doc -> appendHdkError (HdkErrExtraComment extra_doc))

-- See Note [Adding Haddock comments to the syntax tree].
class HasHaddock a where
  addHaddock :: a -> HdkA a

instance HasHaddock a => HasHaddock [a] where
  addHaddock = traverse addHaddock

instance HasHaddock (Located HsModule) where
  addHaddock (L l_mod mod) = do
    headerDocs <-
      for @Maybe (hsmodName mod) $ \(L l_name _) ->
      extendHdkA l_name $ liftHdkA $ do
        -- todo: register keyword location of 'module', see Note [Register keyword location]
        docs <-
          inLocRange (locRangeTo (getBufPos (srcSpanStart l_name))) $
          takeHdkComments mkDocNext
        selectDocString docs
    hsmodExports' <- traverse @Maybe addHaddock (hsmodExports mod)
    traverse_ registerHdkA (hsmodImports mod)
    let layout_info = hsmodLayout mod
    hsmodDecls' <- addHaddockInterleaveItems layout_info (mkDocHsDecl layout_info) (hsmodDecls mod)
    pure $ L l_mod $
      mod { hsmodExports = hsmodExports'
          , hsmodDecls = hsmodDecls'
          , hsmodHaddockModHeader = join @Maybe headerDocs }

-- Only for module exports, not module imports.
--
--    module M (a, b, c) where   -- use on this [LIE GhcPs]
--    import I (a, b, c)         -- do not use here!
--
-- Imports cannot have documentation comments anyway.
instance HasHaddock (Located [LIE GhcPs]) where
  addHaddock (L l_exports exports) =
    extendHdkA l_exports $ do
      exports' <- addHaddockInterleaveItems NoLayoutInfo mkDocIE exports
      registerLocHdkA (srcLocSpan (srcSpanEnd l_exports)) -- Do not consume comments after the closing parenthesis
      pure $ L l_exports exports'

-- Needed to use 'addHaddockInterleaveItems' in 'instance HasHaddock (Located [LIE GhcPs])'.
instance HasHaddock (LIE GhcPs) where
  addHaddock a = a <$ registerHdkA a

{- Add Haddock items to a list of non-Haddock items.
Used to process export lists (with mkDocIE) and declarations (with mkDocHsDecl).

For example:

  module M where
    -- | Comment on D
    data D = MkD  -- ^ Comment on MkD
    data C = MkC  -- ^ Comment on MkC
    -- ^ Comment on C

In this case, we should produce four HsDecl items (pseudo-code):

  1. DocD (DocCommentNext "Comment on D")
  2. TyClD (DataDecl "D" ... [ConDeclH98 "MkD" ... (Just "Comment on MkD")])
  3. TyClD (DataDecl "C" ... [ConDeclH98 "MkC" ... (Just "Comment on MkC")])
  4. DocD (DocCommentPrev "Comment on C")

The inputs to addHaddockInterleaveItems are:

  * layout_info :: LayoutInfo

    In the example above, note that the indentation level inside the module is
    2 spaces. It would be represented as layout_info = VirtualBraces 2.

    It is used to delimit the search space for comments when processing
    declarations. Here, we restrict indentation levels to >=(2+1), so that when
    we look up comment on MkC, we get "Comment on MkC" but not "Comment on C".

  * get_doc_item :: PsLocated HdkComment -> Maybe a

    This is the function used to look up documentation comments.
    In the above example, get_doc_item = mkDocHsDecl layout_info,
    and it will produce the following parts of the output:

      DocD (DocCommentNext "Comment on D")
      DocD (DocCommentPrev "Comment on C")

  * The list of items. These are the declarations that will be annotated with
    documentation comments.

    Before processing:
       TyClD (DataDecl "D" ... [ConDeclH98 "MkD" ... Nothing])
       TyClD (DataDecl "C" ... [ConDeclH98 "MkC" ... Nothing])

    After processing:
       TyClD (DataDecl "D" ... [ConDeclH98 "MkD" ... (Just "Comment on MkD")])
       TyClD (DataDecl "C" ... [ConDeclH98 "MkC" ... (Just "Comment on MkC")])
-}
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
    go [] = liftHdkA (takeHdkComments get_doc_item)
    go (item : items) = do
      docItems <- liftHdkA (takeHdkComments get_doc_item)
      item' <- with_layout_info (addHaddock item)
      other_items <- go items
      pure $ docItems ++ item':other_items

    with_layout_info :: HdkA a -> HdkA a
    with_layout_info = case layout_info of
      NoLayoutInfo -> id
      ExplicitBraces -> id
      VirtualBraces n ->
        let loc_range = mempty { loc_range_col = ColumnFrom (n+1) }
        in hoistHdkA (inLocRange loc_range)

mkDocHsDecl :: LayoutInfo -> PsLocated HdkComment -> Maybe (LHsDecl GhcPs)
mkDocHsDecl layout_info a = mapLoc (DocD noExtField) <$> mkDocDecl layout_info a

mkDocDecl :: LayoutInfo -> PsLocated HdkComment -> Maybe LDocDecl
mkDocDecl layout_info (L l_comment hdk_comment)
  | indent_mismatch = Nothing
  | otherwise =
    Just $ L (mkSrcSpanPs l_comment) $
      case hdk_comment of
        HdkCommentNext doc -> DocCommentNext doc
        HdkCommentPrev doc -> DocCommentPrev doc
        HdkCommentNamed s doc -> DocCommentNamed s doc
        HdkCommentSection n doc -> DocGroup n doc
  where
    --  'indent_mismatch' checks if the documentation comment has the exact
    --  indentation level expected by the parent node.
    --
    --  For example, when extracting documentation comments between class
    --  method declarations, there are three cases to consider:
    --
    --  1. Indent matches (indent_mismatch=False):
    --         class C a where
    --           f :: a -> a
    --           -- ^ doc on f
    --
    --  2. Indented too much (indent_mismatch=True):
    --         class C a where
    --           f :: a -> a
    --             -- ^ indent mismatch
    --
    --  3. Indented too little (indent_mismatch=True):
    --         class C a where
    --           f :: a -> a
    --         -- ^ indent mismatch
    indent_mismatch = case layout_info of
      NoLayoutInfo -> False
      ExplicitBraces -> False
      VirtualBraces n -> n /= srcSpanStartCol (psRealSpan l_comment)

mkDocIE :: PsLocated HdkComment -> Maybe (LIE GhcPs)
mkDocIE (L l_comment hdk_comment) =
  case hdk_comment of
    HdkCommentSection n doc -> Just $ L l (IEGroup noExtField n doc)
    HdkCommentNamed s _doc -> Just $ L l (IEDocNamed noExtField s)
    HdkCommentNext doc -> Just $ L l (IEDoc noExtField doc)
    _ -> Nothing
  where l = mkSrcSpanPs l_comment

mkDocNext :: PsLocated HdkComment -> Maybe LHsDocString
mkDocNext (L l (HdkCommentNext doc)) = Just $ L (mkSrcSpanPs l) doc
mkDocNext _ = Nothing

mkDocPrev :: PsLocated HdkComment -> Maybe LHsDocString
mkDocPrev (L l (HdkCommentPrev doc)) = Just $ L (mkSrcSpanPs l) doc
mkDocPrev _ = Nothing

instance HasHaddock (LHsDecl GhcPs) where
  addHaddock ldecl =
    extendHdkA (getLoc ldecl) $
    traverse @Located addHaddock ldecl

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
        -- todo: register keyword location of 'where', see Note [Register keyword location]
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
        -- todo: register keyword location of 'where', see Note [Register keyword location]
        where_cls' <-
          addHaddockInterleaveItems tcdLayout (mkDocHsDecl tcdLayout) $
          flattenBindsAndSigs (tcdMeths, tcdSigs, tcdATs, tcdATDefs, [], [])
        pure $
          let (tcdMeths', tcdSigs', tcdATs', tcdATDefs', _, tcdDocs) = partitionBindsAndSigs where_cls'
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
    pure $ ForD noExtField decl'

  -- Other declarations
  addHaddock d = pure d

instance HasHaddock (HsDeriving GhcPs) where
  addHaddock lderivs =
    extendHdkA (getLoc lderivs) $
    traverse @Located addHaddock lderivs

instance HasHaddock (LHsDerivingClause GhcPs) where
  addHaddock lderiv =
    extendHdkA (getLoc lderiv) $
    for @Located lderiv $ \deriv ->
    case deriv of
      HsDerivingClause { deriv_clause_strategy, deriv_clause_tys } -> do
        let
          -- 'stock', 'anyclass', and 'newtype' strategies come
          -- before the clause types.
          --
          -- 'via' comes after.
          --
          (register_strategy_before, register_strategy_after) =
            case deriv_clause_strategy of
              Nothing -> (pure (), pure ())
              Just (L l (ViaStrategy _)) -> (registerLocHdkA l, pure ())
              Just (L l _) -> (registerLocHdkA l, pure ())
        register_strategy_before
        deriv_clause_tys' <-
          extendHdkA (getLoc deriv_clause_tys) $
          traverse @Located addHaddock deriv_clause_tys
        register_strategy_after
        pure HsDerivingClause
          { deriv_clause_ext = noExtField,
            deriv_clause_strategy,
            deriv_clause_tys = deriv_clause_tys' }

instance HasHaddock (LConDecl GhcPs) where
  addHaddock (L l_con_decl con_decl) =
    extendHdkA l_con_decl $
    case con_decl of
      ConDeclGADT { con_g_ext, con_names, con_forall, con_qvars, con_mb_cxt, con_args, con_res_ty } -> do
        con_doc' <- discardWriterT $ getConDoc (getLoc (head con_names))
        con_args' <-
          case con_args of
            PrefixCon ts -> PrefixCon <$> addHaddock ts
            RecCon (L l_rec flds) -> do
              flds' <- traverse (discardWriterT . addHaddockConDeclField) flds
              pure $ RecCon (L l_rec flds')
            InfixCon _ _ -> panic "ConDeclGADT InfixCon"
        con_res_ty' <- addHaddock con_res_ty
        pure $ L l_con_decl $
          ConDeclGADT { con_g_ext, con_names, con_forall, con_qvars, con_mb_cxt,
                        con_doc = con_doc',
                        con_args = con_args',
                        con_res_ty = con_res_ty' }
      ConDeclH98 { con_ext, con_name, con_forall, con_ex_tvs, con_mb_cxt, con_args } ->
        addConTrailingDoc (srcSpanEnd l_con_decl) $
        runWriterT $
        case con_args of
          PrefixCon ts -> do
            con_doc' <- getConDoc (getLoc con_name)
            ts' <- traverse addHaddockConDeclFieldTy ts
            pure $ L l_con_decl $
              ConDeclH98 { con_ext, con_name, con_forall, con_ex_tvs, con_mb_cxt,
                           con_doc = con_doc',
                           con_args = PrefixCon ts' }
          InfixCon t1 t2 -> do
            t1' <- addHaddockConDeclFieldTy t1
            con_doc' <- getConDoc (getLoc con_name)
            t2' <- addHaddockConDeclFieldTy t2
            pure $ L l_con_decl $
              ConDeclH98 { con_ext, con_name, con_forall, con_ex_tvs, con_mb_cxt,
                           con_doc = con_doc',
                           con_args = InfixCon t1' t2' }
          RecCon (L l_rec flds) -> do
            con_doc' <- getConDoc (getLoc con_name)
            flds' <- traverse addHaddockConDeclField flds
            pure $ L l_con_decl $
              ConDeclH98 { con_ext, con_name, con_forall, con_ex_tvs, con_mb_cxt,
                           con_doc = con_doc',
                           con_args = RecCon (L l_rec flds') }
      XConDecl (ConDeclGADTPrefixPs { con_gp_names, con_gp_ty }) -> do
        con_gp_doc' <- discardWriterT $ getConDoc (getLoc (head con_gp_names))
        con_gp_ty' <- addHaddock con_gp_ty
        pure $ L l_con_decl $
          XConDecl (ConDeclGADTPrefixPs
            { con_gp_names,
              con_gp_ty = con_gp_ty',
              con_gp_doc = con_gp_doc' })

discardWriterT :: Functor m => WriterT w m a -> m a
discardWriterT = fmap fst . runWriterT

getConDoc
  :: SrcSpan
  -> WriterT HasInnerDocs HdkA (Maybe LHsDocString)
getConDoc l =
  WriterT $ extendHdkA l $ liftHdkA $ do
    mDoc <- getPrevNextDoc l
    return (mDoc, HasInnerDocs (isJust mDoc))

addHaddockConDeclFieldTy
  :: HsScaled GhcPs (LHsType GhcPs)
  -> WriterT HasInnerDocs HdkA (HsScaled GhcPs (LHsType GhcPs))
addHaddockConDeclFieldTy (HsScaled mult (L l t)) =
  WriterT $ extendHdkA l $ liftHdkA $ do
    mDoc <- getPrevNextDoc l
    return (HsScaled mult (mkLHsDocTy (L l t) mDoc),
            HasInnerDocs (isJust mDoc))

addHaddockConDeclField
  :: LConDeclField GhcPs
  -> WriterT HasInnerDocs HdkA (LConDeclField GhcPs)
addHaddockConDeclField (L l_fld fld) =
  WriterT $ extendHdkA l_fld $ liftHdkA $ do
    cd_fld_doc <- getPrevNextDoc l_fld
    return (L l_fld (fld { cd_fld_doc }),
            HasInnerDocs (isJust cd_fld_doc))

-- See Note [Trailing comment on constructor declaration]
newtype HasInnerDocs = HasInnerDocs Bool

instance Semigroup HasInnerDocs where (<>) = coerce (||)
instance Monoid HasInnerDocs where mempty = coerce False

-- See Note [Trailing comment on constructor declaration]
addConTrailingDoc
  :: SrcLoc
  -> HdkA (LConDecl GhcPs, HasInnerDocs)
  -> HdkA (LConDecl GhcPs)
addConTrailingDoc l_sep = hoistHdkA $ \m -> do
  (L l con_decl, HasInnerDocs has_inner_docs) <-
    inLocRange (locRangeTo (getBufPos l_sep)) $
    m
  case con_decl of
    ConDeclH98{} -> do
      trailingDocs <-
        inLocRange (locRangeFrom (getBufPos l_sep)) $
        takeHdkComments mkDocPrev
      if null trailingDocs
      then return (L l con_decl)
      else do
        if has_inner_docs then do
          let mk_doc_ty ::       HsScaled GhcPs (LHsType GhcPs)
                        -> HdkM (HsScaled GhcPs (LHsType GhcPs))
              mk_doc_ty x@(HsScaled _ (L _ HsDocTy{})) =
                x <$ reportExtraDocs trailingDocs
              mk_doc_ty (HsScaled mult (L l' t)) = do
                doc <- selectDocString trailingDocs
                return $ HsScaled mult (mkLHsDocTy (L l' t) doc)
          let mk_doc_fld ::       LConDeclField GhcPs
                         -> HdkM (LConDeclField GhcPs)
              mk_doc_fld x@(L _ (ConDeclField { cd_fld_doc = Just _ })) =
                x <$ reportExtraDocs trailingDocs
              mk_doc_fld (L l' con_fld) = do
                doc <- selectDocString trailingDocs
                return $ L l' (con_fld { cd_fld_doc = doc })
          con_args' <- case con_args con_decl of
            x@(PrefixCon [])    -> x <$ reportExtraDocs trailingDocs
            x@(RecCon (L _ [])) -> x <$ reportExtraDocs trailingDocs
            PrefixCon ts -> PrefixCon <$> mapLastM mk_doc_ty ts
            InfixCon t1 t2 -> InfixCon t1 <$> mk_doc_ty t2
            RecCon (L l_rec flds) -> do
              flds' <- mapLastM mk_doc_fld flds
              return (RecCon (L l_rec flds'))
          return $ L l (con_decl { con_args = con_args' })
        else do
          con_doc' <- selectDocString (con_doc con_decl `mcons` trailingDocs)
          return $ L l (con_decl { con_doc = con_doc' })
    _ -> panic "addConTrailingDoc: non-H98 ConDecl"

-- cons an element to a list, if exists
mcons :: Maybe a -> [a] -> [a]
mcons = maybe id (:)

instance HasHaddock a => HasHaddock (HsScaled GhcPs a) where
  addHaddock (HsScaled mult a) = HsScaled mult <$> addHaddock a

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

addHaddockWildCardBndrs
  :: (a -> HdkA a)
  -> HsWildCardBndrs GhcPs a
  -> HdkA (HsWildCardBndrs GhcPs a)
addHaddockWildCardBndrs f (HsWC _ t) = HsWC noExtField <$> f t

addHaddockImplicitBndrs
  :: (a -> HdkA a)
  -> HsImplicitBndrs GhcPs a
  -> HdkA (HsImplicitBndrs GhcPs a)
addHaddockImplicitBndrs f (HsIB _ t) = HsIB noExtField <$> f t

instance HasHaddock (LHsSigWcType GhcPs) where
  addHaddock = addHaddockWildCardBndrs addHaddock

instance HasHaddock (LHsSigType GhcPs) where
  addHaddock = addHaddockImplicitBndrs addHaddock

instance HasHaddock (LHsType GhcPs) where
  addHaddock (L l t) =
    extendHdkA l $
    case t of
      HsForAllTy _ tele body -> do
        registerLocHdkA (getForAllTeleLoc tele)
        body' <- addHaddock body
        pure $ L l (HsForAllTy noExtField tele body')
      HsQualTy _ lhs rhs -> do
        registerHdkA lhs
        rhs' <- addHaddock rhs
        pure $ L l (HsQualTy noExtField lhs rhs')
      HsFunTy _ mult lhs rhs -> do
        lhs' <- addHaddock lhs
        rhs' <- addHaddock rhs
        pure $ L l (HsFunTy noExtField mult lhs' rhs')
      _ -> liftHdkA $ do
        mDoc <- getPrevNextDoc l
        return (mkLHsDocTy (L l t) mDoc)

getPrevNextDoc :: SrcSpan -> HdkM (Maybe LHsDocString)
getPrevNextDoc l = do
  let (l_start, l_end) = (srcSpanStart l, srcSpanEnd l)
      before_t = locRangeTo (getBufPos l_start)
      after_t = locRangeFrom (getBufPos l_end)
  nextDocs <- inLocRange before_t $ takeHdkComments mkDocNext
  prevDocs <- inLocRange after_t $ takeHdkComments mkDocPrev
  selectDocString (nextDocs ++ prevDocs)

mkLHsDocTy :: LHsType GhcPs -> Maybe LHsDocString -> LHsType GhcPs
mkLHsDocTy t Nothing = t
mkLHsDocTy t (Just doc) = L (getLoc t) (HsDocTy noExtField t doc)

getForAllTeleLoc :: HsForAllTelescope GhcPs -> SrcSpan
getForAllTeleLoc tele =
  foldr combineSrcSpans noSrcSpan $
  case tele of
    HsForAllVis{ hsf_vis_bndrs } -> map getLoc hsf_vis_bndrs
    HsForAllInvis { hsf_invis_bndrs } -> map getLoc hsf_invis_bndrs

-- | Represents a predicate on BufPos:
--
--   LowerLocBound |   BufPos -> Bool
--   --------------+-----------------
--   StartOfFile   |   const True
--   StartLoc p    |   (>= p)
--
--  The semigroup instance corresponds to (&&).
--
--  We don't use the  BufPos -> Bool  representation
--  as it would lead to redundant checks.
--
--  That is, instead of
--
--      (pos >= 20) && (pos >= 30) && (pos >= 40)
--
--  We'd rather only do the (>=40) check. So we reify the predicate to make
--  sure we only check for the most restrictive bound.
data LowerLocBound = StartOfFile | StartLoc BufPos

instance Semigroup LowerLocBound where
  StartOfFile <> l = l
  l <> StartOfFile = l
  StartLoc l1 <> StartLoc l2 = StartLoc (max l1 l2)

instance Monoid LowerLocBound where
  mempty = StartOfFile

-- | Represents a predicate on BufPos:
--
--   UpperLocBound |   BufPos -> Bool
--   --------------+-----------------
--   EndOfFile     |   const True
--   EndLoc p      |   (<= p)
--
--  The semigroup instance corresponds to (&&).
--
--  We don't use the  BufPos -> Bool  representation
--  as it would lead to redundant checks.
--
--  That is, instead of
--
--      (pos <= 40) && (pos <= 30) && (pos <= 20)
--
--  We'd rather only do the (<=20) check. So we reify the predicate to make
--  sure we only check for the most restrictive bound.
data UpperLocBound = EndOfFile | EndLoc BufPos

instance Semigroup UpperLocBound where
  EndOfFile <> l = l
  l <> EndOfFile = l
  EndLoc l1 <> EndLoc l2 = EndLoc (min l1 l2)

instance Monoid UpperLocBound where
  mempty = EndOfFile

-- | Represents a predicate on the column number.
--
--   ColumnBound   |   Int -> Bool
--   --------------+-----------------
--   ColumnFrom n  |   (>=n)
--
--  The semigroup instance corresponds to (&&).
--
newtype ColumnBound = ColumnFrom Int -- n >= 1

instance Semigroup ColumnBound where
  ColumnFrom n <> ColumnFrom m = ColumnFrom (max n m)

instance Monoid ColumnBound where
  mempty = ColumnFrom 1

-- | A location range for extracting documentation comments.
data LocRange =
  LocRange
    { loc_range_from :: LowerLocBound,
      loc_range_to   :: UpperLocBound,
      loc_range_col  :: ColumnBound }

instance Semigroup LocRange where
  LocRange from1 to1 col1 <> LocRange from2 to2 col2 =
    LocRange (from1 <> from2) (to1 <> to2) (col1 <> col2)

instance Monoid LocRange where
  mempty = LocRange mempty mempty mempty

-- The location range from the specified position to the end of the file.
locRangeFrom :: Maybe BufPos -> LocRange
locRangeFrom (Just l) = mempty { loc_range_from = StartLoc l }
locRangeFrom Nothing = mempty

-- The location range from the start of the file to the specified position.
locRangeTo :: Maybe BufPos -> LocRange
locRangeTo (Just l) = mempty { loc_range_to = EndLoc l }
locRangeTo Nothing = mempty

-- Restrict the range in which a HdkM computation will look up comments:
--
--   inLocRange r1 $
--   inLocRange r2 $
--     takeHdkComments ...  -- Only takes comments in the (r1 <> r2) location range.
--
-- Note that it does not blindly override the range but tightens it using (<>).
-- At many use sites, you will see something along the lines of:
--
--   inLocRange (locRangeTo end_pos) $ ...
--
-- And 'locRangeTo' defines a location range from the start of the file to
-- 'end_pos'. This does not mean that we now search for every comment from the
-- start of the file, as this restriction will be combined with other
-- restrictions. Somewhere up the callstack we might have:
--
--   inLocRange (locRangeFrom start_pos) $ ...
--
-- The net result is that the location range is delimited by 'start_pos' on
-- one side and by 'end_pos' on the other side.
--
-- In 'HdkA', every (<*>) may restrict the location range of its
-- subcomputations.
inLocRange :: LocRange -> HdkM a -> HdkM a
inLocRange r (HdkM m) = HdkM (local (mappend r) m)

-- Take the Haddock comments that satisfy the matching function,
-- leaving the rest pending.
takeHdkComments :: forall a. (PsLocated HdkComment -> Maybe a) -> HdkM [a]
takeHdkComments f =
  mkHdkM $ \range ->
  case range of
    LocRange hdk_from hdk_to hdk_col ->
      zoom_pending $
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

    zoom_pending
      :: InlineState [PsLocated HdkComment] x
      -> InlineState HdkSt x
    zoom_pending m =
      \hdk_st ->
          let (a, pending') = m (hdk_st_pending hdk_st)
          in (a, hdk_st { hdk_st_pending = pending' })

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
  -- 'cmpBufSpan' is safe here with the following assumptions:
  --
  -- * 'LHsDecl' produced by 'decl_cls' in Parser.y always have a 'BufSpan'
  -- * 'partitionBindsAndSigs' does not discard this 'BufSpan'
  mergeListsBy cmpBufSpan [
    map_l (\b -> ValD noExtField b) (bagToList all_bs),
    map_l (\s -> SigD noExtField s) all_ss,
    map_l (\t -> TyClD noExtField (FamDecl noExtField t)) all_ts,
    map_l (\tfi -> InstD noExtField (TyFamInstD noExtField tfi)) all_tfis,
    map_l (\dfi -> InstD noExtField (DataFamInstD noExtField dfi)) all_dfis,
    map_l (\d -> DocD noExtField d) all_docs
  ]
  where
    map_l :: (a -> b) -> [Located a] -> [Located b]
    map_l f = map (mapLoc f)

{- Note [Register keyword location]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
At the moment, 'addHaddock' erroneously associates some comments with
constructs that are separated by a keyword. For example:

    data Foo -- | Comment for MkFoo
      where MkFoo :: Foo

The issue stems from the lack of location information for keywords. We could
utilize API Annotations for this purpose, but not without modification. For
example, API Annotations operate on RealSrcSpan, whereas we need BufSpan.

Also, there's work towards making API Annotations available in-tree (not in
a separate Map), see #17638. This change should make the fix very easy (it
is not as easy with the current design).

See also testsuite/tests/haddock/should_compile_flag_haddock/T17544_kw.hs
-}
