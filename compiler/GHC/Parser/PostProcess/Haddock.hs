{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

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

This approach codifies an important principle:

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
import GHC.Driver.Flags ( WarningFlag(..) )
import GHC.Utils.Panic
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
import qualified Data.Monoid

import GHC.Parser.Lexer
import GHC.Parser.Errors
import GHC.Utils.Misc (mergeListsBy, filterOut, mapLastM, (<&&>))

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

  HdkM a ≈
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
we have to use 'flattenBindsAndSigs' to traverse it in the correct order.
-}

-- | Add Haddock documentation accumulated in the parser state
-- to a parsed HsModule.
--
-- Reports badly positioned comments when -Winvalid-haddock is enabled.
addHaddockToModule :: Located HsModule -> P (Located HsModule)
addHaddockToModule lmod = do
  pState <- getPState
  let all_comments = toList (hdk_comments pState)
      initial_hdk_st = HdkSt all_comments []
      (lmod', final_hdk_st) = runHdkA (addHaddock lmod) initial_hdk_st
      hdk_warnings = collectHdkWarnings final_hdk_st
        -- lmod':        module with Haddock comments inserted into the AST
        -- hdk_warnings: warnings accumulated during AST/comment processing
  mapM_ reportHdkWarning hdk_warnings
  return lmod'

reportHdkWarning :: HdkWarn -> P ()
reportHdkWarning (HdkWarnInvalidComment (L l _)) =
  addWarning Opt_WarnInvalidHaddock $ WarnHaddockInvalidPos (mkSrcSpanPs l)
reportHdkWarning (HdkWarnExtraComment (L l _)) =
  addWarning Opt_WarnInvalidHaddock $ WarnHaddockIgnoreMulti l

collectHdkWarnings :: HdkSt -> [HdkWarn]
collectHdkWarnings HdkSt{ hdk_st_pending, hdk_st_warnings } =
  map HdkWarnInvalidComment hdk_st_pending -- leftover Haddock comments not inserted into the AST
  ++ hdk_st_warnings

{- *********************************************************************
*                                                                      *
*       addHaddock: a family of functions that processes the AST       *
*    in concrete syntax order, adding documentation comments to it     *
*                                                                      *
********************************************************************* -}

-- HasHaddock is a convenience class for overloading the addHaddock operation.
-- Alternatively, we could define a family of monomorphic functions:
--
--    addHaddockSomeTypeX    :: SomeTypeX    -> HdkA SomeTypeX
--    addHaddockAnotherTypeY :: AnotherTypeY -> HdkA AnotherTypeY
--    addHaddockOneMoreTypeZ :: OneMoreTypeZ -> HdkA OneMoreTypeZ
--
-- But having a single name for all of them is just easier to read, and makes it clear
-- that they all are of the form  t -> HdkA t  for some t.
--
-- If you need to handle a more complicated scenario that doesn't fit this
-- pattern, it's always possible to define separate functions outside of this
-- class, as is done in case of e.g. addHaddockConDeclField.
--
-- See Note [Adding Haddock comments to the syntax tree].
class HasHaddock a where
  addHaddock :: a -> HdkA a

instance HasHaddock a => HasHaddock [a] where
  addHaddock = traverse addHaddock

--    -- | Module header comment
--    module M (
--        -- * Export list comment
--        Item1,
--        Item2,
--        -- * Export list comment
--        item3,
--        item4
--      ) where
--
instance HasHaddock (Located HsModule) where
  addHaddock (L l_mod mod) = do
    -- Step 1, get the module header documentation comment:
    --
    --    -- | Module header comment
    --    module M where
    --
    -- Only do this when the module header exists.
    headerDocs <-
      for @Maybe (hsmodName mod) $ \(L l_name _) ->
      extendHdkA l_name $ liftHdkA $ do
        -- todo: register keyword location of 'module', see Note [Register keyword location]
        docs <-
          inLocRange (locRangeTo (getBufPos (srcSpanStart l_name))) $
          takeHdkComments mkDocNext
        selectDocString docs

    -- Step 2, process documentation comments in the export list:
    --
    --  module M (
    --        -- * Export list comment
    --        Item1,
    --        Item2,
    --        -- * Export list comment
    --        item3,
    --        item4
    --    ) where
    --
    -- Only do this when the export list exists.
    hsmodExports' <- traverse @Maybe addHaddock (hsmodExports mod)

    -- Step 3, register the import section to reject invalid comments:
    --
    --   import Data.Maybe
    --   -- | rejected comment (cannot appear here)
    --   import Data.Bool
    --
    traverse_ registerHdkA (hsmodImports mod)

    -- Step 4, process declarations:
    --
    --    module M where
    --      -- | Comment on D
    --      data D = MkD  -- ^ Comment on MkD
    --      data C = MkC  -- ^ Comment on MkC
    --      -- ^ Comment on C
    --
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
instance HasHaddock (LocatedL [LocatedA (IE GhcPs)]) where
  addHaddock (L l_exports exports) =
    extendHdkA (locA l_exports) $ do
      exports' <- addHaddockInterleaveItems NoLayoutInfo mkDocIE exports
      registerLocHdkA (srcLocSpan (srcSpanEnd (locA l_exports))) -- Do not consume comments after the closing parenthesis
      pure $ L l_exports exports'

-- Needed to use 'addHaddockInterleaveItems' in 'instance HasHaddock (Located [LIE GhcPs])'.
instance HasHaddock (LocatedA (IE GhcPs)) where
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

instance HasHaddock (LocatedA (HsDecl GhcPs)) where
  addHaddock ldecl =
    extendHdkA (getLocA ldecl) $
    traverse @LocatedA addHaddock ldecl

-- Process documentation comments *inside* a declaration, for example:
--
--    data T = MkT -- ^ Comment on MkT (inside DataDecl)
--    f, g
--      :: Int  -- ^ Comment on Int   (inside TypeSig)
--      -> Bool -- ^ Comment on Bool  (inside TypeSig)
--
-- Comments that relate to the entire declaration are processed elsewhere:
--
--    -- | Comment on T (not processed in this instance)
--    data T = MkT
--
--    -- | Comment on f, g (not processed in this instance)
--    f, g :: Int -> Bool
--    f = ...
--    g = ...
--
-- Such comments are inserted into the syntax tree as DocD declarations
-- by addHaddockInterleaveItems, and then associated with other declarations
-- in GHC.HsToCore.Docs (see DeclDocMap).
--
-- In this instance, we only process comments that relate to parts of the
-- declaration, not to the declaration itself.
instance HasHaddock (HsDecl GhcPs) where

  -- Type signatures:
  --
  --    f, g
  --      :: Int  -- ^ Comment on Int
  --      -> Bool -- ^ Comment on Bool
  --
  addHaddock (SigD _ (TypeSig x names t)) = do
      traverse_ registerHdkA names
      t' <- addHaddock t
      pure (SigD noExtField (TypeSig x names t'))

  -- Pattern synonym type signatures:
  --
  --    pattern MyPat
  --      :: Bool       -- ^ Comment on Bool
  --      -> Maybe Bool -- ^ Comment on Maybe Bool
  --
  addHaddock (SigD _ (PatSynSig x names t)) = do
    traverse_ registerHdkA names
    t' <- addHaddock t
    pure (SigD noExtField (PatSynSig x names t'))

  -- Class method signatures and default signatures:
  --
  --   class C x where
  --      method_of_c
  --        :: Maybe x -- ^ Comment on Maybe x
  --        -> IO ()   -- ^ Comment on IO ()
  --      default method_of_c
  --        :: Eq x
  --        => Maybe x -- ^ Comment on Maybe x
  --        -> IO ()   -- ^ Comment on IO ()
  --
  addHaddock (SigD _ (ClassOpSig x is_dflt names t)) = do
    traverse_ registerHdkA names
    t' <- addHaddock t
    pure (SigD noExtField (ClassOpSig x is_dflt names t'))

  -- Data/newtype declarations:
  --
  --   data T = MkT -- ^ Comment on MkT
  --            A   -- ^ Comment on A
  --            B   -- ^ Comment on B
  --
  --   data G where
  --     -- | Comment on MkG
  --     MkG :: A    -- ^ Comment on A
  --         -> B    -- ^ Comment on B
  --         -> G
  --
  --   newtype N = MkN { getN :: Natural }  -- ^ Comment on N
  --     deriving newtype (Eq  {- ^ Comment on Eq  N -})
  --     deriving newtype (Ord {- ^ Comment on Ord N -})
  --
  addHaddock (TyClD x decl)
    | DataDecl { tcdDExt, tcdLName, tcdTyVars, tcdFixity, tcdDataDefn = defn } <- decl
    = do
        registerHdkA tcdLName
        defn' <- addHaddock defn
        pure $
          TyClD x (DataDecl {
            tcdDExt,
            tcdLName, tcdTyVars, tcdFixity,
            tcdDataDefn = defn' })

  -- Class declarations:
  --
  --  class C a where
  --      -- | Comment on the first method
  --      first_method :: a -> Bool
  --      second_method :: a -> String
  --      -- ^ Comment on the second method
  --
  addHaddock (TyClD _ decl)
    | ClassDecl { tcdCExt = (x,tcdLayout),
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
              decl' = ClassDecl { tcdCExt = (x,tcdLayout)
                                , tcdCtxt, tcdLName, tcdTyVars, tcdFixity, tcdFDs
                                , tcdSigs = tcdSigs'
                                , tcdMeths = tcdMeths'
                                , tcdATs = tcdATs'
                                , tcdATDefs = tcdATDefs'
                                , tcdDocs }
          in TyClD noExtField decl'

  -- Data family instances:
  --
  --    data instance D Bool where ... (same as data/newtype declarations)
  --    data instance D Bool = ...     (same as data/newtype declarations)
  --
  addHaddock (InstD _ decl)
    | DataFamInstD { dfid_ext, dfid_inst } <- decl
    , DataFamInstDecl { dfid_eqn } <- dfid_inst
    = do
      dfid_eqn' <- case dfid_eqn of
        FamEqn { feqn_ext, feqn_tycon, feqn_bndrs, feqn_pats, feqn_fixity, feqn_rhs }
          -> do
            registerHdkA feqn_tycon
            feqn_rhs' <- addHaddock feqn_rhs
            pure $ FamEqn {
                feqn_ext,
                feqn_tycon, feqn_bndrs, feqn_pats, feqn_fixity,
                feqn_rhs = feqn_rhs' }
      pure $ InstD noExtField (DataFamInstD {
        dfid_ext,
        dfid_inst = DataFamInstDecl { dfid_eqn = dfid_eqn' } })

  -- Type synonyms:
  --
  --    type T = Int -- ^ Comment on Int
  --
  addHaddock (TyClD _ decl)
    | SynDecl { tcdSExt, tcdLName, tcdTyVars, tcdFixity, tcdRhs } <- decl
    = do
        registerHdkA tcdLName
        -- todo: register keyword location of '=', see Note [Register keyword location]
        tcdRhs' <- addHaddock tcdRhs
        pure $
          TyClD noExtField (SynDecl {
            tcdSExt,
            tcdLName, tcdTyVars, tcdFixity,
            tcdRhs = tcdRhs' })

  -- Foreign imports:
  --
  --    foreign import ccall unsafe
  --      o :: Float     -- ^ The input float
  --        -> IO Float  -- ^ The output float
  --
  addHaddock (ForD _ decl) = do
    registerHdkA (fd_name decl)
    fd_sig_ty' <- addHaddock (fd_sig_ty decl)
    pure $ ForD noExtField (decl{ fd_sig_ty = fd_sig_ty' })

  -- Other declarations
  addHaddock d = pure d

-- The right-hand side of a data/newtype declaration or data family instance.
instance HasHaddock (HsDataDefn GhcPs) where
  addHaddock defn@HsDataDefn{} = do

    -- Register the kind signature:
    --    data D :: Type -> Type        where ...
    --    data instance D Bool :: Type  where ...
    traverse_ @Maybe registerHdkA (dd_kindSig defn)
    -- todo: register keyword location of '=' or 'where', see Note [Register keyword location]

    -- Process the data constructors:
    --
    --    data T
    --      = MkT1 Int Bool  -- ^ Comment on MkT1
    --      | MkT2 Char Int  -- ^ Comment on MkT2
    --
    dd_cons' <- addHaddock (dd_cons defn)

    -- Process the deriving clauses:
    --
    --   newtype N = MkN Natural
    --     deriving (Eq  {- ^ Comment on Eq  N -})
    --     deriving (Ord {- ^ Comment on Ord N -})
    --
    dd_derivs' <- addHaddock (dd_derivs defn)

    pure $ defn { dd_cons = dd_cons',
                  dd_derivs = dd_derivs' }

-- Process the deriving clauses of a data/newtype declaration.
-- Not used for standalone deriving.
instance HasHaddock (Located [Located (HsDerivingClause GhcPs)]) where
  addHaddock lderivs =
    extendHdkA (getLoc lderivs) $
    traverse @Located addHaddock lderivs

-- Process a single deriving clause of a data/newtype declaration:
--
--  newtype N = MkN Natural
--    deriving newtype (Eq  {- ^ Comment on Eq  N -})
--    deriving (Ord {- ^ Comment on Ord N -}) via Down N
--
-- Not used for standalone deriving.
instance HasHaddock (Located (HsDerivingClause GhcPs)) where
  addHaddock lderiv =
    extendHdkA (getLoc lderiv) $
    for @Located lderiv $ \deriv ->
    case deriv of
      HsDerivingClause { deriv_clause_ext, deriv_clause_strategy, deriv_clause_tys } -> do
        let
          -- 'stock', 'anyclass', and 'newtype' strategies come
          -- before the clause types.
          --
          -- 'via' comes after.
          --
          -- See tests/.../T11768.hs
          (register_strategy_before, register_strategy_after) =
            case deriv_clause_strategy of
              Nothing -> (pure (), pure ())
              Just (L l (ViaStrategy _)) -> (pure (), registerLocHdkA l)
              Just (L l _) -> (registerLocHdkA l, pure ())
        register_strategy_before
        deriv_clause_tys' <- addHaddock deriv_clause_tys
        -- deriv_clause_tys' <-
        --   extendHdkA (getLocA deriv_clause_tys) $
        --   traverse @LocatedC addHaddock deriv_clause_tys
        register_strategy_after
        pure HsDerivingClause
          { deriv_clause_ext,
            deriv_clause_strategy,
            deriv_clause_tys = deriv_clause_tys' }

-- Process the types in a single deriving clause, which may come in one of the
-- following forms:
--
--    1. A singular type constructor:
--          deriving Eq -- ^ Comment on Eq
--
--    2. A list of comma-separated types surrounded by enclosing parentheses:
--          deriving ( Eq  -- ^ Comment on Eq
--                   , C a -- ^ Comment on C a
--                   )
instance HasHaddock (LocatedC (DerivClauseTys GhcPs)) where
  addHaddock (L l_dct dct) =
    extendHdkA (locA l_dct) $
    case dct of
      DctSingle x ty -> do
        ty' <- addHaddock ty
        pure $ L l_dct $ DctSingle x ty'
      DctMulti x tys -> do
        tys' <- addHaddock tys
        pure $ L l_dct $ DctMulti x tys'

-- Process a single data constructor declaration, which may come in one of the
-- following forms:
--
--    1. H98-syntax PrefixCon:
--          data T =
--            MkT    -- ^ Comment on MkT
--              Int  -- ^ Comment on Int
--              Bool -- ^ Comment on Bool
--
--    2. H98-syntax InfixCon:
--          data T =
--            Int   -- ^ Comment on Int
--              :+  -- ^ Comment on (:+)
--            Bool  -- ^ Comment on Bool
--
--    3. H98-syntax RecCon:
--          data T =
--            MkT { int_field :: Int,     -- ^ Comment on int_field
--                  bool_field :: Bool }  -- ^ Comment on bool_field
--
--    4. GADT-syntax PrefixCon:
--          data T where
--            -- | Comment on MkT
--            MkT :: Int  -- ^ Comment on Int
--                -> Bool -- ^ Comment on Bool
--                -> T
--
--    5. GADT-syntax RecCon:
--          data T where
--            -- | Comment on MkT
--            MkT :: { int_field :: Int,     -- ^ Comment on int_field
--                     bool_field :: Bool }  -- ^ Comment on bool_field
--                -> T
--
instance HasHaddock (LocatedA (ConDecl GhcPs)) where
  addHaddock (L l_con_decl con_decl) =
    extendHdkA (locA l_con_decl) $
    case con_decl of
      ConDeclGADT { con_g_ext, con_names, con_bndrs, con_mb_cxt, con_g_args, con_res_ty } -> do
        -- discardHasInnerDocs is ok because we don't need this info for GADTs.
        con_doc' <- discardHasInnerDocs $ getConDoc (getLocA (head con_names))
        con_g_args' <-
          case con_g_args of
            PrefixConGADT ts -> PrefixConGADT <$> addHaddock ts
            RecConGADT (L l_rec flds) -> do
              -- discardHasInnerDocs is ok because we don't need this info for GADTs.
              flds' <- traverse (discardHasInnerDocs . addHaddockConDeclField) flds
              pure $ RecConGADT (L l_rec flds')
        con_res_ty' <- addHaddock con_res_ty
        pure $ L l_con_decl $
          ConDeclGADT { con_g_ext, con_names, con_bndrs, con_mb_cxt,
                        con_doc = con_doc',
                        con_g_args = con_g_args',
                        con_res_ty = con_res_ty' }
      ConDeclH98 { con_ext, con_name, con_forall, con_ex_tvs, con_mb_cxt, con_args } ->
        addConTrailingDoc (srcSpanEnd $ locA l_con_decl) $
        case con_args of
          PrefixCon ts -> do
            con_doc' <- getConDoc (getLocA con_name)
            ts' <- traverse addHaddockConDeclFieldTy ts
            pure $ L l_con_decl $
              ConDeclH98 { con_ext, con_name, con_forall, con_ex_tvs, con_mb_cxt,
                           con_doc = con_doc',
                           con_args = PrefixCon ts' }
          InfixCon t1 t2 -> do
            t1' <- addHaddockConDeclFieldTy t1
            con_doc' <- getConDoc (getLocA con_name)
            t2' <- addHaddockConDeclFieldTy t2
            pure $ L l_con_decl $
              ConDeclH98 { con_ext, con_name, con_forall, con_ex_tvs, con_mb_cxt,
                           con_doc = con_doc',
                           con_args = InfixCon t1' t2' }
          RecCon (L l_rec flds) -> do
            con_doc' <- getConDoc (getLocA con_name)
            flds' <- traverse addHaddockConDeclField flds
            pure $ L l_con_decl $
              ConDeclH98 { con_ext, con_name, con_forall, con_ex_tvs, con_mb_cxt,
                           con_doc = con_doc',
                           con_args = RecCon (L l_rec flds') }

-- Keep track of documentation comments on the data constructor or any of its
-- fields.
--
-- See Note [Trailing comment on constructor declaration]
type ConHdkA = WriterT HasInnerDocs HdkA

-- Does the data constructor declaration have any inner (non-trailing)
-- documentation comments?
--
-- Example when HasInnerDocs is True:
--
--   data X =
--      MkX       -- ^ inner comment
--        Field1  -- ^ inner comment
--        Field2  -- ^ inner comment
--        Field3  -- ^ trailing comment
--
-- Example when HasInnerDocs is False:
--
--   data Y = MkY Field1 Field2 Field3  -- ^ trailing comment
--
-- See Note [Trailing comment on constructor declaration]
newtype HasInnerDocs = HasInnerDocs Bool
  deriving (Semigroup, Monoid) via Data.Monoid.Any

-- Run ConHdkA by discarding the HasInnerDocs info when we have no use for it.
--
-- We only do this when processing data declarations that use GADT syntax,
-- because only the H98 syntax declarations have special treatment for the
-- trailing documentation comment.
--
-- See Note [Trailing comment on constructor declaration]
discardHasInnerDocs :: ConHdkA a -> HdkA a
discardHasInnerDocs = fmap fst . runWriterT

-- Get the documentation comment associated with the data constructor in a
-- data/newtype declaration.
getConDoc
  :: SrcSpan  -- Location of the data constructor
  -> ConHdkA (Maybe LHsDocString)
getConDoc l =
  WriterT $ extendHdkA l $ liftHdkA $ do
    mDoc <- getPrevNextDoc l
    return (mDoc, HasInnerDocs (isJust mDoc))

-- Add documentation comment to a data constructor field.
-- Used for PrefixCon and InfixCon.
addHaddockConDeclFieldTy
  :: HsScaled GhcPs (LHsType GhcPs)
  -> ConHdkA (HsScaled GhcPs (LHsType GhcPs))
addHaddockConDeclFieldTy (HsScaled mult (L l t)) =
  WriterT $ extendHdkA (locA l) $ liftHdkA $ do
    mDoc <- getPrevNextDoc (locA l)
    return (HsScaled mult (mkLHsDocTy (L l t) mDoc),
            HasInnerDocs (isJust mDoc))

-- Add documentation comment to a data constructor field.
-- Used for RecCon.
addHaddockConDeclField
  :: LConDeclField GhcPs
  -> ConHdkA (LConDeclField GhcPs)
addHaddockConDeclField (L l_fld fld) =
  WriterT $ extendHdkA (locA l_fld) $ liftHdkA $ do
    cd_fld_doc <- getPrevNextDoc (locA l_fld)
    return (L l_fld (fld { cd_fld_doc }),
            HasInnerDocs (isJust cd_fld_doc))

-- 1. Process a H98-syntax data constructor declaration in a context with no
--    access to the trailing documentation comment (by running the provided
--    ConHdkA computation).
--
-- 2. Then grab the trailing comment (if it exists) and attach it where
--    appropriate: either to the data constructor itself or to its last field,
--    depending on HasInnerDocs.
--
-- See Note [Trailing comment on constructor declaration]
addConTrailingDoc
  :: SrcLoc  -- The end of a data constructor declaration.
             -- Any docprev comment past this point is considered trailing.
  -> ConHdkA (LConDecl GhcPs)
  -> HdkA (LConDecl GhcPs)
addConTrailingDoc l_sep =
    hoistHdkA add_trailing_doc . runWriterT
  where
    add_trailing_doc
      :: HdkM (LConDecl GhcPs, HasInnerDocs)
      -> HdkM (LConDecl GhcPs)
    add_trailing_doc m = do
      (L l con_decl, HasInnerDocs has_inner_docs) <-
        inLocRange (locRangeTo (getBufPos l_sep)) m
          -- inLocRange delimits the context so that the inner computation
          -- will not consume the trailing documentation comment.
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
                    -- Happens in the following case:
                    --
                    --    data T =
                    --      MkT
                    --        -- | Comment on SomeField
                    --        SomeField
                    --        -- ^ Another comment on SomeField? (rejected)
                    --
                    -- See tests/.../haddockExtraDocs.hs
                    x <$ reportExtraDocs trailingDocs
                  mk_doc_ty (HsScaled mult (L l' t)) = do
                    doc <- selectDocString trailingDocs
                    return $ HsScaled mult (mkLHsDocTy (L l' t) doc)
              let mk_doc_fld ::       LConDeclField GhcPs
                             -> HdkM (LConDeclField GhcPs)
                  mk_doc_fld x@(L _ (ConDeclField { cd_fld_doc = Just _ })) =
                    -- Happens in the following case:
                    --
                    --    data T =
                    --      MkT {
                    --        -- | Comment on SomeField
                    --        someField :: SomeField
                    --      } -- ^ Another comment on SomeField? (rejected)
                    --
                    -- See tests/.../haddockExtraDocs.hs
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
              return $ L l (con_decl{ con_args = con_args' })
            else do
              con_doc' <- selectDocString (con_doc con_decl `mcons` trailingDocs)
              return $ L l (con_decl{ con_doc = con_doc' })
        _ -> panic "addConTrailingDoc: non-H98 ConDecl"

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

This makes the trailing comment context-sensitive. Example:
      data T =
        -- | comment 1
        MkT Int Bool -- ^ comment 2

    Here, "comment 2" applies to the Bool field.
    But if we removed "comment 1", then "comment 2" would be apply to the data
    constructor rather than its field.

All of this applies to H98-style data declarations only.
GADTSyntax data constructors don't have any special treatment for the trailing comment.

We implement this in two steps:

  1. Process the data constructor declaration in a delimited context where the
     trailing documentation comment is not visible. Delimiting the context is done
     in addConTrailingDoc.

     When processing the declaration, track whether the constructor or any of
     its fields have a documentation comment associated with them.
     This is done using WriterT HasInnerDocs, see ConHdkA.

  2. Depending on whether HasInnerDocs is True or False, attach the
     trailing documentation comment to the data constructor itself
     or to its last field.
-}

instance HasHaddock a => HasHaddock (HsScaled GhcPs a) where
  addHaddock (HsScaled mult a) = HsScaled mult <$> addHaddock a

instance HasHaddock a => HasHaddock (HsWildCardBndrs GhcPs a) where
  addHaddock (HsWC _ t) = HsWC noExtField <$> addHaddock t

instance HasHaddock (LocatedA (HsSigType GhcPs)) where
  addHaddock (L l (HsSig{sig_bndrs = outer_bndrs, sig_body = body})) =
    extendHdkA (locA l) $ do
      case outer_bndrs of
        HsOuterImplicit{} -> pure ()
        HsOuterExplicit{hso_bndrs = bndrs} ->
          registerLocHdkA (getLHsTyVarBndrsLoc bndrs)
      body' <- addHaddock body
      pure $ L l $ HsSig noExtField outer_bndrs body'

-- Process a type, adding documentation comments to function arguments
-- and the result. Many formatting styles are supported.
--
--  my_function ::
--      forall a.
--      Eq a =>
--      Maybe a ->  -- ^ Comment on Maybe a  (function argument)
--      Bool ->     -- ^ Comment on Bool     (function argument)
--      String      -- ^ Comment on String   (the result)
--
--  my_function
--      :: forall a. Eq a
--      => Maybe a     -- ^ Comment on Maybe a  (function argument)
--      -> Bool        -- ^ Comment on Bool     (function argument)
--      -> String      -- ^ Comment on String   (the result)
--
--  my_function ::
--      forall a. Eq a =>
--      -- | Comment on Maybe a (function argument)
--      Maybe a ->
--      -- | Comment on Bool (function argument)
--      Bool ->
--      -- | Comment on String (the result)
--      String
--
-- This is achieved by simply ignoring (not registering the location of) the
-- function arrow (->).
instance HasHaddock (LocatedA (HsType GhcPs)) where
  addHaddock (L l t) =
    extendHdkA (locA l) $
    case t of

      -- forall a b c. t
      HsForAllTy x tele body -> do
        registerLocHdkA (getForAllTeleLoc tele)
        body' <- addHaddock body
        pure $ L l (HsForAllTy x tele body')

      -- (Eq a, Num a) => t
      HsQualTy x mlhs rhs -> do
        traverse_ registerHdkA mlhs
        rhs' <- addHaddock rhs
        pure $ L l (HsQualTy x mlhs rhs')

      -- arg -> res
      HsFunTy u mult lhs rhs -> do
        lhs' <- addHaddock lhs
        rhs' <- addHaddock rhs
        pure $ L l (HsFunTy u mult lhs' rhs')

      -- other types
      _ -> liftHdkA $ do
        mDoc <- getPrevNextDoc (locA l)
        return (mkLHsDocTy (L l t) mDoc)

{- *********************************************************************
*                                                                      *
*      HdkA: a layer over HdkM that propagates location information    *
*                                                                      *
********************************************************************* -}

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
--  * is delimited by the surrounding computations
--
--  Therefore, a 'HdkA' computation must be always considered in the context in
--  which it is used.
data HdkA a =
  HdkA
    !(Maybe BufSpan) -- Just b  <=> BufSpan occupied by the processed AST element.
                     --             The surrounding computations will not look inside.
                     --
                     -- Nothing <=> No BufSpan (e.g. when the HdkA is constructed by 'pure' or 'liftHdkA').
                     --             The surrounding computations are not delimited.

    !(HdkM a) -- The stateful computation that looks up Haddock comments and
              -- adds them to the resulting AST node.

  deriving (Functor)

instance Applicative HdkA where
  HdkA l1 m1 <*> HdkA l2 m2 =
    HdkA
      (l1 <> l2)  -- The combined BufSpan that covers both subcomputations.
                  --
                  -- The Semigroup instance for Maybe quite conveniently does the right thing:
                  --    Nothing <> b       = b
                  --    a       <> Nothing = a
                  --    Just a  <> Just b  = Just (a <> b)

      (delim1 m1 <*> delim2 m2) -- Stateful computations are run in left-to-right order,
                                -- without any smart reordering strategy. So users of this
                                -- operation must take care to traverse the AST
                                -- in concrete syntax order.
                                -- See Note [Smart reordering in HdkA (or lack of thereof)]
                                --
                                -- Each computation is delimited ("sandboxed")
                                -- in a way that it doesn't see any Haddock
                                -- comments past the neighbouring AST node.
                                -- These delim1/delim2 are key to how HdkA operates.
    where
      -- Delimit the LHS by the location information from the RHS
      delim1 = inLocRange (locRangeTo (fmap @Maybe bufSpanStart l2))
      -- Delimit the RHS by the location information from the LHS
      delim2 = inLocRange (locRangeFrom (fmap @Maybe bufSpanEnd l1))

  pure a =
    -- Return a value without performing any stateful computation, and without
    -- any delimiting effect on the surrounding computations.
    liftHdkA (pure a)

{- Note [Smart reordering in HdkA (or lack of thereof)]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When traversing the AST, the user must take care to traverse it in concrete
syntax order.

For example, when processing HsFunTy, it's important to get it right and write
it like so:

      HsFunTy _ mult lhs rhs -> do
        lhs' <- addHaddock lhs
        rhs' <- addHaddock rhs
        pure $ L l (HsFunTy noExtField mult lhs' rhs')

Rather than like so:

      HsFunTy _ mult lhs rhs -> do
        rhs' <- addHaddock rhs   -- bad! wrong order
        lhs' <- addHaddock lhs   -- bad! wrong order
        pure $ L l (HsFunTy noExtField mult lhs' rhs')

This is somewhat bug-prone, so we could try to fix this with some Applicative
magic. When we define (<*>) for HdkA, why not reorder the computations as
necessary? In pseudo-code:

  a1 <*> a2 | a1 `before` a2 = ... normal processing ...
            | otherwise      = a1 <**> a2

While this trick could work for any two *adjacent* AST elements out of order
(as in HsFunTy example above), it would fail in more elaborate scenarios (e.g.
processing a list of declarations out of order).

If it's not obvious why this trick doesn't work, ponder this: it's a bit like trying to get
a sorted list by defining a 'smart' concatenation operator in the following manner:

  a ?++ b | a <= b    = a ++ b
          | otherwise = b ++ a

At first glance it seems to work:

  ghci> [1] ?++ [2] ?++ [3]
  [1,2,3]

  ghci> [2] ?++ [1] ?++ [3]
  [1,2,3]                     -- wow, sorted!

But it actually doesn't:

  ghci> [3] ?++ [1] ?++ [2]
  [1,3,2]                     -- not sorted...
-}

-- Run a HdkA computation in an unrestricted LocRange. This is only used at the
-- top level to run the final computation for the entire module.
runHdkA :: HdkA a -> HdkSt -> (a, HdkSt)
runHdkA (HdkA _ m) = unHdkM m mempty

-- Let the neighbours know about an item at this location.
--
-- Consider this example:
--
--  class -- | peculiarly placed comment
--    MyClass a where
--        my_method :: a -> a
--
-- How do we know to reject the "peculiarly placed comment" instead of
-- associating it with my_method? Its indentation level matches.
--
-- But clearly, there's "MyClass a where" separating the comment and my_method.
-- To take it into account, we must register its location using registerLocHdkA
-- or registerHdkA.
--
-- See Note [Register keyword location].
-- See Note [Adding Haddock comments to the syntax tree].
registerLocHdkA :: SrcSpan -> HdkA ()
registerLocHdkA l = HdkA (getBufSpan l) (pure ())

-- Let the neighbours know about an item at this location.
-- A small wrapper over registerLocHdkA.
--
-- See Note [Adding Haddock comments to the syntax tree].
registerHdkA :: GenLocated (SrcSpanAnn' a) e -> HdkA ()
registerHdkA a = registerLocHdkA (getLocA a)

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


{- *********************************************************************
*                                                                      *
*              HdkM: a stateful computation to associate               *
*          accumulated documentation comments with AST nodes           *
*                                                                      *
********************************************************************* -}

-- The state of 'HdkM' contains a list of pending Haddock comments. We go
-- over the AST, looking up these comments using 'takeHdkComments' and removing
-- them from the state. The remaining, un-removed ones are ignored with a
-- warning (-Winvalid-haddock). Also, using a state means we never use the same
-- Haddock twice.
--
-- See Note [Adding Haddock comments to the syntax tree].
newtype HdkM a = HdkM (ReaderT LocRange (State HdkSt) a)
  deriving (Functor, Applicative, Monad)

-- | The state of HdkM.
data HdkSt =
  HdkSt
    { hdk_st_pending :: [PsLocated HdkComment]
        -- a list of pending (unassociated with an AST node)
        -- Haddock comments, sorted by location: in ascending order of the starting 'BufPos'
    , hdk_st_warnings :: [HdkWarn]
        -- accumulated warnings (order doesn't matter)
    }

-- | Warnings accumulated in HdkM.
data HdkWarn
  = HdkWarnInvalidComment (PsLocated HdkComment)
  | HdkWarnExtraComment LHsDocString

-- 'HdkM' without newtype wrapping/unwrapping.
type InlineHdkM a = LocRange -> HdkSt -> (a, HdkSt)

mkHdkM :: InlineHdkM a -> HdkM a
unHdkM :: HdkM a -> InlineHdkM a
mkHdkM = coerce
unHdkM = coerce

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
  mkHdkM $
    \(LocRange hdk_from hdk_to hdk_col) ->
    \hdk_st ->
      let
        comments = hdk_st_pending hdk_st
        (comments_before_range, comments') = break (is_after hdk_from) comments
        (comments_in_range, comments_after_range) = span (is_before hdk_to <&&> is_indented hdk_col) comments'
        (items, other_comments) = foldr add_comment ([], []) comments_in_range
        remaining_comments = comments_before_range ++ other_comments ++ comments_after_range
        hdk_st' = hdk_st{ hdk_st_pending = remaining_comments }
      in
        (items, hdk_st')
  where
    is_after    StartOfFile    _               = True
    is_after    (StartLoc l)   (L l_comment _) = bufSpanStart (psBufSpan l_comment) >= l
    is_before   EndOfFile      _               = True
    is_before   (EndLoc l)     (L l_comment _) = bufSpanStart (psBufSpan l_comment) <= l
    is_indented (ColumnFrom n) (L l_comment _) = srcSpanStartCol (psRealSpan l_comment) >= n

    add_comment
      :: PsLocated HdkComment
      -> ([a], [PsLocated HdkComment])
      -> ([a], [PsLocated HdkComment])
    add_comment hdk_comment (items, other_hdk_comments) =
      case f hdk_comment of
        Just item -> (item : items, other_hdk_comments)
        Nothing -> (items, hdk_comment : other_hdk_comments)

-- Get the docnext or docprev comment for an AST node at the given source span.
getPrevNextDoc :: SrcSpan -> HdkM (Maybe LHsDocString)
getPrevNextDoc l = do
  let (l_start, l_end) = (srcSpanStart l, srcSpanEnd l)
      before_t = locRangeTo (getBufPos l_start)
      after_t = locRangeFrom (getBufPos l_end)
  nextDocs <- inLocRange before_t $ takeHdkComments mkDocNext
  prevDocs <- inLocRange after_t $ takeHdkComments mkDocPrev
  selectDocString (nextDocs ++ prevDocs)

appendHdkWarning :: HdkWarn -> HdkM ()
appendHdkWarning e = HdkM (ReaderT (\_ -> modify append_warn))
  where
    append_warn hdk_st = hdk_st { hdk_st_warnings = e : hdk_st_warnings hdk_st }

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
  traverse_ (\extra_doc -> appendHdkWarning (HdkWarnExtraComment extra_doc))

{- *********************************************************************
*                                                                      *
*      Matching functions for extracting documentation comments        *
*                                                                      *
********************************************************************* -}

mkDocHsDecl :: LayoutInfo -> PsLocated HdkComment -> Maybe (LHsDecl GhcPs)
mkDocHsDecl layout_info a = mapLoc (DocD noExtField) <$> mkDocDecl layout_info a

mkDocDecl :: LayoutInfo -> PsLocated HdkComment -> Maybe LDocDecl
mkDocDecl layout_info (L l_comment hdk_comment)
  | indent_mismatch = Nothing
  | otherwise =
    Just $ L (noAnnSrcSpan $ mkSrcSpanPs l_comment) $
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
  where l = noAnnSrcSpan $ mkSrcSpanPs l_comment

mkDocNext :: PsLocated HdkComment -> Maybe LHsDocString
mkDocNext (L l (HdkCommentNext doc)) = Just $ L (mkSrcSpanPs l) doc
mkDocNext _ = Nothing

mkDocPrev :: PsLocated HdkComment -> Maybe LHsDocString
mkDocPrev (L l (HdkCommentPrev doc)) = Just $ L (mkSrcSpanPs l) doc
mkDocPrev _ = Nothing


{- *********************************************************************
*                                                                      *
*                   LocRange: a location range                         *
*                                                                      *
********************************************************************* -}

-- A location range for extracting documentation comments.
data LocRange =
  LocRange
    { loc_range_from :: !LowerLocBound,
      loc_range_to   :: !UpperLocBound,
      loc_range_col  :: !ColumnBound }

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

-- Represents a predicate on BufPos:
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
data LowerLocBound = StartOfFile | StartLoc !BufPos

instance Semigroup LowerLocBound where
  StartOfFile <> l = l
  l <> StartOfFile = l
  StartLoc l1 <> StartLoc l2 = StartLoc (max l1 l2)

instance Monoid LowerLocBound where
  mempty = StartOfFile

-- Represents a predicate on BufPos:
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
data UpperLocBound = EndOfFile | EndLoc !BufPos

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
newtype ColumnBound = ColumnFrom Int -- n >= GHC.Types.SrcLoc.leftmostColumn

instance Semigroup ColumnBound where
  ColumnFrom n <> ColumnFrom m = ColumnFrom (max n m)

instance Monoid ColumnBound where
  mempty = ColumnFrom leftmostColumn


{- *********************************************************************
*                                                                      *
*                   AST manipulation utilities                         *
*                                                                      *
********************************************************************* -}

mkLHsDocTy :: LHsType GhcPs -> Maybe LHsDocString -> LHsType GhcPs
mkLHsDocTy t Nothing = t
mkLHsDocTy t (Just doc) = L (getLoc t) (HsDocTy noAnn t doc)

getForAllTeleLoc :: HsForAllTelescope GhcPs -> SrcSpan
getForAllTeleLoc tele =
  case tele of
    HsForAllVis{ hsf_vis_bndrs } -> getLHsTyVarBndrsLoc hsf_vis_bndrs
    HsForAllInvis { hsf_invis_bndrs } -> getLHsTyVarBndrsLoc hsf_invis_bndrs

getLHsTyVarBndrsLoc :: [LHsTyVarBndr flag GhcPs] -> SrcSpan
getLHsTyVarBndrsLoc bndrs = foldr combineSrcSpans noSrcSpan $ map getLocA bndrs

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
  -- + 'LHsDecl' produced by 'decl_cls' in Parser.y always have a 'BufSpan'
  -- + 'partitionBindsAndSigs' does not discard this 'BufSpan'
  mergeListsBy cmpBufSpanA [
    mapLL (\b -> ValD noExtField b) (bagToList all_bs),
    mapLL (\s -> SigD noExtField s) all_ss,
    mapLL (\t -> TyClD noExtField (FamDecl noExtField t)) all_ts,
    mapLL (\tfi -> InstD noExtField (TyFamInstD noExtField tfi)) all_tfis,
    mapLL (\dfi -> InstD noExtField (DataFamInstD noAnn dfi)) all_dfis,
    mapLL (\d -> DocD noExtField d) all_docs
  ]

cmpBufSpanA :: GenLocated (SrcSpanAnn' a1) a2 -> GenLocated (SrcSpanAnn' a3) a2 -> Ordering
cmpBufSpanA (L la a) (L lb b) = cmpBufSpan (L (locA la) a) (L (locA lb) b)

{- *********************************************************************
*                                                                      *
*                   General purpose utilities                          *
*                                                                      *
********************************************************************* -}

-- Cons an element to a list, if exists.
mcons :: Maybe a -> [a] -> [a]
mcons = maybe id (:)

-- Map a function over a list of located items.
mapLL :: (a -> b) -> [GenLocated l a] -> [GenLocated l b]
mapLL f = map (mapLoc f)

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
