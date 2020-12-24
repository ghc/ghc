{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module Types
  where

import GHC
-- import GHC.Hs.Extension
-- import GHC.Parser.Lexer (AddApiAnn(..))
-- import GHC.Types.Basic hiding (EP)
-- import GHC.Types.Name.Reader
-- import GHC.Types.SrcLoc
import GHC.Utils.Outputable hiding ( (<>) )
import GHC.Driver.Session
import GHC.Driver.Ppr
-- import Control.Monad.Identity
-- import Control.Monad.RWS
import Data.Data (Data, toConstr,cast)
-- import Data.Foldable
-- import Data.List (sortBy, elemIndex)
-- import Data.Maybe (fromMaybe)
-- import Data.Ord (comparing)

import qualified Data.Map as Map
import qualified Data.Set as Set

-- import qualified GHC
-- import Lookup

-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- | This structure holds a complete set of annotations for an AST
type Anns = Map.Map AnnKey Annotation

emptyAnns :: Anns
emptyAnns = Map.empty

-- | For every @Located a@, use the @SrcSpan@ and constructor name of
-- a as the key, to store the standard annotation.
-- These are used to maintain context in the AP and EP monads
data AnnKey   = AnnKey RealSrcSpan AnnConName
                  deriving (Eq, Data, Ord)
-- deriving instance Ord SrcSpan

-- More compact Show instance
instance Show AnnKey where
  show (AnnKey ss cn) = "AnnKey " ++ showPprUnsafe ss ++ " " ++ show cn

mkAnnKeyPrim :: (Data a) => Located a -> AnnKey
mkAnnKeyPrim (L l a) = AnnKey (realSrcSpan l) (annGetConstr a)

mkAnnKeyPrimA :: (Data a) => LocatedA a -> AnnKey
mkAnnKeyPrimA (L l a) = AnnKey (realSrcSpan $ locA l) (annGetConstr a)

-- Holds the name of a constructor
data AnnConName = CN { unConName :: String }
                 deriving (Eq, Ord, Data)

-- More compact show instance
instance Show AnnConName where
  show (CN s) = "CN " ++ show s

annGetConstr :: (Data a) => a -> AnnConName
annGetConstr a = CN (show $ toConstr a)

-- |Make an unwrapped @AnnKey@ for the @LHsDecl@ case, a normal one otherwise.
mkAnnKey :: (Data a) => Located a -> AnnKey
mkAnnKey ld =
  case cast ld :: Maybe (LHsDecl GhcPs) of
    Just d -> declFun mkAnnKeyPrimA d
    Nothing -> mkAnnKeyPrim ld


type Pos = (Int,Int)

deltaRow, deltaColumn :: DeltaPos -> Int
deltaRow (DP (r, _)) = r
deltaColumn (DP (_, c)) = c

-- ---------------------------------------------------------------------

annNone :: Annotation
annNone = Ann (DP (0,0)) [] [] [] Nothing Nothing

data Annotation = Ann
  {
    -- The first three fields relate to interfacing up into the AST
    annEntryDelta      :: !DeltaPos
    -- ^ Offset used to get to the start of the SrcSpan, from whatever the prior
    -- output was, including all annPriorComments (field below).
  , annPriorComments   :: ![(Comment,  DeltaPos)]
    -- ^ Comments coming after the last non-comment output of the preceding
    -- element but before the SrcSpan being annotated by this Annotation. If
    -- these are changed then annEntryDelta (field above) must also change to
    -- match.
  , annFollowingComments   :: ![(Comment,  DeltaPos)]
    -- ^ Comments coming after the last output for the element subject to this
    -- Annotation. These will only be added by AST transformations, and care
    -- must be taken not to disturb layout of following elements.

  -- The next three fields relate to interacing down into the AST
  , annsDP             :: ![(KeywordId, DeltaPos)]
    -- ^ Annotations associated with this element.
  , annSortKey         :: !(Maybe [RealSrcSpan])
    -- ^ Captures the sort order of sub elements. This is needed when the
    -- sub-elements have been split (as in a HsLocalBind which holds separate
    -- binds and sigs) or for infix patterns where the order has been
    -- re-arranged. It is captured explicitly so that after the Delta phase a
    -- SrcSpan is used purely as an index into the annotations, allowing
    -- transformations of the AST including the introduction of new Located
    -- items or re-arranging existing ones.
  , annCapturedSpan    :: !(Maybe AnnKey)
    -- ^ Occasionally we must calculate a SrcSpan for an unlocated list of
    -- elements which we must remember for the Print phase. e.g. the statements
    -- in a HsLet or HsDo. These must be managed as a group because they all
    -- need eo be vertically aligned for the Haskell layout rules, and this
    -- guarantees this property in the presence of AST edits.

  } deriving (Eq)

-- ---------------------------------------------------------------------

declFun :: (forall a . Data a => LocatedA a -> b) -> LHsDecl GhcPs -> b
declFun f (L l de) =
  case de of
      TyClD _ d       -> f (L l d)
      InstD _ d       -> f (L l d)
      DerivD _ d      -> f (L l d)
      ValD _ d        -> f (L l d)
      SigD _ d        -> f (L l d)
      KindSigD _ d    -> f (L l d)
      DefD _ d        -> f (L l d)
      ForD _ d        -> f (L l d)
      WarningD _ d    -> f (L l d)
      AnnD _ d        -> f (L l d)
      RuleD _ d       -> f (L l d)
      SpliceD _ d     -> f (L l d)
      DocD _ d        -> f (L l d)
      RoleAnnotD _ d  -> f (L l d)

-- ---------------------------------------------------------------------

data ACS' a = ACS
  { acs :: !(Map.Map a Int) -- ^ how many levels each AstContext should
                            -- propagate down the AST. Removed when it hits zero
  } deriving (Show)

instance Semigroup (ACS' AstContext) where
  (<>) = mappend

instance Monoid (ACS' AstContext) where
  mempty = ACS mempty
  -- ACS a `mappend` ACS b = ACS (a `mappend` b)
  ACS a `mappend` ACS b = ACS (Map.unionWith max a b)
  -- For Data.Map, mappend == union, which is a left-biased replace for key collisions

type AstContextSet = ACS' AstContext
-- data AstContextSet = ACS
--   { acs :: !(Map.Map AstContext Int) -- ^ how many levels each AstContext should
--                                      -- propagate down the AST. Removed when it
--                                      -- hits zero
--   } deriving (Show)

defaultACS :: AstContextSet
defaultACS = ACS Map.empty

-- instance Outputable AstContextSet where
instance (Show a) => Outputable (ACS' a) where
  ppr x = text $ show x

data AstContext = -- LambdaExpr
                  CaseAlt
                | NoPrecedingSpace
                | HasHiding
                | AdvanceLine
                | NoAdvanceLine
                | Intercalate -- This item may have a list separator following
                | InIE -- possible 'type' or 'pattern'
                | PrefixOp
                | PrefixOpDollar
                | InfixOp -- RdrName may be used as an infix operator
                | ListStart -- Identifies first element of a list in layout, so its indentation can me managed differently
                | ListItem -- Identifies subsequent elements of a list in layout
                | TopLevelDecl -- top level declaration
                | NoDarrow
                | AddVbar
                | Deriving
                | Parens -- TODO: Not currently used?
                | ExplicitNeverActive
                | InGadt
                | InRecCon
                | InClassDecl
                | InSpliceDecl
                | LeftMost -- Is this the leftmost operator in a chain of OpApps?
                | InTypeApp -- HsTyVar in a TYPEAPP context. Has AnnAt
                          -- TODO:AZ: do we actually need this?

                -- Next four used to identify current list context
                | CtxOnly
                | CtxFirst
                | CtxMiddle
                | CtxLast
                | CtxPos Int -- 0 for first, increasing for subsequent

                -- Next are used in tellContext to push context up the tree
                | FollowingLine
                deriving (Eq, Ord, Show)


data ListContexts = LC { lcOnly,lcInitial,lcMiddle,lcLast :: !(Set.Set AstContext) }
  deriving (Eq,Show)

-- ---------------------------------------------------------------------

data Rigidity = NormalLayout | RigidLayout deriving (Eq, Ord, Show)

-- -- ---------------------------------------------------------------------
-- -- | This structure holds a complete set of annotations for an AST
-- type Anns = Map.Map AnnKey Annotation

-- emptyAnns :: Anns
-- emptyAnns = Map.empty

-- -- | For every @Located a@, use the @SrcSpan@ and constructor name of
-- -- a as the key, to store the standard annotation.
-- -- These are used to maintain context in the AP and EP monads
-- data AnnKey   = AnnKey SrcSpan AnnConName
--                   deriving (Eq, Data, Ord)
-- deriving instance Ord SrcSpan

-- -- More compact Show instance
-- instance Show AnnKey where
--   show (AnnKey ss cn) = "AnnKey " ++ showPprUnsafe ss ++ " " ++ show cn

-- mkAnnKeyPrim :: (Data a) => Located a -> AnnKey
-- mkAnnKeyPrim (L l a) = AnnKey l (annGetConstr a)

-- mkAnnKeyPrimA :: (Data a) => LocatedA a -> AnnKey
-- mkAnnKeyPrimA (L l a) = AnnKey (locA l) (annGetConstr a)

-- -- Holds the name of a constructor
-- data AnnConName = CN { unConName :: String }
--                  deriving (Eq, Ord, Data)

-- -- More compact show instance
-- instance Show AnnConName where
--   show (CN s) = "CN " ++ show s

-- annGetConstr :: (Data a) => a -> AnnConName
-- annGetConstr a = CN (show $ toConstr a)

-- -- |Make an unwrapped @AnnKey@ for the @LHsDecl@ case, a normal one otherwise.
-- mkAnnKey :: (Data a) => Located a -> AnnKey
-- mkAnnKey ld =
--   case cast ld :: Maybe (LHsDecl GhcPs) of
--     Just d -> declFun mkAnnKeyPrimA d
--     Nothing -> mkAnnKeyPrim ld


-- type Pos = (Int,Int)

-- -- | A relative positions, row then column
-- newtype DeltaPos = DP (Int,Int) deriving (Show,Eq,Ord,Data)

-- deltaRow, deltaColumn :: DeltaPos -> Int
-- deltaRow (DP (r, _)) = r
-- deltaColumn (DP (_, c)) = c

-- ---------------------------------------------------------------------

-- | A Haskell comment. The @AnnKeywordId@ is present if it has been converted
-- from an @AnnKeywordId@ because the annotation must be interleaved into the
-- stream and does not have a well-defined position
data Comment = Comment
    {
      commentContents   :: !String -- ^ The contents of the comment including separators

    -- AZ:TODO: commentIdentifier is a misnomer, should be commentSrcSpan, it is
    -- the thing we use to decide where in the output stream the comment should
    -- go.
    , commentAnchor :: !Anchor
    , commentOrigin :: !(Maybe AnnKeywordId) -- ^ We sometimes turn syntax into comments in order to process them properly.
    }
  deriving (Eq)

instance Show Comment where
  show (Comment cs ss o) = "(Comment " ++ show cs ++ " " ++ showPprUnsafe ss ++ " " ++ show o ++ ")"

instance Ord Comment where
  compare (Comment _ ss1 _) (Comment _ ss2 _) = compare (anchor ss1) (anchor ss2)

instance Outputable Comment where
  ppr x = text (show x)

-- | The different syntactic elements which are not represented in the
-- AST.
data KeywordId = G AnnKeywordId  -- ^ A normal keyword
               | AnnSemiSep          -- ^ A separating comma
               | AnnTypeApp          -- ^ Visible type application annotation
               | AnnComment Comment
               | AnnString String    -- ^ Used to pass information from
                                     -- Delta to Print when we have to work
                                     -- out details from the original
                                     -- SrcSpan.
               deriving (Eq)

instance Show KeywordId where
  show (G gc)          = "(G " ++ show gc ++ ")"
  show AnnSemiSep      = "AnnSemiSep"
  show AnnTypeApp      = "AnnTypeApp"
  show (AnnComment dc) = "(AnnComment " ++ show dc ++ ")"
  show (AnnString s)   = "(AnnString " ++ s ++ ")"

-- | Marks the start column of a layout block.
newtype LayoutStartCol = LayoutStartCol { getLayoutStartCol :: Int }
  deriving (Eq, Num)

instance Show LayoutStartCol where
  show (LayoutStartCol sc) = "(LayoutStartCol " ++ show sc ++ ")"
-- ---------------------------------------------------------------------

-- Duplicated here so it can be used in show instances
showGhc :: (Outputable a) => a -> String
showGhc = showPprUnsafe

