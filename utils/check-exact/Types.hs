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

import GHC hiding (EpaComment)
import GHC.Utils.Outputable hiding ( (<>) )
import Data.Data (Data)

-- ---------------------------------------------------------------------

type Pos = (Int,Int)

-- ---------------------------------------------------------------------

data Rigidity = NormalLayout | RigidLayout deriving (Eq, Ord, Show)

-- ---------------------------------------------------------------------

-- | A Haskell comment. The @AnnKeywordId@ is present if it has been converted
-- from an @AnnKeywordId@ because the annotation must be interleaved into the
-- stream and does not have a well-defined position
data Comment = Comment
    {
      commentContents   :: !String -- ^ The contents of the comment including separators
    , commentAnchor :: !Anchor
    , commentPriorTok :: !RealSrcSpan
    , commentOrigin :: !(Maybe AnnKeywordId) -- ^ We sometimes turn syntax into comments in order to process them properly.
    }
  deriving (Data, Eq)

instance Show Comment where
  show (Comment cs ss r o)
    = "(Comment " ++ show cs ++ " " ++ showPprUnsafe ss ++ " " ++ show r ++ " " ++ show o ++ ")"

instance Ord Comment where
  -- When we have CPP injected comments with a fake filename, or LINE
  -- pragma, the file name changes, so we need to compare the
  -- locations only, with out the filename.
  compare (Comment _ ss1 _ _) (Comment _ ss2 _ _) = compareAnchor ss1 ss2
    where
      ss2pos ss = (srcSpanStartLine ss,srcSpanStartCol ss)

compareAnchor :: EpaLocation -> EpaLocation -> Ordering
compareAnchor a1 a2 = go a1 a2
  where
      ss2pos ss = (srcSpanStartLine ss,srcSpanStartCol ss)

      go (EpaSpan r1 _) (EpaSpan r2 _) = compare (ss2pos r1) (ss2pos r2)
      go (EpaSpan _ _) (EpaDelta _ _) = GT
      go (EpaDelta _ _) (EpaSpan _ _) = LT
      go (EpaDelta _ _) (EpaDelta _ _) = EQ

instance Outputable Comment where
  ppr x = text (show x)

-- | Marks the start column of a layout block.
newtype LayoutStartCol = LayoutStartCol { getLayoutStartCol :: Int }
  deriving (Eq, Num)

instance Show LayoutStartCol where
  show (LayoutStartCol sc) = "(LayoutStartCol " ++ show sc ++ ")"

-- ---------------------------------------------------------------------

-- Duplicated here so it can be used in show instances
showGhc :: (Outputable a) => a -> String
showGhc = showPprUnsafe
