{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
module M where

import Control.Monad.State
import GHC.Hs
import GHC.Types.SrcLoc
import Type.Reflection
import Data.Data (Data, gmapM)

type HsModule1 = HsModule GhcPs

type GenericM m = forall a. Data a => a -> m a

everywhereM :: forall m. Monad m => GenericM m -> GenericM m
everywhereM f = go
  where
    go :: GenericM m
    go x = do
      x' <- gmapM go x
      f x'

-- | 'State' with comments.
type WithComments = State [LEpaComment]

relocateComments :: HsModule1 -> [LEpaComment] -> HsModule1
relocateComments = evalState . relocateCommentsBeforeTopLevelDecls

-- | This function locates comments located before top-level declarations.
relocateCommentsBeforeTopLevelDecls :: HsModule1 -> WithComments HsModule1
relocateCommentsBeforeTopLevelDecls = everywhereM (applyM f)
  where
    f epa = insertCommentsByPos (const True) insertPriorComments epa

-- | This function applies the given function to all 'EpAnn's.
applyM ::
     forall a. Typeable a
  => (forall b. EpAnn b -> WithComments (EpAnn b))
  -> (a -> WithComments a)
applyM f
  | App g _ <- typeRep @a
  , Just HRefl <- eqTypeRep g (typeRep @EpAnn) = f
  | otherwise = pure

insertCommentsByPos ::
     (RealSrcSpan -> Bool)
  -> (EpAnnComments -> [LEpaComment] -> EpAnnComments)
  -> EpAnn a
  -> WithComments (EpAnn a)
insertCommentsByPos cond = insertComments (cond . epaLocationRealSrcSpan . getLoc)

insertComments ::
     (LEpaComment -> Bool)
  -> (EpAnnComments -> [LEpaComment] -> EpAnnComments)
  -> EpAnn a
  -> WithComments (EpAnn a)
insertComments cond inserter epa@EpAnn {..} = do
  coms <- drainComments cond
  pure $ epa {comments = inserter comments coms}

insertPriorComments :: EpAnnComments -> [LEpaComment] -> EpAnnComments
insertPriorComments (EpaComments prior) cs =
  EpaComments (prior ++ cs)
insertPriorComments (EpaCommentsBalanced prior following) cs =
  EpaCommentsBalanced (prior ++ cs) following

drainComments :: (LEpaComment -> Bool) -> WithComments [LEpaComment]
drainComments cond = undefined
