{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.GHC.ExactPrint.Transform
--
-- This module is currently under heavy development, and no promises are made
-- about API stability. Use with care.
--
-- We welcome any feedback / contributions on this, as it is the main point of
-- the library.
--
-----------------------------------------------------------------------------
module Transform
        (
        -- * The Transform Monad
          Transform
        , TransformT(..)
        , hoistTransform
        , runTransform
        , runTransformT
        , runTransformFrom
        , runTransformFromT

        -- * Transform monad operations
        , logTr
        , logDataWithAnnsTr
        , uniqueSrcSpanT

        -- ** Managing declarations, in Transform monad
        , HasTransform (..)
        , HasDecls (..)
        , hsDeclsPatBind, hsDeclsPatBindD
        , replaceDeclsPatBind, replaceDeclsPatBindD
        , modifyDeclsT
        , modifyValD
        -- *** Utility, does not manage layout
        , hsDeclsValBinds, replaceDeclsValbinds
        , WithWhere(..)

        -- ** New gen functions
        , noAnnSrcSpanDP, noAnnSrcSpanDPI
        , noAnnSrcSpanDP0, noAnnSrcSpanDP0I
        , noAnnSrcSpanDP1
        , noAnnSrcSpanDPn
        , d0, d1, dn
        , m0, m1, mn
        , addComma

        -- ** Managing lists, Transform monad
        , insertAt
        , insertAtStart
        , insertAtEnd
        , insertAfter
        , insertBefore

        -- *** Low level operations used in 'HasDecls'
        , balanceComments
        , balanceCommentsList
        , balanceCommentsList'
        , anchorEof

        -- ** Managing lists, pure functions
        , captureOrder, captureOrderBinds
        , captureLineSpacing
        , captureMatchLineSpacing
        , captureTypeSigSpacing

        -- * Operations
        , isUniqueSrcSpan

        -- * Pure functions
        , setEntryDP, setEntryDPDecl
        , getEntryDP
        , transferEntryDP, transferEntryDPI
        , transferEntryDP'
        , wrapSig, wrapDecl
        , decl2Sig, decl2Bind
        ) where

import Types
import Utils
import Orphans (Default(..))

import Control.Monad.RWS
import qualified Control.Monad.Fail as Fail

import GHC  hiding (parseModule, parsedSource)
import GHC.Data.Bag
import GHC.Data.FastString
import qualified GHC.Data.Strict as Strict

import Data.Data
import Data.List ( sortBy )
import Data.Maybe

import Data.Functor.Identity
import Control.Monad.State

------------------------------------------------------------------------------
-- Transformation of source elements

-- | Monad type for updating the AST and managing the annotations at the same
-- time. The W state is used to generate logging information if required.
type Transform = TransformT Identity

-- |Monad transformer version of 'Transform' monad
newtype TransformT m a = TransformT { unTransformT :: RWST () [String] Int m a }
                deriving (Monad,Applicative,Functor
                         ,MonadReader ()
                         ,MonadWriter [String]
                         ,MonadState Int
                         ,MonadTrans
                         )

instance Fail.MonadFail m => Fail.MonadFail (TransformT m) where
    fail msg = TransformT $ RWST $ \_ _ -> Fail.fail msg

-- | Run a transformation in the 'Transform' monad, returning the updated
-- annotations and any logging generated via 'logTr'
runTransform :: Transform a -> (a,Int,[String])
runTransform f = runTransformFrom 0 f

runTransformT :: TransformT m a -> m (a,Int,[String])
runTransformT f = runTransformFromT 0 f

-- | Run a transformation in the 'Transform' monad, returning the updated
-- annotations and any logging generated via 'logTr', allocating any new
-- SrcSpans from the provided initial value.
runTransformFrom :: Int -> Transform a -> (a,Int,[String])
runTransformFrom seed f = runRWS (unTransformT f) () seed

-- |Run a monad transformer stack for the 'TransformT' monad transformer
runTransformFromT :: Int -> TransformT m a -> m (a,Int,[String])
runTransformFromT seed f = runRWST (unTransformT f) () seed

-- | Change inner monad of 'TransformT'.
hoistTransform :: (forall x. m x -> n x) -> TransformT m a -> TransformT n a
hoistTransform nt (TransformT m) = TransformT (mapRWST nt m)

-- |Log a string to the output of the Monad
logTr :: (Monad m) => String -> TransformT m ()
logTr str = tell [str]

-- |Log a representation of the given AST with annotations to the output of the
-- Monad
logDataWithAnnsTr :: (Monad m) => (Data a) => String -> a -> TransformT m ()
logDataWithAnnsTr str ast = do
  logTr $ str ++ showAst ast

-- ---------------------------------------------------------------------

-- |If we need to add new elements to the AST, they need their own
-- 'SrcSpan' for this.
uniqueSrcSpanT :: (Monad m) => TransformT m SrcSpan
uniqueSrcSpanT = do
  col <- get
  put (col + 1 )
  let pos = mkSrcLoc (mkFastString "ghc-exactprint") (-1) col
  return $ mkSrcSpan pos pos

-- |Test whether a given 'SrcSpan' was generated by 'uniqueSrcSpanT'
isUniqueSrcSpan :: SrcSpan -> Bool
isUniqueSrcSpan ss = srcSpanStartLine' ss == -1

srcSpanStartLine' :: SrcSpan -> Int
srcSpanStartLine' (RealSrcSpan s _) = srcSpanStartLine s
srcSpanStartLine' _ = 0

-- ---------------------------------------------------------------------

-- |If a list has been re-ordered or had items added, capture the new order in
-- the appropriate 'AnnSortKey' attached to the 'Annotation' for the list.
captureOrder :: [LocatedA b] -> AnnSortKey [RealSrcSpan]
captureOrder ls = AnnSortKey $ map (rs . getLocA) ls

captureOrderBinds :: [LHsDecl GhcPs] -> AnnSortKey [DeclTag]
captureOrderBinds ls = AnnSortKey $ map go ls
  where
    go (L _ (TyClD _ _))      = TyClDTag
    go (L _ (InstD _ _))      = InstDTag
    go (L _ (DerivD _ _))     = DerivDTag
    go (L _ (ValD _ _))       = ValDTag
    go (L _ (SigD _ _))       = SigDTag
    go (L _ (KindSigD _ _))   = KindSigDTag
    go (L _ (DefD _ _))       = DefDTag
    go (L _ (ForD _ _))       = ForDTag
    go (L _ (WarningD _ _))   = WarningDTag
    go (L _ (AnnD _ _))       = AnnDTag
    go (L _ (RuleD _ _))      = RuleDTag
    go (L _ (SpliceD _ _))    = SpliceDTag
    go (L _ (DocD _ _))       = DocDTag
    go (L _ (RoleAnnotD _ _)) = RoleAnnotDTag
    go (L _ (XHsDecl _))      = error "captureOrderBinds"

-- ---------------------------------------------------------------------

captureMatchLineSpacing :: LHsDecl GhcPs -> LHsDecl GhcPs
captureMatchLineSpacing (L l (ValD x (FunBind a b (MG c (L d ms )))))
                       = L l (ValD x (FunBind a b (MG c (L d ms'))))
    where
      ms' :: [LMatch GhcPs (LHsExpr GhcPs)]
      ms' = captureLineSpacing ms
captureMatchLineSpacing d = d

captureLineSpacing :: [LocatedA e] -> [LocatedA e]
captureLineSpacing [] = []
captureLineSpacing [d] = [d]
captureLineSpacing ds = map snd $ go (map to ds)
  where
    to :: LocatedA e -> (Int, LocatedA e)
    to d = (fst $ ss2pos $ rs $ getHasLoc d,d)

    go :: [(Int, LocatedA e)] -> [(Int, LocatedA e)]
    go [] = []
    go [d] = [d]
    go ((l1,de1):(l2,d2):ds) = (l1,de1):go ((l2,d2'):ds)
      where
        d2' = setEntryDP d2 (deltaPos (l2-l1) 0)

-- ---------------------------------------------------------------------

captureTypeSigSpacing :: LHsDecl GhcPs -> LHsDecl GhcPs
captureTypeSigSpacing (L l (SigD x (TypeSig (EpAnn anc (AnnSig dc rs') cs) ns (HsWC xw ty))))
  = (L l (SigD x (TypeSig (EpAnn anc (AnnSig dc' rs') cs) ns (HsWC xw ty'))))
  where
    -- we want DPs for the distance from the end of the ns to the
    -- AnnDColon, and to the start of the ty
    AddEpAnn kw dca = dc
    rd = case last ns of
      L (EpAnnS anc' _ _) _ -> anchor anc' -- TODO MovedAnchor?
    dc' = case dca of
      EpaSpan (RealSrcSpan r _) -> AddEpAnn kw (EpaDelta (ss2delta (ss2posEnd rd) r) [])
      _ -> AddEpAnn kw dca

    -- ---------------------------------

    ty' :: LHsSigType GhcPs
    ty' = case ty
                 `debug` ("captureTypeSigSpacing:ty=" ++ showAst ty)
                   of
      (L (EpAnnS anc0 a c) b)
        -> let
              anc' = case anc0 of
                EpaDelta _  _ -> anc0
                _ -> case dca of
                  -- EpaSpan  _ -> error "todo"
                  EpaSpan  _ -> EpaDelta (SameLine 1) []
                  EpaDelta _ _ -> EpaDelta (SameLine 1) []
           in (L (EpAnnS anc' a c) b)

captureTypeSigSpacing s = s

-- ---------------------------------------------------------------------

-- |Pure function to convert a 'LHsDecl' to a 'LHsBind'. This does
-- nothing to any annotations that may be attached to either of the elements.
-- It is used as a utility function in 'replaceDecls'
decl2Bind :: LHsDecl GhcPs -> [LHsBind GhcPs]
decl2Bind (L l (ValD _ s)) = [L l s]
decl2Bind _                      = []

-- |Pure function to convert a 'LSig' to a 'LHsBind'. This does
-- nothing to any annotations that may be attached to either of the elements.
-- It is used as a utility function in 'replaceDecls'
decl2Sig :: LHsDecl GhcPs -> [LSig GhcPs]
decl2Sig (L l (SigD _ s)) = [L l s]
decl2Sig _                = []

-- ---------------------------------------------------------------------

setEntryDPDecl :: LHsDecl GhcPs -> DeltaPos -> LHsDecl GhcPs
setEntryDPDecl decl@(L _  (ValD x (FunBind a b (MG c (L d ms ))))) dp
                   = L l' (ValD x (FunBind a b (MG c (L d ms'))))
    where
      L l' _ = setEntryDP decl dp
      ms' :: [LMatch GhcPs (LHsExpr GhcPs)]
      ms' = case ms of
        [] -> []
        (m0':ms0) -> setEntryDP m0' dp : ms0
setEntryDPDecl d dp = setEntryDP d dp

-- ---------------------------------------------------------------------

-- |Set the true entry 'DeltaPos' from the annotation for a given AST
-- element. This is the 'DeltaPos' ignoring any comments.
setEntryDP :: LocatedAnS t a -> DeltaPos -> LocatedAnS t a
setEntryDP (L (EpAnnS _ an (EpaComments [])) a) dp
  = L (EpAnnS (EpaDelta dp []) an (EpaComments [])) a
setEntryDP (L (EpAnnS (EpaDelta d csd) an cs) a) dp
  = L (EpAnnS (EpaDelta d' csd') an cs') a
  where
    (d', csd', cs') = case cs of
      EpaComments (h:t) ->
        let
          (dp0,c') = go h
        in
          (dp0, c':t++csd, EpaComments [])
      EpaCommentsBalanced (h:t) ts ->
        let
          (dp0,c') = go h
        in
          (dp0, c':t++csd, EpaCommentsBalanced [] ts)
      EpaCommentsBalanced [] ts ->
          (d, csd, EpaCommentsBalanced [] ts)
    go (L (EpaDelta ma c0) c) = (d,  L (EpaDelta ma c0) c)
    go (L (EpaSpan _)      c) = (d,  L (EpaDelta dp []) c)
setEntryDP (L (EpAnnS (EpaSpan (RealSrcSpan r _)) an cs) a) dp
  = case sortEpaComments (priorComments cs) of
      [] -> L (EpAnnS (EpaDelta dp []) an cs) a
      (L ca c:cs') ->
        L (EpAnnS (EpaDelta edp csd) an cs'') a
              where
                -- cs'' = setPriorComments cs (L (EpaDelta dp []) c:cs')
                cs'' = setPriorComments cs []
                csd = L (EpaDelta dp []) c:cs'
                lc = head $ reverse $ (L ca c:cs')
                delta = case getLoc lc of
                          EpaSpan (RealSrcSpan rr _) -> tweakDelta $ ss2delta (ss2pos rr) r
                          _ -> DifferentLine 1 0
                line = getDeltaLine delta
                col = deltaColumn delta
                edp' = if line == 0 then SameLine col
                                    else DifferentLine line col
                edp = edp' `debug` ("setEntryDP :" ++ showGhc (edp', (getLoc lc), r))


-- |Set the true entry 'DeltaPos' from the annotation for a given AST
-- element. This is the 'DeltaPos' ignoring any comments.
setEntryDPI :: Default t => LocatedAn t a -> DeltaPos -> LocatedAn t a
setEntryDPI (L (SrcSpanAnn EpAnnNotUsed l) a) dp
  = L (SrcSpanAnn
           (EpAnn (EpaDelta dp []) def emptyComments)
           l) a
setEntryDPI (L (SrcSpanAnn (EpAnn _ an (EpaComments [])) l) a) dp
  = L (SrcSpanAnn
           (EpAnn (EpaDelta dp []) an (EpaComments []))
           l) a
setEntryDPI (L (SrcSpanAnn (EpAnn (EpaDelta d _) an cs) l) a) dp
  = L (SrcSpanAnn
           (EpAnn (EpaDelta d' []) an cs')
           l) a
  where
    (d',cs') = case cs of
      EpaComments (h:t) ->
        let
          (dp0,c') = go h
        in
          (dp0, EpaComments (c':t))
      EpaCommentsBalanced (h:t) ts ->
        let
          (dp0,c') = go h
        in
          (dp0, EpaCommentsBalanced (c':t) ts)
      _ -> (dp, cs)
    go (L (EpaDelta ma c0) c) = (d,  L (EpaDelta ma c0) c)
    go (L (EpaSpan _)      c) = (d,  L (EpaDelta dp []) c)
setEntryDPI (L (SrcSpanAnn (EpAnn (EpaSpan (RealSrcSpan r _)) an cs) l) a) dp
  = case sortEpaComments (priorComments cs) of
      [] ->
        L (SrcSpanAnn
               (EpAnn (EpaDelta dp []) an cs)
               l) a
      (L ca c:cs') ->
        L (SrcSpanAnn
               (EpAnn (EpaDelta edp []) an cs'')
               l) a
              where
                cs'' = setPriorComments cs (L (EpaDelta dp []) c:cs')
                lc = head $ reverse $ (L ca c:cs')
                -- delta = tweakDelta $ ss2delta (ss2pos $ anchor $ getLoc lc) r
                delta = case getLoc lc of
                          EpaSpan (RealSrcSpan rr _) -> tweakDelta $ ss2delta (ss2pos rr) r
                          EpaSpan _ -> tweakDelta (SameLine 0)
                          EpaDelta dp _ -> tweakDelta dp
                line = getDeltaLine delta
                col = deltaColumn delta
                edp' = if line == 0 then SameLine col
                                    else DifferentLine line col
                edp = edp' `debug` ("setEntryDPI :" ++ showGhc (edp', (getLoc lc), r))

-- ---------------------------------------------------------------------

getEntryDP :: LocatedAn t a -> DeltaPos
getEntryDP (L (SrcSpanAnn (EpAnn (EpaDelta dp []) _ _) _) _) = dp
getEntryDP _ = SameLine 1

-- ---------------------------------------------------------------------

addEpaLocationDelta :: LayoutStartCol -> RealSrcSpan -> EpaLocation -> EpaLocation
addEpaLocationDelta _off _anc (EpaDelta d cs) = EpaDelta d cs
addEpaLocationDelta  off  anc (EpaSpan (RealSrcSpan r _))
  = EpaDelta (adjustDeltaForOffset off (ss2deltaEnd anc r)) []
addEpaLocationDelta _off _anc loc = loc

-- Set the entry DP for an element coming after an existing keyword annotation
setEntryDPFromAnchor :: LayoutStartCol -> EpaLocation -> LocatedA t -> LocatedA t
setEntryDPFromAnchor  off (EpaSpan (RealSrcSpan anc _)) ll@(L la _) = setEntryDP ll dp'
  where
    -- r = case la of
    --   (EpAnnS (Anchor r' _) _ _) -> r'
    -- dp' = adjustDeltaForOffset off (ss2deltaEnd anc r)
    dp' = case la of
      (EpAnnS (EpaSpan (RealSrcSpan r' _)) _ _) -> adjustDeltaForOffset off (ss2deltaEnd anc r')
      (EpAnnS (EpaSpan _) _ _)                  -> adjustDeltaForOffset off (SameLine 0)
      (EpAnnS (EpaDelta dp _) _ _) -> adjustDeltaForOffset off dp
setEntryDPFromAnchor _off _ (L la a) = L la a

-- ---------------------------------------------------------------------

-- |Take the annEntryDelta associated with the first item and associate it with the second.
-- Also transfer any comments occuring before it.
transferEntryDPI :: (Monad m, Monoid t2, Typeable t1, Typeable t2)
  => LocatedAn t1 a -> LocatedAn t2 b -> TransformT m (LocatedAn t2 b)
transferEntryDPI (L (SrcSpanAnn EpAnnNotUsed l1) _) (L (SrcSpanAnn EpAnnNotUsed _) b) = do
  logTr $ "transferEntryDP': EpAnnNotUsed,EpAnnNotUsed"
  return (L (SrcSpanAnn EpAnnNotUsed l1) b)
transferEntryDPI (L (SrcSpanAnn (EpAnn anc _an cs) _l1) _) (L (SrcSpanAnn EpAnnNotUsed l2) b) = do
  logTr $ "transferEntryDP': EpAnn,EpAnnNotUsed"
  return (L (SrcSpanAnn (EpAnn anc mempty cs) l2) b)
transferEntryDPI (L (SrcSpanAnn (EpAnn anc1 an1 cs1) _l1) _) (L (SrcSpanAnn (EpAnn _anc2 an2 cs2) l2) b) = do
  logTr $ "transferEntryDP': EpAnn,EpAnn"
  -- Problem: if the original had preceding comments, blindly
  -- transferring the location is not correct
  case priorComments cs1 of
    [] -> return (L (SrcSpanAnn (EpAnn anc1 (combine an1 an2) cs2) l2) b)
    -- TODO: what happens if the receiving side already has comments?
    (L anc _:_) -> do
      logDataWithAnnsTr "transferEntryDP':priorComments anc=" anc
      return (L (SrcSpanAnn (EpAnn anc1 (combine an1 an2) (cs1 <> cs2)) l2) b)
transferEntryDPI (L (SrcSpanAnn EpAnnNotUsed _l1) _) (L (SrcSpanAnn (EpAnn anc2 an2 cs2) l2) b) = do
  logTr $ "transferEntryDP': EpAnnNotUsed,EpAnn"
  return (L (SrcSpanAnn (EpAnn anc2' an2 cs2) l2) b)
    where
      anc2' = case anc2 of
        -- Anchor _a op   -> Anchor (realSrcSpan "transferEntryDP" l2) op
        EpaDelta _dp _cs -> anc2
        EpaSpan _        -> EpaSpan (RealSrcSpan (realSrcSpan "transferEntryDP" l2) Strict.Nothing)

transferEntryDP :: (Monad m, Typeable an)
  => LocatedAnS an a -> LocatedAnS an b -> TransformT m (LocatedAnS an b)
transferEntryDP (L (EpAnnS anc1 an1 cs1) _) (L (EpAnnS _anc2 an2 cs2) b) = do
  logTr $ "transferEntryDP': EpAnn,EpAnn"
  -- Problem: if the original had preceding comments, blindly
  -- transferring the location is not correct
  case priorComments cs1 of
    [] -> return (L (EpAnnS anc1 (combine an1 an2) cs2) b)
    -- TODO: what happens if the receiving side already has comments?
    (L anc _:_) -> do
      logDataWithAnnsTr "transferEntryDP':priorComments anc=" anc
      return (L (EpAnnS anc1 (combine an1 an2) (cs1 <> cs2)) b)

-- |If a and b are the same type return first arg, else return second
combine :: (Typeable a, Typeable b) => a -> b -> b
combine x y = fromMaybe y (cast x)

-- |Take the annEntryDelta associated with the first item and associate it with the second.
-- Also transfer any comments occuring before it.
-- TODO: call transferEntryDP, and use pushDeclDP
transferEntryDP' :: (Monad m) => LHsDecl GhcPs -> LHsDecl GhcPs -> TransformT m (LHsDecl GhcPs)
transferEntryDP' la lb = do
  (L l2 b) <- transferEntryDP la lb
  return (L l2 (pushDeclDP b (SameLine 0)))


pushDeclDP :: HsDecl GhcPs -> DeltaPos -> HsDecl GhcPs
pushDeclDP (ValD x (FunBind a b (MG c (L d  ms )))) dp
          = ValD x (FunBind a b (MG c (L d' ms')))
    where
      L d' _ = setEntryDPI (L d ms) dp
      ms' :: [LMatch GhcPs (LHsExpr GhcPs)]
      ms' = case ms of
        [] -> []
        (m0':ms0) -> setEntryDP m0' dp : ms0
pushDeclDP d _dp = d

-- ---------------------------------------------------------------------

balanceCommentsList :: (Monad m) => [LHsDecl GhcPs] -> TransformT m [LHsDecl GhcPs]
balanceCommentsList [] = return []
balanceCommentsList [x] = return [x]
balanceCommentsList (a:b:ls) = do
  (a',b') <- balanceComments a b
  r <- balanceCommentsList (b':ls)
  return (a':r)

-- |The GHC parser puts all comments appearing between the end of one AST
-- item and the beginning of the next as 'annPriorComments' for the second one.
-- This function takes two adjacent AST items and moves any 'annPriorComments'
-- from the second one to the 'annFollowingComments' of the first if they belong
-- to it instead. This is typically required before deleting or duplicating
-- either of the AST elements.
balanceComments :: (Monad m)
  => LHsDecl GhcPs -> LHsDecl GhcPs
  -> TransformT m (LHsDecl GhcPs, LHsDecl GhcPs)
balanceComments first second = do
  case first of
    (L l (ValD x fb@(FunBind{}))) -> do
      (L l' fb',second') <- balanceCommentsFB (L l fb) second
      return (L l' (ValD x fb'), second')
    _ -> balanceComments' first second

-- |Once 'balanceComments' has been called to move trailing comments to a
-- 'FunBind', these need to be pushed down from the top level to the last
-- 'Match' if that 'Match' needs to be manipulated.
balanceCommentsFB :: (Monad m)
  => LHsBind GhcPs -> LocatedA b -> TransformT m (LHsBind GhcPs, LocatedA b)
balanceCommentsFB (L lf (FunBind x n (MG o (L lm matches)))) second = do
  debugM $ "balanceCommentsFB entered: " ++ showGhc (ss2range $ locA lf)
  -- There are comments on lf.  We need to
  -- + Keep the prior ones here
  -- + move the interior ones to the first match,
  -- + move the trailing ones to the last match.
  let
    (before,middle,after) = case s_entry lf of
        EpaSpan (RealSrcSpan ss _) ->
          let
            split = splitCommentsEnd ss (s_comments lf)
            split2 = splitCommentsStart ss  (EpaComments (sortEpaComments $ priorComments split))

            before = sortEpaComments $ priorComments split2
            middle = sortEpaComments $ getFollowingComments split2
            after  = sortEpaComments $ getFollowingComments split
          in (before,middle,after)
        _ -> (priorComments $ s_comments lf,
              [],
              getFollowingComments $ s_comments lf)

    lf' = setCommentsEpAnnS lf (EpaComments before)
  debugM $ "balanceCommentsFB (before, after): " ++ showAst (before, after)
  debugM $ "balanceCommentsFB lf': " ++ showAst lf'
  -- let matches' = case matches of
  let matches' :: [LocatedA (Match GhcPs (LHsExpr GhcPs))]
      matches' = case matches of
                    (L lm' m':ms') ->
                      (L (addCommentsToEpAnnS lm' (EpaComments middle )) m':ms')
                    _ -> error "balanceCommentsFB3"
  matches'' <- balanceCommentsList' matches'
  let (m,ms) = case reverse matches'' of
                 (L lm' m':ms') ->
                   (L (addCommentsToEpAnnS lm' (EpaCommentsBalanced [] after)) m',ms')
                 _ -> error "balanceCommentsFB4"
  debugM $ "balanceCommentsFB: (m,ms):" ++ showAst (m,ms)
  (m',second') <- balanceComments' m second
  m'' <- balanceCommentsMatch m'
  let (m''',lf'') = case ms of
        [] -> moveLeadingComments m'' lf'
        _  -> (m'',lf')
  debugM $ "balanceCommentsFB: (lf'', m'''):" ++ showAst (lf'',m''')
  debugM $ "balanceCommentsFB done"
  let bind = L lf'' (FunBind x n (MG o (L lm (reverse (m''':ms)))))
  debugM $ "balanceCommentsFB returning:" ++ showAst bind
  balanceComments' (packFunBind bind) second'
balanceCommentsFB f s = balanceComments' f s

-- | Move comments on the same line as the end of the match into the
-- GRHS, prior to the binds
balanceCommentsMatch :: (Monad m)
  => LMatch GhcPs (LHsExpr GhcPs) -> TransformT m (LMatch GhcPs (LHsExpr GhcPs))
balanceCommentsMatch (L l (Match am mctxt pats (GRHSs xg grhss binds))) = do
  logTr $ "balanceCommentsMatch: (logInfo)=" ++ showAst (logInfo)
  return (L l'' (Match am mctxt pats (GRHSs xg grhss' binds')))
  where
    simpleBreak (r,_) = r /= 0
    an1 = l
    -- anc1 = addCommentOrigDeltas $ s_comments an1
    anc1 = s_comments an1
    cs1f = getFollowingComments anc1
    (move',stay') = break simpleBreak (trailingCommentsDeltas (anchorFromLocatedA (L l ())) cs1f)
    move = map snd move'
    stay = map snd stay'
    (l'', grhss', binds', logInfo)
      = case reverse grhss of
          [] -> (l, [], binds,                 (EpaComments [], noSrcSpanA))
          (L lg g@(GRHS EpAnnNotUsed _grs _rhs):gs)
            -> (l, reverse (L lg g:gs), binds, (EpaComments [], noSrcSpanA))
          (L lg (GRHS ag grs rhs):gs) ->
            let
              anc1' = setFollowingComments anc1 stay
              an1' = setCommentsEpAnnS l anc1'

              -- ---------------------------------
              (moved,bindsm) = pushTrailingComments WithWhere (EpaCommentsBalanced [] move) binds
              -- ---------------------------------

              (EpAnn anc an lgc) = ag
              -- lgc' = splitCommentsEnd (realSrcSpan "balanceCommentsMatch" $ locA lg) $ addCommentOrigDeltas lgc
              lgc' = splitCommentsEnd (realSrcSpan "balanceCommentsMatch" $ locA lg) lgc
              ag' = if moved
                      then EpAnn anc an lgc'
                      else EpAnn anc an (lgc' <> (EpaCommentsBalanced [] move))
              -- ag' = EpAnn anc an lgc'

            in (an1', (reverse $ (L lg (GRHS ag' grs rhs):gs)), bindsm, (anc1',an1'))

pushTrailingComments :: WithWhere -> EpAnnComments -> HsLocalBinds GhcPs -> (Bool, HsLocalBinds GhcPs)
pushTrailingComments _ _cs b@EmptyLocalBinds{} = (False, b)
pushTrailingComments _ _cs (HsIPBinds _ _) = error "TODO: pushTrailingComments:HsIPBinds"
pushTrailingComments w cs lb@(HsValBinds an _)
  = (True, HsValBinds an' vb)
  where
    decls = hsDeclsLocalBinds lb
    (an', decls') = case reverse decls of
      [] -> (addCommentsToEpAnn (spanHsLocaLBinds lb) an cs, decls)
      (L la d:ds) -> (an, L (addCommentsToEpAnnS la cs) d:ds)
    (vb,_ws2) = case runTransform (replaceDeclsValbinds w lb (reverse decls')) of
      ((HsValBinds _ vb'), _, ws2') -> (vb', ws2')
      _ -> (ValBinds NoAnnSortKey emptyBag [], [])


balanceCommentsList' :: (Monad m) => [LocatedA a] -> TransformT m [LocatedA a]
balanceCommentsList' [] = return []
balanceCommentsList' [x] = return [x]
balanceCommentsList' (a:b:ls) = do
  logTr $ "balanceCommentsList' entered"
  (a',b') <- balanceComments' a b
  r <- balanceCommentsList' (b':ls)
  return (a':r)

-- |Prior to moving an AST element, make sure any trailing comments belonging to
-- it are attached to it, and not the following element. Of necessity this is a
-- heuristic process, to be tuned later. Possibly a variant should be provided
-- with a passed-in decision function.
-- The initial situation is that all comments for a given anchor appear as prior comments
-- Many of these should in fact be following comments for the previous anchor
balanceComments' :: (Monad m) => LocatedA a -> LocatedA b -> TransformT m (LocatedA a, LocatedA b)
balanceComments' la1 la2 = do
  debugM $ "balanceComments': (anc1)=" ++ showAst (anc1)
  debugM $ "balanceComments': (cs1s)=" ++ showAst (cs1s)
  debugM $ "balanceComments': (cs1stay,cs1move)=" ++ showAst (cs1stay,cs1move)
  debugM $ "balanceComments': (an1',an2')=" ++ showAst (an1',an2')
  return (la1', la2')
  where
    simpleBreak n (r,_) = r > n
    L an1 f = la1
    L an2 s = la2
    anc1 = s_comments an1
    anc2 = s_comments an2

    cs1s = splitCommentsEnd (anchorFromLocatedA la1) anc1
    cs1p = priorCommentsDeltas    (anchorFromLocatedA la1) (priorComments        cs1s)
    cs1f = trailingCommentsDeltas (anchorFromLocatedA la1) (getFollowingComments cs1s)

    cs2s = splitCommentsEnd (anchorFromLocatedA la2) anc2
    cs2p = priorCommentsDeltas    (anchorFromLocatedA la2) (priorComments        cs2s)
    cs2f = trailingCommentsDeltas (anchorFromLocatedA la2) (getFollowingComments cs2s)

    -- Split cs1f into those that belong on an1 and ones that must move to an2
    (cs1move,cs1stay) = break (simpleBreak 1) cs1f

    (stay'',move') = break (simpleBreak 1) cs2p
    -- Need to also check for comments more closely attached to la1,
    -- ie trailing on the same line
    (move'',stay') = break (simpleBreak 0) (trailingCommentsDeltas (anchorFromLocatedA la1) (map snd stay''))
    move = sortEpaComments $ map snd (cs1move ++ move'' ++ move')
    stay = sortEpaComments $ map snd (cs1stay ++ stay')

    an1' = setCommentsEpAnnS (getLoc la1) (EpaCommentsBalanced (map snd cs1p) move)
    an2' = setCommentsEpAnnS (getLoc la2) (EpaCommentsBalanced stay (map snd cs2f))
    la1' = L an1' f
    la2' = L an2' s

-- | Like commentsDeltas, but calculates the delta from the end of the anchor, not the start
trailingCommentsDeltas :: RealSrcSpan -> [LEpaComment]
               -> [(Int, LEpaComment)]
trailingCommentsDeltas _ [] = []
trailingCommentsDeltas rs (la@(L (EpaDelta dp _) _):las)
  = (getDeltaLine dp, la): trailingCommentsDeltas rs las
trailingCommentsDeltas rs (la@(L l _):las)
  = deltaComment rs la : trailingCommentsDeltas (anchor l) las
  where
    deltaComment rs' (L loc c) = (abs(ll - al), L loc c)
      where
        (al,_) = ss2posEnd rs'
        (ll,_) = ss2pos (anchor loc)

-- AZ:TODO: this is identical to commentsDeltas
priorCommentsDeltas :: RealSrcSpan -> [LEpaComment]
                    -> [(Int, LEpaComment)]
priorCommentsDeltas rs cs = go rs (reverse $ sortEpaComments cs)
  where
    go :: RealSrcSpan -> [LEpaComment] -> [(Int, LEpaComment)]
    go _ [] = []
    go rs' (la@(L (EpaDelta dp _) _):las) = (deltaLine dp, la) : go rs' las
    go rs' (la@(L l _):las) = deltaComment rs' la : go (anchor l) las

    deltaComment :: RealSrcSpan -> LEpaComment -> (Int, LEpaComment)
    deltaComment rs' (L loc c) = (abs(ll - al), L loc c)
      where
        (al,_) = ss2pos rs'
        (ll,_) = ss2pos (anchor loc)


-- ---------------------------------------------------------------------

-- | Split comments into ones occuring before the end of the reference
-- span, and those after it.
splitCommentsEnd :: RealSrcSpan -> EpAnnComments -> EpAnnComments
splitCommentsEnd p (EpaComments cs) = cs'
  where
    cmp (L (EpaSpan (RealSrcSpan l _)) _) = ss2pos l > ss2posEnd p
    cmp (L _ _) = True
    (before, after) = break cmp cs
    cs' = case after of
      [] -> EpaComments cs
      _ -> EpaCommentsBalanced before after
splitCommentsEnd p (EpaCommentsBalanced cs ts) = EpaCommentsBalanced cs' ts'
  where
    cmp (L (EpaSpan (RealSrcSpan l _)) _) = ss2pos l > ss2posEnd p
    cmp (L _ _) = True
    (before, after) = break cmp cs
    cs' = before
    ts' = after <> ts

-- | Split comments into ones occuring before the start of the reference
-- span, and those after it.
splitCommentsStart :: RealSrcSpan -> EpAnnComments -> EpAnnComments
splitCommentsStart p (EpaComments cs) = cs'
  where
    cmp (L (EpaSpan (RealSrcSpan l _)) _) = ss2pos l > ss2posEnd p
    cmp (L _ _) = True
    (before, after) = break cmp cs
    cs' = case after of
      [] -> EpaComments cs
      _ -> EpaCommentsBalanced before after
splitCommentsStart p (EpaCommentsBalanced cs ts) = EpaCommentsBalanced cs' ts'
  where
    cmp (L (EpaSpan (RealSrcSpan l _)) _) = ss2pos l > ss2posEnd p
    cmp (L _ _) = True
    (before, after) = break cmp cs
    cs' = before
    ts' = after <> ts

moveLeadingComments :: (Data t, Data u, Monoid u)
  => LocatedAnS t a -> EpAnnS u -> (LocatedAnS t a, EpAnnS u)
moveLeadingComments (L la a) lb = (L la' a, lb')
  `debug` ("moveLeadingComments: (before, after, la', lb'):" ++ showAst (before, after, la', lb'))
  where
    split = splitCommentsEnd (realSrcSpan "moveLeadingComments" $ locA la) (s_comments la)
    before = sortEpaComments $ priorComments split
    after = sortEpaComments $ getFollowingComments split

    -- TODO: need to set an entry delta on lb' to zero, and move the
    -- original spacing to the first comment.

    la' = setCommentsEpAnnS la (EpaCommentsBalanced [] after)
    lb' = addCommentsToEpAnnS lb (EpaCommentsBalanced before [])

-- | A GHC comment includes the span of the preceding (non-comment)
-- token.  Takes an original list of comments, and converts the
-- 'Anchor's to have a have a `MovedAnchor` operation based on the
-- original locations.
commentOrigDeltas :: [LEpaComment] -> [LEpaComment]
commentOrigDeltas [] = []
commentOrigDeltas lcs = map commentOrigDelta lcs

addCommentOrigDeltas :: EpAnnComments -> EpAnnComments
addCommentOrigDeltas (EpaComments cs) = EpaComments (commentOrigDeltas cs)
addCommentOrigDeltas (EpaCommentsBalanced pcs fcs)
  = EpaCommentsBalanced (commentOrigDeltas pcs) (commentOrigDeltas fcs)

addCommentOrigDeltasAnn :: (EpAnn a) -> (EpAnn a)
addCommentOrigDeltasAnn EpAnnNotUsed   = EpAnnNotUsed
addCommentOrigDeltasAnn (EpAnn e a cs) = EpAnn e a (addCommentOrigDeltas cs)

addCommentOrigDeltasEpAnnS :: (EpAnnS a) -> (EpAnnS a)
addCommentOrigDeltasEpAnnS (EpAnnS e a cs) = EpAnnS e a (addCommentOrigDeltas cs)

-- TODO: this is replicating functionality in ExactPrint. Sort out the
-- import loop`
anchorFromLocatedA :: LocatedA a -> RealSrcSpan
anchorFromLocatedA (L (EpAnnS anc _ _) _) = anchor anc

-- ---------------------------------------------------------------------

balanceSameLineComments :: (Monad m)
  => LMatch GhcPs (LHsExpr GhcPs) -> TransformT m (LMatch GhcPs (LHsExpr GhcPs))
balanceSameLineComments (L la (Match anm mctxt pats (GRHSs x grhss lb))) = do
  logTr $ "balanceSameLineComments: (la)=" ++ showGhc (ss2range $ locA la)
  logTr $ "balanceSameLineComments: [logInfo]=" ++ showAst logInfo
  return (L la' (Match anm mctxt pats (GRHSs x grhss' lb)))
  where
    simpleBreak n (r,_) = r > n
    (la',grhss', logInfo) = case reverse grhss of
      [] -> (la,grhss,[])
      (L lg g@(GRHS EpAnnNotUsed _gs _rhs):grs) -> (la,reverse $ (L lg g):grs,[])
      (L lg (GRHS ga gs rhs):grs) -> (la'',reverse $ (L lg (GRHS ga' gs rhs)):grs,[(gac,(csp,csf))])
        where
          an1 = la
          -- anc1 = addCommentOrigDeltas $ s_comments an1
          anc1 = s_comments an1
          (EpAnn anc an _) = ga :: EpAnn GrhsAnn
          (csp,csf) = case anc1 of
            EpaComments cs -> ([],cs)
            EpaCommentsBalanced p f -> (p,f)
          (move',stay') = break (simpleBreak 0) (trailingCommentsDeltas (anchor anc) csf)
          move = map snd move'
          stay = map snd stay'
          cs1 = EpaCommentsBalanced csp stay

          -- gac = addCommentOrigDeltas $ epAnnComments ga
          gac = epAnnComments ga
          gfc = getFollowingComments gac
          gac' = setFollowingComments gac (sortEpaComments $ gfc ++ move)
          ga' = (EpAnn anc an gac')

          la'' = setCommentsEpAnnS la cs1

-- ---------------------------------------------------------------------

anchorEof :: ParsedSource -> ParsedSource
anchorEof (L l m@(HsModule (XModulePs an _lo _ _) _mn _exps _imps _decls)) = L l (m { hsmodExt = (hsmodExt m){ hsmodAnn = an' } })
  where
    an' = addCommentOrigDeltasAnn an

-- ---------------------------------------------------------------------

commentsOrigDeltasDecl :: LHsDecl GhcPs -> LHsDecl GhcPs
commentsOrigDeltasDecl (L an d) = L (addCommentOrigDeltasEpAnnS an) d

-- ---------------------------------------------------------------------

-- | Create a @SrcSpanAnn@ with a @MovedAnchor@ operation using the
-- given @DeltaPos@.
noAnnSrcSpanDP :: (Monoid ann) => DeltaPos -> (EpAnnS ann)
noAnnSrcSpanDP dp
  = EpAnnS (EpaDelta dp []) mempty emptyComments

-- | Create a @SrcSpanAnn@ with a @MovedAnchor@ operation using the
-- given @DeltaPos@.
noAnnSrcSpanDPI :: (Monoid ann) => SrcSpan -> DeltaPos -> SrcSpanAnn' (EpAnn ann)
noAnnSrcSpanDPI l dp
  = SrcSpanAnn (EpAnn (EpaDelta dp []) mempty emptyComments) l

noAnnSrcSpanDP0I :: (Monoid ann) => SrcSpan -> SrcSpanAnn' (EpAnn ann)
noAnnSrcSpanDP0I l = noAnnSrcSpanDPI l (SameLine 0)

noAnnSrcSpanDP0 :: (Monoid ann) => (EpAnnS ann)
noAnnSrcSpanDP0 = noAnnSrcSpanDP (SameLine 0)

noAnnSrcSpanDP1 :: (Monoid ann) => SrcSpan -> SrcSpanAnn' (EpAnn ann)
noAnnSrcSpanDP1 l = noAnnSrcSpanDPI l (SameLine 1)

noAnnSrcSpanDPn :: (Monoid ann) => SrcSpan -> Int -> SrcSpanAnn' (EpAnn ann)
noAnnSrcSpanDPn l s = noAnnSrcSpanDPI l (SameLine s)

d0 :: EpaLocation
d0 = EpaDelta (SameLine 0) []

d1 :: EpaLocation
d1 = EpaDelta (SameLine 1) []

dn :: Int -> EpaLocation
dn n = EpaDelta (SameLine n) []

m0 :: DeltaPos
m0 = SameLine 0

m1 :: DeltaPos
m1 = SameLine 1

mn :: Int -> DeltaPos
mn n = SameLine n

addComma :: SrcSpanAnnA -> SrcSpanAnnA
addComma (EpAnnS anc (AnnListItem as) cs)
  = (EpAnnS anc (AnnListItem (AddCommaAnn d0:as)) cs)

-- ---------------------------------------------------------------------

-- | Insert a declaration into an AST element having sub-declarations
-- (@HasDecls@) according to the given location function.
insertAt :: (HasDecls ast)
              => (LHsDecl GhcPs
                  -> [LHsDecl GhcPs]
                  -> [LHsDecl GhcPs])
              -> ast
              -> LHsDecl GhcPs
              -> Transform ast
insertAt f t decl = do
  oldDecls <- hsDecls t
  logTr $ "oldDecls:" ++ showAst oldDecls
  oldDeclsb <- balanceCommentsList oldDecls
  logTr $ "oldDeclsb:" ++ showAst oldDeclsb
  -- let oldDecls' = map commentsOrigDeltasDecl oldDeclsb
  let oldDecls' = oldDeclsb
  logTr $ "oldDecls':" ++ showAst oldDecls'
  replaceDecls t (f decl oldDecls')

-- |Insert a declaration at the beginning or end of the subdecls of the given
-- AST item
insertAtStart, insertAtEnd :: (HasDecls ast)
              => ast
              -> LHsDecl GhcPs
              -> Transform ast

insertAtEnd   = insertAt (\x xs -> xs ++ [x])

insertAtStart = insertAt insertFirst
  where
    insertFirst x xs =
      case xs of
        [] -> [x]
        (h:t) -> x:setEntryDP h (DifferentLine 2 0):t
                   `debug` ("insertAtStart:h=" ++ showAst h)


-- |Insert a declaration at a specific location in the subdecls of the given
-- AST item
insertAfter, insertBefore :: (HasDecls (LocatedA ast))
                          => LocatedA old
                          -> LocatedA ast
                          -> LHsDecl GhcPs
                          -> Transform (LocatedA ast)
insertAfter (getLocA -> k) = insertAt findAfter
  where
    findAfter x xs =
      case span (\(L l _) -> locA l /= k) xs of
        ([],[]) -> [x]
        (fs,[]) -> fs++[x]
        (fs, b:bs) -> fs ++ (b : x : bs)
      -- let (fs, b:bs) = span (\(L l _) -> locA l /= k) xs
      -- in fs ++ (b : x : bs)
insertBefore (getLocA -> k) = insertAt findBefore
  where
    findBefore x xs =
      let (fs, bs) = span (\(L l _) -> locA l /= k) xs
      in fs ++ (x : bs)

-- =====================================================================
-- start of HasDecls instances
-- =====================================================================

-- |Provide a means to get and process the immediate child declartions of a
-- given AST element.
class (Data t) => HasDecls t where
-- ++AZ++: TODO: add tests to confirm that hsDecls followed by replaceDecls is idempotent

    -- | Return the 'HsDecl's that are directly enclosed in the
    -- given syntax phrase. They are always returned in the wrapped 'HsDecl'
    -- form, even if orginating in local decls. This is safe, as annotations
    -- never attach to the wrapper, only to the wrapped item.
    hsDecls :: (Monad m) => t -> TransformT m [LHsDecl GhcPs]

    -- | Replace the directly enclosed decl list by the given
    --  decl list. Runs in the 'Transform' monad to be able to update list order
    --  annotations, and rebalance comments and other layout changes as needed.
    --
    -- For example, a call on replaceDecls for a wrapped 'FunBind' having no
    -- where clause will convert
    --
    -- @
    -- -- |This is a function
    -- foo = x -- comment1
    -- @
    -- in to
    --
    -- @
    -- -- |This is a function
    -- foo = x -- comment1
    --   where
    --     nn = 2
    -- @
    replaceDecls :: (Monad m) => t -> [LHsDecl GhcPs] -> TransformT m t

-- ---------------------------------------------------------------------

instance HasDecls ParsedSource where
  hsDecls (L _ (HsModule (XModulePs _ _lo _ _) _mn _exps _imps decls)) = return decls

  replaceDecls (L l (HsModule (XModulePs a lo deps haddocks) mname exps imps _decls)) decls
    = do
        logTr "replaceDecls LHsModule"
        -- modifyAnnsT (captureOrder m decls)
        -- let decls' = map packFunDecl decls
        return (L l (HsModule (XModulePs a lo deps haddocks) mname exps imps decls))

-- ---------------------------------------------------------------------

instance HasDecls (LocatedA (Match GhcPs (LocatedA (HsExpr GhcPs)))) where
  hsDecls (L _ (Match _ _ _ (GRHSs _ _ lb))) = return $ hsDeclsLocalBinds lb

  replaceDecls (L l (Match xm c p (GRHSs xr rhs binds))) []
    = do
        logTr "replaceDecls LMatch empty decls"
        binds'' <- replaceDeclsValbinds WithoutWhere binds []
        return (L l (Match xm c p (GRHSs xr rhs binds'')))

  replaceDecls m@(L l (Match xm c p (GRHSs xr rhs binds))) newBinds
    = do
        logTr "replaceDecls LMatch nonempty decls"
        -- Need to throw in a fresh where clause if the binds were empty,
        -- in the annotations.
        (l', rhs') <- case binds of
          EmptyLocalBinds{} -> do
            logTr $ "replaceDecls LMatch empty binds"

            logDataWithAnnsTr "Match.replaceDecls:balancing comments:m" m
            L l' m' <- balanceSameLineComments m
            logDataWithAnnsTr "Match.replaceDecls:(m1')" (L l' m')
            return (l', grhssGRHSs $ m_grhss m')
          _ -> return (l, rhs)
        binds'' <- replaceDeclsValbinds WithWhere binds newBinds
        logDataWithAnnsTr "Match.replaceDecls:binds'" binds''
        return (L l' (Match xm c p (GRHSs xr rhs' binds'')))

-- ---------------------------------------------------------------------

instance HasDecls (LocatedA (HsExpr GhcPs)) where
  hsDecls (L _ (HsLet _ _ decls _ _ex)) = return $ hsDeclsLocalBinds decls
  hsDecls _                             = return []

  replaceDecls (L ll (HsLet x tkLet binds tkIn ex)) newDecls
    = do
        logTr "replaceDecls HsLet"
        let lastAnc = realSrcSpan "replaceDecls" $ spanHsLocaLBinds binds
        -- TODO: may be an intervening comment, take account for lastAnc
        let (tkLet', tkIn', ex',newDecls') = case (tkLet, tkIn) of
              (L (TokenLoc l) ls, L (TokenLoc i) is) ->
                let
                  off = case l of
                          (EpaSpan (RealSrcSpan r _)) -> LayoutStartCol $ snd $ ss2pos r
                          (EpaSpan _) -> LayoutStartCol 0 -- Arbitrary
                          (EpaDelta (SameLine _) _) -> LayoutStartCol 0
                          (EpaDelta (DifferentLine _ c) _) -> LayoutStartCol c
                  ex'' = setEntryDPFromAnchor off i ex
                  newDecls'' = case newDecls of
                    [] -> newDecls
                    (d:ds) -> setEntryDPDecl d (SameLine 0) : ds
                -- in ( EpAnn a (AnnsLet l (addEpaLocationDelta off lastAnc i)) cs
                in ( L (TokenLoc l) ls
                   , L (TokenLoc (addEpaLocationDelta off lastAnc i)) is
                   , ex''
                   , newDecls'')
              (_,_) -> (tkLet, tkIn, ex, newDecls)
        binds' <- replaceDeclsValbinds WithoutWhere binds newDecls'
        return (L ll (HsLet x tkLet' binds' tkIn' ex'))

  -- TODO: does this make sense? Especially as no hsDecls for HsPar
  replaceDecls (L l (HsPar x lpar e rpar)) newDecls
    = do
        logTr "replaceDecls HsPar"
        e' <- replaceDecls e newDecls
        return (L l (HsPar x lpar e' rpar))
  replaceDecls old _new = error $ "replaceDecls (LHsExpr GhcPs) undefined for:" ++ showGhc old

-- ---------------------------------------------------------------------

-- | Extract the immediate declarations for a 'PatBind' wrapped in a 'ValD'. This
-- cannot be a member of 'HasDecls' because a 'FunBind' is not idempotent
-- for 'hsDecls' \/ 'replaceDecls'. 'hsDeclsPatBindD' \/ 'replaceDeclsPatBindD' is
-- idempotent.
hsDeclsPatBindD :: LHsDecl GhcPs -> [LHsDecl GhcPs]
hsDeclsPatBindD (L l (ValD _ d)) = hsDeclsPatBind (L l d)
hsDeclsPatBindD x = error $ "hsDeclsPatBindD called for:" ++ showGhc x

-- | Extract the immediate declarations for a 'PatBind'. This
-- cannot be a member of 'HasDecls' because a 'FunBind' is not idempotent
-- for 'hsDecls' \/ 'replaceDecls'. 'hsDeclsPatBind' \/ 'replaceDeclsPatBind' is
-- idempotent.
hsDeclsPatBind :: LHsBind GhcPs -> [LHsDecl GhcPs]
hsDeclsPatBind (L _ (PatBind _ _ (GRHSs _ _grhs lb))) = hsDeclsLocalBinds lb
hsDeclsPatBind x = error $ "hsDeclsPatBind called for:" ++ showGhc x

-- -------------------------------------

-- | Replace the immediate declarations for a 'PatBind' wrapped in a 'ValD'. This
-- cannot be a member of 'HasDecls' because a 'FunBind' is not idempotent
-- for 'hsDecls' \/ 'replaceDecls'. 'hsDeclsPatBindD' \/ 'replaceDeclsPatBindD' is
-- idempotent.
replaceDeclsPatBindD :: (Monad m) => LHsDecl GhcPs -> [LHsDecl GhcPs]
                     -> TransformT m (LHsDecl GhcPs)
replaceDeclsPatBindD (L l (ValD x d)) newDecls = do
  (L _ d') <- replaceDeclsPatBind (L l d) newDecls
  return (L l (ValD x d'))
replaceDeclsPatBindD x _ = error $ "replaceDeclsPatBindD called for:" ++ showGhc x

-- | Replace the immediate declarations for a 'PatBind'. This
-- cannot be a member of 'HasDecls' because a 'FunBind' is not idempotent
-- for 'hsDecls' \/ 'replaceDecls'. 'hsDeclsPatBind' \/ 'replaceDeclsPatBind' is
-- idempotent.
replaceDeclsPatBind :: (Monad m) => LHsBind GhcPs -> [LHsDecl GhcPs]
                    -> TransformT m (LHsBind GhcPs)
replaceDeclsPatBind (L l (PatBind x a (GRHSs xr rhss binds))) newDecls
    = do
        logTr "replaceDecls PatBind"
        binds'' <- replaceDeclsValbinds WithWhere binds newDecls
        return (L l (PatBind x a (GRHSs xr rhss binds'')))
replaceDeclsPatBind x _ = error $ "replaceDeclsPatBind called for:" ++ showGhc x

-- ---------------------------------------------------------------------

instance HasDecls (LocatedA (Stmt GhcPs (LocatedA (HsExpr GhcPs)))) where
  hsDecls (L _ (LetStmt _ lb))      = return $ hsDeclsLocalBinds lb
  hsDecls (L _ (LastStmt _ e _ _))  = hsDecls e
  hsDecls (L _ (BindStmt _ _pat e)) = hsDecls e
  hsDecls (L _ (BodyStmt _ e _ _))  = hsDecls e
  hsDecls _                         = return []

  replaceDecls (L l (LetStmt x lb)) newDecls
    = do
        lb'' <- replaceDeclsValbinds WithWhere lb newDecls
        return (L l (LetStmt x lb''))
  replaceDecls (L l (LastStmt x e d se)) newDecls
    = do
        e' <- replaceDecls e newDecls
        return (L l (LastStmt x e' d se))
  replaceDecls (L l (BindStmt x pat e)) newDecls
    = do
      e' <- replaceDecls e newDecls
      return (L l (BindStmt x pat e'))

  replaceDecls (L l (BodyStmt x e a b)) newDecls
    = do
      e' <- replaceDecls e newDecls
      return (L l (BodyStmt x e' a b))
  replaceDecls x _newDecls = return x

-- =====================================================================
-- end of HasDecls instances
-- =====================================================================

-- ---------------------------------------------------------------------
-- A @FunBind@ is a container for @[LMatch GhcPs]@
--
-- When being used as a Bind (or Decl), the surrounding context
-- annotations must appear at the FunBind level, so it can be
-- manipulated in the context of other Binds or Decls.
--
-- Surrounding context annotations are specifically prior comments,
-- following comments and trailing annotations.
--
-- But when we unpack the container, by calling @hsDecls@ on a
-- @FunBind@, we must make sure that the component parts fully
-- represent the relationship between them and the surrounding
-- declarations.
--
-- This means pushing the prior context annotations into the first
-- match, and the following ones into the last match when returning
-- @hsDecls@, and undoing this for @replaceDecls@.

-- |Push leading and trailing top level annotations into the @[LMatch GhcPs]@
unpackFunBind :: LHsBind GhcPs -> LHsBind GhcPs
unpackFunBind (L loc (FunBind x1 fid (MG x2 (L lg (L lm m:matches)))))
  = (L loc'' (FunBind x1 fid (MG x2 (L lg (reverse (L llm' lmtch:tail matches'))))))
     -- `debug` ("unpackFunBind: ="
     --          ++ showAst (("loc",loc), ("loc'",loc'), ("loc''",loc''),
     --                      ("lm'",lm'), ("llm",llm), ("llm'", llm')))
  where
    (loc', lm') = transferPriorCommentsA loc lm
    matches' = reverse $ L lm' m:matches
    L llm lmtch = head matches' -- Guaranteed at least one
    (loc'', llm') = transferFollowingA loc' llm

unpackFunBind d = d

-- |Pull leading and trailing annotations from the @[LMatch GhcPs]@ to
-- the top level.
packFunBind :: LHsBind GhcPs -> LHsBind GhcPs
packFunBind (L loc (FunBind x1 fid (MG x2 (L lg (L lm m:matches)))))
  = (L loc'' (FunBind x1 fid (MG x2 (L lg (reverse (L llm' lmtch:tail matches'))))))
     `debug` ("packFunBind: ="
              ++ showAst (("loc",loc), ("loc'",loc'), ("loc''",loc''),
                          ("lm'",lm'), ("llm",llm), ("llm'", llm')))
  where
    (lm', loc') = transferPriorCommentsA lm loc
    matches' = reverse $ L lm' m:matches
    L llm lmtch = head matches' -- Guaranteed at least one
    (llm', loc'') = transferFollowingA llm loc'
packFunBind d = d

packFunDecl :: LHsDecl GhcPs -> LHsDecl GhcPs
packFunDecl (L l (ValD x b)) = L l' (ValD x b')
  where
    L l' b' = packFunBind (L l b)

unpackFunDecl :: LHsDecl GhcPs -> LHsDecl GhcPs
unpackFunDecl (L l (ValD x b)) = L l' (ValD x b')
  where
    L l' b' = unpackFunBind (L l b)

-- TODO: Move to Annotation.hs

transferPriorCommentsA :: SrcSpanAnnA -> SrcSpanAnnA -> (SrcSpanAnnA,  SrcSpanAnnA)
transferPriorCommentsA (EpAnnS a1 an1 cs1) (EpAnnS a2 an2 cs2)
  = (EpAnnS a1 an1 cs1', EpAnnS a2 an2 cs2')
      `debug` ("transferPriorCommentsA: ((cs1, cs2), (cs1', cs2'))=" ++ showAst ((cs1, cs2), (cs1', cs2')))
  where
    pc = priorComments cs1
    fc = getFollowingComments cs1
    cs1' = setFollowingComments emptyComments fc
    cs2' = setPriorComments cs2 (priorComments cs2 <> pc)

transferFollowingA :: SrcSpanAnnA -> SrcSpanAnnA -> (SrcSpanAnnA,  SrcSpanAnnA)
transferFollowingA (EpAnnS a1 an1 cs1) (EpAnnS a2 an2 cs2)
  = (EpAnnS a1 mempty cs1', EpAnnS a2 (an1 <> an2) cs2')
      `debug` ("transferFollowingA: (pc,fc,cs1', cs2')=" ++ showAst (pc,fc,cs1', cs2'))
  where
    pc = priorComments cs1
    fc = getFollowingComments cs1
    cs1' = setPriorComments emptyComments pc
    cs2' = setFollowingComments cs2 fc

-- ---------------------------------------------------------------------

-- |Look up the annotated order and sort the decls accordingly
-- TODO:AZ: this should be pure
orderedDecls :: (Monad m)
             => AnnSortKey [RealSrcSpan] -> [LHsDecl GhcPs] -> TransformT m [LHsDecl GhcPs]
orderedDecls sortKey decls = do
  case sortKey of
    NoAnnSortKey -> do
      -- return decls
      return $ sortBy (\a b -> compare (realSrcSpan "orderedDecls" $ getLocA a) (realSrcSpan "orderedDecls" $ getLocA b)) decls
    AnnSortKey keys -> do
      let ds = map (\s -> (rs $ getLocA s,s)) decls
          ordered = map snd $ orderByKey ds keys
      return ordered

-- orderedDeclsBinds :: (Monad m)
--   => AnnSortKey [DeclTag]
--   -> [LHsDecl GhcPs] -> [LHsDecl GhcPs]
--   -> TransformT m [LHsDecl GhcPs]
-- orderedDeclsBinds sortKey binds sigs = do
--   case sortKey of
--     NoAnnSortKey -> do
--       -- return decls
--       return $ sortBy (\a b ->
--                          compare (realSrcSpan "orderedDecls" $ getLocA a)
--                                  (realSrcSpan "orderedDecls" $ getLocA b)) (binds ++ sigs)
--     AnnSortKey keys -> do
--       let
--         go [] _ _                      = []
--         go (ValDTag:ks) (b:bs) ss = b : go ks bs ss
--         go (SigDTag:ks) bs (s:ss) = s : go ks bs ss
--         go (_:ks) bs ss           =     go ks bs ss

--       return (go keys binds sigs)

-- ---------------------------------------------------------------------

-- hsDeclsValBinds :: (Monad m) => HsLocalBinds GhcPs -> TransformT m [LHsDecl GhcPs]
-- hsDeclsValBinds lb = case lb of
--     HsValBinds _ (ValBinds sortKey bs sigs) -> do
--       let
--         bds = map wrapDecl (bagToList bs)
--         sds = map wrapSig sigs
--       orderedDeclsBinds sortKey bds sds
--     HsValBinds _ (XValBindsLR _) -> error $ "hsDecls.XValBindsLR not valid"
--     HsIPBinds {}       -> return []
--     EmptyLocalBinds {} -> return []

data WithWhere = WithWhere
               | WithoutWhere
               deriving (Eq,Show)

-- | Utility function for returning decls to 'HsLocalBinds'. Use with
-- care, as this does not manage the declaration order, the
-- ordering should be done by the calling function from the 'HsLocalBinds'
-- context in the AST.
replaceDeclsValbinds :: (Monad m)
                     => WithWhere
                     -> HsLocalBinds GhcPs -> [LHsDecl GhcPs]
                     -> TransformT m (HsLocalBinds GhcPs)
replaceDeclsValbinds _ _ [] = do
  return (EmptyLocalBinds NoExtField)
replaceDeclsValbinds w b@(HsValBinds a _) new
    = do
        logTr "replaceDeclsValbinds"
        let oldSpan = spanHsLocaLBinds b
        an <- oldWhereAnnotation a w (realSrcSpan "replaceDeclsValbinds" oldSpan)
        let decs = listToBag $ concatMap decl2Bind new
        let sigs = concatMap decl2Sig new
        let sortKey = captureOrderBinds new
        return (HsValBinds an (ValBinds sortKey decs sigs))
replaceDeclsValbinds _ (HsIPBinds {}) _new    = error "undefined replaceDecls HsIPBinds"
replaceDeclsValbinds w (EmptyLocalBinds _) new
    = do
        logTr "replaceDecls HsLocalBinds"
        an <- newWhereAnnotation w
        let newBinds = concatMap decl2Bind new
            newSigs  = concatMap decl2Sig  new
        let decs = listToBag $ newBinds
        let sigs = newSigs
        let sortKey = captureOrderBinds new
        return (HsValBinds an (ValBinds sortKey decs sigs))

oldWhereAnnotation :: (Monad m)
  => EpAnn AnnList -> WithWhere -> RealSrcSpan -> TransformT m (EpAnn AnnList)
oldWhereAnnotation EpAnnNotUsed ww _oldSpan = do
  let w = case ww of
        WithWhere -> [AddEpAnn AnnWhere (EpaDelta (SameLine 0) [])]
        WithoutWhere -> []
  let anc2' = EpaDelta (SameLine 1) []
  (anc, anc2) <- do
          return (EpaDelta (DifferentLine 1 2) []
                 , anc2')
  let an = EpAnn anc
                  (AnnList (Just anc2) Nothing Nothing w [])
                  emptyComments
  return an
oldWhereAnnotation (EpAnn anc an cs) ww _oldSpan = do
  -- TODO: when we set DP (0,0) for the HsValBinds EpEpaLocation, change the AnnList anchor to have the correct DP too
  let (AnnList ancl o c _r t) = an
  let w = case ww of
        WithWhere -> [AddEpAnn AnnWhere (EpaDelta (SameLine 0) [])]
        WithoutWhere -> []
  (anc', ancl') <- do
        case ww of
          WithWhere -> return (anc, ancl)
          WithoutWhere -> return (anc, ancl)
  let an' = EpAnn anc'
                  (AnnList ancl' o c w t)
                  cs
  return an'

newWhereAnnotation :: (Monad m) => WithWhere -> TransformT m (EpAnn AnnList)
newWhereAnnotation ww = do
  let anc  = EpaDelta (DifferentLine 1 2) []
  let anc2 = EpaDelta (DifferentLine 1 4) []
  let w = case ww of
        WithWhere -> [AddEpAnn AnnWhere (EpaDelta (SameLine 0) [])]
        WithoutWhere -> []
  let an = EpAnn anc
                  (AnnList (Just anc2) Nothing Nothing w [])
                  emptyComments
  return an

-- ---------------------------------------------------------------------

type Decl  = LHsDecl GhcPs
type PMatch = LMatch GhcPs (LHsExpr GhcPs)

-- |Modify a 'LHsBind' wrapped in a 'ValD'. For a 'PatBind' the
-- declarations are extracted and returned after modification. For a
-- 'FunBind' the supplied 'SrcSpan' is used to identify the specific
-- 'Match' to be transformed, for when there are multiple of them.
modifyValD :: forall m t. (HasTransform m)
                => SrcSpan
                -> Decl
                -> (PMatch -> [Decl] -> m ([Decl], Maybe t))
                -> m (Decl,Maybe t)
modifyValD p pb@(L ss (ValD _ (PatBind {} ))) f =
  if (locA ss) == p
     then do
       let ds = hsDeclsPatBindD pb
       (ds',r) <- f (error "modifyValD.PatBind should not touch Match") ds
       pb' <- liftT $ replaceDeclsPatBindD pb ds'
       return (pb',r)
     else return (pb,Nothing)
modifyValD p decl f = do
  (decl',r) <- runStateT (everywhereM (mkM doModLocal) (unpackFunDecl decl)) Nothing
  return (packFunDecl decl',r)
  where
    doModLocal :: PMatch -> StateT (Maybe t) m PMatch
    doModLocal  (match@(L ss _) :: PMatch) = do
         if (locA ss) == p
           then do
             ds <- lift $ liftT $ hsDecls match
                `debug` ("modifyValD: match=" ++ showAst match)
             (ds',r) <- lift $ f match ds
             put r
             match' <- lift $ liftT $ replaceDecls match ds'
             return match'
           else return match

-- ---------------------------------------------------------------------

-- |Used to integrate a @Transform@ into other Monad stacks
class (Monad m) => (HasTransform m) where
  liftT :: Transform a -> m a

instance Monad m => HasTransform (TransformT m) where
  liftT = hoistTransform (return . runIdentity)

-- ---------------------------------------------------------------------

-- | Apply a transformation to the decls contained in @t@
modifyDeclsT :: (HasDecls t,HasTransform m)
             => ([LHsDecl GhcPs] -> m [LHsDecl GhcPs])
             -> t -> m t
modifyDeclsT action t = do
  decls <- liftT $ hsDecls t
  decls' <- action decls
  liftT $ replaceDecls t decls'
