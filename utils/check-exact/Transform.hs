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
        , noAnnSrcSpanDP
        , noAnnSrcSpanDP0
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
        , captureOrder
        , captureLineSpacing
        , captureMatchLineSpacing
        , captureTypeSigSpacing

        -- * Operations
        , isUniqueSrcSpan

        -- * Pure functions
        , setEntryDP
        , getEntryDP
        , transferEntryDP
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
captureOrder :: [LocatedA b] -> AnnSortKey
captureOrder ls = AnnSortKey $ map (rs . getLocA) ls

-- ---------------------------------------------------------------------

captureMatchLineSpacing :: LHsDecl GhcPs -> LHsDecl GhcPs
captureMatchLineSpacing (L l (ValD x (FunBind a b (MG c (L d ms )))))
                       = L l (ValD x (FunBind a b (MG c (L d ms'))))
    where
      ms' :: [LMatch GhcPs (LHsExpr GhcPs)]
      ms' = captureLineSpacing ms
captureMatchLineSpacing d = d

captureLineSpacing :: Default t
                   => [LocatedAn t e] -> [LocatedAn t e]
captureLineSpacing [] = []
captureLineSpacing [d] = [d]
captureLineSpacing (de1:d2:ds) = de1:captureLineSpacing (d2':ds)
  where
    (l1,_) = ss2pos $ rs $ getLocA de1
    (l2,_) = ss2pos $ rs $ getLocA d2
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
      L (SrcSpanAnn EpAnnNotUsed   ll) _ -> realSrcSpan ll
      L (SrcSpanAnn (EpAnn anc' _ _) _) _ -> anchor anc' -- TODO MovedAnchor?
    dc' = case dca of
      EpaSpan r _ -> AddEpAnn kw (EpaDelta (ss2delta (ss2posEnd rd) r) [])
      EpaDelta _ _ -> AddEpAnn kw dca

    -- ---------------------------------

    ty' :: LHsSigType GhcPs
    ty' = case ty of
      (L (SrcSpanAnn EpAnnNotUsed    ll) b)
        -> let
             op = case dca of
               EpaSpan r _ -> MovedAnchor (ss2delta (ss2posEnd r) (realSrcSpan ll))
               EpaDelta _ _ -> MovedAnchor (SameLine 1)
           in (L (SrcSpanAnn (EpAnn (Anchor (realSrcSpan ll) op) mempty emptyComments) ll) b)
      (L (SrcSpanAnn (EpAnn (Anchor r op) a c) ll) b)
        -> let
              op' = case op of
                MovedAnchor _ -> op
                _ -> case dca of
                  EpaSpan dcr _ -> MovedAnchor (ss2delta (ss2posEnd dcr) r)
                  EpaDelta _ _ -> MovedAnchor (SameLine 1)
           in (L (SrcSpanAnn (EpAnn (Anchor r op') a c) ll) b)

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

-- |Convert a 'LSig' into a 'LHsDecl'
wrapSig :: LSig GhcPs -> LHsDecl GhcPs
wrapSig (L l s) = L l (SigD NoExtField s)

-- ---------------------------------------------------------------------

-- |Convert a 'LHsBind' into a 'LHsDecl'
wrapDecl :: LHsBind GhcPs -> LHsDecl GhcPs
wrapDecl (L l s) = L l (ValD NoExtField s)

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
setEntryDP :: Default t => LocatedAn t a -> DeltaPos -> LocatedAn t a
setEntryDP (L (SrcSpanAnn EpAnnNotUsed l) a) dp
  = L (SrcSpanAnn
           (EpAnn (Anchor (realSrcSpan l) (MovedAnchor dp)) def emptyComments)
           l) a
setEntryDP (L (SrcSpanAnn (EpAnn (Anchor r _) an (EpaComments [])) l) a) dp
  = L (SrcSpanAnn
           (EpAnn (Anchor r (MovedAnchor dp)) an (EpaComments []))
           l) a
setEntryDP (L (SrcSpanAnn (EpAnn (Anchor r (MovedAnchor d)) an cs) l) a) dp
  = L (SrcSpanAnn
           (EpAnn (Anchor r (MovedAnchor d')) an cs')
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
    go (L (Anchor rr (MovedAnchor ma)) c) = (d,  L (Anchor rr (MovedAnchor ma)) c)
    go (L (Anchor rr                _) c) = (d,  L (Anchor rr (MovedAnchor dp)) c)
setEntryDP (L (SrcSpanAnn (EpAnn (Anchor r _) an cs) l) a) dp
  = case sortEpaComments (priorComments cs) of
      [] ->
        L (SrcSpanAnn
               (EpAnn (Anchor r (MovedAnchor dp)) an cs)
               l) a
      (L ca c:cs') ->
        L (SrcSpanAnn
               (EpAnn (Anchor r (MovedAnchor edp)) an cs'')
               l) a
              where
                cs'' = setPriorComments cs (L (Anchor (anchor ca) (MovedAnchor dp)) c:cs')
                lc = head $ reverse $ (L ca c:cs')
                delta = tweakDelta $ ss2delta (ss2pos $ anchor $ getLoc lc) r
                line = getDeltaLine delta
                col = deltaColumn delta
                edp' = if line == 0 then SameLine col
                                    else DifferentLine line col
                edp = edp' `debug` ("setEntryDP :" ++ showGhc (edp', (ss2pos $ anchor $ getLoc lc), r))


-- ---------------------------------------------------------------------

getEntryDP :: LocatedAn t a -> DeltaPos
getEntryDP (L (SrcSpanAnn (EpAnn (Anchor _ (MovedAnchor dp)) _ _) _) _) = dp
getEntryDP _ = SameLine 1

-- ---------------------------------------------------------------------

addEpaLocationDelta :: LayoutStartCol -> RealSrcSpan -> EpaLocation -> EpaLocation
addEpaLocationDelta _off _anc (EpaDelta d cs) = EpaDelta d cs
addEpaLocationDelta  off  anc (EpaSpan r _)
  = EpaDelta (adjustDeltaForOffset off (ss2deltaEnd anc r)) []

-- Set the entry DP for an element coming after an existing keyword annotation
setEntryDPFromAnchor :: LayoutStartCol -> EpaLocation -> LocatedA t -> LocatedA t
setEntryDPFromAnchor _off (EpaDelta _ _) (L la a) = L la a
setEntryDPFromAnchor  off (EpaSpan anc _) ll@(L la _) = setEntryDP ll dp'
  where
    r = case la of
      (SrcSpanAnn EpAnnNotUsed l) -> realSrcSpan l
      (SrcSpanAnn (EpAnn (Anchor r' _) _ _) _) -> r'
    dp' = adjustDeltaForOffset off (ss2deltaEnd anc r)

-- ---------------------------------------------------------------------

-- |Take the annEntryDelta associated with the first item and associate it with the second.
-- Also transfer any comments occuring before it.
transferEntryDP :: (Monad m, Monoid t2, Typeable t1, Typeable t2)
  => LocatedAn t1 a -> LocatedAn t2 b -> TransformT m (LocatedAn t2 b)
transferEntryDP (L (SrcSpanAnn EpAnnNotUsed l1) _) (L (SrcSpanAnn EpAnnNotUsed _) b) = do
  logTr $ "transferEntryDP': EpAnnNotUsed,EpAnnNotUsed"
  return (L (SrcSpanAnn EpAnnNotUsed l1) b)
transferEntryDP (L (SrcSpanAnn (EpAnn anc _an cs) _l1) _) (L (SrcSpanAnn EpAnnNotUsed l2) b) = do
  logTr $ "transferEntryDP': EpAnn,EpAnnNotUsed"
  return (L (SrcSpanAnn (EpAnn anc mempty cs) l2) b)
transferEntryDP (L (SrcSpanAnn (EpAnn anc1 an1 cs1) _l1) _) (L (SrcSpanAnn (EpAnn _anc2 an2 cs2) l2) b) = do
  logTr $ "transferEntryDP': EpAnn,EpAnn"
  -- Problem: if the original had preceding comments, blindly
  -- transferring the location is not correct
  case priorComments cs1 of
    [] -> return (L (SrcSpanAnn (EpAnn anc1 (combine an1 an2) cs2) l2) b)
    -- TODO: what happens if the receiving side already has comments?
    (L anc _:_) -> do
      logDataWithAnnsTr "transferEntryDP':priorComments anc=" anc
      return (L (SrcSpanAnn (EpAnn anc1 (combine an1 an2) (cs1 <> cs2)) l2) b)
transferEntryDP (L (SrcSpanAnn EpAnnNotUsed _l1) _) (L (SrcSpanAnn (EpAnn anc2 an2 cs2) l2) b) = do
  logTr $ "transferEntryDP': EpAnnNotUsed,EpAnn"
  return (L (SrcSpanAnn (EpAnn anc2' an2 cs2) l2) b)
    where
      anc2' = case anc2 of
        Anchor _a op   -> Anchor (realSrcSpan l2) op


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
      L d' _ = setEntryDP (L d ms) dp
      ms' :: [LMatch GhcPs (LHsExpr GhcPs)]
      ms' = case ms of
        [] -> []
        (m0':ms0) -> setEntryDP m0' dp : ms0
pushDeclDP d _dp = d

-- ---------------------------------------------------------------------

balanceCommentsList :: (Monad m) => [LHsDecl GhcPs] -> TransformT m [LHsDecl GhcPs]
balanceCommentsList ds = balanceCommentsList'' ds

balanceCommentsList'' :: (Monad m) => [LHsDecl GhcPs] -> TransformT m [LHsDecl GhcPs]
balanceCommentsList'' [] = return []
balanceCommentsList'' [x] = return [x]
balanceCommentsList'' (a:b:ls) = do
  (a',b') <- balanceComments a b
  r <- balanceCommentsList'' (b':ls)
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
  logTr $ "balanceCommentsFB entered: " ++ showGhc (ss2range $ locA lf)
  -- There are comments on lf.  We need to
  -- + Keep the prior ones here
  -- + move the interior ones to the first match,
  -- + move the trailing ones to the last match.
  let
    split = splitCommentsEnd (realSrcSpan $ locA lf) (epAnnComments $ ann lf)
    split2 = splitCommentsStart (realSrcSpan $ locA lf)  (EpaComments (sortEpaComments $ priorComments split))

    before = sortEpaComments $ priorComments split2
    middle = sortEpaComments $ getFollowingComments split2
    after  = sortEpaComments $ getFollowingComments split

    lf' = setCommentsSrcAnn lf (EpaComments before)
  logTr $ "balanceCommentsFB (before, after): " ++ showAst (before, after)
  let matches' = case matches of
                    (L lm' m':ms') ->
                      (L (addCommentsToSrcAnn lm' (EpaComments middle )) m':ms')
                    _ -> error "balanceCommentsFB"
  matches'' <- balanceCommentsList' matches'
  let (m,ms) = case reverse matches'' of
                 (L lm' m':ms') ->
                   (L (addCommentsToSrcAnn lm' (EpaCommentsBalanced [] after)) m',ms')
                 _ -> error "balanceCommentsFB"
  (m',second') <- balanceComments' m second
  m'' <- balanceCommentsMatch m'
  let (m''',lf'') = case ms of
        [] -> moveLeadingComments m'' lf'
        _  -> (m'',lf')
  logTr $ "balanceCommentsMatch done"
  balanceComments' (L lf'' (FunBind x n (MG o (L lm (reverse (m''':ms)))))) second'
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
    (SrcSpanAnn an1 _loc1) = l
    anc1 = addCommentOrigDeltas $ epAnnComments an1
    cs1f = getFollowingComments anc1
    (move',stay') = break simpleBreak (trailingCommentsDeltas (anchorFromLocatedA (L l ())) cs1f)
    move = map snd move'
    stay = map snd stay'
    (l'', grhss', binds', logInfo)
      = case reverse grhss of
          [] -> (l, [], binds,                 (EpaComments [], SrcSpanAnn EpAnnNotUsed noSrcSpan))
          (L lg g@(GRHS EpAnnNotUsed _grs _rhs):gs)
            -> (l, reverse (L lg g:gs), binds, (EpaComments [], SrcSpanAnn EpAnnNotUsed noSrcSpan))
          (L lg (GRHS ag grs rhs):gs) ->
            let
              anc1' = setFollowingComments anc1 stay
              an1' = setCommentsSrcAnn l anc1'

              -- ---------------------------------
              (moved,bindsm) = pushTrailingComments WithWhere (EpaCommentsBalanced [] move) binds
              -- ---------------------------------

              (EpAnn anc an lgc) = ag
              lgc' = splitCommentsEnd (realSrcSpan $ locA lg) $ addCommentOrigDeltas lgc
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
    (decls, _, _ws1) = runTransform (hsDeclsValBinds lb)
    (an', decls') = case reverse decls of
      [] -> (addCommentsToEpAnn (spanHsLocaLBinds lb) an cs, decls)
      (L la d:ds) -> (an, L (addCommentsToSrcAnn la cs) d:ds)
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
  logTr $ "balanceComments': (loc1,loc2)=" ++ showGhc (ss2range loc1,ss2range loc2)
  logTr $ "balanceComments': (anc1)=" ++ showAst (anc1)
  logTr $ "balanceComments': (cs1s)=" ++ showAst (cs1s)
  logTr $ "balanceComments': (cs1stay,cs1move)=" ++ showAst (cs1stay,cs1move)
  logTr $ "balanceComments': (an1',an2')=" ++ showAst (an1',an2')
  return (la1', la2')
  where
    simpleBreak n (r,_) = r > n
    L (SrcSpanAnn an1 loc1) f = la1
    L (SrcSpanAnn an2 loc2) s = la2
    anc1 = addCommentOrigDeltas $ epAnnComments an1
    anc2 = addCommentOrigDeltas $ epAnnComments an2

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

    an1' = setCommentsSrcAnn (getLoc la1) (EpaCommentsBalanced (map snd cs1p) move)
    an2' = setCommentsSrcAnn (getLoc la2) (EpaCommentsBalanced stay (map snd cs2f))
    la1' = L an1' f
    la2' = L an2' s

-- | Like commentsDeltas, but calculates the delta from the end of the anchor, not the start
trailingCommentsDeltas :: RealSrcSpan -> [LEpaComment]
               -> [(Int, LEpaComment)]
trailingCommentsDeltas _ [] = []
trailingCommentsDeltas anc (la@(L l _):las)
  = deltaComment anc la : trailingCommentsDeltas (anchor l) las
  where
    deltaComment anc' (L loc c) = (abs(ll - al), L loc c)
      where
        (al,_) = ss2posEnd anc'
        (ll,_) = ss2pos (anchor loc)

-- AZ:TODO: this is identical to commentsDeltas
priorCommentsDeltas :: RealSrcSpan -> [LEpaComment]
                    -> [(Int, LEpaComment)]
priorCommentsDeltas anc cs = go anc (reverse $ sortEpaComments cs)
  where
    go :: RealSrcSpan -> [LEpaComment] -> [(Int, LEpaComment)]
    go _ [] = []
    go anc' (la@(L l _):las) = deltaComment anc' la : go (anchor l) las

    deltaComment :: RealSrcSpan -> LEpaComment -> (Int, LEpaComment)
    deltaComment anc' (L loc c) = (abs(ll - al), L loc c)
      where
        (al,_) = ss2pos anc'
        (ll,_) = ss2pos (anchor loc)


-- ---------------------------------------------------------------------

-- | Split comments into ones occuring before the end of the reference
-- span, and those after it.
splitCommentsEnd :: RealSrcSpan -> EpAnnComments -> EpAnnComments
splitCommentsEnd p (EpaComments cs) = cs'
  where
    cmp (L (Anchor l _) _) = ss2pos l > ss2posEnd p
    (before, after) = break cmp cs
    cs' = case after of
      [] -> EpaComments cs
      _ -> EpaCommentsBalanced before after
splitCommentsEnd p (EpaCommentsBalanced cs ts) = EpaCommentsBalanced cs' ts'
  where
    cmp (L (Anchor l _) _) = ss2pos l > ss2posEnd p
    (before, after) = break cmp cs
    cs' = before
    ts' = after <> ts

-- | Split comments into ones occuring before the start of the reference
-- span, and those after it.
splitCommentsStart :: RealSrcSpan -> EpAnnComments -> EpAnnComments
splitCommentsStart p (EpaComments cs) = cs'
  where
    cmp (L (Anchor l _) _) = ss2pos l > ss2pos p
    (before, after) = break cmp cs
    cs' = case after of
      [] -> EpaComments cs
      _ -> EpaCommentsBalanced before after
splitCommentsStart p (EpaCommentsBalanced cs ts) = EpaCommentsBalanced cs' ts'
  where
    cmp (L (Anchor l _) _) = ss2pos l > ss2pos p
    (before, after) = break cmp cs
    cs' = before
    ts' = after <> ts

moveLeadingComments :: (Data t, Data u, Monoid t, Monoid u)
  => LocatedAn t a -> SrcAnn u -> (LocatedAn t a, SrcAnn u)
moveLeadingComments from@(L (SrcSpanAnn EpAnnNotUsed _) _) to = (from, to)
moveLeadingComments (L la a) lb = (L la' a, lb')
  `debug` ("moveLeadingComments: (before, after, la', lb'):" ++ showAst (before, after, la', lb'))
  where
    split = splitCommentsEnd (realSrcSpan $ locA la) (epAnnComments $ ann la)
    before = sortEpaComments $ priorComments split
    after = sortEpaComments $ getFollowingComments split

    -- TODO: need to set an entry delta on lb' to zero, and move the
    -- original spacing to the first comment.

    la' = setCommentsSrcAnn la (EpaComments after)
    lb' = addCommentsToSrcAnn lb (EpaCommentsBalanced before [])

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

-- TODO: this is replicating functionality in ExactPrint. Sort out the
-- import loop`
anchorFromLocatedA :: LocatedA a -> RealSrcSpan
anchorFromLocatedA (L (SrcSpanAnn an loc) _)
  = case an of
      EpAnnNotUsed    -> realSrcSpan loc
      (EpAnn anc _ _) -> anchor anc

-- | A GHC comment includes the span of the preceding token.  Take an
-- original comment, and convert the 'Anchor to have a have a
-- `MovedAnchor` operation based on the original location, only if it
-- does not already have one.
commentOrigDelta :: LEpaComment -> LEpaComment
commentOrigDelta (L (GHC.Anchor la _) (GHC.EpaComment t pp))
  = (L (GHC.Anchor la op) (GHC.EpaComment t pp))
                  `debug` ("commentOrigDelta: (la, pp, r,c, op)=" ++ showAst (la, pp, r,c, op))
  where
        (r,c) = ss2posEnd pp

        op' = if r == 0
               then MovedAnchor (ss2delta (r,c+1) la)
               -- then MovedAnchor (ss2delta (r,c+0) la)
               -- else MovedAnchor (ss2delta (r,c)   la)
               else MovedAnchor (tweakDelta $ ss2delta (r,c)   la)
        op = if t == EpaEofComment && op' == MovedAnchor (SameLine 0)
               then MovedAnchor (DifferentLine 1 0)
               else op'

-- ---------------------------------------------------------------------


-- | For comment-related deltas starting on a new line we have an
-- off-by-one problem. Adjust
tweakDelta :: DeltaPos  -> DeltaPos
tweakDelta (SameLine d) = SameLine d
tweakDelta (DifferentLine l d) = DifferentLine l (d-1)

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
          (SrcSpanAnn an1 _loc1) = la
          anc1 = addCommentOrigDeltas $ epAnnComments an1
          (EpAnn anc an _) = ga :: EpAnn GrhsAnn
          (csp,csf) = case anc1 of
            EpaComments cs -> ([],cs)
            EpaCommentsBalanced p f -> (p,f)
          (move',stay') = break (simpleBreak 0) (trailingCommentsDeltas (anchor anc) csf)
          move = map snd move'
          stay = map snd stay'
          cs1 = EpaCommentsBalanced csp stay

          gac = addCommentOrigDeltas $ epAnnComments ga
          gfc = getFollowingComments gac
          gac' = setFollowingComments gac (sortEpaComments $ gfc ++ move)
          ga' = (EpAnn anc an gac')

          an1' = setCommentsSrcAnn la cs1
          la'' = an1'

-- ---------------------------------------------------------------------

anchorEof :: ParsedSource -> ParsedSource
anchorEof (L l m@(HsModule (XModulePs an _lo _ _) _mn _exps _imps _decls)) = L l (m { hsmodExt = (hsmodExt m){ hsmodAnn = an' } })
  where
    an' = addCommentOrigDeltasAnn an

-- ---------------------------------------------------------------------

commentsOrigDeltasDecl :: LHsDecl GhcPs -> LHsDecl GhcPs
commentsOrigDeltasDecl (L (SrcSpanAnn an l) d) = L (SrcSpanAnn an' l) d
  where
    an' = addCommentOrigDeltasAnn an

-- ---------------------------------------------------------------------

-- | Create a @SrcSpanAnn@ with a @MovedAnchor@ operation using the
-- given @DeltaPos@.
noAnnSrcSpanDP :: (Monoid ann) => SrcSpan -> DeltaPos -> SrcSpanAnn' (EpAnn ann)
noAnnSrcSpanDP l dp
  = SrcSpanAnn (EpAnn (Anchor (realSrcSpan l) (MovedAnchor dp)) mempty emptyComments) l

noAnnSrcSpanDP0 :: (Monoid ann) => SrcSpan -> SrcSpanAnn' (EpAnn ann)
noAnnSrcSpanDP0 l = noAnnSrcSpanDP l (SameLine 0)

noAnnSrcSpanDP1 :: (Monoid ann) => SrcSpan -> SrcSpanAnn' (EpAnn ann)
noAnnSrcSpanDP1 l = noAnnSrcSpanDP l (SameLine 1)

noAnnSrcSpanDPn :: (Monoid ann) => SrcSpan -> Int -> SrcSpanAnn' (EpAnn ann)
noAnnSrcSpanDPn l s = noAnnSrcSpanDP l (SameLine s)

d0 :: EpaLocation
d0 = EpaDelta (SameLine 0) []

d1 :: EpaLocation
d1 = EpaDelta (SameLine 1) []

dn :: Int -> EpaLocation
dn n = EpaDelta (SameLine n) []

m0 :: AnchorOperation
m0 = MovedAnchor $ SameLine 0

m1 :: AnchorOperation
m1 = MovedAnchor $ SameLine 1

mn :: Int -> AnchorOperation
mn n = MovedAnchor $ SameLine n

addComma :: SrcSpanAnnA -> SrcSpanAnnA
addComma (SrcSpanAnn EpAnnNotUsed l)
  = (SrcSpanAnn (EpAnn (spanAsAnchor l) (AnnListItem [AddCommaAnn d0]) emptyComments) l)
addComma (SrcSpanAnn (EpAnn anc (AnnListItem as) cs) l)
  = (SrcSpanAnn (EpAnn anc (AnnListItem (AddCommaAnn d0:as)) cs) l)

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
  oldDeclsb <- balanceCommentsList oldDecls
  let oldDecls' = map commentsOrigDeltasDecl oldDeclsb
  replaceDecls t (f decl oldDecls')

-- |Insert a declaration at the beginning or end of the subdecls of the given
-- AST item
insertAtStart, insertAtEnd :: (HasDecls ast)
              => ast
              -> LHsDecl GhcPs
              -> Transform ast

insertAtStart = insertAt (:)
insertAtEnd   = insertAt (\x xs -> xs ++ [x])

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
        return (L l (HsModule (XModulePs a lo deps haddocks) mname exps imps decls))

-- ---------------------------------------------------------------------

instance HasDecls (LocatedA (Match GhcPs (LocatedA (HsExpr GhcPs)))) where
  hsDecls (L _ (Match _ _ _ (GRHSs _ _ lb))) = hsDeclsValBinds lb

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
  hsDecls (L _ (HsLet _ _ decls _ _ex)) = hsDeclsValBinds decls
  hsDecls _                             = return []

  replaceDecls (L ll (HsLet x tkLet binds tkIn ex)) newDecls
    = do
        logTr "replaceDecls HsLet"
        let lastAnc = realSrcSpan $ spanHsLocaLBinds binds
        -- TODO: may be an intervening comment, take account for lastAnc
        let (tkLet', tkIn', ex',newDecls') = case (tkLet, tkIn) of
              (L (TokenLoc l) ls, L (TokenLoc i) is) ->
                let
                  off = case l of
                          (EpaSpan r _) -> LayoutStartCol $ snd $ ss2pos r
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
hsDeclsPatBindD :: (Monad m) => LHsDecl GhcPs -> TransformT m [LHsDecl GhcPs]
hsDeclsPatBindD (L l (ValD _ d)) = hsDeclsPatBind (L l d)
hsDeclsPatBindD x = error $ "hsDeclsPatBindD called for:" ++ showGhc x

-- | Extract the immediate declarations for a 'PatBind'. This
-- cannot be a member of 'HasDecls' because a 'FunBind' is not idempotent
-- for 'hsDecls' \/ 'replaceDecls'. 'hsDeclsPatBind' \/ 'replaceDeclsPatBind' is
-- idempotent.
hsDeclsPatBind :: (Monad m) => LHsBind GhcPs -> TransformT m [LHsDecl GhcPs]
hsDeclsPatBind (L _ (PatBind _ _ (GRHSs _ _grhs lb))) = hsDeclsValBinds lb
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
  hsDecls (L _ (LetStmt _ lb))      = hsDeclsValBinds lb
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

-- |Look up the annotated order and sort the decls accordingly
-- TODO:AZ: this should be pure
orderedDecls :: (Monad m)
             => AnnSortKey -> [LHsDecl GhcPs] -> TransformT m [LHsDecl GhcPs]
orderedDecls sortKey decls = do
  case sortKey of
    NoAnnSortKey -> do
      -- return decls
      return $ sortBy (\a b -> compare (realSrcSpan $ getLocA a) (realSrcSpan $ getLocA b)) decls
    AnnSortKey keys -> do
      let ds = map (\s -> (rs $ getLocA s,s)) decls
          ordered = map snd $ orderByKey ds keys
      return ordered

-- ---------------------------------------------------------------------

hsDeclsValBinds :: (Monad m) => HsLocalBinds GhcPs -> TransformT m [LHsDecl GhcPs]
hsDeclsValBinds lb = case lb of
    HsValBinds _ (ValBinds sortKey bs sigs) -> do
      let
        bds = map wrapDecl (bagToList bs)
        sds = map wrapSig sigs
      orderedDecls sortKey (bds ++ sds)
    HsValBinds _ (XValBindsLR _) -> error $ "hsDecls.XValBindsLR not valid"
    HsIPBinds {}       -> return []
    EmptyLocalBinds {} -> return []

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
        an <- oldWhereAnnotation a w (realSrcSpan oldSpan)
        let decs = listToBag $ concatMap decl2Bind new
        let sigs = concatMap decl2Sig new
        let sortKey = captureOrder new
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
        let sortKey = captureOrder new
        return (HsValBinds an (ValBinds sortKey decs sigs))

oldWhereAnnotation :: (Monad m)
  => EpAnn AnnList -> WithWhere -> RealSrcSpan -> TransformT m (EpAnn AnnList)
oldWhereAnnotation EpAnnNotUsed ww _oldSpan = do
  newSpan <- uniqueSrcSpanT
  let w = case ww of
        WithWhere -> [AddEpAnn AnnWhere (EpaDelta (SameLine 0) [])]
        WithoutWhere -> []
  let anc2' = Anchor (rs newSpan) (MovedAnchor (SameLine 1))
  (anc, anc2) <- do
          newSpan' <- uniqueSrcSpanT
          return ( Anchor (rs newSpan') (MovedAnchor (DifferentLine 1 2))
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
  newSpan <- uniqueSrcSpanT
  let anc  = Anchor (rs newSpan) (MovedAnchor (DifferentLine 1 2))
  let anc2 = Anchor (rs newSpan) (MovedAnchor (DifferentLine 1 4))
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
       ds <- liftT $ hsDeclsPatBindD pb
       (ds',r) <- f (error "modifyValD.PatBind should not touch Match") ds
       pb' <- liftT $ replaceDeclsPatBindD pb ds'
       return (pb',r)
     else return (pb,Nothing)
modifyValD p ast f = do
  (ast',r) <- runStateT (everywhereM (mkM doModLocal) ast) Nothing
  return (ast',r)
  where
    doModLocal :: PMatch -> StateT (Maybe t) m PMatch
    doModLocal  (match@(L ss _) :: PMatch) = do
         if (locA ss) == p
           then do
             ds <- lift $ liftT $ hsDecls match
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
