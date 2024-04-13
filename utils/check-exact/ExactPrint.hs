{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE UndecidableInstances  #-} -- For the (StmtLR GhcPs GhcPs (LocatedA (body GhcPs))) ExactPrint instance
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-incomplete-record-updates #-}

module ExactPrint
  (
    ExactPrint(..)
  , exactPrint
  , exactPrintWithOptions
  , makeDeltaAst

  -- * Configuration
  , EPOptions(epRigidity, epAstPrint, epTokenPrint, epWhitespacePrint, epUpdateAnchors)
  , stringOptions
  , epOptions
  , deltaOptions
  ) where

import GHC
import GHC.Base (NonEmpty(..))
import GHC.Core.Coercion.Axiom (Role(..))
import GHC.Data.Bag
import qualified GHC.Data.BooleanFormula as BF
import GHC.Data.FastString
import GHC.TypeLits
import GHC.Types.Basic hiding (EP)
import GHC.Types.Fixity
import GHC.Types.ForeignCall
import GHC.Types.Name.Reader
import GHC.Types.PkgQual
import GHC.Types.SourceText
import GHC.Types.Var
import GHC.Utils.Outputable hiding ( (<>) )
import GHC.Unit.Module.Warnings
import GHC.Utils.Misc
import GHC.Utils.Panic

import Language.Haskell.Syntax.Basic (FieldLabelString(..))

import Control.Monad (forM, when, unless)
import Control.Monad.Identity (Identity(..))
import qualified Control.Monad.Reader as Reader
import Control.Monad.RWS (MonadReader, RWST, evalRWST, tell, modify, get, gets, ask)
import Control.Monad.Trans (lift)
import Data.Data ( Data )
import Data.Dynamic
import Data.Foldable
import Data.Functor.Const
import qualified Data.Set as Set
import Data.Typeable
import Data.List ( partition, sort, sortBy)
import qualified Data.Map.Strict as Map
import Data.Maybe ( isJust, mapMaybe )
import Data.Void

import Lookup
import Utils
import Types

-- ---------------------------------------------------------------------

exactPrint :: ExactPrint ast => ast -> String
exactPrint ast = snd $ runIdentity (runEP stringOptions (markAnnotated ast))

-- | The additional option to specify the rigidity and printing
-- configuration.
exactPrintWithOptions :: (ExactPrint ast, Monoid b, Monad m)
                      => EPOptions m b
                      -> ast
                      -> m (ast, b)
exactPrintWithOptions r ast =
    runEP r (markAnnotated ast)

-- | Transform concrete annotations into relative annotations which
-- are more useful when transforming an AST. This corresponds to the
-- earlier 'relativiseApiAnns'.
makeDeltaAst :: ExactPrint ast => ast -> ast
makeDeltaAst ast = fst $ runIdentity (runEP deltaOptions (markAnnotated ast))

------------------------------------------------------

type EP w m a = RWST (EPOptions m w) (EPWriter w) EPState m a

runEP :: (Monad m)
      => EPOptions m w
      -> EP w m a -> m (a, w)
runEP epReader action = do
  (ast, w) <- evalRWST action epReader defaultEPState
  return (ast, output w)

-- ---------------------------------------------------------------------

defaultEPState :: EPState
defaultEPState = EPState
             { epPos      = (1,1)
             , dLHS       = 0
             , pMarkLayout = False
             , pLHS = 0
             , dMarkLayout = False
             , dPriorEndPosition = (1,1)
             , uAnchorSpan = badRealSrcSpan
             , uExtraDP = Nothing
             , pAcceptSpan = False
             , epComments = []
             , epCommentsApplied = []
             , epEof = Nothing
             }


-- ---------------------------------------------------------------------
-- The EP monad and basic combinators

-- | The R part of RWS. The environment. Updated via 'local' as we
-- enter a new AST element, having a different anchor point.
data EPOptions m a = EPOptions
            {
              epAstPrint :: forall ast . Data ast => GHC.Located ast -> a -> m a
            , epTokenPrint :: String -> m a
            , epWhitespacePrint :: String -> m a
            , epRigidity :: Rigidity
            , epUpdateAnchors :: Bool
            }

-- | Helper to create a 'EPOptions'
epOptions ::
      (forall ast . Data ast => GHC.Located ast -> a -> m a)
      -> (String -> m a)
      -> (String -> m a)
      -> Rigidity
      -> Bool
      -> EPOptions m a
epOptions astPrint tokenPrint wsPrint rigidity delta = EPOptions
             {
               epAstPrint = astPrint
             , epWhitespacePrint = wsPrint
             , epTokenPrint = tokenPrint
             , epRigidity = rigidity
             , epUpdateAnchors = delta
             }

-- | Options which can be used to print as a normal String.
stringOptions :: EPOptions Identity String
stringOptions = epOptions (\_ b -> return b) return return NormalLayout False

-- | Options which can be used to simply update the AST to be in delta
-- form, without generating output
deltaOptions :: EPOptions Identity ()
deltaOptions = epOptions (\_ _ -> return ()) (\_ -> return ()) (\_ -> return ()) NormalLayout True

data EPWriter a = EPWriter
              { output :: !a }

instance Monoid w => Semigroup (EPWriter w) where
  (EPWriter a) <> (EPWriter b) = EPWriter (a <> b)

instance Monoid w => Monoid (EPWriter w) where
  mempty = EPWriter mempty

data EPState = EPState
             { uAnchorSpan :: !RealSrcSpan -- ^ in pre-changed AST
                                          -- reference frame, from
                                          -- Annotation
             , uExtraDP :: !(Maybe Anchor) -- ^ Used to anchor a
                                             -- list
             , pAcceptSpan :: Bool -- ^ When we have processed an
                                   -- entry of EpaDelta, accept the
                                   -- next `EpaSpan` start as the
                                   -- current output position. i.e. do
                                   -- not advance epPos. Achieved by
                                   -- setting dPriorEndPosition to the
                                   -- end of the span.

             -- Print phase
             , epPos        :: !Pos -- ^ Current output position
             , pMarkLayout  :: !Bool
             , pLHS   :: !LayoutStartCol

             -- Delta phase
             , dPriorEndPosition :: !Pos -- ^ End of Position reached
                                         -- when processing the
                                         -- preceding element
             , dMarkLayout :: !Bool
             , dLHS        :: !LayoutStartCol

             -- Shared
             , epComments :: ![Comment]
             , epCommentsApplied :: ![[Comment]]
             , epEof :: !(Maybe (RealSrcSpan, RealSrcSpan))
             }

-- ---------------------------------------------------------------------

-- AZ:TODO: this can just be a function :: (EpAnn a) -> Entry
class HasEntry ast where
  fromAnn :: ast -> Entry

class HasTrailing a where
  trailing :: a -> [TrailingAnn]
  setTrailing :: a -> [TrailingAnn] -> a

setAnchorEpa :: (HasTrailing an, NoAnn an)
             => EpAnn an -> Anchor -> [TrailingAnn] -> EpAnnComments -> EpAnn an
setAnchorEpa (EpAnn _ an _) anc ts cs = EpAnn anc (setTrailing an ts)          cs

setAnchorHsModule :: HsModule GhcPs -> Anchor -> EpAnnComments -> HsModule GhcPs
setAnchorHsModule hsmod anc cs = hsmod { hsmodExt = (hsmodExt hsmod) {hsmodAnn = an'} }
  where
    anc' = anc
    an' = setAnchorEpa (hsmodAnn $ hsmodExt hsmod) anc' [] cs

setAnchorAn :: (HasTrailing an, NoAnn an)
             => LocatedAn an a -> Anchor -> [TrailingAnn] -> EpAnnComments -> LocatedAn an a
setAnchorAn (L (EpAnn _ an _) a) anc ts cs = (L (EpAnn anc (setTrailing an ts) cs) a)
     -- `debug` ("setAnchorAn: anc=" ++ showAst anc)

setAnchorEpaL :: EpAnn AnnList -> Anchor -> [TrailingAnn] -> EpAnnComments -> EpAnn AnnList
setAnchorEpaL (EpAnn _ an _) anc ts cs = EpAnn anc (setTrailing (an {al_anchor = Nothing}) ts) cs

-- ---------------------------------------------------------------------

-- | Key entry point.  Switches to an independent AST element with its
-- own annotation, calculating new offsets, etc
markAnnotated :: (Monad m, Monoid w, ExactPrint a) => a -> EP w m a
markAnnotated a = enterAnn (getAnnotationEntry a) a

-- | For HsModule, because we do not have a proper SrcSpan, we must
-- indicate to flush trailing comments when done.
data FlushComments = FlushComments
                   | NoFlushComments
                   deriving (Eq, Show)

-- | For GenLocated SrcSpan, we construct an entry location but cannot update it.
data CanUpdateAnchor = CanUpdateAnchor
                     | CanUpdateAnchorOnly
                     | NoCanUpdateAnchor
                   deriving (Eq, Show)

data Entry = Entry Anchor [TrailingAnn] EpAnnComments FlushComments CanUpdateAnchor
           | NoEntryVal

-- | For flagging whether to capture comments in an EpaDelta or not
data CaptureComments = CaptureComments
                     | NoCaptureComments

mkEntry :: Anchor -> [TrailingAnn] -> EpAnnComments -> Entry
mkEntry anc ts cs = Entry anc ts cs NoFlushComments CanUpdateAnchor

instance (HasTrailing a) => HasEntry (EpAnn a) where
  fromAnn (EpAnn anc a cs) = mkEntry anc (trailing a) cs

-- ---------------------------------------------------------------------

instance HasTrailing NoEpAnns where
  trailing _ = []
  setTrailing a _ = a

instance HasTrailing EpaLocation where
  trailing _ = []
  setTrailing a _ = a

instance HasTrailing AddEpAnn where
  trailing _ = []
  setTrailing a _ = a

instance HasTrailing [AddEpAnn] where
  trailing _ = []
  setTrailing a _ = a

instance HasTrailing (AddEpAnn, AddEpAnn) where
  trailing _ = []
  setTrailing a _ = a

instance HasTrailing EpAnnSumPat where
  trailing _ = []
  setTrailing a _ = a

instance HasTrailing AnnList where
  trailing a = al_trailing a
  setTrailing a ts = a { al_trailing = ts }

instance HasTrailing AnnListItem where
  trailing a = lann_trailing a
  setTrailing a ts = a { lann_trailing = ts }

instance HasTrailing AnnPragma where
  trailing _ = []
  setTrailing a _ = a

instance HasTrailing AnnContext where
  trailing (AnnContext ma _opens _closes)
    = case ma of
      Just (UnicodeSyntax, r) -> [AddDarrowUAnn r]
      Just (NormalSyntax,  r) -> [AddDarrowAnn r]
      Nothing -> []

  setTrailing a [AddDarrowUAnn r] = a {ac_darrow = Just (UnicodeSyntax, r)}
  setTrailing a [AddDarrowAnn r] = a{ac_darrow = Just (NormalSyntax, r)}
  setTrailing a [] = a{ac_darrow = Nothing}
  setTrailing a ts = error $ "Cannot setTrailing " ++ showAst ts ++ " for " ++ showAst a


instance HasTrailing AnnParen where
  trailing _ = []
  setTrailing a _ = a

instance HasTrailing AnnsIf where
  trailing _ = []
  setTrailing a _ = a

instance HasTrailing EpAnnHsCase where
  trailing _ = []
  setTrailing a _ = a

instance HasTrailing AnnFieldLabel where
  trailing _ = []
  setTrailing a _ = a

instance HasTrailing AnnProjection where
  trailing _ = []
  setTrailing a _ = a

instance HasTrailing AnnExplicitSum where
  trailing _ = []
  setTrailing a _ = a

instance HasTrailing (Maybe EpAnnUnboundVar) where
  trailing _ = []
  setTrailing a _ = a

instance HasTrailing GrhsAnn where
  trailing _ = []
  setTrailing a _ = a

instance HasTrailing AnnSig where
  trailing _ = []
  setTrailing a _ = a

instance HasTrailing HsRuleAnn where
  trailing _ = []
  setTrailing a _ = a

instance HasTrailing EpAnnImportDecl where
  trailing _ = []
  setTrailing a _ = a

instance HasTrailing AnnsModule where
  -- Report none, as all are used internally
  trailing _ = []
  setTrailing a _ = a

instance HasTrailing NameAnn where
  trailing a = nann_trailing a
  setTrailing a ts = a { nann_trailing = ts }

instance HasTrailing Bool where
  trailing _ = []
  setTrailing a _ = a

-- ---------------------------------------------------------------------

fromAnn' :: (HasEntry a) => a -> Entry
fromAnn' an = case fromAnn an of
  NoEntryVal -> NoEntryVal
  Entry a ts c _ u -> Entry a ts c FlushComments u

-- ---------------------------------------------------------------------

astId :: (Typeable a) => a -> String
astId a = show (typeOf a)

cua :: (Monad m, Monoid w) => CanUpdateAnchor -> EP w m [a] -> EP w m [a]
cua CanUpdateAnchor f = f
cua CanUpdateAnchorOnly _ = return []
cua NoCanUpdateAnchor _ = return []

-- | "Enter" an annotation, by using the associated 'anchor' field as
-- the new reference point for calculating all DeltaPos positions.
-- This is the heart of the exact printing process.
--
-- This is combination of the ghc=exactprint Delta.withAST and
-- Print.exactPC functions and effectively does the delta processing
-- immediately followed by the print processing.  JIT ghc-exactprint.
enterAnn :: (Monad m, Monoid w, ExactPrint a) => Entry -> a -> EP w m a
enterAnn NoEntryVal a = do
  p <- getPosP
  debugM $ "enterAnn:starting:NO ANN:(p,a) =" ++ show (p, astId a)
  r <- exact a
  debugM $ "enterAnn:done:NO ANN:p =" ++ show (p, astId a)
  return r
enterAnn (Entry anchor' trailing_anns cs flush canUpdateAnchor) a = do
  acceptSpan <- getAcceptSpan
  setAcceptSpan False
  case anchor' of
    EpaDelta _ _ -> setAcceptSpan True
    _            -> return ()
  p <- getPosP
  pe0 <- getPriorEndD
  debugM $ "enterAnn:starting:(anchor',p,pe,a) =" ++ show (showAst anchor', p, pe0, astId a)
  prevAnchor <- getAnchorU
  let curAnchor = case anchor' of
        EpaSpan (RealSrcSpan r _) -> r
        _ -> prevAnchor
  debugM $ "enterAnn:(curAnchor):=" ++ show (rs2range curAnchor)
  case canUpdateAnchor of
    CanUpdateAnchor -> pushAppliedComments
    _ -> return ()
  case anchor' of
    EpaDelta _ dcs -> do
      debugM $ "enterAnn:Printing comments:" ++ showGhc (priorComments cs)
      mapM_ printOneComment (concatMap tokComment $ priorComments cs)
      debugM $ "enterAnn:Printing EpaDelta comments:" ++ showGhc dcs
      mapM_ printOneComment (concatMap tokComment dcs)
    _ -> do
      debugM $ "enterAnn:Adding comments:" ++ showGhc (priorComments cs)
      addCommentsA (priorComments cs)
  debugM $ "enterAnn:Added comments"
  printCommentsBefore curAnchor
  priorCs <- cua canUpdateAnchor takeAppliedComments -- no pop
  -- -------------------------
  case anchor' of
    EpaDelta dp _ -> do
      debugM $ "enterAnn: EpaDelta:" ++ show dp
      -- Set the original anchor as prior end, so the rest of this AST
      -- fragment has a reference
      setPriorEndNoLayoutD (ss2pos curAnchor)
    _ -> do
      if acceptSpan
        then setPriorEndNoLayoutD (ss2pos curAnchor)
        else return ()

  -- -------------------------
  if ((fst $ fst $ rs2range curAnchor) >= 0)
    then
      setAnchorU curAnchor
    else
      debugM $ "enterAnn: not calling setAnchorU for : " ++ show (rs2range curAnchor)
  -- -------------------------------------------------------------------
  -- Make sure the running dPriorEndPosition gets updated according to
  -- the change in the current anchor.

  -- Compute the distance from dPriorEndPosition to the start of the new span.

  -- While processing in the context of the prior anchor, we choose to
  -- enter a new Anchor, which has a defined position relative to the
  -- prior anchor, even if we do not actively output anything at that
  -- point.
  -- Is this edp?

  -- -------------------------------------------------------------------
  -- The first part corresponds to the delta phase, so should only use
  -- delta phase variables -----------------------------------
  -- Calculate offset required to get to the start of the SrcSPan
  off <- getLayoutOffsetD
  let spanStart = ss2pos curAnchor
  priorEndAfterComments <- getPriorEndD
  let edp' = adjustDeltaForOffset
               -- Use the propagated offset if one is set
               -- Note that we need to use the new offset if it has
               -- changed.
               off (ss2delta priorEndAfterComments curAnchor)
  debugM $ "enterAnn: (edp',off,priorEndAfterComments,curAnchor):" ++ show (edp',off,priorEndAfterComments,rs2range curAnchor)
  let edp'' = case anchor' of
        EpaDelta dp _ -> dp
        _ -> edp'
  -- ---------------------------------------------
  med <- getExtraDP
  setExtraDP Nothing
  let edp = case med of
        Nothing -> edp''
        Just (EpaDelta dp _) -> dp
                   -- Replace original with desired one. Allows all
                   -- list entry values to be DP (1,0)
        Just (EpaSpan (RealSrcSpan r _)) -> dp
          where
            dp = adjustDeltaForOffset
                   off (ss2delta priorEndAfterComments r)
        Just (EpaSpan (UnhelpfulSpan r)) -> panic $ "enterAnn: UnhelpfulSpan:" ++ show r
  when (isJust med) $ debugM $ "enterAnn:(med,edp)=" ++ showAst (med,edp)
  -- ---------------------------------------------
  -- Preparation complete, perform the action
  when (priorEndAfterComments < spanStart) (do
    debugM $ "enterAnn.dPriorEndPosition:spanStart=" ++ show spanStart
    modify (\s -> s { dPriorEndPosition    = spanStart } ))

  debugM $ "enterAnn: (anchor', curAnchor):" ++ show (anchor', rs2range curAnchor)
  -- debugM $ "enterAnn: (dLHS,spanStart,pec,edp)=" ++ show (off,spanStart,priorEndAfterComments,edp)
  p0 <- getPosP
  d <- getPriorEndD
  debugM $ "enterAnn: (posp, posd)=" ++ show (p0,d)

  -- end of delta phase processing
  -- -------------------------------------------------------------------
  -- start of print phase processing

  advance edp
  debugM $ "enterAnn:exact a starting:" ++ show (showAst anchor')
  a' <- exact a
  debugM $ "enterAnn:exact a done:" ++ show (showAst anchor')
  when (flush == FlushComments) $ do
    debugM $ "flushing comments in enterAnn:" ++ showAst cs
    flushComments (getFollowingComments cs)
    debugM $ "flushing comments in enterAnn done"

  eof <- getEofPos
  case eof of
    Nothing -> return ()
    Just (pos, prior) -> do
       let dp = if pos == prior
             then (DifferentLine 1 0)
             else origDelta pos prior
       debugM $ "EOF:(pos,posEnd,prior,dp) =" ++ showGhc (ss2pos pos, ss2posEnd pos, ss2pos prior, dp)
       printStringAtLsDelta dp ""
       setEofPos Nothing -- Only do this once

  -- Deal with exit from the current anchor
  when (flush == NoFlushComments) $ do
    printCommentsIn curAnchor -- Make sure all comments in the span are printed

  p1 <- getPosP
  pe1 <- getPriorEndD
  debugM $ "enterAnn:done:(anchor,p,pe,a) =" ++ show (showAst anchor', p1, pe1, astId a')

  case anchor' of
    EpaDelta _ _ -> return ()
    EpaSpan (RealSrcSpan rss _) -> do
      setAcceptSpan False
      setPriorEndD (snd $ rs2range rss)
    EpaSpan _ -> return ()

  -- Outside the anchor, mark any trailing
  postCs <- cua canUpdateAnchor takeAppliedCommentsPop
  when (flush == NoFlushComments) $ do
    when ((getFollowingComments cs) /= []) $ do

      -- debugM $ "enterAnn:in:(anchor') =" ++ show (eloc2str anchor')
      debugM $ "starting trailing comments:" ++ showAst (getFollowingComments cs)
      mapM_ printOneComment (concatMap tokComment $ getFollowingComments cs)
      debugM $ "ending trailing comments"
  trailing' <- markTrailing trailing_anns

  -- Update original anchor, comments based on the printing process
  let newAchor = EpaDelta edp []
  let r = case canUpdateAnchor of
            CanUpdateAnchor -> setAnnotationAnchor a' newAchor trailing' (mkEpaComments (priorCs ++ postCs) [])
            CanUpdateAnchorOnly -> setAnnotationAnchor a' newAchor [] emptyComments
            NoCanUpdateAnchor -> a'
  return r

-- ---------------------------------------------------------------------

addCommentsA :: (Monad m, Monoid w) => [LEpaComment] -> EP w m ()
addCommentsA csNew = addComments (concatMap tokComment csNew)

{-
TODO: When we addComments, some may have an anchor that is no longer
valid, as it has been moved and has an anchor_op.

Does an Anchor even make sense for a comment, perhaps it should be an
EpaLocation?

How do we sort them? do we assign a location based on when we add them
to the list, based on the current output pos?  Except the offset is a
delta compared to a reference location.  Need to nail the concept of
the reference location.

By definition it is the current anchor, so work against that. And that
also means that the first entry comment that has moved should not have
a line offset.
-}
addComments :: (Monad m, Monoid w) => [Comment] -> EP w m ()
addComments csNew = do
  -- debugM $ "addComments:" ++ show csNew
  cs <- getUnallocatedComments

  putUnallocatedComments (sort (cs ++ csNew))

-- ---------------------------------------------------------------------

-- | Just before we print out the EOF comments, flush the remaining
-- ones in the state.
flushComments :: (Monad m, Monoid w) => [LEpaComment] -> EP w m ()
flushComments trailing_anns = do
  addCommentsA trailing_anns
  cs <- getUnallocatedComments
  debugM $ "flushing comments starting"
    -- AZ:TODO: is the sort still needed?
  mapM_ printOneComment (sortComments cs)
  putUnallocatedComments []
  debugM $ "flushing comments done"

-- ---------------------------------------------------------------------

-- |In order to interleave annotations into the stream, we turn them into
-- comments. They are removed from the annotation to avoid duplication.
annotationsToComments :: (Monad m, Monoid w)
  => a -> Lens a [AddEpAnn] -> [AnnKeywordId] -> EP w m a
annotationsToComments a l kws = do
  let (newComments, newAnns) = go ([],[]) (view l a)
  addComments newComments
  return (set l (reverse newAnns) a)
  where
    keywords = Set.fromList kws

    go :: ([Comment], [AddEpAnn]) -> [AddEpAnn] -> ([Comment], [AddEpAnn])
    go acc [] = acc
    go (cs',ans) ((AddEpAnn k ss) : ls)
      | Set.member k keywords = go ((mkKWComment k (epaToNoCommentsLocation ss)):cs', ans) ls
      | otherwise             = go (cs', (AddEpAnn k ss):ans)    ls

-- ---------------------------------------------------------------------

-- Temporary function to simply reproduce the "normal" pretty printer output
withPpr :: (Monad m, Monoid w, Outputable a) => a -> EP w m a
withPpr a = do
  ss <- getAnchorU
  debugM $ "withPpr: ss=" ++ show ss
  printStringAtRs' ss (showPprUnsafe a)
  return a

-- ---------------------------------------------------------------------

-- | An AST fragment with an annotation must be able to return the
-- requirements for nesting another one, captured in an 'Entry', and
-- to be able to use the rest of the exactprint machinery to print the
-- element.  In the analogy to Outputable, 'exact' plays the role of
-- 'ppr'.
class (Typeable a) => ExactPrint a where
  getAnnotationEntry :: a -> Entry
  setAnnotationAnchor :: a -> Anchor -> [TrailingAnn] -> EpAnnComments -> a
  exact :: (Monad m, Monoid w) => a -> EP w m a

-- ---------------------------------------------------------------------
-- Start of utility functions
-- ---------------------------------------------------------------------

printSourceText :: (Monad m, Monoid w) => SourceText -> String -> EP w m ()
printSourceText (NoSourceText) txt   =  printStringAdvance txt >> return ()
printSourceText (SourceText   txt) _ =  printStringAdvance (unpackFS txt) >> return ()

-- ---------------------------------------------------------------------

printStringAtSs :: (Monad m, Monoid w) => SrcSpan -> String -> EP w m ()
printStringAtSs ss str = printStringAtRs (realSrcSpan ss) str >> return ()

printStringAtRs :: (Monad m, Monoid w) => RealSrcSpan -> String -> EP w m EpaLocation
printStringAtRs pa str = printStringAtRsC CaptureComments pa str

printStringAtRsC :: (Monad m, Monoid w)
  => CaptureComments -> RealSrcSpan -> String -> EP w m EpaLocation
printStringAtRsC capture pa str = do
  debugM $ "printStringAtRsC: pa=" ++ showAst pa
  printCommentsBefore pa
  pe <- getPriorEndD
  debugM $ "printStringAtRsC:pe=" ++ show pe
  let p = ss2delta pe pa
  p' <- adjustDeltaForOffsetM p
  debugM $ "printStringAtRsC:(p,p')=" ++ show (p,p')
  printStringAtLsDelta p' str
  setPriorEndASTD True pa
  cs' <- case capture of
    CaptureComments -> takeAppliedComments
    NoCaptureComments -> return []
  debugM $ "printStringAtRsC:cs'=" ++ show cs'
  debugM $ "printStringAtRsC:p'=" ++ showAst p'
  debugM $ "printStringAtRsC: (EpaDelta p' [])=" ++ showAst (EpaDelta p' NoComments)
  debugM $ "printStringAtRsC: (EpaDelta p' (map comment2LEpaComment cs'))=" ++ showAst (EpaDelta p' (map comment2LEpaComment cs'))
  return (EpaDelta p' (map comment2LEpaComment cs'))

printStringAtRs' :: (Monad m, Monoid w) => RealSrcSpan -> String -> EP w m ()
printStringAtRs' pa str = printStringAtRsC NoCaptureComments pa str >> return ()

-- ---------------------------------------------------------------------

printStringAtMLoc' :: (Monad m, Monoid w)
  => Maybe EpaLocation -> String -> EP w m (Maybe EpaLocation)
printStringAtMLoc' (Just aa) s = Just <$> printStringAtAA aa s
printStringAtMLoc' Nothing s = do
  printStringAtLsDelta (SameLine 1) s
  return (Just (EpaDelta (SameLine 1) []))

printStringAtMLocL :: (Monad m, Monoid w)
  => EpAnn a -> Lens a (Maybe EpaLocation) -> String -> EP w m (EpAnn a)
printStringAtMLocL (EpAnn anc an cs) l s = do
  r <- go (view l an) s
  return (EpAnn anc (set l r an) cs)
  where
    go (Just aa) str = Just <$> printStringAtAA aa str
    go Nothing str = do
      printStringAtLsDelta (SameLine 1) str
      return (Just (EpaDelta (SameLine 1) []))

printStringAtAA :: (Monad m, Monoid w) => EpaLocation -> String -> EP w m EpaLocation
printStringAtAA el str = printStringAtAAC CaptureComments el str

printStringAtNC :: (Monad m, Monoid w) => NoCommentsLocation -> String -> EP w m NoCommentsLocation
printStringAtNC el str = do
  el' <- printStringAtAAC NoCaptureComments (noCommentsToEpaLocation el) str
  return (epaToNoCommentsLocation el')

printStringAtAAL :: (Monad m, Monoid w)
  => a -> Lens a EpaLocation -> String -> EP w m a
printStringAtAAL an l str = do
  r <- printStringAtAAC CaptureComments (view l an) str
  return (set l r an)

printStringAtAAC :: (Monad m, Monoid w)
  => CaptureComments -> EpaLocation -> String -> EP w m EpaLocation
printStringAtAAC capture (EpaSpan (RealSrcSpan r _)) s = printStringAtRsC capture r s
printStringAtAAC _capture (EpaSpan ss@(UnhelpfulSpan _)) _s = error $ "printStringAtAAC:ss=" ++ show ss
printStringAtAAC capture (EpaDelta d cs) s = do
  mapM_ printOneComment $ concatMap tokComment cs
  pe1 <- getPriorEndD
  p1 <- getPosP
  printStringAtLsDelta d s
  p2 <- getPosP
  pe2 <- getPriorEndD
  debugM $ "printStringAtAA:(pe1,pe2,p1,p2)=" ++ show (pe1,pe2,p1,p2)
  setPriorEndASTPD True (pe1,pe2)
  cs' <- case capture of
    CaptureComments -> takeAppliedComments
    NoCaptureComments -> return []
  debugM $ "printStringAtAA:(pe1,pe2,p1,p2,cs')=" ++ show (pe1,pe2,p1,p2,cs')
  return (EpaDelta d (map comment2LEpaComment cs'))

-- ---------------------------------------------------------------------

markExternalSourceTextE :: (Monad m, Monoid w) => EpaLocation -> SourceText -> String -> EP w m EpaLocation
markExternalSourceTextE l NoSourceText txt   = printStringAtAA l txt
markExternalSourceTextE l (SourceText txt) _ = printStringAtAA l (unpackFS txt)

-- ---------------------------------------------------------------------

markLensMAA :: (Monad m, Monoid w)
  => EpAnn a -> Lens a (Maybe AddEpAnn) -> EP w m (EpAnn a)
markLensMAA epann l = markLensMAA' epann (lepa . l)

markLensMAA' :: (Monad m, Monoid w)
  => a -> Lens a (Maybe AddEpAnn) -> EP w m a
markLensMAA' a l =
  case view l a of
    Nothing -> return a
    Just aa -> do
      aa' <- markAddEpAnn aa
      return (set l (Just aa') a)

-- -------------------------------------

markLensAA :: (Monad m, Monoid w)
  => EpAnn a -> Lens a AddEpAnn -> EP w m (EpAnn a)
markLensAA epann l = markLensAA' epann (lepa . l)

markLensAA' :: (Monad m, Monoid w)
  => a -> Lens a AddEpAnn -> EP w m a
markLensAA' a l = do
  a' <- markKw (view l a)
  return (set l a' a)

-- -------------------------------------

markEpAnnLMS :: (Monad m, Monoid w)
  => EpAnn a -> Lens a [AddEpAnn] -> AnnKeywordId -> Maybe String -> EP w m (EpAnn a)
markEpAnnLMS epann l kw ms = markEpAnnLMS'' epann (lepa . l) kw ms

markEpAnnLMS'' :: (Monad m, Monoid w)
  => a -> Lens a [AddEpAnn] -> AnnKeywordId -> Maybe String -> EP w m a
markEpAnnLMS'' an l kw Nothing = markEpAnnL an l kw
markEpAnnLMS'' a l kw (Just str) = do
  anns <- mapM go (view l a)
  return (set l anns a)
  where
    go :: (Monad m, Monoid w) => AddEpAnn -> EP w m AddEpAnn
    go (AddEpAnn kw' r)
      | kw' == kw = do
          r' <- printStringAtAA r str
          return (AddEpAnn kw' r')
      | otherwise = return (AddEpAnn kw' r)

-- -------------------------------------

markEpAnnMS' :: (Monad m, Monoid w)
  => [AddEpAnn] -> AnnKeywordId -> Maybe String -> EP w m [AddEpAnn]
markEpAnnMS' anns kw Nothing = mark anns kw
markEpAnnMS' anns kw (Just str) = do
  mapM go anns
  where
    go :: (Monad m, Monoid w) => AddEpAnn -> EP w m AddEpAnn
    go (AddEpAnn kw' r)
      | kw' == kw = do
          r' <- printStringAtAA r str
          return (AddEpAnn kw' r')
      | otherwise = return (AddEpAnn kw' r)

-- -------------------------------------

markEpAnnLMS' :: (Monad m, Monoid w)
  => EpAnn a -> Lens a AddEpAnn -> AnnKeywordId -> Maybe String -> EP w m (EpAnn a)
markEpAnnLMS' an l kw ms = markEpAnnLMS0 an (lepa . l) kw ms

markEpAnnLMS0 :: (Monad m, Monoid w)
  => a -> Lens a AddEpAnn -> AnnKeywordId -> Maybe String -> EP w m a
markEpAnnLMS0 an l _kw Nothing = markLensKwA an l
markEpAnnLMS0 a l kw (Just str) = do
  anns <- go (view l a)
  return (set l anns a)
  where
    go :: (Monad m, Monoid w) => AddEpAnn -> EP w m AddEpAnn
    go (AddEpAnn kw' r)
      | kw' == kw = do
          r' <- printStringAtAA r str
          return (AddEpAnn kw' r')
      | otherwise = return (AddEpAnn kw' r)

-- ---------------------------------------------------------------------

markEpToken :: forall m w tok . (Monad m, Monoid w, KnownSymbol tok)
  => EpToken tok -> EP w m (EpToken tok)
markEpToken NoEpTok = return NoEpTok
markEpToken (EpTok aa) = do
  aa' <- printStringAtAA aa (symbolVal (Proxy @tok))
  return (EpTok aa')

markEpUniToken :: forall m w tok utok . (Monad m, Monoid w, KnownSymbol tok, KnownSymbol utok)
  => EpUniToken tok utok -> EP w m (EpUniToken tok utok)
markEpUniToken NoEpUniTok = return NoEpUniTok
markEpUniToken (EpUniTok aa isUnicode)  = do
  aa' <- case isUnicode of
    NormalSyntax  -> printStringAtAA aa (symbolVal (Proxy @tok))
    UnicodeSyntax -> printStringAtAA aa (symbolVal (Proxy @utok))
  return (EpUniTok aa' isUnicode)

-- ---------------------------------------------------------------------

markArrow :: (Monad m, Monoid w) => HsArrow GhcPs -> EP w m (HsArrow GhcPs)
markArrow (HsUnrestrictedArrow arr) = do
  arr' <- markEpUniToken arr
  return (HsUnrestrictedArrow arr')
markArrow (HsLinearArrow (EpPct1 pct1 arr)) = do
  pct1' <- markEpToken pct1
  arr' <- markEpUniToken arr
  return (HsLinearArrow (EpPct1 pct1' arr'))
markArrow (HsLinearArrow (EpLolly arr)) = do
  arr' <- markEpToken arr
  return (HsLinearArrow (EpLolly arr'))
markArrow (HsExplicitMult (pct, arr) t) = do
  pct' <- markEpToken pct
  t' <- markAnnotated t
  arr' <- markEpUniToken arr
  return (HsExplicitMult (pct', arr') t')

-- ---------------------------------------------------------------------

markAnnCloseP :: (Monad m, Monoid w) => EpAnn AnnPragma -> EP w m (EpAnn AnnPragma)
markAnnCloseP an = markEpAnnLMS' an lapr_close AnnClose (Just "#-}")

markAnnCloseP' :: (Monad m, Monoid w) => AnnPragma -> EP w m AnnPragma
markAnnCloseP' an = markEpAnnLMS0 an lapr_close AnnClose (Just "#-}")

markAnnOpenP :: (Monad m, Monoid w) => EpAnn AnnPragma -> SourceText -> String -> EP w m (EpAnn AnnPragma)
markAnnOpenP an NoSourceText txt   = markEpAnnLMS' an lapr_open AnnOpen (Just txt)
markAnnOpenP an (SourceText txt) _ = markEpAnnLMS' an lapr_open AnnOpen (Just $ unpackFS txt)

markAnnOpenP' :: (Monad m, Monoid w) => AnnPragma -> SourceText -> String -> EP w m AnnPragma
markAnnOpenP' an NoSourceText txt   = markEpAnnLMS0 an lapr_open AnnOpen (Just txt)
markAnnOpenP' an (SourceText txt) _ = markEpAnnLMS0 an lapr_open AnnOpen (Just $ unpackFS txt)

markAnnOpen :: (Monad m, Monoid w)
  => [AddEpAnn] -> SourceText -> String -> EP w m [AddEpAnn]
markAnnOpen an NoSourceText txt   = markEpAnnLMS'' an lidl AnnOpen (Just txt)
markAnnOpen an (SourceText txt) _ = markEpAnnLMS'' an lidl AnnOpen (Just $ unpackFS txt)

markAnnOpen' :: (Monad m, Monoid w)
  => Maybe EpaLocation -> SourceText -> String -> EP w m (Maybe EpaLocation)
markAnnOpen' ms NoSourceText txt   = printStringAtMLoc' ms txt
markAnnOpen' ms (SourceText txt) _ = printStringAtMLoc' ms $ unpackFS txt

markAnnOpen'' :: (Monad m, Monoid w)
  => EpaLocation -> SourceText -> String -> EP w m EpaLocation
markAnnOpen'' el NoSourceText txt   = printStringAtAA el txt
markAnnOpen'' el (SourceText txt) _ = printStringAtAA el $ unpackFS txt

-- ---------------------------------------------------------------------
{-
data AnnParen
  = AnnParen {
      ap_adornment :: ParenType,
      ap_open      :: EpaLocation,
      ap_close     :: EpaLocation
      } deriving (Data)
-}
markOpeningParen, markClosingParen :: (Monad m, Monoid w) => AnnParen -> EP w m AnnParen
markOpeningParen an = markParen an lfst
markClosingParen an = markParen an lsnd

markParen :: (Monad m, Monoid w) => AnnParen -> (forall a. Lens (a,a) a) -> EP w m AnnParen
markParen (AnnParen pt o c) l = do
  loc' <- markKwA (view l $ kw pt) (view l (o, c))
  let (o',c') = set l loc' (o,c)
  return (AnnParen pt o' c')
  where
    kw AnnParens       = (AnnOpenP,  AnnCloseP)
    kw AnnParensHash   = (AnnOpenPH, AnnClosePH)
    kw AnnParensSquare = (AnnOpenS, AnnCloseS)

-- ---------------------------------------------------------------------
-- Bare bones Optics
-- Base on From https://hackage.haskell.org/package/lens-tutorial-1.0.3/docs/Control-Lens-Tutorial.html

type Lens    a b = forall f . Functor f => (b -> f        b) -> (a -> f        a)
type Getting a b =                         (b -> Const  b b) -> (a -> Const b  a)
type ASetter a b =                         (b -> Identity b) -> (a -> Identity a)

view :: MonadReader s m => Getting s a -> m a
view l = Reader.asks (getConst . l Const)
{-# INLINE view #-}

over :: ASetter a b -> (b -> b) -> (a -> a)
over l f = runIdentity . l (Identity . f)
{-# INLINE over #-}

set  :: Lens a b -> b -> a -> a
set lens b = over lens (\_ -> b)
{-# INLINE set #-}

{-
Question: How do I combine lenses?

Answer: You compose them, using function composition (Yes, really!)

You can think of the function composition operator as having this type:

(.) :: Lens' a b -> Lens' b c -> Lens' a c
-}

-- ---------------------------------------------------------------------
-- Lenses

-- data EpAnn ann
--   = EpAnn { entry   :: !Anchor
--            , anns     :: !ann
--            , comments :: !EpAnnComments
--            }

lepa :: Lens (EpAnn a) a
lepa k epAnn = fmap (\newAnns -> epAnn { anns = newAnns })
                    (k (anns epAnn))

-- data AnnsModule
--   = AnnsModule {
--     am_main  :: [AddEpAnn],
--     am_decls :: [TrailingAnn],
--     am_cs    :: [LEpaComment],
--     am_eof   :: Maybe (RealSrcSpan, RealSrcSpan)
--     } deriving (Data, Eq)

lam_main :: Lens AnnsModule [AddEpAnn]
lam_main k annsModule = fmap (\newAnns -> annsModule { am_main = newAnns })
                             (k (am_main annsModule))

-- lam_decls :: Lens AnnsModule AnnList
-- lam_decls k annsModule = fmap (\newAnns -> annsModule { am_decls = newAnns })
--                               (k (am_decls annsModule))


-- data EpAnnImportDecl = EpAnnImportDecl
--   { importDeclAnnImport    :: EpaLocation
--   , importDeclAnnPragma    :: Maybe (EpaLocation, EpaLocation)
--   , importDeclAnnSafe      :: Maybe EpaLocation
--   , importDeclAnnQualified :: Maybe EpaLocation
--   , importDeclAnnPackage   :: Maybe EpaLocation
--   , importDeclAnnAs        :: Maybe EpaLocation
--   } deriving (Data)

limportDeclAnnImport :: Lens EpAnnImportDecl EpaLocation
limportDeclAnnImport k annImp = fmap (\new -> annImp { importDeclAnnImport = new })
                                     (k (importDeclAnnImport annImp))

-- limportDeclAnnPragma :: Lens EpAnnImportDecl (Maybe (EpaLocation, EpaLocation))
-- limportDeclAnnPragma k annImp = fmap (\new -> annImp { importDeclAnnPragma = new })
--                                      (k (importDeclAnnPragma annImp))

limportDeclAnnSafe :: Lens EpAnnImportDecl (Maybe EpaLocation)
limportDeclAnnSafe k annImp = fmap (\new -> annImp { importDeclAnnSafe = new })
                                     (k (importDeclAnnSafe annImp))

limportDeclAnnQualified :: Lens EpAnnImportDecl (Maybe EpaLocation)
limportDeclAnnQualified k annImp = fmap (\new -> annImp { importDeclAnnQualified = new })
                                     (k (importDeclAnnQualified annImp))

limportDeclAnnPackage :: Lens EpAnnImportDecl (Maybe EpaLocation)
limportDeclAnnPackage k annImp = fmap (\new -> annImp { importDeclAnnPackage = new })
                                     (k (importDeclAnnPackage annImp))

-- limportDeclAnnAs :: Lens EpAnnImportDecl (Maybe EpaLocation)
-- limportDeclAnnAs k annImp = fmap (\new -> annImp { importDeclAnnAs = new })
--                                      (k (importDeclAnnAs annImp))

-- -------------------------------------

-- data AnnList
--   = AnnList {
--       al_anchor    :: Maybe Anchor, -- ^ start point of a list having layout
--       al_open      :: Maybe AddEpAnn,
--       al_close     :: Maybe AddEpAnn,
--       al_rest      :: [AddEpAnn], -- ^ context, such as 'where' keyword
--       al_trailing  :: [TrailingAnn] -- ^ items appearing after the
--                                     -- list, such as '=>' for a
--                                     -- context
--       } deriving (Data,Eq)

lal_open :: Lens AnnList (Maybe AddEpAnn)
lal_open k parent = fmap (\new -> parent { al_open = new })
                           (k (al_open parent))

lal_close :: Lens AnnList (Maybe AddEpAnn)
lal_close k parent = fmap (\new -> parent { al_close = new })
                           (k (al_close parent))

lal_rest :: Lens AnnList [AddEpAnn]
lal_rest k parent = fmap (\new -> parent { al_rest = new })
                           (k (al_rest parent))

-- lal_trailing :: Lens AnnList [TrailingAnn]
-- lal_trailing k parent = fmap (\new -> parent { al_trailing = new })
--                            (k (al_trailing parent))

-- -------------------------------------

lapr_rest :: Lens AnnPragma [AddEpAnn]
lapr_rest k parent = fmap (\newAnns -> parent { apr_rest = newAnns })
                          (k (apr_rest parent))

lapr_open :: Lens AnnPragma AddEpAnn
lapr_open k parent = fmap (\new -> parent { apr_open = new })
                          (k (apr_open parent))

lapr_close :: Lens AnnPragma AddEpAnn
lapr_close k parent = fmap (\new -> parent { apr_close = new })
                          (k (apr_close parent))

lidl :: Lens [AddEpAnn] [AddEpAnn]
lidl k parent = fmap (\new -> new)
                     (k parent)

lid :: Lens a a
lid k parent = fmap (\new -> new)
                    (k parent)

lfst :: Lens (a,a) a
lfst k parent = fmap (\new -> (new, snd parent))
                     (k (fst parent))

lsnd :: Lens (a,a) a
lsnd k parent = fmap (\new -> (fst parent, new))
                     (k (snd parent))

-- -------------------------------------
-- data AnnExplicitSum
--   = AnnExplicitSum {
--       aesOpen       :: EpaLocation,
--       aesBarsBefore :: [EpaLocation],
--       aesBarsAfter  :: [EpaLocation],
--       aesClose      :: EpaLocation
--       } deriving Data

laesOpen :: Lens AnnExplicitSum EpaLocation
laesOpen k parent = fmap (\new -> parent { aesOpen = new })
                         (k (aesOpen parent))

laesBarsBefore :: Lens AnnExplicitSum [EpaLocation]
laesBarsBefore k parent = fmap (\new -> parent { aesBarsBefore = new })
                               (k (aesBarsBefore parent))

laesBarsAfter :: Lens AnnExplicitSum [EpaLocation]
laesBarsAfter k parent = fmap (\new -> parent { aesBarsAfter = new })
                               (k (aesBarsAfter parent))

laesClose :: Lens AnnExplicitSum EpaLocation
laesClose k parent = fmap (\new -> parent { aesClose = new })
                               (k (aesClose parent))

-- -------------------------------------
-- data AnnFieldLabel
--   = AnnFieldLabel {
--       afDot :: Maybe EpaLocation
--       } deriving Data

lafDot :: Lens AnnFieldLabel (Maybe EpaLocation)
lafDot k parent = fmap (\new -> parent { afDot = new })
                         (k (afDot parent))

-- -------------------------------------
-- data AnnProjection
--   = AnnProjection {
--       apOpen  :: EpaLocation, -- ^ '('
--       apClose :: EpaLocation  -- ^ ')'
--       } deriving Data

lapOpen :: Lens AnnProjection EpaLocation
lapOpen k parent = fmap (\new -> parent { apOpen = new })
                         (k (apOpen parent))

lapClose :: Lens AnnProjection EpaLocation
lapClose k parent = fmap (\new -> parent { apClose = new })
                         (k (apClose parent))

-- -------------------------------------
-- data AnnsIf
--   = AnnsIf {
--       aiIf       :: EpaLocation,
--       aiThen     :: EpaLocation,
--       aiElse     :: EpaLocation,
--       aiThenSemi :: Maybe EpaLocation,
--       aiElseSemi :: Maybe EpaLocation
--       } deriving Data

laiIf :: Lens AnnsIf EpaLocation
laiIf k parent = fmap (\new -> parent { aiIf = new })
                      (k (aiIf parent))

laiThen :: Lens AnnsIf EpaLocation
laiThen k parent = fmap (\new -> parent { aiThen = new })
                        (k (aiThen parent))

laiElse :: Lens AnnsIf EpaLocation
laiElse k parent = fmap (\new -> parent { aiElse = new })
                        (k (aiElse parent))

laiThenSemi :: Lens AnnsIf (Maybe EpaLocation)
laiThenSemi k parent = fmap (\new -> parent { aiThenSemi = new })
                            (k (aiThenSemi parent))

laiElseSemi :: Lens AnnsIf (Maybe EpaLocation)
laiElseSemi k parent = fmap (\new -> parent { aiElseSemi = new })
                            (k (aiElseSemi parent))

-- -------------------------------------

-- data AnnParen
--   = AnnParen {
--       ap_adornment :: ParenType,
--       ap_open      :: EpaLocation,
--       ap_close     :: EpaLocation
--       } deriving (Data)

-- lap_open :: Lens AnnParen EpaLocation
-- lap_open k parent = fmap (\new -> parent { ap_open = new })
--                          (k (ap_open parent))

-- lap_close :: Lens AnnParen EpaLocation
-- lap_close k parent = fmap (\new -> parent { ap_close = new })
--                           (k (ap_close parent))

-- -------------------------------------
-- data EpAnnHsCase = EpAnnHsCase
--       { hsCaseAnnCase :: EpaLocation
--       , hsCaseAnnOf   :: EpaLocation
--       , hsCaseAnnsRest :: [AddEpAnn]
--       } deriving Data

lhsCaseAnnCase :: Lens EpAnnHsCase EpaLocation
lhsCaseAnnCase k parent = fmap (\new -> parent { hsCaseAnnCase = new })
                               (k (hsCaseAnnCase parent))

lhsCaseAnnOf :: Lens EpAnnHsCase EpaLocation
lhsCaseAnnOf k parent = fmap (\new -> parent { hsCaseAnnOf = new })
                               (k (hsCaseAnnOf parent))

lhsCaseAnnsRest :: Lens EpAnnHsCase [AddEpAnn]
lhsCaseAnnsRest k parent = fmap (\new -> parent { hsCaseAnnsRest = new })
                                (k (hsCaseAnnsRest parent))

-- ---------------------------------------------------------------------

-- data HsRuleAnn
--   = HsRuleAnn
--        { ra_tyanns :: Maybe (AddEpAnn, AddEpAnn)
--                  -- ^ The locations of 'forall' and '.' for forall'd type vars
--                  -- Using AddEpAnn to capture possible unicode variants
--        , ra_tmanns :: Maybe (AddEpAnn, AddEpAnn)
--                  -- ^ The locations of 'forall' and '.' for forall'd term vars
--                  -- Using AddEpAnn to capture possible unicode variants
--        , ra_rest :: [AddEpAnn]
--        } deriving (Data, Eq)

lra_tyanns :: Lens HsRuleAnn (Maybe (AddEpAnn, AddEpAnn))
lra_tyanns k parent = fmap (\new -> parent { ra_tyanns = new })
                               (k (ra_tyanns parent))

ff :: Maybe (a,b) -> (Maybe a,Maybe b)
ff Nothing = (Nothing, Nothing)
ff (Just (a,b)) = (Just a, Just b)


gg :: (Maybe a,Maybe b) -> Maybe (a,b)
gg (Nothing, Nothing) = Nothing
gg (Just a, Just b) = Just (a,b)
gg _ = error "gg:expecting two Nothing or two Just"

lff :: Lens (Maybe (a,b)) (Maybe a,Maybe b)
lff k parent = fmap (\new -> gg new)
                    (k (ff parent))

-- (.) :: Lens' a b -> Lens' b c -> Lens' a c
lra_tyanns_fst :: Lens HsRuleAnn (Maybe AddEpAnn)
lra_tyanns_fst = lra_tyanns . lff . lfst

lra_tyanns_snd :: Lens HsRuleAnn (Maybe AddEpAnn)
lra_tyanns_snd = lra_tyanns . lff . lsnd

lra_tmanns :: Lens HsRuleAnn (Maybe (AddEpAnn, AddEpAnn))
lra_tmanns k parent = fmap (\new -> parent { ra_tmanns = new })
                               (k (ra_tmanns parent))

lra_tmanns_fst :: Lens HsRuleAnn (Maybe AddEpAnn)
lra_tmanns_fst = lra_tmanns . lff . lfst

lra_tmanns_snd :: Lens HsRuleAnn (Maybe AddEpAnn)
lra_tmanns_snd = lra_tmanns . lff . lsnd

lra_rest :: Lens HsRuleAnn [AddEpAnn]
lra_rest k parent = fmap (\new -> parent { ra_rest = new })
                                (k (ra_rest parent))


-- ---------------------------------------------------------------------
-- data GrhsAnn
--   = GrhsAnn {
--       ga_vbar :: Maybe EpaLocation, -- TODO:AZ do we need this?
--       ga_sep  :: AddEpAnn -- ^ Match separator location
--       } deriving (Data)

lga_vbar :: Lens GrhsAnn (Maybe EpaLocation)
lga_vbar k parent = fmap (\new -> parent { ga_vbar = new })
                                (k (ga_vbar parent))

lga_sep :: Lens GrhsAnn AddEpAnn
lga_sep k parent = fmap (\new -> parent { ga_sep = new })
                                (k (ga_sep parent))

-- ---------------------------------------------------------------------
-- data AnnSig
--   = AnnSig {
--       asDcolon :: AddEpAnn, -- Not an EpaAnchor to capture unicode option
--       asRest   :: [AddEpAnn]
--       } deriving Data

lasDcolon :: Lens AnnSig AddEpAnn
lasDcolon k parent = fmap (\new -> parent { asDcolon = new })
                                (k (asDcolon parent))

lasRest :: Lens AnnSig [AddEpAnn]
lasRest k parent = fmap (\new -> parent { asRest = new })
                                (k (asRest parent))

-- ---------------------------------------------------------------------
-- data EpAnnSumPat = EpAnnSumPat
--       { sumPatParens      :: [AddEpAnn]
--       , sumPatVbarsBefore :: [EpaLocation]
--       , sumPatVbarsAfter  :: [EpaLocation]
--       } deriving Data

lsumPatParens :: Lens EpAnnSumPat [AddEpAnn]
lsumPatParens k parent = fmap (\new -> parent { sumPatParens = new })
                              (k (sumPatParens parent))

lsumPatVbarsBefore :: Lens EpAnnSumPat [EpaLocation]
lsumPatVbarsBefore k parent = fmap (\new -> parent { sumPatVbarsBefore = new })
                              (k (sumPatVbarsBefore parent))

lsumPatVbarsAfter :: Lens EpAnnSumPat [EpaLocation]
lsumPatVbarsAfter k parent = fmap (\new -> parent { sumPatVbarsAfter = new })
                              (k (sumPatVbarsAfter parent))

-- End of lenses
-- ---------------------------------------------------------------------

markLensKwA :: (Monad m, Monoid w)
  => a -> Lens a AddEpAnn -> EP w m a
markLensKwA a l = do
  loc <- markKw (view l a)
  return (set l loc a)

markLensKw' :: (Monad m, Monoid w)
  => EpAnn a -> Lens a EpaLocation -> AnnKeywordId -> EP w m (EpAnn a)
markLensKw' (EpAnn anc a cs) l kw = do
  loc <- markKwA kw (view l a)
  return (EpAnn anc (set l loc a) cs)

markLensKw :: (Monad m, Monoid w)
  => a -> Lens a EpaLocation -> AnnKeywordId -> EP w m a
markLensKw a l kw = do
  loc <- markKwA kw (view l a)
  return (set l loc a)

markAnnKwAllL :: (Monad m, Monoid w)
  => a -> Lens a [EpaLocation] -> AnnKeywordId -> EP w m a
markAnnKwAllL a l kw = do
  anns <- mapM (markKwA kw) (view l a)
  return (set l anns a)

markLensKwM :: (Monad m, Monoid w)
  => EpAnn a -> Lens a (Maybe EpaLocation) -> AnnKeywordId -> EP w m (EpAnn a)
markLensKwM (EpAnn anc a cs) l kw = do
  new <- go (view l a)
  return (EpAnn anc (set l new a) cs)
  where
    go Nothing = return Nothing
    go (Just s) = Just <$> markKwA kw s

markLensKwM' :: (Monad m, Monoid w)
  => a -> Lens a (Maybe EpaLocation) -> AnnKeywordId -> EP w m a
markLensKwM' a l kw = do
  new <- go (view l a)
  return (set l new a)
  where
    go Nothing = return Nothing
    go (Just s) = Just <$> markKwA kw s

-- ---------------------------------------------------------------------

markEpAnnL' :: (Monad m, Monoid w)
  => EpAnn ann -> Lens ann [AddEpAnn] -> AnnKeywordId -> EP w m (EpAnn ann)
markEpAnnL' epann l kw = markEpAnnL epann (lepa . l) kw

markEpAnnL :: (Monad m, Monoid w)
  => ann -> Lens ann [AddEpAnn] -> AnnKeywordId -> EP w m ann
markEpAnnL a l kw = do
  anns <- mark (view l a) kw
  return (set l anns a)

-- -------------------------------------

markEpAnnAllL :: (Monad m, Monoid w)
  => EpAnn ann -> Lens ann [AddEpAnn] -> AnnKeywordId -> EP w m (EpAnn ann)
markEpAnnAllL (EpAnn anc a cs) l kw = do
  anns <- mapM doit (view l a)
  return (EpAnn anc (set l anns a) cs)
  where
    doit an@(AddEpAnn ka _)
      = if ka == kw
          then markKw an
          else return an

markEpAnnAllL' :: (Monad m, Monoid w)
  => ann -> Lens ann [AddEpAnn] -> AnnKeywordId -> EP w m ann
markEpAnnAllL' a l kw = do
  anns <- mapM doit (view l a)
  return (set l anns a)
  where
    doit an@(AddEpAnn ka _)
      = if ka == kw
          then markKw an
          else return an

markAddEpAnn :: (Monad m, Monoid w) => AddEpAnn -> EP w m AddEpAnn
markAddEpAnn a@(AddEpAnn kw _) = do
  r <- mark [a] kw
  case r of
    [a'] -> return a'
    _ -> error "Should not happen: markAddEpAnn"

mark :: (Monad m, Monoid w) => [AddEpAnn] -> AnnKeywordId -> EP w m [AddEpAnn]
mark anns kw = do
  case find' kw anns of
    (lead, Just aa, end) -> do
      aa' <- markKw aa
      return (lead ++ [aa'] ++ end)
    (_lead, Nothing, _end) -> case find' (unicodeAnn kw) anns of
      (leadu, Just aau, endu) -> do
        aau' <- markKw aau
        return (leadu ++ [aau'] ++ endu)
      (_,Nothing,_) -> return anns

-- | Find for update, returning lead section of the list, item if
-- found, and tail of the list
find' :: AnnKeywordId -> [AddEpAnn] -> ([AddEpAnn], Maybe AddEpAnn, [AddEpAnn])
find' kw anns = (lead, middle, end)
  where
    (lead, rest) = break (\(AddEpAnn k _) -> k == kw) anns
    (middle,end) = case rest of
      [] -> (Nothing, [])
      (x:xs) -> (Just x, xs)

markKw :: (Monad m, Monoid w) => AddEpAnn -> EP w m AddEpAnn
markKw an = markKwC CaptureComments an

markKwC :: (Monad m, Monoid w) => CaptureComments -> AddEpAnn -> EP w m AddEpAnn
markKwC capture (AddEpAnn kw ss) = do
  ss' <- markKwAC capture kw ss
  return (AddEpAnn kw ss')

-- | This should be the main driver of the process, managing printing keywords.
-- It returns the 'EpaDelta' variant of the passed in 'EpaLocation'
markKwA :: (Monad m, Monoid w) => AnnKeywordId -> EpaLocation -> EP w m EpaLocation
markKwA kw aa = markKwAC CaptureComments kw aa

markKwAC :: (Monad m, Monoid w)
  => CaptureComments -> AnnKeywordId -> EpaLocation -> EP w m EpaLocation
markKwAC capture kw aa = printStringAtAAC capture aa (keywordToString kw)

-- | Print a keyword encoded in a 'TrailingAnn'
markKwT :: (Monad m, Monoid w) => TrailingAnn -> EP w m TrailingAnn
markKwT (AddSemiAnn ss)    = AddSemiAnn    <$> markKwA AnnSemi ss
markKwT (AddCommaAnn ss)   = AddCommaAnn   <$> markKwA AnnComma ss
markKwT (AddVbarAnn ss)    = AddVbarAnn    <$> markKwA AnnVbar ss
markKwT (AddDarrowAnn ss)  = AddDarrowAnn  <$> markKwA AnnDarrow ss
markKwT (AddDarrowUAnn ss) = AddDarrowUAnn <$> markKwA AnnDarrowU ss

-- ---------------------------------------------------------------------

markAnnList :: (Monad m, Monoid w)
  => EpAnn AnnList -> EP w m a -> EP w m (EpAnn AnnList, a)
markAnnList ann action = do
  markAnnListA ann $ \a -> do
    r <- action
    return (a,r)

markAnnList' :: (Monad m, Monoid w)
  => AnnList -> EP w m a -> EP w m (AnnList, a)
markAnnList' ann action = do
  markAnnListA' ann $ \a -> do
    r <- action
    return (a,r)

markAnnListA :: (Monad m, Monoid w)
  => EpAnn AnnList
  -> (EpAnn AnnList -> EP w m (EpAnn AnnList, a))
  -> EP w m (EpAnn AnnList, a)
markAnnListA an action = do
  an0 <- markLensMAA an lal_open
  an1 <- markEpAnnAllL an0 lal_rest AnnSemi
  (an2, r) <- action an1
  an3 <- markLensMAA an2 lal_close
  return (an3, r)

markAnnListA' :: (Monad m, Monoid w)
  => AnnList
  -> (AnnList -> EP w m (AnnList, a))
  -> EP w m (AnnList, a)
markAnnListA' an action = do
  an0 <- markLensMAA' an lal_open
  an1 <- markEpAnnAllL' an0 lal_rest AnnSemi
  (an2, r) <- action an1
  an3 <- markLensMAA' an2 lal_close
  return (an3, r)

-- ---------------------------------------------------------------------

printCommentsBefore :: (Monad m, Monoid w) => RealSrcSpan -> EP w m ()
printCommentsBefore ss = do
  cs <- commentAllocationBefore ss
  debugM $ "printCommentsBefore: (ss): " ++ showPprUnsafe (rs2range ss)
  -- debugM $ "printComments: (ss,comment locations): " ++ showPprUnsafe (rs2range ss,map commentLoc cs)
  mapM_ printOneComment cs

printCommentsIn :: (Monad m, Monoid w) => RealSrcSpan -> EP w m ()
printCommentsIn ss = do
  cs <- commentAllocationIn ss
  debugM $ "printCommentsIn: (ss): " ++ showPprUnsafe (rs2range ss)
  -- debugM $ "printComments: (ss,comment locations): " ++ showPprUnsafe (rs2range ss,map commentLoc cs)
  mapM_ printOneComment cs
  debugM $ "printCommentsIn:done"

-- ---------------------------------------------------------------------

printOneComment :: (Monad m, Monoid w) => Comment -> EP w m ()
printOneComment c@(Comment _str loc _r _mo) = do
  debugM $ "printOneComment:c=" ++ showGhc c
  dp <-case loc of
    EpaDelta dp _ -> return dp
    EpaSpan (RealSrcSpan r _) -> do
        pe <- getPriorEndD
        debugM $ "printOneComment:pe=" ++ showGhc pe
        let dp = ss2delta pe r
        debugM $ "printOneComment:(dp,pe,loc)=" ++ showGhc (dp,pe,loc)
        adjustDeltaForOffsetM dp
    EpaSpan (UnhelpfulSpan _) -> return (SameLine 0)
  mep <- getExtraDP
  dp' <- case mep of
    Just (EpaDelta edp _) -> do
      debugM $ "printOneComment:edp=" ++ show edp
      adjustDeltaForOffsetM edp
    _ -> return dp
  -- Start of debug printing
  LayoutStartCol dOff <- getLayoutOffsetD
  debugM $ "printOneComment:(dp,dp',dOff,loc)=" ++ showGhc (dp,dp',dOff,loc)
  -- End of debug printing
  updateAndApplyComment c dp'
  printQueuedComment c dp'

updateAndApplyComment :: (Monad m, Monoid w) => Comment -> DeltaPos -> EP w m ()
updateAndApplyComment (Comment str anc pp mo) dp = do
  applyComment (Comment str anc' pp mo)
  where
    (r,c) = ss2posEnd pp
    dp'' = case anc of
      EpaDelta dp1 _ -> dp1
      EpaSpan (RealSrcSpan la _) ->
           if r == 0
             then (ss2delta (r,c+0) la)
             else (ss2delta (r,c)   la)
      EpaSpan (UnhelpfulSpan _) -> SameLine 0
    dp' = case anc of
      EpaSpan (RealSrcSpan r1 _) ->
          if pp == r1
                 then dp
                 else dp''
      _ -> dp''
    op' = case dp' of
            SameLine n -> if n >= 0
                            then EpaDelta dp' NoComments
                            else EpaDelta dp NoComments
            _ -> EpaDelta dp' NoComments
    anc' = if str == "" && op' == EpaDelta (SameLine 0) NoComments -- EOF comment
           then EpaDelta dp NoComments
           else EpaDelta dp NoComments

-- ---------------------------------------------------------------------

commentAllocationBefore :: (Monad m, Monoid w) => RealSrcSpan -> EP w m [Comment]
commentAllocationBefore ss = do
  cs <- getUnallocatedComments
  -- Note: The CPP comment injection may change the file name in the
  -- RealSrcSpan, which affects comparison, as the Ord instance for
  -- RealSrcSpan compares the file first. So we sort via ss2pos
  -- TODO: this is inefficient, use Pos all the way through
  let (earlier,later) = partition (\(Comment _str loc _r _mo) ->
                                     case loc of
                                       EpaSpan (RealSrcSpan r _) -> (ss2pos r) <= (ss2pos ss)
                                       _ -> True -- Choose one
                                  ) cs
  putUnallocatedComments later
  -- debugM $ "commentAllocation:(ss,earlier,later)" ++ show (rs2range ss,earlier,later)
  return earlier

commentAllocationIn :: (Monad m, Monoid w) => RealSrcSpan -> EP w m [Comment]
commentAllocationIn ss = do
  cs <- getUnallocatedComments
  -- Note: The CPP comment injection may change the file name in the
  -- RealSrcSpan, which affects comparison, as the Ord instance for
  -- RealSrcSpan compares the file first. So we sort via ss2pos
  -- TODO: this is inefficient, use Pos all the way through
  let (earlier,later) = partition (\(Comment _str loc _r _mo) ->
                                     case loc of
                                       EpaSpan (RealSrcSpan r _) -> (ss2posEnd r) <= (ss2posEnd ss)
                                       _ -> True -- Choose one
                                  ) cs
  putUnallocatedComments later
  -- debugM $ "commentAllocation:(ss,earlier,later)" ++ show (rs2range ss,earlier,later)
  return earlier
-- ---------------------------------------------------------------------

markAnnotatedWithLayout :: (Monad m, Monoid w) => ExactPrint ast => ast -> EP w m ast
markAnnotatedWithLayout a = setLayoutBoth $ markAnnotated a

-- ---------------------------------------------------------------------

markTopLevelList :: (Monad m, Monoid w) => ExactPrint ast => [ast] -> EP w m [ast]
markTopLevelList ls = mapM (\a -> setLayoutTopLevelP $ markAnnotated a) ls

-- ---------------------------------------------------------------------
-- End of utility functions
-- ---------------------------------------------------------------------
-- Start of ExactPrint instances
-- ---------------------------------------------------------------------

-- | Bare Located elements are simply stripped off without further
-- processing.
instance (ExactPrint a) => ExactPrint (Located a) where
  getAnnotationEntry (L l _) = case l of
    UnhelpfulSpan _ -> NoEntryVal
    _ -> Entry (hackSrcSpanToAnchor l) [] emptyComments NoFlushComments CanUpdateAnchorOnly

  setAnnotationAnchor (L l a) _anc _ts _cs = L l a

  exact (L l a) = L l <$> markAnnotated a

instance (ExactPrint a) => ExactPrint (LocatedE a) where
  getAnnotationEntry (L l _) = Entry l [] emptyComments NoFlushComments CanUpdateAnchorOnly
  setAnnotationAnchor (L _ a) anc _ts _cs = L anc a

  exact (L la a) = do
    debugM $ "LocatedE a:la loc=" ++ show (ss2range $ locA la)
    a' <- markAnnotated a
    return (L la a')

instance (ExactPrint a) => ExactPrint (LocatedA a) where
  getAnnotationEntry = entryFromLocatedA
  setAnnotationAnchor la anc ts cs = setAnchorAn la anc ts cs
  exact (L la a) = do
    debugM $ "LocatedA a:la loc=" ++ show (ss2range $ locA la)
    a' <- markAnnotated a
    return (L la a')

instance (ExactPrint a) => ExactPrint (LocatedAn NoEpAnns a) where
  getAnnotationEntry = entryFromLocatedA
  setAnnotationAnchor la anc ts cs = setAnchorAn la anc ts cs
  exact (L la a) = do
    a' <- markAnnotated a
    return (L la a')

instance (ExactPrint a) => ExactPrint [a] where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor ls _ _ _ = ls
  exact ls = mapM markAnnotated ls

instance (ExactPrint a) => ExactPrint (Maybe a) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor ma _ _ _ = ma
  exact ma = mapM markAnnotated ma

-- ---------------------------------------------------------------------

-- | 'Located (HsModule GhcPs)' corresponds to 'ParsedSource'
instance ExactPrint (HsModule GhcPs) where
  getAnnotationEntry hsmod = fromAnn' (hsmodAnn $ hsmodExt hsmod)
  -- A bit pointless actually changing anything here
  setAnnotationAnchor hsmod anc _ts cs = setAnchorHsModule hsmod anc cs
                   `debug` ("setAnnotationAnchor hsmod called" ++ showAst (anc,cs))

  exact (HsModule (XModulePs an lo mdeprec mbDoc) mmn mexports imports decls) = do

    let mbDoc' = mbDoc

    (an0, mmn' , mdeprec', mexports') <-
      case mmn of
        Nothing -> return (an, mmn, mdeprec, mexports)
        Just m -> do
          an0 <- markEpAnnL' an lam_main AnnModule
          m' <- markAnnotated m

          mdeprec' <- setLayoutTopLevelP $ markAnnotated mdeprec

          mexports' <- setLayoutTopLevelP $ markAnnotated mexports

          an1 <- setLayoutTopLevelP $ markEpAnnL' an0 lam_main AnnWhere

          return (an1, Just m', mdeprec', mexports')

    lo0 <- case lo of
        EpExplicitBraces open close -> do
          open' <- markEpToken open
          return (EpExplicitBraces open' close)
        _ -> return lo

    am_decls' <- markTrailing (am_decls $ anns an0)
    imports' <- markTopLevelList imports

    case lo of
        EpExplicitBraces _ _ -> return ()
        _ -> do
          -- Get rid of the balance of the preceding comments before starting on the decls
          flushComments []
          putUnallocatedComments []

    decls' <- markTopLevelList (filter removeDocDecl decls)

    lo1 <- case lo0 of
        EpExplicitBraces open close -> do
          close' <- markEpToken close
          return (EpExplicitBraces open close')
        _ -> return lo

    -- Print EOF
    case am_eof $ anns an of
      Nothing -> return ()
      Just (pos, prior) -> do
        debugM $ "am_eof:" ++ showGhc (pos, prior)
        setEofPos (Just (pos, prior))

    let anf = an0 { anns = (anns an0) { am_decls = am_decls' }}
    debugM $ "HsModule, anf=" ++ showAst anf

    return (HsModule (XModulePs anf lo1 mdeprec' mbDoc') mmn' mexports' imports' decls')


removeDocDecl :: LHsDecl GhcPs -> Bool
removeDocDecl (L _ DocD{}) = False
removeDocDecl _ = True

-- ---------------------------------------------------------------------

instance ExactPrint ModuleName where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor n _anc _ cs = n
     `debug` ("ModuleName.setAnnotationAnchor:cs=" ++ showAst cs)
  exact n = do
    debugM $ "ModuleName: " ++ showPprUnsafe n
    withPpr n

-- ---------------------------------------------------------------------

instance ExactPrint (LocatedP (WarningTxt GhcPs)) where
  getAnnotationEntry = entryFromLocatedA
  setAnnotationAnchor = setAnchorAn

  exact (L an (WarningTxt mb_cat src ws)) = do
    an0 <- markAnnOpenP an src "{-# WARNING"
    mb_cat' <- markAnnotated mb_cat
    an1 <- markEpAnnL' an0 lapr_rest AnnOpenS
    ws' <- markAnnotated ws
    an2 <- markEpAnnL' an1 lapr_rest AnnCloseS
    an3 <- markAnnCloseP an2
    return (L an3 (WarningTxt mb_cat' src ws'))

  exact (L an (DeprecatedTxt src ws)) = do
    an0 <- markAnnOpenP an src "{-# DEPRECATED"
    an1 <- markEpAnnL' an0 lapr_rest AnnOpenS
    ws' <- markAnnotated ws
    an2 <- markEpAnnL' an1 lapr_rest AnnCloseS
    an3 <- markAnnCloseP an2
    return (L an3 (DeprecatedTxt src ws'))

instance ExactPrint InWarningCategory where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (InWarningCategory tkIn source (L l wc)) = do
      tkIn' <- markEpToken tkIn
      L _ (_,wc') <- markAnnotated (L l (source, wc))
      return (InWarningCategory tkIn' source (L l wc'))

instance ExactPrint (SourceText, WarningCategory) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (st, WarningCategory wc) = do
      case st of
          NoSourceText -> printStringAdvance $ "\"" ++ (unpackFS wc) ++ "\""
          SourceText src -> printStringAdvance $ (unpackFS src)
      return (st, WarningCategory wc)

-- ---------------------------------------------------------------------

instance ExactPrint (ImportDecl GhcPs) where
  getAnnotationEntry idecl = fromAnn (ideclAnn $ ideclExt idecl)
  setAnnotationAnchor idecl anc ts cs = idecl { ideclExt
                    = (ideclExt idecl) { ideclAnn = setAnchorEpa (ideclAnn $ ideclExt idecl) anc ts cs} }

  exact (ImportDecl (XImportDeclPass ann msrc impl)
                     modname mpkg src safeflag qualFlag mAs hiding) = do

    ann0 <- markLensKw' ann limportDeclAnnImport AnnImport
    let (EpAnn _anc an _cs) = ann0

    -- "{-# SOURCE" and "#-}"
    importDeclAnnPragma' <-
      case msrc of
        SourceText _txt -> do
          debugM $ "ImportDecl sourcetext"
          case importDeclAnnPragma an of
            Just (mo, mc) -> do
              mo' <- markAnnOpen'' mo msrc "{-# SOURCE"
              mc' <- printStringAtAA mc "#-}"
              return $ Just (mo', mc')
            Nothing ->  do
              _ <- markAnnOpen' Nothing msrc "{-# SOURCE"
              printStringAtLsDelta (SameLine 1) "#-}"
              return Nothing
        NoSourceText -> return (importDeclAnnPragma an)
    ann1 <- if safeflag
      then (markLensKwM ann0 limportDeclAnnSafe AnnSafe)
      else return ann0
    ann2 <-
      case qualFlag of
        QualifiedPre  -- 'qualified' appears in prepositive position.
          -> printStringAtMLocL ann1 limportDeclAnnQualified "qualified"
        _ -> return ann1
    ann3 <-
      case mpkg of
       RawPkgQual (StringLiteral src' v _) ->
         printStringAtMLocL ann2 limportDeclAnnPackage (sourceTextToString src' (show v))
       _ -> return ann2
    modname' <- markAnnotated modname

    ann4 <-
      case qualFlag of
        QualifiedPost  -- 'qualified' appears in postpositive position.
          -> printStringAtMLocL ann3 limportDeclAnnQualified "qualified"
        _ -> return ann3

    (importDeclAnnAs', mAs') <-
      case mAs of
        Nothing -> return (importDeclAnnAs an, Nothing)
        Just m0 -> do
          a <- printStringAtMLoc' (importDeclAnnAs an) "as"
          m'' <- markAnnotated m0
          return (a, Just m'')

    hiding' <-
      case hiding of
        Nothing -> return hiding
        Just (isHiding,lie) -> do
          lie' <- markAnnotated lie
          return (Just (isHiding, lie'))

    let (EpAnn anc' an' cs') = ann4
    let an2 = an' { importDeclAnnAs = importDeclAnnAs'
                  , importDeclAnnPragma = importDeclAnnPragma'
                  }

    return (ImportDecl (XImportDeclPass (EpAnn anc' an2 cs') msrc impl)
                     modname' mpkg src safeflag qualFlag mAs' hiding')


-- ---------------------------------------------------------------------

instance ExactPrint HsDocString where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (MultiLineDocString decorator (x :| xs)) = do
    printStringAdvance ("-- " ++ printDecorator decorator)
    pe <- getPriorEndD
    debugM $ "MultiLineDocString: (pe,x)=" ++ showAst (pe,x)
    x' <- markAnnotated x
    xs' <- markAnnotated (map dedentDocChunk xs)
    return (MultiLineDocString decorator (x' :| xs'))
  exact x = do
    -- TODO: can this happen?
    debugM $ "Not exact printing:" ++ showAst x
    return x


instance ExactPrint HsDocStringChunk where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _ = a
  exact chunk = do
    printStringAdvance ("--" ++ unpackHDSC chunk)
    return chunk


instance ExactPrint a => ExactPrint (WithHsDocIdentifiers a GhcPs) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _ = a
  exact (WithHsDocIdentifiers ds ids) = do
    ds' <- exact ds
    return (WithHsDocIdentifiers ds' ids)

-- ---------------------------------------------------------------------

instance ExactPrint (HsDecl GhcPs) where
  getAnnotationEntry (TyClD      _ _) = NoEntryVal
  getAnnotationEntry (InstD      _ _) = NoEntryVal
  getAnnotationEntry (DerivD     _ _) = NoEntryVal
  getAnnotationEntry (ValD       _ _) = NoEntryVal
  getAnnotationEntry (SigD       _ _) = NoEntryVal
  getAnnotationEntry (KindSigD   _ _) = NoEntryVal
  getAnnotationEntry (DefD       _ _) = NoEntryVal
  getAnnotationEntry (ForD       _ _) = NoEntryVal
  getAnnotationEntry (WarningD   _ _) = NoEntryVal
  getAnnotationEntry (AnnD       _ _) = NoEntryVal
  getAnnotationEntry (RuleD      _ _) = NoEntryVal
  getAnnotationEntry (SpliceD    _ _) = NoEntryVal
  getAnnotationEntry (DocD       _ _) = NoEntryVal
  getAnnotationEntry (RoleAnnotD _ _) = NoEntryVal

  -- We do not recurse, the generic traversal using this feature
  -- should do that for us.
  setAnnotationAnchor d _ _ _ = d

  exact (TyClD       x d) = TyClD       x <$> markAnnotated d
  exact (InstD       x d) = InstD       x <$> markAnnotated d
  exact (DerivD      x d) = DerivD      x <$> markAnnotated d
  exact (ValD        x d) = ValD        x <$> markAnnotated d
  exact (SigD        x d) = SigD        x <$> markAnnotated d
  exact (KindSigD    x d) = KindSigD    x <$> markAnnotated d
  exact (DefD        x d) = DefD        x <$> markAnnotated d
  exact (ForD        x d) = ForD        x <$> markAnnotated d
  exact (WarningD    x d) = WarningD    x <$> markAnnotated d
  exact (AnnD        x d) = AnnD        x <$> markAnnotated d
  exact (RuleD       x d) = RuleD       x <$> markAnnotated d
  exact (SpliceD     x d) = SpliceD     x <$> markAnnotated d
  exact (DocD        x d) = DocD        x <$> markAnnotated d
  exact (RoleAnnotD  x d) = RoleAnnotD  x <$> markAnnotated d

-- ---------------------------------------------------------------------

instance ExactPrint (InstDecl GhcPs) where
  getAnnotationEntry (ClsInstD     _ _) = NoEntryVal
  getAnnotationEntry (DataFamInstD _ _) = NoEntryVal
  getAnnotationEntry (TyFamInstD   _ _) = NoEntryVal

  setAnnotationAnchor d _ _ _ = d


  exact (ClsInstD     a  cid) = do
    cid' <- markAnnotated cid
    return (ClsInstD     a  cid')
  exact (DataFamInstD a decl) = do
    d' <- markAnnotated (DataFamInstDeclWithContext noAnn TopLevel decl)
    return (DataFamInstD a (dc_d d'))
  exact (TyFamInstD a eqn) = do
    eqn' <- markAnnotated eqn
    return (TyFamInstD a eqn')

-- ---------------------------------------------------------------------

data DataFamInstDeclWithContext
  = DataFamInstDeclWithContext
    { _dc_a :: [AddEpAnn]
    , _dc_f :: TopLevelFlag
    , dc_d :: DataFamInstDecl GhcPs
    }

instance ExactPrint DataFamInstDeclWithContext where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _ = a
  exact (DataFamInstDeclWithContext an c d) = do
    debugM $ "starting DataFamInstDeclWithContext:an=" ++ showAst an
    (an', d') <- exactDataFamInstDecl an c d
    return (DataFamInstDeclWithContext an' c d')

-- ---------------------------------------------------------------------

exactDataFamInstDecl :: (Monad m, Monoid w)
                     => [AddEpAnn] -> TopLevelFlag -> DataFamInstDecl GhcPs
                     -> EP w m ([AddEpAnn], DataFamInstDecl GhcPs)
exactDataFamInstDecl an top_lvl
  (DataFamInstDecl (FamEqn { feqn_ext    = an2
                           , feqn_tycon  = tycon
                           , feqn_bndrs  = bndrs
                           , feqn_pats   = pats
                           , feqn_fixity = fixity
                           , feqn_rhs    = defn })) = do
    (an', an2', tycon', bndrs', _,  _mc, defn') <- exactDataDefn an2 pp_hdr defn
                                                 -- See Note [an and an2 in exactDataFamInstDecl]
    return
      (an',
       DataFamInstDecl ( FamEqn { feqn_ext    = an2'
                                , feqn_tycon  = tycon'
                                , feqn_bndrs  = bndrs'
                                , feqn_pats   = pats
                                , feqn_fixity = fixity
                                , feqn_rhs    = defn' }))
                    `debug` ("exactDataFamInstDecl: defn' derivs:" ++ showAst (dd_derivs defn'))
  where
    pp_hdr :: (Monad m, Monoid w)
           => Maybe (LHsContext GhcPs)
           -> EP w m ( [AddEpAnn]
                     , LocatedN RdrName
                     , HsOuterTyVarBndrs () GhcPs
                     , HsFamEqnPats GhcPs
                     , Maybe (LHsContext GhcPs))
    pp_hdr mctxt = do
      an0 <- case top_lvl of
               TopLevel -> markEpAnnL an lidl AnnInstance -- TODO: maybe in toplevel
               NotTopLevel -> return an
      exactHsFamInstLHS an0 tycon bndrs pats fixity mctxt

{-
Note [an and an2 in exactDataFamInstDecl]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The exactDataFamInstDecl function is called to render a
DataFamInstDecl within its surrounding context. This context is
rendered via the 'pp_hdr' function, which uses the exact print
annotations from that context, named 'an'.  The EPAs used for
rendering the DataDefn are contained in the FamEqn, and are called
'an2'.

-}

-- ---------------------------------------------------------------------

instance ExactPrint (DerivDecl GhcPs) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (DerivDecl (mw, an) typ ms mov) = do
    an0 <- markEpAnnL an lidl AnnDeriving
    ms' <- mapM markAnnotated ms
    an1 <- markEpAnnL an0 lidl AnnInstance
    mw' <- mapM markAnnotated mw
    mov' <- mapM markAnnotated mov
    typ' <- markAnnotated typ
    return (DerivDecl (mw', an1) typ' ms' mov')

-- ---------------------------------------------------------------------

instance ExactPrint (ForeignDecl GhcPs) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (ForeignImport an n ty fimport) = do
    an0 <- markEpAnnL an lidl AnnForeign
    an1 <- markEpAnnL an0 lidl AnnImport

    fimport' <- markAnnotated fimport

    n' <- markAnnotated n
    an2 <- markEpAnnL an1 lidl AnnDcolon
    ty' <- markAnnotated ty
    return (ForeignImport an2 n' ty' fimport')

  exact (ForeignExport an n ty fexport) = do
    an0 <- markEpAnnL an lidl AnnForeign
    an1 <- markEpAnnL an0 lidl AnnExport
    fexport' <- markAnnotated fexport
    n' <- markAnnotated n
    an2 <- markEpAnnL an1 lidl AnnDcolon
    ty' <- markAnnotated ty
    return (ForeignExport an2 n' ty' fexport')

-- ---------------------------------------------------------------------

instance ExactPrint (ForeignImport GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ _ = a
  exact (CImport (L ls src) cconv safety@(L l _) mh imp) = do
    cconv' <- markAnnotated cconv
    safety' <- if notDodgyE l
        then markAnnotated safety
        else return safety
    ls' <- if notDodgyE ls
        then markExternalSourceTextE ls src ""
        else return ls
    return (CImport (L ls' src) cconv' safety' mh imp)

-- ---------------------------------------------------------------------

instance ExactPrint (ForeignExport GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ _ = a
  exact (CExport (L ls src) spec) = do
    debugM $ "CExport starting"
    spec' <- markAnnotated spec
    ls' <- if notDodgyE ls
        then markExternalSourceTextE ls src ""
        else return ls
    return (CExport (L ls' src) spec')

-- ---------------------------------------------------------------------

instance ExactPrint CExportSpec where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ _ = a
  exact (CExportStatic st lbl cconv) = do
    debugM $ "CExportStatic starting"
    cconv' <- markAnnotated cconv
    return (CExportStatic st lbl cconv')

-- ---------------------------------------------------------------------

instance ExactPrint Safety where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ _ = a
  exact = withPpr

-- ---------------------------------------------------------------------

instance ExactPrint CCallConv where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ _ = a
  exact = withPpr

-- ---------------------------------------------------------------------

instance ExactPrint (WarnDecls GhcPs) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (Warnings (an,src) warns) = do
    an0 <- markAnnOpen an src "{-# WARNING" -- Note: might be {-# DEPRECATED
    warns' <- markAnnotated warns
    an1 <- markEpAnnLMS'' an0 lidl AnnClose (Just "#-}")
    return (Warnings (an1,src) warns')

-- ---------------------------------------------------------------------

instance ExactPrint (WarnDecl GhcPs) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (Warning (ns_spec, an) lns  (WarningTxt mb_cat src ls )) = do
    mb_cat' <- markAnnotated mb_cat
    ns_spec' <- exactNsSpec ns_spec
    lns' <- markAnnotated lns
    an0 <- markEpAnnL an lidl AnnOpenS -- "["
    ls' <- markAnnotated ls
    an1 <- markEpAnnL an0 lidl AnnCloseS -- "]"
    return (Warning (ns_spec', an1) lns'  (WarningTxt mb_cat' src ls'))
    -- return (Warning an1 lns'  (WarningTxt mb_cat' src ls'))

  exact (Warning (ns_spec, an) lns (DeprecatedTxt src ls)) = do
    ns_spec' <- exactNsSpec ns_spec
    lns' <- markAnnotated lns
    an0 <- markEpAnnL an lidl AnnOpenS -- "["
    ls' <- markAnnotated ls
    an1 <- markEpAnnL an0 lidl AnnCloseS -- "]"
    return (Warning (ns_spec', an1) lns' (DeprecatedTxt src ls'))
    -- return (Warning an1 lns' (DeprecatedTxt src ls'))

exactNsSpec :: (Monad m, Monoid w) => NamespaceSpecifier -> EP w m NamespaceSpecifier
exactNsSpec NoNamespaceSpecifier = pure NoNamespaceSpecifier
exactNsSpec (TypeNamespaceSpecifier type_) = do
  type_' <- markEpToken type_
  pure (TypeNamespaceSpecifier type_')
exactNsSpec (DataNamespaceSpecifier data_) = do
  data_' <- markEpToken data_
  pure (DataNamespaceSpecifier data_')

-- ---------------------------------------------------------------------

instance ExactPrint StringLiteral where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (StringLiteral src fs mcomma) = do
    printSourceText src (show (unpackFS fs))
    mcomma' <- mapM (\r -> printStringAtNC r ",") mcomma
    return (StringLiteral src fs mcomma')

-- ---------------------------------------------------------------------

instance ExactPrint FastString where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  -- TODO: https://ghc.haskell.org/trac/ghc/ticket/10313 applies.
  -- exact fs = printStringAdvance (show (unpackFS fs))
  exact fs = printStringAdvance (unpackFS fs) >> return fs

-- ---------------------------------------------------------------------

instance ExactPrint (RuleDecls GhcPs) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _ = a
  exact (HsRules (an, src) rules) = do
    an0 <-
      case src of
        NoSourceText      -> markEpAnnLMS'' an lidl AnnOpen  (Just "{-# RULES")
        SourceText srcTxt -> markEpAnnLMS'' an lidl AnnOpen  (Just $ unpackFS srcTxt)
    rules' <- markAnnotated rules
    an1 <- markEpAnnLMS'' an0 lidl AnnClose (Just "#-}")
    return (HsRules (an1,src) rules')

-- ---------------------------------------------------------------------

instance ExactPrint (RuleDecl GhcPs) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (HsRule (an,nsrc) (L ln n) act mtybndrs termbndrs lhs rhs) = do
    (L ln' _) <- markAnnotated (L ln (nsrc, n))
    an0 <- markActivation an lra_rest act
    (an1, mtybndrs') <-
      case mtybndrs of
        Nothing -> return (an0, Nothing)
        Just bndrs -> do
          an1 <-  markLensMAA' an0 lra_tyanns_fst  -- AnnForall
          bndrs' <- mapM markAnnotated bndrs
          an2 <- markLensMAA' an1 lra_tyanns_snd  -- AnnDot
          return (an2, Just bndrs')

    an2 <- markLensMAA' an1 lra_tmanns_fst  -- AnnForall
    termbndrs' <- mapM markAnnotated termbndrs
    an3 <- markLensMAA' an2 lra_tmanns_snd  -- AnnDot

    lhs' <- markAnnotated lhs
    an4 <- markEpAnnL an3 lra_rest AnnEqual
    rhs' <- markAnnotated rhs
    return (HsRule (an4,nsrc) (L ln' n) act mtybndrs' termbndrs' lhs' rhs')

markActivation :: (Monad m, Monoid w)
  => a -> Lens a [AddEpAnn] -> Activation -> EP w m a
markActivation an l act = do
  case act of
    ActiveBefore src phase -> do
      an0 <- markEpAnnL an l AnnOpenS --  '['
      an1 <- markEpAnnL an0 l AnnTilde -- ~
      an2 <- markEpAnnLMS'' an1 l AnnVal (Just (toSourceTextWithSuffix src (show phase) ""))
      an3 <- markEpAnnL an2 l AnnCloseS -- ']'
      return an3
    ActiveAfter src phase -> do
      an0 <- markEpAnnL an l AnnOpenS --  '['
      an1 <- markEpAnnLMS'' an0 l AnnVal (Just (toSourceTextWithSuffix src (show phase) ""))
      an2 <- markEpAnnL an1 l AnnCloseS -- ']'
      return an2
    NeverActive -> do
      an0 <- markEpAnnL an l AnnOpenS --  '['
      an1 <- markEpAnnL an0 l AnnTilde -- ~
      an2 <- markEpAnnL an1 l AnnCloseS -- ']'
      return an2
    _ -> return an

-- ---------------------------------------------------------------------

instance ExactPrint (SpliceDecl GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (SpliceDecl x splice flag) = do
    splice' <- markAnnotated splice
    return (SpliceDecl x splice' flag)

-- ---------------------------------------------------------------------

instance ExactPrint (DocDecl GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  -- We print these as plain comments instead, do a NOP here.
  exact v = return v

-- ---------------------------------------------------------------------

instance ExactPrint (RoleAnnotDecl GhcPs) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (RoleAnnotDecl an ltycon roles) = do
    an0 <- markEpAnnL an lidl AnnType
    an1 <- markEpAnnL an0 lidl AnnRole
    ltycon' <- markAnnotated ltycon
    let markRole (L l (Just r)) = do
          (L _ r') <- markAnnotated (L l r)
          return (L l (Just r'))
        markRole (L l Nothing) = do
          printStringAtSs (locA l) "_"
          return (L l Nothing)
    roles' <- mapM markRole roles
    return (RoleAnnotDecl an1 ltycon' roles')

-- ---------------------------------------------------------------------

instance ExactPrint Role where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ _ = a
  exact = withPpr

-- ---------------------------------------------------------------------

instance ExactPrint (RuleBndr GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (RuleBndr x ln) = do
    ln' <- markAnnotated ln
    return (RuleBndr x ln')
  exact (RuleBndrSig an ln (HsPS x ty)) = do
    an0 <- markEpAnnL an lidl AnnOpenP -- "("
    ln' <- markAnnotated ln
    an1 <- markEpAnnL an0 lidl AnnDcolon
    ty' <- markAnnotated ty
    an2 <- markEpAnnL an1 lidl AnnCloseP -- ")"
    return (RuleBndrSig an2 ln' (HsPS x ty'))

-- ---------------------------------------------------------------------

instance (ExactPrint body) => ExactPrint (FamEqn GhcPs body) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor fe _ _ _s = fe
  exact (FamEqn { feqn_ext = an
                , feqn_tycon  = tycon
                , feqn_bndrs  = bndrs
                , feqn_pats   = pats
                , feqn_fixity = fixity
                , feqn_rhs    = rhs }) = do
    (an0, tycon', bndrs', pats', _) <- exactHsFamInstLHS an tycon bndrs pats fixity Nothing
    an1 <- markEpAnnL an0 lidl AnnEqual
    rhs' <- markAnnotated rhs
    return (FamEqn { feqn_ext = an1
                   , feqn_tycon  = tycon'
                   , feqn_bndrs  = bndrs'
                   , feqn_pats   = pats'
                   , feqn_fixity = fixity
                   , feqn_rhs    = rhs' })

-- ---------------------------------------------------------------------

exactHsFamInstLHS ::
      (Monad m, Monoid w)
   => [AddEpAnn]
   -> LocatedN RdrName
   -> HsOuterTyVarBndrs () GhcPs
   -> HsFamEqnPats GhcPs
   -> LexicalFixity
   -> Maybe (LHsContext GhcPs)
   -> EP w m ( [AddEpAnn]
             , LocatedN RdrName
             , HsOuterTyVarBndrs () GhcPs
             , HsFamEqnPats GhcPs, Maybe (LHsContext GhcPs))
exactHsFamInstLHS an thing bndrs typats fixity mb_ctxt = do
  an0 <- markEpAnnL an lidl AnnForall
  bndrs' <- markAnnotated bndrs
  an1 <- markEpAnnL an0 lidl AnnDot
  mb_ctxt' <- mapM markAnnotated mb_ctxt
  (an2, thing', typats') <- exact_pats an1 typats
  return (an2, thing', bndrs', typats', mb_ctxt')
  where
    exact_pats :: (Monad m, Monoid w)
      => [AddEpAnn] -> HsFamEqnPats GhcPs -> EP w m ([AddEpAnn], LocatedN RdrName, HsFamEqnPats GhcPs)
    exact_pats an' (patl:patr:pats)
      | Infix <- fixity
      = let exact_op_app = do
              an0 <- markEpAnnAllL' an' lidl AnnOpenP
              patl' <- markAnnotated patl
              thing' <- markAnnotated thing
              patr' <- markAnnotated patr
              an1 <- markEpAnnAllL' an0 lidl AnnCloseP
              return (an1, thing', [patl',patr'])
        in case pats of
             [] -> exact_op_app
             _  -> do
               (an0, thing', p) <- exact_op_app
               pats' <- mapM markAnnotated pats
               return (an0, thing', p++pats')

    exact_pats an' pats = do
      an0 <- markEpAnnAllL' an' lidl AnnOpenP
      thing' <- markAnnotated thing
      pats' <- markAnnotated pats
      an1 <- markEpAnnAllL' an0 lidl AnnCloseP
      return (an1, thing', pats')

-- ---------------------------------------------------------------------

instance (ExactPrint tm, ExactPrint ty, Outputable tm, Outputable ty)
     =>  ExactPrint (HsArg GhcPs tm ty) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact a@(HsValArg _ tm)   = markAnnotated tm >> return a
  exact a@(HsTypeArg at ty) = markEpToken at >> markAnnotated ty >> return a
  exact x@(HsArgPar _sp)    = withPpr x -- Does not appear in original source

-- ---------------------------------------------------------------------

instance ExactPrint (ClsInstDecl GhcPs) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (ClsInstDecl { cid_ext = (mbWarn, an, sortKey)
                     , cid_poly_ty = inst_ty, cid_binds = binds
                     , cid_sigs = sigs, cid_tyfam_insts = ats
                     , cid_overlap_mode = mbOverlap
                     , cid_datafam_insts = adts })
      = do
          (mbWarn', an0, mbOverlap', inst_ty') <- top_matter
          an1 <- markEpAnnL an0 lidl AnnOpenC
          an2 <- markEpAnnAllL' an1 lid AnnSemi
          ds <- withSortKey sortKey
                               [(ClsAtdTag, prepareListAnnotationA ats),
                                (ClsAtdTag, prepareListAnnotationF an adts),
                                (ClsMethodTag, prepareListAnnotationA (bagToList binds)),
                                (ClsSigTag, prepareListAnnotationA sigs)
                               ]
          an3 <- markEpAnnL an2 lidl AnnCloseC -- '}'
          let
            ats'   = undynamic ds
            adts'  = undynamic ds
            binds' = listToBag $ undynamic ds
            sigs'  = undynamic ds
          return (ClsInstDecl { cid_ext = (mbWarn', an3, sortKey)
                              , cid_poly_ty = inst_ty', cid_binds = binds'
                              , cid_sigs = sigs', cid_tyfam_insts = ats'
                              , cid_overlap_mode = mbOverlap'
                              , cid_datafam_insts = adts' })

      where
        top_matter = do
          an0 <- markEpAnnL an lidl AnnInstance
          mw <- mapM markAnnotated mbWarn
          mo <- mapM markAnnotated mbOverlap
          it <- markAnnotated inst_ty
          an1 <- markEpAnnL an0 lidl AnnWhere -- Optional
          return (mw, an1, mo,it)

-- ---------------------------------------------------------------------

instance ExactPrint (TyFamInstDecl GhcPs) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact d@(TyFamInstDecl { tfid_xtn = an, tfid_eqn = eqn }) = do
    an0 <- markEpAnnL an lidl AnnType
    an1 <- markEpAnnL an0 lidl AnnInstance
    eqn' <- markAnnotated eqn
    return (d { tfid_xtn = an1, tfid_eqn = eqn' })

-- ---------------------------------------------------------------------

instance ExactPrint (LocatedP OverlapMode) where
  getAnnotationEntry = entryFromLocatedA
  setAnnotationAnchor = setAnchorAn

  -- NOTE: NoOverlap is only used in the typechecker
  exact (L an (NoOverlap src)) = do
    an0 <- markAnnOpenP an src "{-# NO_OVERLAP"
    an1 <- markAnnCloseP an0
    return (L an1 (NoOverlap src))

  exact (L an (Overlappable src)) = do
    an0 <- markAnnOpenP an src "{-# OVERLAPPABLE"
    an1 <- markAnnCloseP an0
    return (L an1 (Overlappable src))

  exact (L an (Overlapping src)) = do
    an0 <- markAnnOpenP an src "{-# OVERLAPPING"
    an1 <- markAnnCloseP an0
    return (L an1 (Overlapping src))

  exact (L an (Overlaps src)) = do
    an0 <- markAnnOpenP an src "{-# OVERLAPS"
    an1 <- markAnnCloseP an0
    return (L an1 (Overlaps src))

  exact (L an (Incoherent src)) = do
    an0 <- markAnnOpenP an src "{-# INCOHERENT"
    an1 <- markAnnCloseP an0
    return (L an1 (Incoherent src))

  exact (L an (NonCanonical src)) = do
    an0 <- markAnnOpenP an src "{-# INCOHERENT"
    an1 <- markAnnCloseP an0
    return (L an1 (Incoherent src))

-- ---------------------------------------------------------------------

instance ExactPrint (HsBind GhcPs) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (FunBind x fid matches) = do
    matches' <- markAnnotated matches
    let
      fun_id' = case unLoc (mg_alts matches') of
        [] -> fid
        (L _ m:_) -> case m_ctxt m of
          FunRhs f _ _ -> f
          _ -> fid
    return (FunBind x fun_id' matches')

  exact (PatBind x pat q grhss) = do
    pat' <- markAnnotated pat
    grhss' <- markAnnotated grhss
    return (PatBind x pat' q grhss')
  exact (PatSynBind x bind) = do
    bind' <- markAnnotated bind
    return (PatSynBind x bind')

  exact x = error $ "HsBind: exact for " ++ showAst x

-- ---------------------------------------------------------------------

instance ExactPrint (PatSynBind GhcPs GhcPs) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (PSB{ psb_ext = an
            , psb_id = psyn, psb_args = details
            , psb_def = pat
            , psb_dir = dir }) = do
    an0 <- markEpAnnL an lidl AnnPattern
    (an1, psyn', details') <-
      case details of
        InfixCon v1 v2 -> do
          v1' <- markAnnotated v1
          psyn' <- markAnnotated psyn
          v2' <- markAnnotated v2
          return (an0, psyn',InfixCon v1' v2')
        PrefixCon tvs vs -> do
          psyn' <- markAnnotated psyn
          tvs' <- markAnnotated tvs
          vs' <- markAnnotated vs
          return (an0, psyn', PrefixCon tvs' vs')
        RecCon vs -> do
          psyn' <- markAnnotated psyn
          an1 <- markEpAnnL an0 lidl AnnOpenC  -- '{'
          vs' <- markAnnotated vs
          an2 <- markEpAnnL an1 lidl AnnCloseC -- '}'
          return (an2, psyn', RecCon vs')

    (an2, pat', dir') <-
      case dir of
        Unidirectional           -> do
          an2 <- markEpAnnL an1 lidl AnnLarrow
          pat' <- markAnnotated pat
          return (an2, pat', dir)
        ImplicitBidirectional    -> do
          an2 <- markEpAnnL an1 lidl AnnEqual
          pat' <- markAnnotated pat
          return (an2, pat', dir)
        ExplicitBidirectional mg -> do
          an2 <- markEpAnnL an1 lidl AnnLarrow
          pat' <- markAnnotated pat
          an3 <- markEpAnnL an2 lidl  AnnWhere
          mg' <- markAnnotated mg
          return (an3, pat', ExplicitBidirectional mg')

    return (PSB{ psb_ext = an2
               , psb_id = psyn', psb_args = details'
               , psb_def = pat'
               , psb_dir = dir' })


-- ---------------------------------------------------------------------

instance ExactPrint (RecordPatSynField GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ _ = a
  exact r@(RecordPatSynField { recordPatSynField = v }) = markAnnotated v
        >> return r

-- ---------------------------------------------------------------------

instance ExactPrint (Match GhcPs (LocatedA (HsCmd GhcPs))) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (Match an mctxt pats grhss) =
    exactMatch (Match an mctxt pats grhss)

-- -------------------------------------

instance ExactPrint (Match GhcPs (LocatedA (HsExpr GhcPs))) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (Match an mctxt pats grhss) =
    exactMatch (Match an mctxt pats grhss)

-- ---------------------------------------------------------------------

exactMatch :: (Monad m, Monoid w, ExactPrint (GRHSs GhcPs body))
           => (Match GhcPs body) -> EP w m (Match GhcPs body)
exactMatch (Match an mctxt pats grhss) = do

  debugM $ "exact Match entered"

  (an0, mctxt', pats') <-
    case mctxt of
      FunRhs fun fixity strictness -> do
        debugM $ "exact Match FunRhs:" ++ showPprUnsafe fun
        an0' <-
          case strictness of
            SrcStrict -> markEpAnnL an lidl AnnBang
            _ -> pure an
        case fixity of
          Prefix -> do
            an' <- annotationsToComments an0' lidl [AnnOpenP,AnnCloseP]
            fun' <- markAnnotated fun
            pats' <- markAnnotated pats
            return (an', FunRhs fun' fixity strictness, pats')
          Infix ->
            case pats of
              (p1:p2:rest)
                | null rest -> do
                    p1'  <- markAnnotated p1
                    fun' <- markAnnotated fun
                    p2'  <- markAnnotated p2
                    return (an0', FunRhs fun' fixity strictness, [p1',p2'])
                | otherwise -> do
                    an0  <- markEpAnnL an0' lidl AnnOpenP
                    p1'  <- markAnnotated p1
                    fun' <- markAnnotated fun
                    p2'  <- markAnnotated p2
                    an1  <- markEpAnnL an0 lidl AnnCloseP
                    rest' <- mapM markAnnotated rest
                    return (an1, FunRhs fun' fixity strictness, p1':p2':rest')
              _ -> panic "FunRhs"

      -- ToDo: why is LamSingle treated differently?
      LamAlt LamSingle -> do
        an0' <- markEpAnnL an lidl AnnLam
        pats' <- markAnnotated pats
        return (an0', LamAlt LamSingle, pats')
      LamAlt v -> do
        pats' <- markAnnotated pats
        return (an, LamAlt v, pats')

      CaseAlt -> do
        pats' <- markAnnotated pats
        return (an, CaseAlt, pats')
      _ -> do
        mctxt' <- withPpr mctxt
        return (an, mctxt', pats)

  grhss' <- markAnnotated grhss

  return (Match an0 mctxt' pats' grhss')

-- ---------------------------------------------------------------------

instance ExactPrint (GRHSs GhcPs (LocatedA (HsExpr GhcPs))) where
  getAnnotationEntry (GRHSs _ _ _) = NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (GRHSs cs grhss binds) = do
    addCommentsA $ priorComments cs
    addCommentsA $ getFollowingComments cs
    grhss' <- markAnnotated grhss
    binds' <- markAnnotated binds
    -- The comments will be added back as they are printed
    return (GRHSs emptyComments grhss' binds')


instance ExactPrint (GRHSs GhcPs (LocatedA (HsCmd GhcPs))) where
  getAnnotationEntry (GRHSs _ _ _) = NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (GRHSs cs grhss binds) = do
    addCommentsA $ priorComments cs
    addCommentsA $ getFollowingComments cs
    grhss' <- markAnnotated grhss
    binds' <- markAnnotated binds
    -- The comments will be added back as they are printed
    return (GRHSs emptyComments grhss' binds')

-- ---------------------------------------------------------------------

instance ExactPrint (HsLocalBinds GhcPs) where
  getAnnotationEntry (HsValBinds an _) = fromAnn an
  getAnnotationEntry (HsIPBinds{}) = NoEntryVal
  getAnnotationEntry (EmptyLocalBinds{}) = NoEntryVal

  setAnnotationAnchor (HsValBinds an a) anc ts cs = HsValBinds (setAnchorEpaL an anc ts cs) a
  setAnnotationAnchor a _ _ _ = a

  exact (HsValBinds an valbinds) = do
    debugM $ "exact HsValBinds: an=" ++ showAst an
    an0 <- markEpAnnL' an lal_rest AnnWhere

    case al_anchor $ anns an of
      Just anc -> do
        when (not $ isEmptyValBinds valbinds) $ setExtraDP (Just anc)
      _ -> return ()

    (an1, valbinds') <- markAnnList an0 $ markAnnotatedWithLayout valbinds
    debugM $ "exact HsValBinds: an1=" ++ showAst an1
    return (HsValBinds an1 valbinds')

  exact (HsIPBinds an bs) = do
    (as, ipb) <- markAnnList an (markEpAnnL' an lal_rest AnnWhere
                           >> markAnnotated bs
                           >>= \bs' -> return (HsIPBinds an bs'::HsLocalBinds GhcPs))
    case ipb of
      HsIPBinds _ bs' -> return (HsIPBinds as bs'::HsLocalBinds GhcPs)
      _ -> error "should not happen HsIPBinds"
  exact b@(EmptyLocalBinds _) = return b


-- ---------------------------------------------------------------------
instance ExactPrint (HsValBindsLR GhcPs GhcPs) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (ValBinds sortKey binds sigs) = do
    decls <- setLayoutBoth $ mapM markAnnotated $ hsDeclsValBinds (ValBinds sortKey binds sigs)
    let
      binds' = listToBag $ concatMap decl2Bind decls
      sigs'  =             concatMap decl2Sig decls
    return (ValBinds sortKey binds' sigs')
  exact (XValBindsLR _) = panic "XValBindsLR"

undynamic :: Typeable a => [Dynamic] -> [a]
undynamic ds = mapMaybe fromDynamic ds

-- ---------------------------------------------------------------------

instance ExactPrint (HsIPBinds GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact b@(IPBinds _ binds) = setLayoutBoth $ markAnnotated binds >> return b

-- ---------------------------------------------------------------------

instance ExactPrint (IPBind GhcPs) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (IPBind an lr rhs) = do
    lr' <- markAnnotated lr
    an0 <- markEpAnnL an lidl AnnEqual
    rhs' <- markAnnotated rhs
    return (IPBind an0 lr' rhs')


-- ---------------------------------------------------------------------

instance ExactPrint HsIPName where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact i@(HsIPName fs) = printStringAdvance ("?" ++ (unpackFS fs)) >> return i

-- ---------------------------------------------------------------------
-- Managing lists which have been separated, e.g. Sigs and Binds

prepareListAnnotationF :: (Monad m, Monoid w) =>
  [AddEpAnn] -> [LDataFamInstDecl GhcPs] -> [(RealSrcSpan,EP w m Dynamic)]
prepareListAnnotationF an ls = map (\b -> (realSrcSpan $ getLocA b, go b)) ls
  where
    go (L l a) = do
      d' <- markAnnotated (DataFamInstDeclWithContext an NotTopLevel a)
      return (toDyn (L l (dc_d d')))

prepareListAnnotationA :: (Monad m, Monoid w, ExactPrint (LocatedAn an a))
  => [LocatedAn an a] -> [(RealSrcSpan,EP w m Dynamic)]
prepareListAnnotationA ls = map (\b -> (realSrcSpan $ getLocA b,go b)) ls
  where
    go b = do
      b' <- markAnnotated b
      return (toDyn b')

withSortKey :: (Monad m, Monoid w)
  => AnnSortKey DeclTag -> [(DeclTag, [(RealSrcSpan, EP w m Dynamic)])] -> EP w m [Dynamic]
withSortKey annSortKey xs = do
  debugM $ "withSortKey:annSortKey=" ++ showAst annSortKey
  let ordered = case annSortKey of
                  NoAnnSortKey -> sortBy orderByFst $ concatMap snd xs
                  AnnSortKey _keys -> orderedDecls annSortKey (Map.fromList xs)
  mapM snd ordered
orderByFst :: Ord a => (a, b1) -> (a, b2) -> Ordering
orderByFst (a,_) (b,_) = compare a b

-- ---------------------------------------------------------------------

instance ExactPrint (Sig GhcPs) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (TypeSig an vars ty)  = do
    (an', vars', ty') <- exactVarSig an vars ty
    return (TypeSig an' vars' ty')

  exact (PatSynSig an lns typ) = do
    an0 <- markEpAnnL an lasRest AnnPattern
    lns' <- markAnnotated lns
    an1 <- markLensAA' an0 lasDcolon
    typ' <- markAnnotated typ
    return (PatSynSig an1 lns' typ')

  exact (ClassOpSig an is_deflt vars ty)
    | is_deflt  = do
        an0 <- markEpAnnL an lasRest AnnDefault
        (an1, vars',ty') <- exactVarSig an0 vars ty
        return (ClassOpSig an1 is_deflt vars' ty')
    | otherwise = do
        (an0, vars',ty') <- exactVarSig an vars ty
        return (ClassOpSig an0 is_deflt vars' ty')

  exact (FixSig an (FixitySig x names (Fixity src v fdir))) = do
    let fixstr = case fdir of
         InfixL -> "infixl"
         InfixR -> "infixr"
         InfixN -> "infix"
    an0 <- markEpAnnLMS'' an  lidl AnnInfix (Just fixstr)
    an1 <- markEpAnnLMS'' an0 lidl AnnVal (Just (sourceTextToString src (show v)))
    names' <- markAnnotated names
    return (FixSig an1 (FixitySig x names' (Fixity src v fdir)))

  exact (InlineSig an ln inl) = do
    an0 <- markAnnOpen an (inl_src inl) "{-# INLINE"
    an1 <- markActivation an0 id (inl_act inl)
    ln' <- markAnnotated ln
    an2 <- markEpAnnLMS'' an1 lidl AnnClose (Just "#-}")
    return (InlineSig an2 ln' inl)

  exact (SpecSig an ln typs inl) = do
    an0 <- markAnnOpen an (inl_src inl) "{-# SPECIALISE" -- Note: may be {-# SPECIALISE_INLINE
    an1 <- markActivation an0 lidl (inl_act inl)
    ln' <- markAnnotated ln
    an2 <- markEpAnnL an1 lidl AnnDcolon
    typs' <- markAnnotated typs
    an3 <- markEpAnnLMS'' an2 lidl AnnClose (Just "#-}")
    return (SpecSig an3 ln' typs' inl)

  exact (SpecInstSig (an,src) typ) = do
    an0 <- markAnnOpen an src "{-# SPECIALISE"
    an1 <- markEpAnnL an0 lidl AnnInstance
    typ' <- markAnnotated typ
    an2 <- markEpAnnLMS'' an1 lidl AnnClose (Just "#-}")
    return (SpecInstSig (an2,src) typ')

  exact (MinimalSig (an,src) formula) = do
    an0 <- markAnnOpen an src "{-# MINIMAL"
    formula' <- markAnnotated formula
    an1 <- markEpAnnLMS'' an0 lidl AnnClose (Just "#-}")
    return (MinimalSig (an1,src) formula')

  exact (SCCFunSig (an,src) ln ml) = do
    an0 <- markAnnOpen an src "{-# SCC"
    ln' <- markAnnotated ln
    ml' <- markAnnotated ml
    an1 <- markEpAnnLMS'' an0 lidl AnnClose (Just "#-}")
    return (SCCFunSig (an1,src) ln' ml')

  exact (CompleteMatchSig (an,src) cs mty) = do
    an0 <- markAnnOpen an src "{-# COMPLETE"
    cs' <- markAnnotated cs
    (an1, mty') <-
      case mty of
        Nothing -> return (an0, mty)
        Just ty -> do
          an1 <- markEpAnnL an0 lidl AnnDcolon
          ty' <- markAnnotated ty
          return (an1, Just ty')
    an2 <- markEpAnnLMS'' an1 lidl AnnClose (Just "#-}")
    return (CompleteMatchSig (an2,src) cs' mty')

-- ---------------------------------------------------------------------

exactVarSig :: (Monad m, Monoid w, ExactPrint a)
  => AnnSig -> [LocatedN RdrName] -> a -> EP w m (AnnSig, [LocatedN RdrName], a)
exactVarSig an vars ty = do
  vars' <- mapM markAnnotated vars
  an0 <- markLensAA' an lasDcolon
  ty' <- markAnnotated ty
  return (an0, vars', ty')

-- ---------------------------------------------------------------------

instance ExactPrint (StandaloneKindSig GhcPs) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (StandaloneKindSig an vars sig) = do
    an0 <- markEpAnnL an lidl AnnType
    vars' <- markAnnotated vars
    an1 <- markEpAnnL an0 lidl AnnDcolon
    sig' <- markAnnotated sig
    return (StandaloneKindSig an1 vars' sig')

-- ---------------------------------------------------------------------

instance ExactPrint (DefaultDecl GhcPs) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (DefaultDecl an tys) = do
    an0 <- markEpAnnL an lidl AnnDefault
    an1 <- markEpAnnL an0 lidl AnnOpenP
    tys' <- markAnnotated tys
    an2 <- markEpAnnL an1 lidl AnnCloseP
    return (DefaultDecl an2 tys')

-- ---------------------------------------------------------------------

instance ExactPrint (AnnDecl GhcPs) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (HsAnnotation (an, src) prov e) = do
    an0 <- markAnnOpenP' an src "{-# ANN"
    (an1, prov') <-
      case prov of
        (ValueAnnProvenance n) -> do
          n' <- markAnnotated n
          return (an0, ValueAnnProvenance n')
        (TypeAnnProvenance n) -> do
          an1 <- markEpAnnL an0 lapr_rest AnnType
          n' <- markAnnotated n
          return (an1, TypeAnnProvenance n')
        ModuleAnnProvenance -> do
          an1 <- markEpAnnL an lapr_rest AnnModule
          return (an1, prov)

    e' <- markAnnotated e
    an2 <- markAnnCloseP' an1
    return (HsAnnotation (an2,src) prov' e')

-- ---------------------------------------------------------------------

instance ExactPrint (BF.BooleanFormula (LocatedN RdrName)) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (BF.Var x)  = do
    x' <- markAnnotated x
    return (BF.Var x')
  exact (BF.Or ls)  = do
    ls' <- markAnnotated ls
    return (BF.Or ls')
  exact (BF.And ls) = do
    ls' <- markAnnotated ls
    return (BF.And ls')
  exact (BF.Parens x)  = do
    x' <- markAnnotated x
    return (BF.Parens x')

-- ---------------------------------------------------------------------

instance (ExactPrint body) => ExactPrint (HsWildCardBndrs GhcPs body) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ _= a
  exact (HsWC x ty) = do
    ty' <- markAnnotated ty
    return (HsWC x ty')

-- ---------------------------------------------------------------------

instance ExactPrint (GRHS GhcPs (LocatedA (HsExpr GhcPs))) where
  getAnnotationEntry (GRHS an _ _) = fromAnn an
  setAnnotationAnchor (GRHS an a b) anc ts cs = GRHS (setAnchorEpa an anc ts cs) a b

  exact (GRHS an guards expr) = do
    an0 <- if null guards
             then return an
             else markLensKwM an lga_vbar AnnVbar
    guards' <- markAnnotated guards
    an1 <- markLensAA an0 lga_sep -- Mark the matchSeparator for these GRHSs
    expr' <- markAnnotated expr
    return (GRHS an1 guards' expr')

instance ExactPrint (GRHS GhcPs (LocatedA (HsCmd GhcPs))) where
  getAnnotationEntry (GRHS ann _ _) = fromAnn ann
  setAnnotationAnchor (GRHS an a b) anc ts cs = GRHS (setAnchorEpa an anc ts cs) a b

  exact (GRHS an guards expr) = do
    an0 <- markLensKwM an lga_vbar AnnVbar
    guards' <- markAnnotated guards
    an1 <- markLensAA an0 lga_sep -- Mark the matchSeparator for these GRHSs
    expr' <- markAnnotated expr
    return (GRHS an1 guards' expr')

-- ---------------------------------------------------------------------

instance ExactPrint (HsExpr GhcPs) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _s = a

  exact (HsVar x n) = do
    -- The parser inserts a placeholder value for a record pun rhs. This must be
    -- filtered.
    let pun_RDR = "pun-right-hand-side"
    n' <- if (showPprUnsafe n /= pun_RDR)
      then markAnnotated n
      else return n
    return (HsVar x n')
  exact x@(HsUnboundVar an _) = do
    case an of
      Just (EpAnnUnboundVar (ob,cb) l) -> do
        printStringAtAA ob "`" >> return ()
        printStringAtAA l  "_" >> return ()
        printStringAtAA cb "`" >> return ()
        return x
      _ -> do
        printStringAtLsDelta (SameLine 0) "_"
        return x
  exact x@(HsOverLabel _ src l) = do
    printStringAtLsDelta (SameLine 0) "#"
    case src of
      NoSourceText   -> printStringAtLsDelta (SameLine 0) (unpackFS l)
      SourceText txt -> printStringAtLsDelta (SameLine 0) (unpackFS txt)
    return x

  exact x@(HsIPVar _ (HsIPName n))
    = printStringAdvance ("?" ++ unpackFS n) >> return x

  exact x@(HsOverLit _an ol) = do
    let str = case ol_val ol of
                HsIntegral   (IL src _ _) -> src
                HsFractional (FL { fl_text = src }) -> src
                HsIsString src _          -> src
    case str of
      SourceText s -> printStringAdvance (unpackFS s) >> return ()
      NoSourceText -> withPpr x >> return ()
    return x

  exact (HsLit an lit) = do
    lit' <- withPpr lit
    return (HsLit an lit')

  exact (HsLam an lam_variant mg) = do
    an0 <- mark an AnnLam
    an1 <- case lam_variant of
             LamSingle -> return an0
             LamCase -> mark an0 AnnCase
             LamCases -> mark an0 AnnCases
    mg' <- markAnnotated mg
    return (HsLam an1 lam_variant mg')

  exact (HsApp an e1 e2) = do
    p <- getPosP
    debugM $ "HsApp entered. p=" ++ show p
    e1' <- markAnnotated e1
    e2' <- markAnnotated e2
    return (HsApp an e1' e2')
  exact (HsAppType at fun arg) = do
    fun' <- markAnnotated fun
    at' <- markEpToken at
    arg' <- markAnnotated arg
    return (HsAppType at' fun' arg')
  exact (OpApp an e1 e2 e3) = do
    e1' <- markAnnotated e1
    e2' <- markAnnotated e2
    e3' <- markAnnotated e3
    return (OpApp an e1' e2' e3')

  exact (NegApp an e s) = do
    an0 <- markEpAnnL an lidl AnnMinus
    e' <- markAnnotated e
    return (NegApp an0 e' s)

  exact (HsPar (lpar, rpar) e) = do
    lpar' <- markEpToken lpar
    e' <- markAnnotated e
    debugM $ "HsPar closing paren"
    rpar' <- markEpToken rpar
    debugM $ "HsPar done"
    return (HsPar (lpar', rpar') e')

  exact (SectionL an expr op) = do
    expr' <- markAnnotated expr
    op' <- markAnnotated op
    return (SectionL an expr' op')

  exact (SectionR an op expr) = do
    op' <- markAnnotated op
    expr' <- markAnnotated expr
    return (SectionR an op' expr')

  exact (ExplicitTuple an args b) = do
    an0 <- if b == Boxed then markEpAnnL an lidl AnnOpenP
                         else markEpAnnL an lidl AnnOpenPH

    args' <- mapM markAnnotated args

    an1 <- if b == Boxed then markEpAnnL an0 lidl AnnCloseP
                         else markEpAnnL an0 lidl AnnClosePH
    debugM $ "ExplicitTuple done"
    return (ExplicitTuple an1 args' b)

  exact (ExplicitSum an alt arity expr) = do
    an0 <- markLensKw an laesOpen AnnOpenPH
    an1 <- markAnnKwAllL an0 laesBarsBefore AnnVbar
    expr' <- markAnnotated expr
    an2 <- markAnnKwAllL an1 laesBarsAfter AnnVbar
    an3 <- markLensKw an2 laesClose AnnClosePH
    return (ExplicitSum an3 alt arity expr')

  exact (HsCase an e alts) = do
    an0 <- markLensKw an lhsCaseAnnCase AnnCase
    e' <- markAnnotated e
    an1 <- markLensKw an0 lhsCaseAnnOf AnnOf
    an2 <- markEpAnnL an1 lhsCaseAnnsRest AnnOpenC
    an3 <- markEpAnnAllL' an2 lhsCaseAnnsRest AnnSemi
    alts' <- setLayoutBoth $ markAnnotated alts
    an4 <- markEpAnnL an3 lhsCaseAnnsRest AnnCloseC
    return (HsCase an4 e' alts')

  exact (HsIf an e1 e2 e3) = do
    an0 <- markLensKw an laiIf AnnIf
    e1' <- markAnnotated e1
    an1 <- markLensKwM' an0 laiThenSemi AnnSemi
    an2 <- markLensKw an1 laiThen AnnThen
    e2' <- markAnnotated e2
    an3 <- markLensKwM' an2 laiElseSemi AnnSemi
    an4 <- markLensKw an3 laiElse AnnElse
    e3' <- markAnnotated e3
    return (HsIf an4 e1' e2' e3')

  exact (HsMultiIf an mg) = do
    an0 <- markEpAnnL an lidl AnnIf
    an1 <- markEpAnnL an0 lidl AnnOpenC -- optional
    mg' <- markAnnotated mg
    an2 <- markEpAnnL an1 lidl AnnCloseC -- optional
    return (HsMultiIf an2 mg')

  exact (HsLet (tkLet, tkIn) binds e) = do
    setLayoutBoth $ do -- Make sure the 'in' gets indented too
      tkLet' <- markEpToken tkLet
      binds' <- setLayoutBoth $ markAnnotated binds
      tkIn' <- markEpToken tkIn
      e' <- markAnnotated e
      return (HsLet (tkLet',tkIn') binds' e')

  exact (HsDo an do_or_list_comp stmts) = do
    debugM $ "HsDo"
    (an',stmts') <- markAnnListA' an $ \a -> exactDo a do_or_list_comp stmts
    return (HsDo an' do_or_list_comp stmts')

  exact (ExplicitList an es) = do
    debugM $ "ExplicitList start"
    an0 <- markLensMAA' an lal_open
    es' <- markAnnotated es
    an1 <- markLensMAA' an0 lal_close
    debugM $ "ExplicitList end"
    return (ExplicitList an1 es')
  exact (RecordCon an con_id binds) = do
    con_id' <- markAnnotated con_id
    an0 <- markEpAnnL an lidl AnnOpenC
    binds' <- markAnnotated binds
    an1 <- markEpAnnL an0 lidl AnnCloseC
    return (RecordCon an1 con_id' binds')
  exact (RecordUpd an expr fields) = do
    expr' <- markAnnotated expr
    an0 <- markEpAnnL an lidl AnnOpenC
    fields' <- markAnnotated fields
    an1 <- markEpAnnL an0 lidl AnnCloseC
    return (RecordUpd an1 expr' fields')
  exact (HsGetField an expr field) = do
    expr' <- markAnnotated expr
    field' <- markAnnotated field
    return (HsGetField an expr' field')
  exact (HsProjection an flds) = do
    an0 <- markLensKw an lapOpen AnnOpenP
    flds' <- mapM markAnnotated flds
    an1 <- markLensKw an0 lapClose AnnCloseP
    return (HsProjection an1 flds')
  exact (ExprWithTySig an expr sig) = do
    expr' <- markAnnotated expr
    an0 <- markEpAnnL an lidl AnnDcolon
    sig' <- markAnnotated sig
    return (ExprWithTySig an0 expr' sig')
  exact (ArithSeq an s seqInfo) = do
    an0 <- markEpAnnL an lidl AnnOpenS -- '['
    (an1, seqInfo') <-
      case seqInfo of
        From e -> do
          e' <- markAnnotated e
          an' <- markEpAnnL an0 lidl AnnDotdot
          return (an', From e')
        FromTo e1 e2 -> do
          e1' <- markAnnotated e1
          an' <- markEpAnnL an0 lidl AnnDotdot
          e2' <- markAnnotated e2
          return (an', FromTo e1' e2')
        FromThen e1 e2 -> do
          e1' <- markAnnotated e1
          an' <- markEpAnnL an0 lidl AnnComma
          e2' <- markAnnotated e2
          an'' <- markEpAnnL an' lidl AnnDotdot
          return (an'', FromThen e1' e2')
        FromThenTo e1 e2 e3 -> do
          e1' <- markAnnotated e1
          an' <- markEpAnnL an0 lidl AnnComma
          e2' <- markAnnotated e2
          an'' <- markEpAnnL an' lidl AnnDotdot
          e3' <- markAnnotated e3
          return (an'', FromThenTo e1' e2' e3')
    an2 <- markEpAnnL an1 lidl AnnCloseS -- ']'
    return (ArithSeq an2 s seqInfo')


  exact (HsTypedBracket an e) = do
    an0 <- markEpAnnLMS'' an lidl AnnOpen (Just "[||")
    an1 <- markEpAnnLMS'' an0 lidl AnnOpenE (Just "[e||")
    e' <- markAnnotated e
    an2 <- markEpAnnLMS'' an1 lidl AnnClose (Just "||]")
    return (HsTypedBracket an2 e')

  exact (HsUntypedBracket an (ExpBr a e)) = do
    an0 <- markEpAnnL an  lidl AnnOpenEQ -- "[|"
    an1 <- markEpAnnL an0 lidl AnnOpenE  -- "[e|" -- optional
    e' <- markAnnotated e
    an2 <- markEpAnnL an1 lidl AnnCloseQ -- "|]"
    return (HsUntypedBracket an2 (ExpBr a e'))

  exact (HsUntypedBracket an (PatBr a e)) = do
    an0 <- markEpAnnLMS'' an lidl AnnOpen (Just "[p|")
    e' <- markAnnotated e
    an1 <- markEpAnnL an0 lidl AnnCloseQ -- "|]"
    return (HsUntypedBracket an1 (PatBr a e'))

  exact (HsUntypedBracket an (DecBrL a e)) = do
    an0 <- markEpAnnLMS'' an lidl AnnOpen (Just "[d|")
    an1 <- markEpAnnL an0 lidl AnnOpenC
    e' <- markAnnotated e
    an2 <- markEpAnnL an1 lidl AnnCloseC
    an3 <- markEpAnnL an2 lidl AnnCloseQ -- "|]"
    return (HsUntypedBracket an3 (DecBrL a e'))

  exact (HsUntypedBracket an (TypBr a e)) = do
    an0 <- markEpAnnLMS'' an lidl AnnOpen (Just "[t|")
    e' <- markAnnotated e
    an1 <- markEpAnnL an0 lidl AnnCloseQ -- "|]"
    return (HsUntypedBracket an1 (TypBr a e'))

  exact (HsUntypedBracket an (VarBr a b e)) = do
    (an0, e') <- if b
      then do
        an' <- markEpAnnL an lidl AnnSimpleQuote
        e' <- markAnnotated e
        return (an', e')
      else do
        an' <- markEpAnnL an lidl AnnThTyQuote
        e' <- markAnnotated e
        return (an', e')
    return (HsUntypedBracket an0 (VarBr a b e'))

  exact (HsTypedSplice an s)   = do
    an0 <- markEpAnnL an lidl AnnDollarDollar
    s' <- exact s
    return (HsTypedSplice an0 s')

  exact (HsUntypedSplice an s) = do
    s' <- exact s
    return (HsUntypedSplice an s')

  exact (HsProc an p c) = do
    debugM $ "HsProc start"
    an0 <- markEpAnnL an lidl AnnProc
    p' <- markAnnotated p
    an1 <- markEpAnnL an0 lidl AnnRarrow
    debugM $ "HsProc after AnnRarrow"
    c' <- markAnnotated c
    return (HsProc an1 p' c')

  exact (HsStatic an e) = do
    an0 <- markEpAnnL an lidl AnnStatic
    e' <- markAnnotated e
    return (HsStatic an0 e')

  exact (HsPragE a prag e) = do
    prag' <- markAnnotated prag
    e' <- markAnnotated e
    return (HsPragE a prag' e')

  exact (HsEmbTy toktype t) = do
    toktype' <- markEpToken toktype
    t' <- markAnnotated t
    return (HsEmbTy toktype' t')

  exact x = error $ "exact HsExpr for:" ++ showAst x

-- ---------------------------------------------------------------------

exactDo :: (Monad m, Monoid w, ExactPrint (LocatedAn an a))
        => AnnList -> HsDoFlavour -> LocatedAn an a
        -> EP w m (AnnList, LocatedAn an a)
exactDo an (DoExpr m)    stmts = exactMdo an m AnnDo           >>= \an0 -> markMaybeDodgyStmts an0 stmts
exactDo an GhciStmtCtxt  stmts = markEpAnnL an lal_rest AnnDo >>= \an0 -> markMaybeDodgyStmts an0 stmts
exactDo an (MDoExpr m)   stmts = exactMdo an m AnnMdo          >>= \an0 -> markMaybeDodgyStmts an0 stmts
exactDo an ListComp      stmts = markMaybeDodgyStmts an stmts
exactDo an MonadComp     stmts = markMaybeDodgyStmts an stmts

exactMdo :: (Monad m, Monoid w)
  => AnnList -> Maybe ModuleName -> AnnKeywordId -> EP w m AnnList
exactMdo an Nothing            kw = markEpAnnL    an lal_rest kw
exactMdo an (Just module_name) kw = markEpAnnLMS'' an lal_rest kw (Just n)
    where
      n = (moduleNameString module_name) ++ "." ++ (keywordToString kw)

markMaybeDodgyStmts :: (Monad m, Monoid w, ExactPrint (LocatedAn an a))
  => AnnList -> LocatedAn an a -> EP w m (AnnList, LocatedAn an a)
markMaybeDodgyStmts an stmts =
  if isGoodSrcSpan (getLocA stmts)
    then do
      r <- markAnnotatedWithLayout stmts
      return (an, r)
    else return (an, stmts)

notDodgyE :: EpaLocation -> Bool
notDodgyE anc =
  case anc of
    EpaSpan s -> isGoodSrcSpan s
    EpaDelta{} -> True

-- ---------------------------------------------------------------------
instance ExactPrint (HsPragE GhcPs) where
  getAnnotationEntry HsPragSCC{}  = NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (HsPragSCC (an,st) sl) = do
    an0 <- markAnnOpenP' an st "{-# SCC"
    let txt = sourceTextToString (sl_st sl) (unpackFS $ sl_fs sl)
    an1 <- markEpAnnLMS'' an0 lapr_rest AnnVal    (Just txt) -- optional
    an2 <- markEpAnnLMS'' an1 lapr_rest AnnValStr (Just txt) -- optional
    an3 <- markAnnCloseP' an2
    return (HsPragSCC (an3,st) sl)


-- ---------------------------------------------------------------------

instance ExactPrint (HsUntypedSplice GhcPs) where
  getAnnotationEntry _ = NoEntryVal

  setAnnotationAnchor a _ _  _= a

  exact (HsUntypedSpliceExpr an e) = do
    an0 <- markEpAnnL an lidl AnnDollar
    e' <- markAnnotated e
    return (HsUntypedSpliceExpr an0 e')

  exact (HsQuasiQuote an q (L l fs)) = do
    -- The quasiquote string does not honour layout offsets. Store
    -- the colOffset for now.
    -- TODO: use local?
    oldOffset <- getLayoutOffsetP
    EPState{pMarkLayout} <- get
    unless pMarkLayout $ setLayoutOffsetP 0
    printStringAdvance
            -- Note: Lexer.x does not provide unicode alternative. 2017-02-26
            ("[" ++ (showPprUnsafe q) ++ "|" ++ (unpackFS fs) ++ "|]")
    unless pMarkLayout $ setLayoutOffsetP oldOffset
    return (HsQuasiQuote an q (L l fs))

-- ---------------------------------------------------------------------

-- TODO:AZ: combine these instances
instance ExactPrint (MatchGroup GhcPs (LocatedA (HsExpr GhcPs))) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ _ = a
  exact (MG x matches) = do
    -- TODO:AZ use SortKey, in MG ann.
    matches' <- markAnnotated matches
    return (MG x matches')

instance ExactPrint (MatchGroup GhcPs (LocatedA (HsCmd GhcPs))) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ _ = a
  exact (MG x matches) = do
    -- TODO:AZ use SortKey, in MG ann.
    matches' <- if isGoodSrcSpan (getLocA matches)
      then markAnnotated matches
      else return matches
    return (MG x matches')

-- ---------------------------------------------------------------------

instance (ExactPrint body) => ExactPrint (HsRecFields GhcPs body) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ _ = a
  exact (HsRecFields fields mdot) = do
    fields' <- markAnnotated fields
    mdot' <- case mdot of
      Nothing -> return Nothing
      Just (L ss d) -> do
        ss' <- printStringAtAA ss ".."
        return $ Just (L ss' d)
      -- Note: mdot contains the SrcSpan where the ".." appears, if present
    return (HsRecFields fields' mdot')

-- ---------------------------------------------------------------------

instance (ExactPrint body)
    => ExactPrint (HsFieldBind (LocatedA (FieldOcc GhcPs)) body) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (HsFieldBind an f arg isPun) = do
    debugM $ "HsFieldBind"
    f' <- markAnnotated f
    (an0, arg') <- if isPun then return (an, arg)
             else do
               an0 <- markEpAnnL an lidl AnnEqual
               arg' <- markAnnotated arg
               return (an0, arg')
    return (HsFieldBind an0 f' arg' isPun)

-- ---------------------------------------------------------------------

instance (ExactPrint body)
    => ExactPrint (HsFieldBind (LocatedAn NoEpAnns (FieldLabelStrings GhcPs)) body) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (HsFieldBind an f arg isPun) = do
    debugM $ "HsFieldBind FieldLabelStrings"
    f' <- markAnnotated f
    (an0, arg') <- if isPun then return (an, arg)
             else do
               an0 <- markEpAnnL an lidl AnnEqual
               arg' <- markAnnotated arg
               return (an0, arg')
    return (HsFieldBind an0 f' arg' isPun)

-- ---------------------------------------------------------------------

instance (ExactPrint (LocatedA body))
    => ExactPrint (HsFieldBind (LocatedA (AmbiguousFieldOcc GhcPs)) (LocatedA body)) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (HsFieldBind an f arg isPun) = do
    debugM $ "HsRecUpdField"
    f' <- markAnnotated f
    an0 <- if isPun then return an
             else markEpAnnL an lidl AnnEqual
    arg' <- if isPun
              then return arg
              else markAnnotated arg
    return (HsFieldBind an0 f' arg' isPun)

-- ---------------------------------------------------------------------
instance ExactPrint (LHsRecUpdFields GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact flds@(RegularRecUpdFields    { recUpdFields  = rbinds }) = do
    debugM $ "RegularRecUpdFields"
    rbinds' <- markAnnotated rbinds
    return $ flds { recUpdFields = rbinds' }
  exact flds@(OverloadedRecUpdFields { olRecUpdFields = pbinds }) = do
    debugM $ "OverloadedRecUpdFields"
    pbinds' <- markAnnotated pbinds
    return $ flds { olRecUpdFields = pbinds' }

-- ---------------------------------------------------------------------

instance ExactPrint (FieldLabelStrings GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ _ = a
  exact (FieldLabelStrings fs) = FieldLabelStrings <$> markAnnotated fs

-- ---------------------------------------------------------------------

instance ExactPrint (DotFieldOcc GhcPs) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (DotFieldOcc an (L loc (FieldLabelString fs))) = do
    an0 <- markLensKwM' an lafDot  AnnDot
    -- The field name has a SrcSpanAnnN, print it as a
    -- LocatedN RdrName
    L loc' _ <- markAnnotated (L loc (mkVarUnqual fs))
    return (DotFieldOcc an0 (L loc' (FieldLabelString fs)))

-- ---------------------------------------------------------------------

instance ExactPrint (HsTupArg GhcPs) where
  getAnnotationEntry (Present _ _) = NoEntryVal
  getAnnotationEntry (Missing (EpAnn _ False _)) = NoEntryVal
  getAnnotationEntry (Missing an)   = fromAnn an

  setAnnotationAnchor (Present a b) _ _ _ = Present a b
  setAnnotationAnchor (Missing an)   anc ts cs = Missing (setAnchorEpa an anc ts cs)

  exact (Present a e) = Present a <$> markAnnotated e

  exact a@(Missing (EpAnn _ False _)) = return a
  exact a@(Missing _) = printStringAdvance "," >> return a

-- ---------------------------------------------------------------------

instance ExactPrint (HsCmdTop GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ _ = a
  exact (HsCmdTop a cmd) = HsCmdTop a <$> markAnnotated cmd

-- ---------------------------------------------------------------------

instance ExactPrint (HsCmd GhcPs) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (HsCmdArrApp an arr arg o isRightToLeft) = do
    if isRightToLeft
      then do
        arr' <- markAnnotated arr
        an0 <- markKw an
        arg' <- markAnnotated arg
        return (HsCmdArrApp an0 arr' arg' o isRightToLeft)
      else do
        arg' <- markAnnotated arg
        an0 <- markKw an
        arr' <- markAnnotated arr
        return (HsCmdArrApp an0 arr' arg' o isRightToLeft)

  exact (HsCmdArrForm an e fixity mf cs) = do
    an0 <- markLensMAA' an lal_open
    (e',cs') <- case (fixity, cs) of
      (Infix, (arg1:argrest)) -> do
        arg1' <- markAnnotated arg1
        e' <- markAnnotated e
        argrest' <- markAnnotated argrest
        return (e', arg1':argrest')
      (Prefix, _) -> do
        e' <- markAnnotated e
        cs' <- markAnnotated cs
        return (e', cs')
      (Infix, []) -> error "Not possible"
    an1 <- markLensMAA' an0 lal_close
    return (HsCmdArrForm an1 e' fixity mf cs')

  exact (HsCmdApp an e1 e2) = do
    e1' <- markAnnotated e1
    e2' <- markAnnotated e2
    return (HsCmdApp an e1' e2')

  exact (HsCmdLam an lam_variant matches) = do
    an0 <- markEpAnnL an lidl AnnLam
    an1 <- case lam_variant of
             LamSingle -> return an0
             LamCase -> markEpAnnL an0 lidl AnnCase
             LamCases -> markEpAnnL an0 lidl AnnCases
    matches' <- markAnnotated matches
    return (HsCmdLam an1 lam_variant matches')

  exact (HsCmdPar (lpar, rpar) e) = do
    lpar' <- markEpToken lpar
    e' <- markAnnotated e
    rpar' <- markEpToken rpar
    return (HsCmdPar (lpar', rpar') e')

  exact (HsCmdCase an e alts) = do
    an0 <- markLensKw an lhsCaseAnnCase AnnCase
    e' <- markAnnotated e
    an1 <- markLensKw an0 lhsCaseAnnOf AnnOf
    an2 <- markEpAnnL an1 lhsCaseAnnsRest AnnOpenC
    an3 <- markEpAnnAllL' an2 lhsCaseAnnsRest AnnSemi
    alts' <- markAnnotated alts
    an4 <- markEpAnnL an3 lhsCaseAnnsRest AnnCloseC
    return (HsCmdCase an4 e' alts')

  exact (HsCmdIf an a e1 e2 e3) = do
    an0 <- markLensKw an laiIf AnnIf
    e1' <- markAnnotated e1
    an1 <- markLensKwM' an0 laiThenSemi AnnSemi
    an2 <- markLensKw an1 laiThen AnnThen
    e2' <- markAnnotated e2
    an3 <- markLensKwM' an2 laiElseSemi AnnSemi
    an4 <- markLensKw an3 laiElse AnnElse
    e3' <- markAnnotated e3
    return (HsCmdIf an4 a e1' e2' e3')

  exact (HsCmdLet (tkLet, tkIn) binds e) = do
    setLayoutBoth $ do -- Make sure the 'in' gets indented too
      tkLet' <- markEpToken tkLet
      binds' <- setLayoutBoth $ markAnnotated binds
      tkIn' <- markEpToken tkIn
      e' <- markAnnotated e
      return (HsCmdLet (tkLet', tkIn') binds' e')

  exact (HsCmdDo an es) = do
    debugM $ "HsCmdDo"
    an0 <- markEpAnnL an lal_rest AnnDo
    es' <- markAnnotated es
    return (HsCmdDo an0 es')

-- ---------------------------------------------------------------------

instance (
  ExactPrint (LocatedA (body GhcPs)),
                 Anno (StmtLR GhcPs GhcPs (LocatedA (body GhcPs))) ~ SrcSpanAnnA,
           Anno [GenLocated SrcSpanAnnA (StmtLR GhcPs GhcPs (LocatedA (body GhcPs)))] ~ SrcSpanAnnL,
           (ExactPrint (LocatedL [LocatedA (StmtLR GhcPs GhcPs (LocatedA (body GhcPs)))])))
   => ExactPrint (StmtLR GhcPs GhcPs (LocatedA (body GhcPs))) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _s = a

  exact (LastStmt a body b c) = do
    debugM $ "LastStmt"
    body' <- markAnnotated body
    return (LastStmt a body' b c)

  exact (BindStmt an pat body) = do
    debugM $ "BindStmt"
    pat' <- markAnnotated pat
    an0 <- markEpAnnL an lidl AnnLarrow
    body' <- markAnnotated body
    return (BindStmt an0 pat' body')

  exact (ApplicativeStmt _ _body _) = do
    error $ "ApplicativeStmt is introduced in the renamer"

  exact (BodyStmt a body b c) = do
    debugM $ "BodyStmt"
    body' <- markAnnotated body
    return (BodyStmt a body' b c)

  exact (LetStmt an binds) = do
    debugM $ "LetStmt"
    an0 <- markEpAnnL an lidl AnnLet
    binds' <- markAnnotated binds
    return (LetStmt an0 binds')

  exact (ParStmt a pbs b c) = do
    debugM $ "ParStmt"
    pbs' <- markAnnotated pbs
    return (ParStmt a pbs' b c)

  exact (TransStmt an form stmts b using by c d e) = do
    debugM $ "TransStmt"
    stmts' <- markAnnotated stmts
    (an', by', using') <- exactTransStmt an by using form
    return (TransStmt an' form stmts' b using' by' c d e)

  exact (RecStmt an stmts a b c d e) = do
    debugM $ "RecStmt"
    an0 <- markEpAnnL an lal_rest AnnRec
    (an1, stmts') <- markAnnList' an0 (markAnnotated stmts)
    return (RecStmt an1 stmts' a b c d e)

-- ---------------------------------------------------------------------

instance ExactPrint (ParStmtBlock GhcPs GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ _ = a
  exact (ParStmtBlock a stmts b c) = do
    stmts' <- markAnnotated stmts
    return (ParStmtBlock a stmts' b c)

exactTransStmt :: (Monad m, Monoid w)
  => [AddEpAnn] -> Maybe (LHsExpr GhcPs) -> (LHsExpr GhcPs) -> TransForm
  -> EP w m ([AddEpAnn], Maybe (LHsExpr GhcPs), (LHsExpr GhcPs))
exactTransStmt an by using ThenForm = do
  debugM $ "exactTransStmt:ThenForm"
  an0 <- markEpAnnL an lidl AnnThen
  using' <- markAnnotated using
  case by of
    Nothing -> return (an0, by, using')
    Just b -> do
      an1 <- markEpAnnL an0 lidl AnnBy
      b' <- markAnnotated b
      return (an1, Just b', using')
exactTransStmt an by using GroupForm = do
  debugM $ "exactTransStmt:GroupForm"
  an0 <- markEpAnnL an lidl AnnThen
  an1 <- markEpAnnL an0 lidl AnnGroup
  (an2, by') <- case by of
    Nothing -> return (an1, by)
    Just b -> do
      an2 <- markEpAnnL an1 lidl AnnBy
      b' <- markAnnotated b
      return (an2, Just b')
  an3 <- markEpAnnL an2 lidl AnnUsing
  using' <- markAnnotated using
  return (an3, by', using')

-- ---------------------------------------------------------------------

instance ExactPrint (TyClDecl GhcPs) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _s = a

  exact (FamDecl a decl) = do
    decl' <- markAnnotated decl
    return (FamDecl a decl')

  exact (SynDecl { tcdSExt = an
                 , tcdLName = ltycon, tcdTyVars = tyvars, tcdFixity = fixity
                 , tcdRhs = rhs }) = do
    -- There may be arbitrary parens around parts of the constructor
    -- that are infix.  Turn these into comments so that they feed
    -- into the right place automatically
    an0 <- annotationsToComments an lidl [AnnOpenP,AnnCloseP]
    an1 <- markEpAnnL an0 lidl AnnType

    (_anx, ltycon', tyvars',_,_) <- exactVanillaDeclHead ltycon tyvars fixity Nothing
    an2 <- markEpAnnL an1 lidl AnnEqual
    rhs' <- markAnnotated rhs
    return (SynDecl { tcdSExt = an2
                    , tcdLName = ltycon', tcdTyVars = tyvars', tcdFixity = fixity
                    , tcdRhs = rhs' })

  -- TODO: add a workaround for https://gitlab.haskell.org/ghc/ghc/-/issues/20452
  exact (DataDecl { tcdDExt = an, tcdLName = ltycon, tcdTyVars = tyvars
                  , tcdFixity = fixity, tcdDataDefn = defn }) = do
    (_, an', ltycon', tyvars', _, _mctxt', defn') <-
      exactDataDefn an (exactVanillaDeclHead ltycon tyvars fixity) defn
    return (DataDecl { tcdDExt = an', tcdLName = ltycon', tcdTyVars = tyvars'
                     , tcdFixity = fixity, tcdDataDefn = defn' })

  -- -----------------------------------

  exact (ClassDecl {tcdCExt = (an, lo, sortKey),
                    tcdCtxt = context, tcdLName = lclas, tcdTyVars = tyvars,
                    tcdFixity = fixity,
                    tcdFDs  = fds,
                    tcdSigs = sigs, tcdMeths = methods,
                    tcdATs = ats, tcdATDefs = at_defs,
                    tcdDocs = _docs})
      -- TODO: add a test that demonstrates tcdDocs
      | null sigs && isEmptyBag methods && null ats && null at_defs -- No "where" part
      = do
          (an0, fds', lclas', tyvars',context') <- top_matter
          an1 <- markEpAnnL an0 lidl AnnOpenC
          an2 <- markEpAnnL an1 lidl AnnCloseC
          return (ClassDecl {tcdCExt = (an2, lo, sortKey),
                             tcdCtxt = context', tcdLName = lclas', tcdTyVars = tyvars',
                             tcdFixity = fixity,
                             tcdFDs  = fds',
                             tcdSigs = sigs, tcdMeths = methods,
                             tcdATs = ats, tcdATDefs = at_defs,
                             tcdDocs = _docs})

      | otherwise       -- Laid out
      = do
          (an0, fds', lclas', tyvars',context') <- top_matter
          an1 <- markEpAnnL    an0 lidl AnnOpenC
          an2 <- markEpAnnAllL' an1 lidl AnnSemi
          ds <- withSortKey sortKey
                               [(ClsSigTag, prepareListAnnotationA sigs),
                                (ClsMethodTag, prepareListAnnotationA (bagToList methods)),
                                (ClsAtTag, prepareListAnnotationA ats),
                                (ClsAtdTag, prepareListAnnotationA at_defs)
                             -- ++ prepareListAnnotation docs
                               ]
          an3 <- markEpAnnL an2 lidl AnnCloseC
          let
            sigs'    = undynamic ds
            methods' = listToBag $ undynamic ds
            ats'     = undynamic ds
            at_defs' = undynamic ds
          return (ClassDecl {tcdCExt = (an3, lo, sortKey),
                             tcdCtxt = context', tcdLName = lclas', tcdTyVars = tyvars',
                             tcdFixity = fixity,
                             tcdFDs  = fds',
                             tcdSigs = sigs', tcdMeths = methods',
                             tcdATs = ats', tcdATDefs = at_defs',
                             tcdDocs = _docs})
      where
        top_matter = do
          an' <- annotationsToComments an lidl  [AnnOpenP, AnnCloseP]
          an0 <- markEpAnnL an' lidl AnnClass
          (_, lclas', tyvars',_,context') <-  exactVanillaDeclHead lclas tyvars fixity context
          (an1, fds') <- if (null fds)
            then return (an0, fds)
            else do
              an1 <- markEpAnnL an0 lidl AnnVbar
              fds' <- markAnnotated fds
              return (an1, fds')
          an2 <- markEpAnnL an1 lidl AnnWhere
          return (an2, fds', lclas', tyvars',context')


-- ---------------------------------------------------------------------

instance ExactPrint (FunDep GhcPs) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (FunDep an ls rs') = do
    ls' <- markAnnotated ls
    an0 <- markEpAnnL an lidl AnnRarrow
    rs'' <- markAnnotated rs'
    return (FunDep an0 ls' rs'')

-- ---------------------------------------------------------------------

instance ExactPrint (FamilyDecl GhcPs) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (FamilyDecl { fdExt = an
                    , fdInfo = info
                    , fdTopLevel = top_level
                    , fdLName = ltycon
                    , fdTyVars = tyvars
                    , fdFixity = fixity
                    , fdResultSig = L lr result
                    , fdInjectivityAnn = mb_inj }) = do
    an0 <- exactFlavour an info
    an1 <- exact_top_level an0
    an2 <- annotationsToComments an1 lidl [AnnOpenP,AnnCloseP]
    (_, ltycon', tyvars',_,_) <- exactVanillaDeclHead ltycon tyvars fixity Nothing
    (an3, result') <- exact_kind an2
    (an4, mb_inj') <-
      case mb_inj of
        Nothing -> return (an3, mb_inj)
        Just inj -> do
          an4 <- markEpAnnL an3 lidl AnnVbar
          inj' <- markAnnotated inj
          return (an4, Just inj')
    (an5, info') <-
             case info of
               ClosedTypeFamily mb_eqns -> do
                 an5 <- markEpAnnL an4 lidl AnnWhere
                 an6 <- markEpAnnL an5 lidl AnnOpenC
                 (an7, mb_eqns') <-
                   case mb_eqns of
                     Nothing -> do
                       an7 <- markEpAnnL an6 lidl AnnDotdot
                       return (an7, mb_eqns)
                     Just eqns -> do
                       eqns' <- markAnnotated eqns
                       return (an6, Just eqns')
                 an8 <- markEpAnnL an7 lidl AnnCloseC
                 return (an8, ClosedTypeFamily mb_eqns')
               _ -> return (an4, info)
    return (FamilyDecl { fdExt = an5
                       , fdInfo = info'
                       , fdTopLevel = top_level
                       , fdLName = ltycon'
                       , fdTyVars = tyvars'
                       , fdFixity = fixity
                       , fdResultSig = L lr result'
                       , fdInjectivityAnn = mb_inj' })
    where
      exact_top_level an' =
        case top_level of
          TopLevel    -> markEpAnnL an' lidl AnnFamily
          NotTopLevel -> do
            -- It seems that in some kind of legacy
            -- mode the 'family' keyword is still
            -- accepted.
            markEpAnnL an' lidl AnnFamily

      exact_kind an' =
        case result of
          NoSig    _         -> return (an', result)
          KindSig  x kind    -> do
            an0 <- markEpAnnL an' lidl AnnDcolon
            kind' <- markAnnotated kind
            return (an0, KindSig  x kind')
          TyVarSig x tv_bndr -> do
            an0 <- markEpAnnL an' lidl AnnEqual
            tv_bndr' <- markAnnotated tv_bndr
            return (an0, TyVarSig x tv_bndr')


exactFlavour :: (Monad m, Monoid w) => [AddEpAnn] -> FamilyInfo GhcPs -> EP w m [AddEpAnn]
exactFlavour an DataFamily            = markEpAnnL an lidl AnnData
exactFlavour an OpenTypeFamily        = markEpAnnL an lidl AnnType
exactFlavour an (ClosedTypeFamily {}) = markEpAnnL an lidl AnnType

-- ---------------------------------------------------------------------

exactDataDefn
  :: (Monad m, Monoid w)
  => [AddEpAnn]
  -> (Maybe (LHsContext GhcPs) -> EP w m ([AddEpAnn]
                                         , LocatedN RdrName
                                         , a
                                         , b
                                         , Maybe (LHsContext GhcPs))) -- Printing the header
  -> HsDataDefn GhcPs
  -> EP w m ( [AddEpAnn] -- ^ from exactHdr
            , [AddEpAnn] -- ^ updated one passed in
            , LocatedN RdrName, a, b, Maybe (LHsContext GhcPs), HsDataDefn GhcPs)
exactDataDefn an exactHdr
                 (HsDataDefn { dd_ext = x, dd_ctxt = context
                             , dd_cType = mb_ct
                             , dd_kindSig = mb_sig
                             , dd_cons = condecls, dd_derivs = derivings }) = do

  an' <- annotationsToComments an lidl [AnnOpenP, AnnCloseP]

  an0 <- case condecls of
    DataTypeCons is_type_data _ -> do
      an0' <- if is_type_data
                then markEpAnnL an' lidl AnnType
                else return an'
      markEpAnnL an0' lidl AnnData
    NewTypeCon   _ -> markEpAnnL an' lidl AnnNewtype

  an1 <- markEpAnnL an0 lidl AnnInstance -- optional
  mb_ct' <- mapM markAnnotated mb_ct
  (anx, ln', tvs', b, mctxt') <- exactHdr context
  (an2, mb_sig') <- case mb_sig of
    Nothing -> return (an1, Nothing)
    Just kind -> do
      an2 <- markEpAnnL an1 lidl AnnDcolon
      kind' <- markAnnotated kind
      return (an2, Just kind')
  an3 <- if (needsWhere condecls)
    then markEpAnnL an2 lidl AnnWhere
    else return an2
  an4 <- markEpAnnL an3 lidl AnnOpenC
  (an5, condecls') <- exact_condecls an4 (toList condecls)
  let condecls'' = case condecls of
        DataTypeCons d _ -> DataTypeCons d condecls'
        NewTypeCon _     -> case condecls' of
          [decl] -> NewTypeCon decl
          _ -> panic "exacprint NewTypeCon"
  an6 <- markEpAnnL an5 lidl AnnCloseC
  derivings' <- mapM markAnnotated derivings
  return (anx, an6, ln', tvs', b, mctxt',
                 (HsDataDefn { dd_ext = x, dd_ctxt = context
                             , dd_cType = mb_ct'
                             , dd_kindSig = mb_sig'
                             , dd_cons = condecls'', dd_derivs = derivings' }))


exactVanillaDeclHead :: (Monad m, Monoid w)
                     => LocatedN RdrName
                     -> LHsQTyVars GhcPs
                     -> LexicalFixity
                     -> Maybe (LHsContext GhcPs)
                     -> EP w m ( [AddEpAnn]
                               , LocatedN RdrName
                               , LHsQTyVars GhcPs
                               , (), Maybe (LHsContext GhcPs))
exactVanillaDeclHead thing tvs@(HsQTvs { hsq_explicit = tyvars }) fixity context = do
  let
    exact_tyvars (varl:varsr)
      | hvarsr : tvarsr@(_ : _) <- varsr
      , fixity == Infix = do
          varl' <- markAnnotated varl
          thing' <- markAnnotated thing
          hvarsr' <- markAnnotated hvarsr
          tvarsr' <- markAnnotated tvarsr
          return (thing', varl':hvarsr':tvarsr')
      | fixity == Infix = do
          varl' <- markAnnotated varl
          thing' <- markAnnotated thing
          varsr' <- markAnnotated varsr
          return (thing', varl':varsr')
      | otherwise = do
          thing' <- markAnnotated thing
          vs <- mapM markAnnotated (varl:varsr)
          return (thing', vs)
    exact_tyvars [] = do
      thing' <- markAnnotated thing
      return (thing', [])
  context' <- mapM markAnnotated context
  (thing', tyvars') <- exact_tyvars tyvars
  return (noAnn, thing', tvs { hsq_explicit = tyvars' }, (), context')

-- ---------------------------------------------------------------------

instance ExactPrint (InjectivityAnn GhcPs) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _ = a
  exact (InjectivityAnn an lhs rhs) = do
    an0 <- markEpAnnL an lidl AnnVbar
    lhs' <- markAnnotated lhs
    an1 <- markEpAnnL an0 lidl AnnRarrow
    rhs' <- mapM markAnnotated rhs
    return (InjectivityAnn an1 lhs' rhs')

-- ---------------------------------------------------------------------

class Typeable flag => ExactPrintTVFlag flag where
  exactTVDelimiters :: (Monad m, Monoid w)
    => [AddEpAnn] -> flag -> EP w m (HsTyVarBndr flag GhcPs)
    -> EP w m ([AddEpAnn], (HsTyVarBndr flag GhcPs))

instance ExactPrintTVFlag () where
  exactTVDelimiters an _ thing_inside = do
    an0 <- markEpAnnAllL' an lid AnnOpenP
    r <- thing_inside
    an1 <- markEpAnnAllL' an0 lid AnnCloseP
    return (an1, r)

instance ExactPrintTVFlag Specificity where
  exactTVDelimiters an s thing_inside = do
    an0 <- markEpAnnAllL' an lid open
    r <- thing_inside
    an1 <- markEpAnnAllL' an0 lid close
    return (an1, r)
    where
      (open, close) = case s of
        SpecifiedSpec -> (AnnOpenP, AnnCloseP)
        InferredSpec  -> (AnnOpenC, AnnCloseC)

instance ExactPrintTVFlag (HsBndrVis GhcPs) where
  exactTVDelimiters an0 bvis thing_inside = do
    case bvis of
      HsBndrRequired _ -> return ()
      HsBndrInvisible at -> markEpToken at >> return ()
    an1 <- markEpAnnAllL' an0 lid AnnOpenP
    r <- thing_inside
    an2 <- markEpAnnAllL' an1 lid AnnCloseP
    return (an2, r)

instance ExactPrintTVFlag flag => ExactPrint (HsTyVarBndr flag GhcPs) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (UserTyVar an flag n) = do
    r <- exactTVDelimiters an flag $ do
           n' <- markAnnotated n
           return (UserTyVar an flag n')
    case r of
      (an', UserTyVar _ flag'' n'') -> return (UserTyVar an' flag'' n'')
      _ -> error "KindedTyVar should never happen here"
  exact (KindedTyVar an flag n k) = do
    r <- exactTVDelimiters an flag $ do
          n' <- markAnnotated n
          an0 <- markEpAnnL an lidl AnnDcolon
          k' <- markAnnotated k
          return (KindedTyVar an0 flag n' k')
    case r of
      (an',KindedTyVar _ flag'' n'' k'') -> return (KindedTyVar an' flag'' n'' k'')
      _ -> error "UserTyVar should never happen here"

-- ---------------------------------------------------------------------

instance ExactPrint (HsType GhcPs) where
  getAnnotationEntry _         = NoEntryVal
  setAnnotationAnchor a _ _ _s = a

  exact (HsForAllTy { hst_xforall = an
                    , hst_tele = tele, hst_body = ty }) = do
    tele' <- markAnnotated tele
    ty' <- markAnnotated ty
    return (HsForAllTy { hst_xforall = an
                       , hst_tele = tele', hst_body = ty' })

  exact (HsQualTy an ctxt ty) = do
    ctxt' <- markAnnotated ctxt
    ty' <- markAnnotated ty
    return (HsQualTy an ctxt' ty')
  exact (HsTyVar an promoted name) = do
    an0 <- if (promoted == IsPromoted)
             then markEpAnnL an lidl AnnSimpleQuote
             else return an
    name' <- markAnnotated name
    return (HsTyVar an0 promoted name')
  exact (HsAppTy an t1 t2) = do
    t1' <- markAnnotated t1
    t2' <- markAnnotated t2
    return (HsAppTy an t1' t2')
  exact (HsAppKindTy at ty ki) = do
    ty' <- markAnnotated ty
    at' <- markEpToken at
    ki' <- markAnnotated ki
    return (HsAppKindTy at' ty' ki')
  exact (HsFunTy an mult ty1 ty2) = do
    ty1' <- markAnnotated ty1
    mult' <- markArrow mult
    ty2' <- markAnnotated ty2
    return (HsFunTy an mult' ty1' ty2')
  exact (HsListTy an tys) = do
    an0 <- markOpeningParen an
    tys' <- markAnnotated tys
    an1 <- markClosingParen an0
    return (HsListTy an1 tys')
  exact (HsTupleTy an con tys) = do
    an0 <- markOpeningParen an
    tys' <- markAnnotated tys
    an1 <- markClosingParen an0
    return (HsTupleTy an1 con tys')
  exact (HsSumTy an tys) = do
    an0 <- markOpeningParen an
    tys' <- markAnnotated tys
    an1 <- markClosingParen an0
    return (HsSumTy an1 tys')
  exact (HsOpTy an promoted t1 lo t2) = do
    an0 <- if (isPromoted promoted)
        then markEpAnnL an lidl AnnSimpleQuote
        else return an
    t1' <- markAnnotated t1
    lo' <- markAnnotated lo
    t2' <- markAnnotated t2
    return (HsOpTy an0 promoted t1' lo' t2')
  exact (HsParTy an ty) = do
    an0 <- markOpeningParen an
    ty' <- markAnnotated ty
    an1 <- markClosingParen an0
    return (HsParTy an1 ty')
  exact (HsIParamTy an n t) = do
    n' <- markAnnotated n
    an0 <- markEpAnnL an lidl AnnDcolon
    t' <- markAnnotated t
    return (HsIParamTy an0 n' t')
  exact (HsStarTy an isUnicode) = do
    if isUnicode
        then printStringAdvance "\x2605" -- Unicode star
        else printStringAdvance "*"
    return (HsStarTy an isUnicode)
  exact (HsKindSig an ty k) = do
    ty' <- markAnnotated ty
    an0 <- markEpAnnL an lidl AnnDcolon
    k' <- markAnnotated k
    return (HsKindSig an0 ty' k')
  exact (HsSpliceTy a splice) = do
    splice' <- markAnnotated splice
    return (HsSpliceTy a splice')
  exact (HsDocTy an ty doc) = do
    ty' <- markAnnotated ty
    return (HsDocTy an ty' doc)
  exact (HsBangTy an (HsSrcBang mt up str) ty) = do
    an0 <-
      case mt of
        NoSourceText -> return an
        SourceText src -> do
          debugM $ "HsBangTy: src=" ++ showAst src
          an0 <- markEpAnnMS' an AnnOpen  (Just $ unpackFS src)
          an1 <- markEpAnnMS' an0 AnnClose (Just "#-}")
          debugM $ "HsBangTy: done unpackedness"
          return an1
    an1 <-
      case str of
        SrcLazy     -> mark an0 AnnTilde
        SrcStrict   -> mark an0 AnnBang
        NoSrcStrict -> return an0
    ty' <- markAnnotated ty
    return (HsBangTy an1 (HsSrcBang mt up str) ty')
  exact (HsExplicitListTy an prom tys) = do
    an0 <- if (isPromoted prom)
             then mark an AnnSimpleQuote
             else return an
    an1 <- mark an0 AnnOpenS
    tys' <- markAnnotated tys
    an2 <- mark an1 AnnCloseS
    return (HsExplicitListTy an2 prom tys')
  exact (HsExplicitTupleTy an tys) = do
    an0 <- mark an AnnSimpleQuote
    an1 <- mark an0 AnnOpenP
    tys' <- markAnnotated tys
    an2 <- mark an1 AnnCloseP
    return (HsExplicitTupleTy an2 tys')
  exact (HsTyLit a lit) = do
    case lit of
      (HsNumTy src v) -> printSourceText src (show v)
      (HsStrTy src v) -> printSourceText src (show v)
      (HsCharTy src v) -> printSourceText src (show v)
    return (HsTyLit a lit)
  exact t@(HsWildCardTy _) = printStringAdvance "_" >> return t
  exact x = error $ "missing match for HsType:" ++ showAst x

-- ---------------------------------------------------------------------

instance ExactPrint (HsForAllTelescope GhcPs) where
  getAnnotationEntry (HsForAllVis an _)   = fromAnn an
  getAnnotationEntry (HsForAllInvis an _) = fromAnn an

  setAnnotationAnchor (HsForAllVis an a) anc ts cs = HsForAllVis (setAnchorEpa an anc ts cs) a
  setAnnotationAnchor (HsForAllInvis an a) anc ts cs = HsForAllInvis (setAnchorEpa an anc ts cs) a

  exact (HsForAllVis an bndrs)   = do
    an0 <- markLensAA an lfst -- AnnForall
    bndrs' <- markAnnotated bndrs
    an1 <- markLensAA an0 lsnd -- AnnRarrow
    return (HsForAllVis an1 bndrs')

  exact (HsForAllInvis an bndrs) = do
    an0 <- markLensAA an lfst -- AnnForall
    bndrs' <- markAnnotated bndrs
    an1 <- markLensAA an0 lsnd -- AnnDot
    return (HsForAllInvis an1 bndrs')

-- ---------------------------------------------------------------------

instance ExactPrint (HsDerivingClause GhcPs) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (HsDerivingClause { deriv_clause_ext      = an
                          , deriv_clause_strategy = dcs
                          , deriv_clause_tys      = dct }) = do
    an0 <- markEpAnnL an lidl AnnDeriving
    exact_strat_before
    dct' <- markAnnotated dct
    exact_strat_after
    return (HsDerivingClause { deriv_clause_ext      = an0
                             , deriv_clause_strategy = dcs
                             , deriv_clause_tys      = dct' })
      where
        (exact_strat_before, exact_strat_after) =
          case dcs of
            Just v@(L _ ViaStrategy{}) -> (pure (), markAnnotated v >> pure ())
            _                          -> (mapM_ markAnnotated dcs, pure ())

-- ---------------------------------------------------------------------

instance ExactPrint (DerivStrategy GhcPs) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (StockStrategy an)    = do
    an0 <- markEpAnnL an lid AnnStock
    return (StockStrategy an0)
  exact (AnyclassStrategy an) = do
    an0 <- markEpAnnL an lid AnnAnyclass
    return (AnyclassStrategy an0)
  exact (NewtypeStrategy an)  = do
    an0 <- markEpAnnL an lid AnnNewtype
    return (NewtypeStrategy an0)
  exact (ViaStrategy (XViaStrategyPs an ty)) = do
    an0 <- markEpAnnL an lid AnnVia
    ty' <- markAnnotated ty
    return (ViaStrategy (XViaStrategyPs an0 ty'))

-- ---------------------------------------------------------------------

instance (ExactPrint a) => ExactPrint (LocatedC a) where
  getAnnotationEntry (L sann _) = fromAnn sann
  setAnnotationAnchor = setAnchorAn

  exact (L (EpAnn anc (AnnContext ma opens closes) cs) a) = do
    opens' <- mapM (markKwA AnnOpenP) opens
    a' <- markAnnotated a
    closes' <- mapM (markKwA AnnCloseP) closes
    return (L (EpAnn anc (AnnContext ma opens' closes') cs) a')

-- ---------------------------------------------------------------------

instance ExactPrint (DerivClauseTys GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (DctSingle x ty) = do
    ty' <- markAnnotated ty
    return (DctSingle x ty')
  exact (DctMulti x tys) = do
    tys' <- markAnnotated tys
    return (DctMulti x tys')

-- ---------------------------------------------------------------------

instance ExactPrint (HsSigType GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (HsSig a bndrs ty) = do
    bndrs' <- markAnnotated bndrs
    ty' <- markAnnotated ty
    return (HsSig a bndrs' ty')

-- ---------------------------------------------------------------------

instance ExactPrint (LocatedN RdrName) where
  getAnnotationEntry (L sann _) = fromAnn sann
  setAnnotationAnchor = setAnchorAn

  exact (L (EpAnn anc ann cs) n) = do
    ann' <-
      case ann of
        NameAnn a o l c t -> do
          mn <- markName a o (Just (l,n)) c
          case mn of
            (o', (Just (l',_n)), c') -> do
              return (NameAnn a o' l' c' t)
            _ -> error "ExactPrint (LocatedN RdrName)"
        NameAnnCommas a o commas c t -> do
          let (kwo,kwc) = adornments a
          (AddEpAnn _ o') <- markKwC NoCaptureComments (AddEpAnn kwo o)
          commas' <- forM commas (\loc -> locFromAdd <$> markKwC NoCaptureComments (AddEpAnn AnnComma loc))
          (AddEpAnn _ c') <- markKwC NoCaptureComments (AddEpAnn kwc c)
          return (NameAnnCommas a o' commas' c' t)
        NameAnnBars a o bars c t -> do
          let (kwo,kwc) = adornments a
          (AddEpAnn _ o') <- markKwC NoCaptureComments (AddEpAnn kwo o)
          bars' <- forM bars (\loc -> locFromAdd <$> markKwC NoCaptureComments (AddEpAnn AnnVbar loc))
          (AddEpAnn _ c') <- markKwC NoCaptureComments (AddEpAnn kwc c)
          return (NameAnnBars a o' bars' c' t)
        NameAnnOnly a o c t -> do
          (o',_,c') <- markName a o Nothing c
          return (NameAnnOnly a o' c' t)
        NameAnnRArrow unicode o nl c t -> do
          o' <- case o of
            Just o0 -> do
              (AddEpAnn _ o') <- markKwC NoCaptureComments (AddEpAnn AnnOpenP o0)
              return (Just o')
            Nothing -> return Nothing
          (AddEpAnn _ nl') <-
            if unicode
              then markKwC NoCaptureComments (AddEpAnn AnnRarrowU nl)
              else markKwC NoCaptureComments (AddEpAnn AnnRarrow nl)
          c' <- case c of
            Just c0 -> do
              (AddEpAnn _ c') <- markKwC NoCaptureComments (AddEpAnn AnnCloseP c0)
              return (Just c')
            Nothing -> return Nothing
          return (NameAnnRArrow unicode o' nl' c' t)
        NameAnnQuote q name t -> do
          debugM $ "NameAnnQuote"
          (AddEpAnn _ q') <- markKwC NoCaptureComments (AddEpAnn AnnSimpleQuote q)
          (L name' _) <- markAnnotated (L name n)
          return (NameAnnQuote q' name' t)
        NameAnnTrailing t -> do
          _anc' <- printUnicode anc n
          return (NameAnnTrailing t)
    return (L (EpAnn anc ann' cs) n)

locFromAdd :: AddEpAnn -> EpaLocation
locFromAdd (AddEpAnn _ loc) = loc

printUnicode :: (Monad m, Monoid w) => Anchor -> RdrName -> EP w m Anchor
printUnicode anc n = do
  let str = case (showPprUnsafe n) of
            -- TODO: unicode support?
              "forall" -> if spanLength (anchor anc) == 1 then "∀" else "forall"
              s -> s
  loc <- printStringAtAAC NoCaptureComments (EpaDelta (SameLine 0) []) str
  case loc of
    EpaSpan _ -> return anc
    EpaDelta dp [] -> return $ EpaDelta dp []
    EpaDelta _ _cs -> error "printUnicode should not capture comments"


markName :: (Monad m, Monoid w)
  => NameAdornment -> EpaLocation -> Maybe (EpaLocation,RdrName) -> EpaLocation
  -> EP w m (EpaLocation, Maybe (EpaLocation,RdrName), EpaLocation)
markName adorn open mname close = do
  let (kwo,kwc) = adornments adorn
  (AddEpAnn _ open') <- markKwC CaptureComments (AddEpAnn kwo open)
  mname' <-
    case mname of
      Nothing -> return Nothing
      Just (name, a) -> do
        name' <- printStringAtAAC CaptureComments name (showPprUnsafe a)
        return (Just (name',a))
  (AddEpAnn _ close') <- markKwC CaptureComments (AddEpAnn kwc close)
  return (open', mname', close')

adornments :: NameAdornment -> (AnnKeywordId, AnnKeywordId)
adornments NameParens     = (AnnOpenP, AnnCloseP)
adornments NameParensHash = (AnnOpenPH, AnnClosePH)
adornments NameBackquotes = (AnnBackquote, AnnBackquote)
adornments NameSquare     = (AnnOpenS, AnnCloseS)

markTrailing :: (Monad m, Monoid w) => [TrailingAnn] -> EP w m [TrailingAnn]
markTrailing ts = do
  p <- getPosP
  debugM $ "markTrailing:" ++ showPprUnsafe (p,ts)
  mapM markKwT ts

-- ---------------------------------------------------------------------

-- based on pp_condecls in Decls.hs
exact_condecls :: (Monad m, Monoid w)
  => [AddEpAnn] -> [LConDecl GhcPs] -> EP w m ([AddEpAnn],[LConDecl GhcPs])
exact_condecls an cs
  | gadt_syntax                  -- In GADT syntax
  = do
      cs' <- mapM markAnnotated cs
      return (an, cs')
  | otherwise                    -- In H98 syntax
  = do
      an0 <- markEpAnnL an lidl AnnEqual
      cs' <- mapM markAnnotated cs
      return (an0, cs')
  where
    gadt_syntax = case cs of
      []                      -> False
      (L _ ConDeclH98{}  : _) -> False
      (L _ ConDeclGADT{} : _) -> True

-- ---------------------------------------------------------------------

instance ExactPrint (ConDecl GhcPs) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _ = a

-- based on pprConDecl
  exact (ConDeclH98 { con_ext = an
                    , con_name = con
                    , con_forall = has_forall
                    , con_ex_tvs = ex_tvs
                    , con_mb_cxt = mcxt
                    , con_args = args
                    , con_doc = doc }) = do
    an0 <- if has_forall
      then markEpAnnL an lidl AnnForall
      else return an
    ex_tvs' <- mapM markAnnotated ex_tvs
    an1 <- if has_forall
      then markEpAnnL an0 lidl AnnDot
      else return an0
    mcxt' <- mapM markAnnotated mcxt
    an2 <- if (isJust mcxt)
      then markEpAnnL an1 lidl AnnDarrow
      else return an1

    (con', args') <- exact_details args
    return (ConDeclH98 { con_ext = an2
                       , con_name = con'
                       , con_forall = has_forall
                       , con_ex_tvs = ex_tvs'
                       , con_mb_cxt = mcxt'
                       , con_args = args'
                       , con_doc = doc })

    where
    -- In ppr_details: let's not print the multiplicities (they are always 1, by
    -- definition) as they do not appear in an actual declaration.
      exact_details (InfixCon t1 t2) = do
        t1' <- markAnnotated t1
        con' <- markAnnotated con
        t2' <- markAnnotated t2
        return (con', InfixCon t1' t2')
      exact_details (PrefixCon tyargs tys) = do
        con' <- markAnnotated con
        tyargs' <- markAnnotated tyargs
        tys' <- markAnnotated tys
        return (con', PrefixCon tyargs' tys')
      exact_details (RecCon fields) = do
        con' <- markAnnotated con
        fields' <- markAnnotated fields
        return (con', RecCon fields')

  -- -----------------------------------

  exact (ConDeclGADT { con_g_ext = (dcol, an)
                     , con_names = cons
                     , con_bndrs = bndrs
                     , con_mb_cxt = mcxt, con_g_args = args
                     , con_res_ty = res_ty, con_doc = doc }) = do
    cons' <- mapM markAnnotated cons
    dcol' <- markEpUniToken dcol
    an1 <- annotationsToComments an lidl  [AnnOpenP, AnnCloseP]

    -- Work around https://gitlab.haskell.org/ghc/ghc/-/issues/20558
    bndrs' <- case bndrs of
      L _ (HsOuterImplicit _) -> return bndrs
      _ -> markAnnotated bndrs

    mcxt' <- mapM markAnnotated mcxt
    an2 <- if (isJust mcxt)
      then markEpAnnL an1 lidl AnnDarrow
      else return an1
    args' <-
      case args of
          (PrefixConGADT x args0) -> do
            args0' <- mapM markAnnotated args0
            return (PrefixConGADT x args0')
          (RecConGADT rarr fields) -> do
            fields' <- markAnnotated fields
            rarr' <- markEpUniToken rarr
            return (RecConGADT rarr' fields')
    res_ty' <- markAnnotated res_ty
    return (ConDeclGADT { con_g_ext = (dcol', an2)
                        , con_names = cons'
                        , con_bndrs = bndrs'
                        , con_mb_cxt = mcxt', con_g_args = args'
                        , con_res_ty = res_ty', con_doc = doc })

-- ---------------------------------------------------------------------

instance ExactPrint Void where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ _ = a
  exact x = return x

-- ---------------------------------------------------------------------

instance ExactPrintTVFlag flag => ExactPrint (HsOuterTyVarBndrs flag GhcPs) where
  getAnnotationEntry (HsOuterImplicit _) = NoEntryVal
  getAnnotationEntry (HsOuterExplicit an _) = fromAnn an

  setAnnotationAnchor (HsOuterImplicit a) _ _ _ = HsOuterImplicit a
  setAnnotationAnchor (HsOuterExplicit an a) anc ts cs = HsOuterExplicit (setAnchorEpa an anc ts cs) a

  exact b@(HsOuterImplicit _) = pure b
  exact (HsOuterExplicit an bndrs) = do
    an0 <- markLensAA an lfst -- "forall"
    bndrs' <- markAnnotated bndrs
    an1 <- markLensAA an0 lsnd -- "."
    return (HsOuterExplicit an1 bndrs')

-- ---------------------------------------------------------------------

instance ExactPrint (ConDeclField GhcPs) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (ConDeclField an names ftype mdoc) = do
    names' <- markAnnotated names
    an0 <- markEpAnnL an lidl AnnDcolon
    ftype' <- markAnnotated ftype
    return (ConDeclField an0 names' ftype' mdoc)

-- ---------------------------------------------------------------------

instance ExactPrint (FieldOcc GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ _ = a
  exact f@(FieldOcc _ n) = markAnnotated n >> return f

-- ---------------------------------------------------------------------

instance ExactPrint (AmbiguousFieldOcc GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ _ = a
  exact f@(Unambiguous _ n) = markAnnotated n >> return f
  exact f@(Ambiguous   _ n) = markAnnotated n >> return f

-- ---------------------------------------------------------------------

instance (ExactPrint a) => ExactPrint (HsScaled GhcPs a) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ _ = a
  exact (HsScaled arr t) = do
    t' <- markAnnotated t
    arr' <- markArrow arr
    return (HsScaled arr' t')

-- ---------------------------------------------------------------------

instance ExactPrint (LocatedP CType) where
  getAnnotationEntry = entryFromLocatedA
  setAnnotationAnchor = setAnchorAn

  exact (L an (CType stp mh (stct,ct))) = do
    an0 <- markAnnOpenP an stp "{-# CTYPE"
    an1 <- case mh of
             Nothing -> return an0
             Just (Header srcH _h) ->
               markEpAnnLMS an0 lapr_rest AnnHeader (Just (toSourceTextWithSuffix srcH "" ""))
    an2 <- markEpAnnLMS an1 lapr_rest AnnVal (Just (toSourceTextWithSuffix stct (unpackFS ct) ""))
    an3 <- markAnnCloseP an2
    return (L an3 (CType stp mh (stct,ct)))

-- ---------------------------------------------------------------------

instance ExactPrint (SourceText, RuleName) where
  -- We end up at the right place from the Located wrapper
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (st, rn)
    = printStringAdvance (toSourceTextWithSuffix st (unpackFS rn) "")
      >> return (st, rn)


-- =====================================================================
-- LocatedL instances start --
--
-- Each is dealt with specifically, as they have
-- different wrapping annotations in the al_rest zone.
--
-- In future, the annotation could perhaps be improved, with an
-- 'al_pre' and 'al_post' set of annotations to be simply sorted and
-- applied.
-- ---------------------------------------------------------------------

instance ExactPrint (LocatedL [LocatedA (IE GhcPs)]) where
  getAnnotationEntry = entryFromLocatedA
  setAnnotationAnchor = setAnchorAn

  exact (L an ies) = do
    debugM $ "LocatedL [LIE"
    an0 <- markEpAnnL' an lal_rest AnnHiding
    p <- getPosP
    debugM $ "LocatedL [LIE:p=" ++ showPprUnsafe p
    (an1, ies') <- markAnnList an0 (markAnnotated ies)
    return (L an1 ies')

instance (ExactPrint (Match GhcPs (LocatedA body)))
   => ExactPrint (LocatedL [LocatedA (Match GhcPs (LocatedA body))]) where
  getAnnotationEntry = entryFromLocatedA
  setAnnotationAnchor = setAnchorAn
  exact (L an a) = do
    debugM $ "LocatedL [LMatch"
    -- TODO: markAnnList?
    an0 <- markEpAnnAllL an lal_rest AnnWhere
    an1 <- markLensMAA an0 lal_open
    an2 <- markEpAnnAllL an1 lal_rest AnnSemi
    a' <- markAnnotated a
    an3 <- markLensMAA an2 lal_close
    return (L an3 a')

instance ExactPrint (LocatedL [LocatedA (StmtLR GhcPs GhcPs (LocatedA (HsExpr GhcPs)))]) where
  getAnnotationEntry = entryFromLocatedA
  setAnnotationAnchor = setAnchorAn
  exact (L an stmts) = do
    debugM $ "LocatedL [ExprLStmt"
    (an'', stmts') <- markAnnList an $ do
      case snocView stmts of
        Just (initStmts, ls@(L _ (LastStmt _ _body _ _))) -> do
          debugM $ "LocatedL [ExprLStmt: snocView"
          ls' <- markAnnotated ls
          initStmts' <- markAnnotated initStmts
          return (initStmts' ++ [ls'])
        _ -> do
          markAnnotated stmts
    return (L an'' stmts')

instance ExactPrint (LocatedL [LocatedA (StmtLR GhcPs GhcPs (LocatedA (HsCmd GhcPs)))]) where
  getAnnotationEntry = entryFromLocatedA
  setAnnotationAnchor = setAnchorAn
  exact (L ann es) = do
    debugM $ "LocatedL [CmdLStmt"
    an0 <- markLensMAA ann lal_open
    es' <- mapM markAnnotated es
    an1 <- markLensMAA an0 lal_close
    return (L an1 es')

instance ExactPrint (LocatedL [LocatedA (ConDeclField GhcPs)]) where
  getAnnotationEntry = entryFromLocatedA
  setAnnotationAnchor = setAnchorAn
  exact (L an fs) = do
    debugM $ "LocatedL [LConDeclField"
    (an', fs') <- markAnnList an (markAnnotated fs)
    return (L an' fs')

instance ExactPrint (LocatedL (BF.BooleanFormula (LocatedN RdrName))) where
  getAnnotationEntry = entryFromLocatedA
  setAnnotationAnchor = setAnchorAn
  exact (L an bf) = do
    debugM $ "LocatedL [LBooleanFormula"
    (an', bf') <- markAnnList an (markAnnotated bf)
    return (L an' bf')

-- ---------------------------------------------------------------------
-- LocatedL instances end --
-- =====================================================================

instance ExactPrint (IE GhcPs) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (IEVar depr ln doc) = do
    depr' <- markAnnotated depr
    ln' <- markAnnotated ln
    doc' <- markAnnotated doc
    return (IEVar depr' ln' doc')
  exact (IEThingAbs (depr, an) thing doc) = do
    depr' <- markAnnotated depr
    thing' <- markAnnotated thing
    doc' <- markAnnotated doc
    return (IEThingAbs (depr', an) thing' doc')
  exact (IEThingAll (depr, an) thing doc) = do
    depr' <- markAnnotated depr
    thing' <- markAnnotated thing
    an0 <- markEpAnnL an  lidl AnnOpenP
    an1 <- markEpAnnL an0 lidl AnnDotdot
    an2 <- markEpAnnL an1 lidl AnnCloseP
    doc' <- markAnnotated doc
    return (IEThingAll (depr', an2) thing' doc')

  exact (IEThingWith (depr, an) thing wc withs doc) = do
    depr' <- markAnnotated depr
    thing' <- markAnnotated thing
    an0 <- markEpAnnL an lidl AnnOpenP
    (an1, wc', withs') <-
      case wc of
        NoIEWildcard -> do
          withs'' <- markAnnotated withs
          return (an0, wc, withs'')
        IEWildcard pos -> do
          let (bs, as) = splitAt pos withs
          bs' <- markAnnotated bs
          an1 <- markEpAnnL an0 lidl AnnDotdot
          an2 <- markEpAnnL an1 lidl AnnComma
          as' <- markAnnotated as
          return (an2, wc, bs'++as')
    an2 <- markEpAnnL an1 lidl AnnCloseP
    doc' <- markAnnotated doc
    return (IEThingWith (depr', an2) thing' wc' withs' doc')

  exact (IEModuleContents (depr, an) m) = do
    depr' <- markAnnotated depr
    an0 <- markEpAnnL an lidl AnnModule
    m' <- markAnnotated m
    return (IEModuleContents (depr', an0) m')

  -- These three exist to not error out, but are no-ops The contents
  -- appear as "normal" comments too, which we process instead.
  exact (IEGroup x lev doc) = do
    return (IEGroup x lev doc)
  exact (IEDoc x doc) = do
    return (IEDoc x doc)
  exact (IEDocNamed x str) = do
    return (IEDocNamed x str)

-- ---------------------------------------------------------------------

instance ExactPrint (IEWrappedName GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (IEName x n) = do
    n' <- markAnnotated n
    return (IEName x n')
  exact (IEPattern r n) = do
    r' <- printStringAtAA r "pattern"
    n' <- markAnnotated n
    return (IEPattern r' n')
  exact (IEType r n) = do
    r' <- printStringAtAA r "type"
    n' <- markAnnotated n
    return (IEType r' n')

-- ---------------------------------------------------------------------

instance ExactPrint (Pat GhcPs) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (WildPat w) = do
    anchor' <- getAnchorU
    debugM $ "WildPat:anchor'=" ++ show anchor'
    _ <- printStringAtRs anchor' "_"
    return (WildPat w)
  exact (VarPat x n) = do
    -- The parser inserts a placeholder value for a record pun rhs. This must be
    -- filtered.
    let pun_RDR = "pun-right-hand-side"
    n' <- if (showPprUnsafe n /= pun_RDR)
      then markAnnotated n
      else return n
    return (VarPat x n')
  exact (LazyPat an pat) = do
    an0 <- markEpAnnL an lidl AnnTilde
    pat' <- markAnnotated pat
    return (LazyPat an0 pat')
  exact (AsPat at n pat) = do
    n' <- markAnnotated n
    at' <- markEpToken at
    pat' <- markAnnotated pat
    return (AsPat at' n' pat')
  exact (ParPat (lpar, rpar) pat) = do
    lpar' <- markEpToken lpar
    pat' <- markAnnotated pat
    rpar' <- markEpToken rpar
    return (ParPat (lpar', rpar') pat')

  exact (BangPat an pat) = do
    an0 <- markEpAnnL an lidl AnnBang
    pat' <- markAnnotated pat
    return (BangPat an0 pat')

  exact (ListPat an pats) = do
    (an', pats') <- markAnnList' an (markAnnotated pats)
    return (ListPat an' pats')

  exact (TuplePat an pats boxity) = do
    an0 <- case boxity of
             Boxed   -> markEpAnnL an lidl AnnOpenP
             Unboxed -> markEpAnnL an lidl AnnOpenPH
    pats' <- markAnnotated pats
    an1 <- case boxity of
             Boxed   -> markEpAnnL an0 lidl AnnCloseP
             Unboxed -> markEpAnnL an0 lidl AnnClosePH
    return (TuplePat an1 pats' boxity)

  exact (SumPat an pat alt arity) = do
    an0 <- markEpAnnL an lsumPatParens AnnOpenPH
    an1 <- markAnnKwAllL an0 lsumPatVbarsBefore AnnVbar
    pat' <- markAnnotated pat
    an2 <- markAnnKwAllL an1 lsumPatVbarsAfter AnnVbar
    an3 <- markEpAnnL an2 lsumPatParens AnnClosePH
    return (SumPat an3 pat' alt arity)

  exact (ConPat an con details) = do
    (an', con', details') <- exactUserCon an con details
    return (ConPat an' con' details')
  exact (ViewPat an expr pat) = do
    expr' <- markAnnotated expr
    an0 <- markEpAnnL an lidl AnnRarrow
    pat' <- markAnnotated pat
    return (ViewPat an0 expr' pat')
  exact (SplicePat x splice) = do
    splice' <- markAnnotated splice
    return (SplicePat x splice')
  exact p@(LitPat _ lit) = printStringAdvance (hsLit2String lit) >> return p
  exact (NPat an ol mn z) = do
    an0 <- if (isJust mn)
      then markEpAnnL an lidl AnnMinus
      else return an
    ol' <- markAnnotated ol
    return (NPat an0 ol' mn z)

  exact (NPlusKPat an n k lit2 a b) = do
    n' <- markAnnotated n
    an' <- printStringAtAAL an lid "+"
    k' <- markAnnotated k
    return (NPlusKPat an' n' k' lit2 a b)

  exact (SigPat an pat sig) = do
    pat' <- markAnnotated pat
    an0 <- markEpAnnL an lidl AnnDcolon
    sig' <- markAnnotated sig
    return (SigPat an0 pat' sig')

  exact (EmbTyPat toktype tp) = do
    toktype' <- markEpToken toktype
    tp' <- markAnnotated tp
    return (EmbTyPat toktype' tp')

  exact (InvisPat tokat tp) = do
    tokat' <- markEpToken tokat
    tp' <- markAnnotated tp
    pure (InvisPat tokat' tp')

-- ---------------------------------------------------------------------

instance ExactPrint (HsPatSigType GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (HsPS an ty) = do
    ty' <- markAnnotated ty
    return (HsPS an ty')

instance ExactPrint (HsTyPat GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact (HsTP an ty) = do
    ty' <- markAnnotated ty
    return (HsTP an ty')

-- ---------------------------------------------------------------------

instance ExactPrint (HsOverLit GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ _ = a

  exact ol =
    let str = case ol_val ol of
                HsIntegral   (IL src _ _) -> src
                HsFractional (FL{ fl_text = src }) -> src
                HsIsString src _ -> src
    in
      case str of
        SourceText s -> printStringAdvance (unpackFS s) >> return ol
        NoSourceText -> return ol

-- ---------------------------------------------------------------------

hsLit2String :: HsLit GhcPs -> String
hsLit2String lit =
  case lit of
    HsChar       src v   -> toSourceTextWithSuffix src v ""
    HsCharPrim   src p   -> toSourceTextWithSuffix src p ""
    HsString     src v   -> toSourceTextWithSuffix src v ""
    HsStringPrim src v   -> toSourceTextWithSuffix src v ""
    HsInt        _ (IL src _ v)   -> toSourceTextWithSuffix src v ""
    HsIntPrim    src v   -> toSourceTextWithSuffix src v ""
    HsWordPrim   src v   -> toSourceTextWithSuffix src v ""
    HsInt8Prim   src v   -> toSourceTextWithSuffix src v ""
    HsInt16Prim  src v   -> toSourceTextWithSuffix src v ""
    HsInt32Prim  src v   -> toSourceTextWithSuffix src v ""
    HsInt64Prim  src v   -> toSourceTextWithSuffix src v ""
    HsWord8Prim  src v   -> toSourceTextWithSuffix src v ""
    HsWord16Prim src v   -> toSourceTextWithSuffix src v ""
    HsWord32Prim src v   -> toSourceTextWithSuffix src v ""
    HsWord64Prim src v   -> toSourceTextWithSuffix src v ""
    HsInteger    src v _ -> toSourceTextWithSuffix src v ""
    HsRat        _ fl@(FL{fl_text = src }) _ -> toSourceTextWithSuffix src fl ""
    HsFloatPrim  _ fl@(FL{fl_text = src })   -> toSourceTextWithSuffix src fl "#"
    HsDoublePrim _ fl@(FL{fl_text = src })   -> toSourceTextWithSuffix src fl "##"

toSourceTextWithSuffix :: (Show a) => SourceText -> a -> String -> String
toSourceTextWithSuffix (NoSourceText)    alt suffix = show alt ++ suffix
toSourceTextWithSuffix (SourceText txt) _alt suffix = unpackFS txt ++ suffix

sourceTextToString :: SourceText -> String -> String
sourceTextToString NoSourceText alt   = alt
sourceTextToString (SourceText txt) _ = unpackFS txt

-- ---------------------------------------------------------------------

exactUserCon :: (Monad m, Monoid w, ExactPrint con)
  => [AddEpAnn] -> con -> HsConPatDetails GhcPs
  -> EP w m ([AddEpAnn], con, HsConPatDetails GhcPs)
exactUserCon an c (InfixCon p1 p2) = do
  p1' <- markAnnotated p1
  c' <- markAnnotated c
  p2' <- markAnnotated p2
  return (an, c', InfixCon p1' p2')
exactUserCon an c details = do
  c' <- markAnnotated c
  an0 <- markEpAnnL an lidl AnnOpenC
  details' <- exactConArgs details
  an1 <- markEpAnnL an0 lidl AnnCloseC
  return (an1, c', details')

instance ExactPrint (HsConPatTyArg GhcPs) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ _ = a
  exact (HsConPatTyArg at tyarg) = do
    at' <- markEpToken at
    tyarg' <- markAnnotated tyarg
    return (HsConPatTyArg at' tyarg')

exactConArgs :: (Monad m, Monoid w)
  => HsConPatDetails GhcPs -> EP w m (HsConPatDetails GhcPs)
exactConArgs (PrefixCon tyargs pats) = do
  tyargs' <- markAnnotated tyargs
  pats' <- markAnnotated pats
  return (PrefixCon tyargs' pats')
exactConArgs (InfixCon p1 p2) = do
  p1' <- markAnnotated p1
  p2' <- markAnnotated p2
  return (InfixCon p1' p2')
exactConArgs (RecCon rpats) = do
  rpats' <- markAnnotated rpats
  return (RecCon rpats')

-- ---------------------------------------------------------------------

entryFromLocatedA :: (HasTrailing ann) => LocatedAn ann a -> Entry
entryFromLocatedA (L la _) = fromAnn la

-- =====================================================================
-- Utility stuff
-- ---------------------------------------------------------------------

-- |This should be the final point where things are mode concrete,
-- before output.
-- NOTE: despite the name, this is the ghc-exactprint final output for
-- the PRINT phase.
printStringAtLsDelta :: (Monad m, Monoid w) => DeltaPos -> String -> EP w m ()
printStringAtLsDelta cl s = do
  p <- getPosP
  colOffset <- getLayoutOffsetP
  if isGoodDeltaWithOffset cl colOffset
    then do
      printStringAt (undelta p cl colOffset) s
      p' <- getPosP
      d <- getPriorEndD
      debugM $ "printStringAtLsDelta:(pos,p,p',d,s):" ++ show (undelta p cl colOffset,p,p',d,s)
    else return () `debug` ("printStringAtLsDelta:bad delta for (mc,s):" ++ show (cl,s))

-- ---------------------------------------------------------------------

isGoodDeltaWithOffset :: DeltaPos -> LayoutStartCol -> Bool
isGoodDeltaWithOffset dp colOffset = isGoodDelta (deltaPos l c)
  where (l,c) = undelta (0,0) dp colOffset

-- | Print a comment, using the current layout offset to convert the
-- @DeltaPos@ to an absolute position.
printQueuedComment :: (Monad m, Monoid w) => Comment -> DeltaPos -> EP w m ()
printQueuedComment Comment{commentContents} dp = do
  p <- getPosP
  d <- getPriorEndD
  colOffset <- getLayoutOffsetP
  let (dr,dc) = undelta (0,0) dp colOffset
  -- do not lose comments against the left margin
  when (isGoodDelta (deltaPos dr (max 0 dc))) $ do
    printCommentAt (undelta p dp colOffset) commentContents
  p' <- getPosP
  d' <- getPriorEndD
  debugM $ "printQueuedComment: (p,p',d,d')=" ++ show (p,p',d,d')
  debugM $ "printQueuedComment: (p,p',dp,colOffset,undelta)=" ++ show (p,p',dp,colOffset,undelta p dp colOffset)

------------------------------------------------------------------------

setLayoutBoth :: (Monad m, Monoid w) => EP w m a -> EP w m a
setLayoutBoth k = do
  oldLHS <- getLayoutOffsetD
  oldAnchorOffset <- getLayoutOffsetP
  debugM $ "setLayoutBoth: (oldLHS,oldAnchorOffset)=" ++ show (oldLHS,oldAnchorOffset)
  modify (\a -> a { dMarkLayout = True
                  , pMarkLayout = True } )
  let reset = do
        debugM $ "setLayoutBoth:reset: (oldLHS,oldAnchorOffset)=" ++ show (oldLHS,oldAnchorOffset)
        modify (\a -> a { dMarkLayout = False
                        , dLHS = oldLHS
                        , pMarkLayout = False
                        , pLHS = oldAnchorOffset} )
  k <* reset

-- Use 'local', designed for this
setLayoutTopLevelP :: (Monad m, Monoid w) => EP w m a -> EP w m a
setLayoutTopLevelP k = do
  debugM $ "setLayoutTopLevelP entered"
  oldAnchorOffset <- getLayoutOffsetP
  modify (\a -> a { pMarkLayout = False
                  , pLHS = 0} )
  r <- k
  debugM $ "setLayoutTopLevelP:resetting"
  setLayoutOffsetP oldAnchorOffset
  return r

------------------------------------------------------------------------

getPosP :: (Monad m, Monoid w) => EP w m Pos
getPosP = gets epPos

setPosP :: (Monad m, Monoid w) => Pos -> EP w m ()
setPosP l = do
  debugM $ "setPosP:" ++ show l
  modify (\s -> s {epPos = l})

getExtraDP :: (Monad m, Monoid w) => EP w m (Maybe Anchor)
getExtraDP = gets uExtraDP

setExtraDP :: (Monad m, Monoid w) => Maybe Anchor -> EP w m ()
setExtraDP md = do
  debugM $ "setExtraDP:" ++ show md
  modify (\s -> s {uExtraDP = md})

getPriorEndD :: (Monad m, Monoid w) => EP w m Pos
getPriorEndD = gets dPriorEndPosition

getAnchorU :: (Monad m, Monoid w) => EP w m RealSrcSpan
getAnchorU = gets uAnchorSpan

getAcceptSpan ::(Monad m, Monoid w) => EP w m Bool
getAcceptSpan = gets pAcceptSpan

setAcceptSpan ::(Monad m, Monoid w) => Bool -> EP w m ()
setAcceptSpan f =
  modify (\s -> s { pAcceptSpan = f })

setPriorEndD :: (Monad m, Monoid w) => Pos -> EP w m ()
setPriorEndD pe = do
  setPriorEndNoLayoutD pe

setPriorEndNoLayoutD :: (Monad m, Monoid w) => Pos -> EP w m ()
setPriorEndNoLayoutD pe = do
  debugM $ "setPriorEndNoLayoutD:pe=" ++ show pe
  modify (\s -> s { dPriorEndPosition = pe })

setPriorEndASTD :: (Monad m, Monoid w) => Bool -> RealSrcSpan -> EP w m ()
setPriorEndASTD layout pe = setPriorEndASTPD layout (rs2range pe)

setPriorEndASTPD :: (Monad m, Monoid w) => Bool -> (Pos,Pos) -> EP w m ()
setPriorEndASTPD layout pe@(fm,to) = do
  debugM $ "setPriorEndASTD:pe=" ++ show pe
  when layout $ setLayoutStartD (snd fm)
  modify (\s -> s { dPriorEndPosition = to } )

setLayoutStartD :: (Monad m, Monoid w) => Int -> EP w m ()
setLayoutStartD p = do
  EPState{dMarkLayout} <- get
  when dMarkLayout $ do
    debugM $ "setLayoutStartD: setting dLHS=" ++ show p
    modify (\s -> s { dMarkLayout = False
                    , dLHS = LayoutStartCol p})

getLayoutOffsetD :: (Monad m, Monoid w) => EP w m LayoutStartCol
getLayoutOffsetD = gets dLHS

setAnchorU :: (Monad m, Monoid w) => RealSrcSpan -> EP w m ()
setAnchorU rss = do
  debugM $ "setAnchorU:" ++ show (rs2range rss)
  modify (\s -> s { uAnchorSpan = rss })

getEofPos :: (Monad m, Monoid w) => EP w m (Maybe (RealSrcSpan, RealSrcSpan))
getEofPos = gets epEof

setEofPos :: (Monad m, Monoid w) => Maybe (RealSrcSpan, RealSrcSpan) -> EP w m ()
setEofPos l = modify (\s -> s {epEof = l})

-- ---------------------------------------------------------------------

getUnallocatedComments :: (Monad m, Monoid w) => EP w m [Comment]
getUnallocatedComments = gets epComments

putUnallocatedComments :: (Monad m, Monoid w) => [Comment] -> EP w m ()
putUnallocatedComments cs = modify (\s -> s { epComments = cs } )

-- | Push a fresh stack frame for the applied comments gatherer
pushAppliedComments  :: (Monad m, Monoid w) => EP w m ()
pushAppliedComments = modify (\s -> s { epCommentsApplied = []:(epCommentsApplied s) })

-- | Return the comments applied since the last call
-- takeAppliedComments, and clear them, not popping the stack
takeAppliedComments :: (Monad m, Monoid w) => EP w m [Comment]
takeAppliedComments = do
  ccs <- gets epCommentsApplied
  case ccs of
    [] -> do
      modify (\s -> s { epCommentsApplied = [] })
      return []
    h:t -> do
      modify (\s -> s { epCommentsApplied = []:t })
      return (reverse h)

-- | Return the comments applied since the last call
-- takeAppliedComments, and clear them, popping the stack
takeAppliedCommentsPop :: (Monad m, Monoid w) => EP w m [Comment]
takeAppliedCommentsPop = do
  ccs <- gets epCommentsApplied
  case ccs of
    [] -> do
      modify (\s -> s { epCommentsApplied = [] })
      return []
    h:t -> do
      modify (\s -> s { epCommentsApplied = t })
      return (reverse h)

-- | Mark a comment as being applied.  This is used to update comments
-- when doing delta processing
applyComment :: (Monad m, Monoid w) => Comment -> EP w m ()
applyComment c = do
  ccs <- gets epCommentsApplied
  case ccs of
    []    -> modify (\s -> s { epCommentsApplied = [[c]] } )
    (h:t) -> modify (\s -> s { epCommentsApplied = (c:h):t } )

getLayoutOffsetP :: (Monad m, Monoid w) => EP w m LayoutStartCol
getLayoutOffsetP = gets pLHS

setLayoutOffsetP :: (Monad m, Monoid w) => LayoutStartCol -> EP w m ()
setLayoutOffsetP c = do
  debugM $ "setLayoutOffsetP:" ++ show c
  modify (\s -> s { pLHS = c })


-- ---------------------------------------------------------------------

advance :: (Monad m, Monoid w) => DeltaPos -> EP w m ()
advance dp = do
  p <- getPosP
  colOffset <- getLayoutOffsetP
  debugM $ "advance:(p,dp,colOffset,ws)=" ++ show (p,dp,colOffset,undelta p dp colOffset)
  if isGoodDelta dp
    then do
      printWhitespace (undelta p dp colOffset)
      -- Sync point. We only call advance as we start the sub-span
      -- processing, so force the dPriorEndPosition to ???
      p0 <- getPosP
      d <- getPriorEndD
      r <- getAnchorU
      setPriorEndD (fst $ rs2range r)
      debugM $ "advance:after: (posp, posd, posd')=" ++ show (p0,d,fst $ rs2range r)
    else
      return ()

-- ---------------------------------------------------------------------

adjustDeltaForOffsetM :: (Monad m, Monoid w) => DeltaPos -> EP w m DeltaPos
adjustDeltaForOffsetM dp = do
  colOffset <- getLayoutOffsetD
  return (adjustDeltaForOffset colOffset dp)

-- ---------------------------------------------------------------------
-- Printing functions

printString :: (Monad m, Monoid w) => Bool -> String -> EP w m ()
printString layout str = do
  EPState{epPos = (_,c), pMarkLayout} <- get
  EPOptions{epTokenPrint, epWhitespacePrint} <- ask
  when (pMarkLayout && layout) $ do
    debugM $ "printString: setting pLHS to " ++ show c
    modify (\s -> s { pLHS = LayoutStartCol c, pMarkLayout = False } )

  -- Advance position, taking care of any newlines in the string
  let strDP = dpFromString str
      cr = getDeltaLine strDP
  p <- getPosP
  d <- getPriorEndD
  colOffsetP <- getLayoutOffsetP
  colOffsetD <- getLayoutOffsetD
  -- debugM $ "printString:(p,colOffset,strDP,cr)="  ++ show (p,colOffset,strDP,cr)
  if cr == 0
    then do
      setPosP      (undelta p strDP colOffsetP)
      setPriorEndD (undelta d strDP colOffsetD)
    else do
      setPosP      (undelta p strDP 1)
      setPriorEndD (undelta d strDP 1)

  -- Debug stuff
  -- pp <- getPosP
  -- debugM $ "printString: (p,pp,str)" ++ show (p,pp,str)
  -- Debug end

  --
  if not layout && c == 0
    then lift (epWhitespacePrint str) >>= \s -> tell EPWriter { output = s}
    else lift (epTokenPrint      str) >>= \s -> tell EPWriter { output = s}

--------------------------------------------------------

printStringAdvance :: (Monad m, Monoid w) => String -> EP w m ()
printStringAdvance str = do
  ss <- getAnchorU
  _ <- printStringAtRs ss str
  return ()

--------------------------------------------------------

newLine :: (Monad m, Monoid w) => EP w m ()
newLine = do
    (l,_) <- getPosP
    (ld,_) <- getPriorEndD
    printString False "\n"
    setPosP (l+1,1)
    setPriorEndNoLayoutD (ld+1,1)

padUntil :: (Monad m, Monoid w) => Pos -> EP w m ()
padUntil (l,c) = do
    (l1,c1) <- getPosP
    if | l1 == l && c1 <= c -> printString False $ replicate (c - c1) ' '
       | l1 < l             -> newLine >> padUntil (l,c)
       | otherwise          -> return ()

printWhitespace :: (Monad m, Monoid w) => Pos -> EP w m ()
printWhitespace = padUntil

printCommentAt :: (Monad m, Monoid w) => Pos -> String -> EP w m ()
printCommentAt p str = do
  debugM $ "printCommentAt: (pos,str)" ++ show (p,str)
  printWhitespace p >> printString False str

printStringAt :: (Monad m, Monoid w) => Pos -> String -> EP w m ()
printStringAt p str = printWhitespace p >> printString True str
