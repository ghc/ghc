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

import Control.Monad.Identity
import qualified Control.Monad.Reader as Reader
import Control.Monad.RWS
import Data.Data ( Data )
import Data.Dynamic
import Data.Foldable
import Data.Functor.Const
import qualified Data.Set as Set
import Data.Typeable
import Data.List ( partition, sort, sortBy)
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
             , dLHS       = 1
             , pMarkLayout = False
             , pLHS = 1
             , dMarkLayout = False
             , dPriorEndPosition = (1,1)
             , uAnchorSpan = badRealSrcSpan
             , uExtraDP = Nothing
             , epComments = []
             , epCommentsApplied = []
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
             }

-- ---------------------------------------------------------------------

-- AZ:TODO: this can just be a function :: (EpAnn a) -> Entry
class HasEntry ast where
  fromAnn :: ast -> Entry

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

data Entry = Entry Anchor EpAnnComments FlushComments CanUpdateAnchor
           | NoEntryVal

-- | For flagging whether to capture comments in an EpaDelta or not
data CaptureComments = CaptureComments
                     | NoCaptureComments

mkEntry :: Anchor -> EpAnnComments -> Entry
mkEntry anc cs = Entry anc cs NoFlushComments CanUpdateAnchor

instance HasEntry (SrcSpanAnn' (EpAnn an)) where
  fromAnn (SrcSpanAnn EpAnnNotUsed ss) = mkEntry (spanAsAnchor ss) emptyComments
  fromAnn (SrcSpanAnn an _) = fromAnn an

instance HasEntry (EpAnn a) where
  fromAnn (EpAnn anchor _ cs) = mkEntry anchor cs
  fromAnn EpAnnNotUsed = NoEntryVal

-- ---------------------------------------------------------------------

fromAnn' :: (HasEntry a) => a -> Entry
fromAnn' an = case fromAnn an of
  NoEntryVal -> NoEntryVal
  Entry a c _ u -> Entry a c' FlushComments u
    where
      c' = case c of
        EpaComments cs -> EpaCommentsBalanced (filterEofComment False cs) (filterEofComment True cs)
        EpaCommentsBalanced cp ct -> EpaCommentsBalanced cp ct

-- ---------------------------------------------------------------------

astId :: (Typeable a) => a -> String
astId a = show (typeOf a)

cua :: (Monad m, Monoid w) => CanUpdateAnchor -> EP w m [a] -> EP w m [a]
cua CanUpdateAnchor f = f
cua CanUpdateAnchorOnly _ = return []
cua NoCanUpdateAnchor _ = return []

-- | "Enter" an annotation, by using the associated 'anchor' field as
-- the new reference point for calculating all DeltaPos positions.
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
enterAnn (Entry anchor' cs flush canUpdateAnchor) a = do
  p <- getPosP
  debugM $ "enterAnn:starting:(p,a) =" ++ show (p, astId a)
  -- debugM $ "enterAnn:(cs) =" ++ showGhc (cs)
  let curAnchor = anchor anchor' -- As a base for the current AST element
  debugM $ "enterAnn:(curAnchor):=" ++ show (rs2range curAnchor)
  case canUpdateAnchor of
    CanUpdateAnchor -> pushAppliedComments
    _ -> return ()
  addCommentsA (priorComments cs)
  debugM $ "enterAnn:Added comments"
  printComments curAnchor
  priorCs <- cua canUpdateAnchor takeAppliedComments -- no pop
  -- -------------------------
  case anchor_op anchor' of
    MovedAnchor dp -> do
      debugM $ "enterAnn: MovedAnchor:" ++ show dp
      -- Set the original anchor as prior end, so the rest of this AST
      -- fragment has a reference
      setPriorEndNoLayoutD (ss2pos curAnchor)
    _ -> do
      return ()
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
  let edp'' = case anchor_op anchor' of
        MovedAnchor dp -> dp
        _ -> edp'
  -- ---------------------------------------------
  -- let edp = edp''
  med <- getExtraDP
  setExtraDP Nothing
  let edp = case med of
        Nothing -> edp''
        Just (Anchor _ (MovedAnchor dp)) -> dp
                   -- Replace original with desired one. Allows all
                   -- list entry values to be DP (1,0)
        Just (Anchor r _) -> dp
          where
            dp = adjustDeltaForOffset
                   off (ss2delta priorEndAfterComments r)
  when (isJust med) $ debugM $ "enterAnn:(med,edp)=" ++ show (med,edp)
  -- ---------------------------------------------
  -- Preparation complete, perform the action
  when (priorEndAfterComments < spanStart) (do
    debugM $ "enterAnn.dPriorEndPosition:spanStart=" ++ show spanStart
    modify (\s -> s { dPriorEndPosition    = spanStart } ))

  debugM $ "enterAnn: (anchor_op, curAnchor):" ++ show (anchor_op anchor', rs2range curAnchor)
  debugM $ "enterAnn: (dLHS,spanStart,pec,edp)=" ++ show (off,spanStart,priorEndAfterComments,edp)
  p0 <- getPosP
  d <- getPriorEndD
  debugM $ "enterAnn: (posp, posd)=" ++ show (p0,d)

  -- end of delta phase processing
  -- -------------------------------------------------------------------
  -- start of print phase processing

  let mflush = when (flush == FlushComments) $ do
        debugM $ "flushing comments in enterAnn:" ++ showAst cs
        flushComments (getFollowingComments cs ++ filterEofComment True (priorComments cs))

  advance edp
  a' <- exact a
  mflush

  -- end of sub-Anchor processing, start of tail end processing
  postCs <- cua canUpdateAnchor takeAppliedCommentsPop
  when (flush == NoFlushComments) $ do
    when ((getFollowingComments cs) /= []) $ do
      debugM $ "starting trailing comments:" ++ showAst (getFollowingComments cs)
      mapM_ printOneComment (map tokComment $ getFollowingComments cs)
      debugM $ "ending trailing comments"

  let newAchor = anchor' { anchor_op = MovedAnchor edp }
  let r = case canUpdateAnchor of
            CanUpdateAnchor -> setAnnotationAnchor a' newAchor (mkEpaComments (priorCs++ postCs) [])
            CanUpdateAnchorOnly -> setAnnotationAnchor a' newAchor emptyComments
            NoCanUpdateAnchor -> a'
  -- debugM $ "calling setAnnotationAnchor:(curAnchor, newAchor,priorCs,postCs)=" ++ showAst (show (rs2range curAnchor), newAchor, priorCs, postCs)
  -- debugM $ "calling setAnnotationAnchor:(newAchor,postCs)=" ++ showAst (newAchor, postCs)
  debugM $ "enterAnn:done:(p,a) =" ++ show (p0, astId a')
  return r

-- ---------------------------------------------------------------------

addCommentsA :: (Monad m, Monoid w) => [LEpaComment] -> EP w m ()
addCommentsA csNew = addComments (map tokComment csNew)

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
flushComments trailing = do
  addCommentsA (filterEofComment False trailing)
  cs <- getUnallocatedComments
  debugM $ "flushing comments starting"
  mapM_ printOneComment (sortComments cs)
  debugM $ "flushing comments:EOF:trailing:" ++ showAst (trailing)
  debugM $ "flushing comments:EOF:" ++ showAst (filterEofComment True trailing)
  mapM_ printOneComment (map tokComment (filterEofComment True trailing))
  debugM $ "flushing comments done"

filterEofComment :: Bool -> [LEpaComment] -> [LEpaComment]
filterEofComment keep cs = fixCs cs
  where
      notEof com = case com of
       L _ (GHC.EpaComment (EpaEofComment) _) -> keep
       _ -> not keep
      fixCs c = filter notEof c

-- ---------------------------------------------------------------------

-- |In order to interleave annotations into the stream, we turn them into
-- comments. They are removed from the annotation to avoid duplication.
annotationsToComments :: (Monad m, Monoid w)
  => EpAnn a -> Lens a [AddEpAnn] -> [AnnKeywordId] -> EP w m (EpAnn a)
annotationsToComments EpAnnNotUsed _ _kws = return EpAnnNotUsed
annotationsToComments (EpAnn anc a cs) l kws = do
  let (newComments, newAnns) = go ([],[]) (view l a)
  addComments newComments
  return (EpAnn anc (set l (reverse newAnns) a) cs)
  where
    keywords = Set.fromList kws

    go :: ([Comment], [AddEpAnn]) -> [AddEpAnn] -> ([Comment], [AddEpAnn])
    go acc [] = acc
    go (cs',ans) ((AddEpAnn k ss) : ls)
      | Set.member k keywords = go ((mkKWComment k ss):cs', ans) ls
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
  setAnnotationAnchor :: a -> Anchor -> EpAnnComments -> a
  exact :: (Monad m, Monoid w) => a -> EP w m a

-- ---------------------------------------------------------------------
-- Start of utility functions
-- ---------------------------------------------------------------------

printSourceText :: (Monad m, Monoid w) => SourceText -> String -> EP w m ()
printSourceText (NoSourceText) txt   =  printStringAdvance txt >> return ()
printSourceText (SourceText   txt) _ =  printStringAdvance txt >> return ()

-- ---------------------------------------------------------------------

printStringAtSs :: (Monad m, Monoid w) => SrcSpan -> String -> EP w m ()
printStringAtSs ss str = printStringAtRs (realSrcSpan ss) str >> return ()

printStringAtRs :: (Monad m, Monoid w) => RealSrcSpan -> String -> EP w m EpaLocation
printStringAtRs pa str = printStringAtRsC CaptureComments pa str

printStringAtRsC :: (Monad m, Monoid w)
  => CaptureComments -> RealSrcSpan -> String -> EP w m EpaLocation
printStringAtRsC capture pa str = do
  debugM $ "printStringAtRsC: pa=" ++ showAst pa
  printComments pa
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
  debugM $ "printStringAtRsC: (EpaDelta p' [])=" ++ showAst (EpaDelta p' [])
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
printStringAtMLocL EpAnnNotUsed _ _ = return EpAnnNotUsed
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

printStringAtAAL :: (Monad m, Monoid w)
  => EpAnn a -> Lens a EpaLocation -> String -> EP w m (EpAnn a)
printStringAtAAL EpAnnNotUsed _ _ = return EpAnnNotUsed
printStringAtAAL (EpAnn anc an cs) l str = do
  r <- printStringAtAAC CaptureComments (view l an) str
  return (EpAnn anc (set l r an) cs)

printStringAtAAC :: (Monad m, Monoid w)
  => CaptureComments -> EpaLocation -> String -> EP w m EpaLocation
printStringAtAAC capture (EpaSpan r _) s = printStringAtRsC capture r s
printStringAtAAC capture (EpaDelta d cs) s = do
  mapM_ (printOneComment . tokComment) cs
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

markExternalSourceText :: (Monad m, Monoid w) => SrcSpan -> SourceText -> String -> EP w m ()
markExternalSourceText l NoSourceText txt   = printStringAtRs (realSrcSpan l) txt >> return ()
markExternalSourceText l (SourceText txt) _ = printStringAtRs (realSrcSpan l) txt >> return ()

-- ---------------------------------------------------------------------

markLensMAA :: (Monad m, Monoid w) => EpAnn a -> Lens a (Maybe AddEpAnn) -> EP w m (EpAnn a)
markLensMAA EpAnnNotUsed  _  = return EpAnnNotUsed
markLensMAA (EpAnn anc a cs) l =
  case view l a of
    Nothing -> return (EpAnn anc a cs)
    Just aa -> do
      aa' <- markAddEpAnn aa
      return (EpAnn anc (set l (Just aa') a) cs)

markLensAA :: (Monad m, Monoid w) => EpAnn a -> Lens a AddEpAnn -> EP w m (EpAnn a)
markLensAA EpAnnNotUsed  _  = return EpAnnNotUsed
markLensAA (EpAnn anc a cs) l = do
  a' <- markKw (view l a)
  return (EpAnn anc (set l a' a) cs)


markEpAnnLMS :: (Monad m, Monoid w)
  => EpAnn a -> Lens a [AddEpAnn] -> AnnKeywordId -> Maybe String -> EP w m (EpAnn a)
markEpAnnLMS an l kw Nothing = markEpAnnL an l kw
markEpAnnLMS EpAnnNotUsed  _ _ _ = return EpAnnNotUsed
markEpAnnLMS (EpAnn anc a cs) l kw (Just str) = do
  anns <- mapM go (view l a)
  return (EpAnn anc (set l anns a) cs)
  where
    go :: (Monad m, Monoid w) => AddEpAnn -> EP w m AddEpAnn
    go (AddEpAnn kw' r)
      | kw' == kw = do
          r' <- printStringAtAA r str
          return (AddEpAnn kw' r')
      | otherwise = return (AddEpAnn kw' r)

markEpAnnLMS' :: (Monad m, Monoid w)
                => EpAnn a -> Lens a AddEpAnn -> AnnKeywordId -> Maybe String -> EP w m (EpAnn a)
markEpAnnLMS' an l _kw Nothing = markLensKwA an l
markEpAnnLMS' EpAnnNotUsed  _ _ _ = return EpAnnNotUsed
markEpAnnLMS' (EpAnn anc a cs) l kw (Just str) = do
  anns <- go (view l a)
  return (EpAnn anc (set l anns a) cs)
  where
    go :: (Monad m, Monoid w) => AddEpAnn -> EP w m AddEpAnn
    go (AddEpAnn kw' r)
      | kw' == kw = do
          r' <- printStringAtAA r str
          return (AddEpAnn kw' r')
      | otherwise = return (AddEpAnn kw' r)

-- ---------------------------------------------------------------------

markToken :: forall m w tok . (Monad m, Monoid w, KnownSymbol tok)
  => LHsToken tok GhcPs -> EP w m (LHsToken tok GhcPs)
markToken (L NoTokenLoc t) = return (L NoTokenLoc t)
markToken (L (TokenLoc aa) t) = do
  aa' <- printStringAtAA aa (symbolVal (Proxy @tok))
  return (L (TokenLoc aa') t)

markUniToken :: forall m w tok utok. (Monad m, Monoid w, KnownSymbol tok, KnownSymbol utok)
  => LHsUniToken tok utok GhcPs -> EP w m (LHsUniToken tok utok GhcPs)
markUniToken (L l HsNormalTok)  = do
  (L l' _) <- markToken (L l (HsTok @tok))
  return (L l' HsNormalTok)
markUniToken (L l HsUnicodeTok) = do
  (L l' _) <- markToken (L l (HsTok @utok))
  return (L l' HsUnicodeTok)

-- ---------------------------------------------------------------------

markArrow :: (Monad m, Monoid w) => HsArrow GhcPs -> EP w m (HsArrow GhcPs)
markArrow (HsUnrestrictedArrow arr) = do
  arr' <- markUniToken arr
  return (HsUnrestrictedArrow arr')
markArrow (HsLinearArrow (HsPct1 pct1 arr)) = do
  pct1' <- markToken pct1
  arr' <- markUniToken arr
  return (HsLinearArrow (HsPct1 pct1' arr'))
markArrow (HsLinearArrow (HsLolly arr)) = do
  arr' <- markToken arr
  return (HsLinearArrow (HsLolly arr'))
markArrow (HsExplicitMult pct t arr) = do
  pct' <- markToken pct
  t' <- markAnnotated t
  arr' <- markUniToken arr
  return (HsExplicitMult pct' t' arr')

-- ---------------------------------------------------------------------

markAnnCloseP :: (Monad m, Monoid w) => EpAnn AnnPragma -> EP w m (EpAnn AnnPragma)
markAnnCloseP an = markEpAnnLMS' an lapr_close AnnClose (Just "#-}")

markAnnOpenP :: (Monad m, Monoid w) => EpAnn AnnPragma -> SourceText -> String -> EP w m (EpAnn AnnPragma)
markAnnOpenP an NoSourceText txt   = markEpAnnLMS' an lapr_open AnnOpen (Just txt)
markAnnOpenP an (SourceText txt) _ = markEpAnnLMS' an lapr_open AnnOpen (Just txt)

markAnnOpen :: (Monad m, Monoid w) => EpAnn [AddEpAnn] -> SourceText -> String -> EP w m (EpAnn [AddEpAnn])
markAnnOpen an NoSourceText txt   = markEpAnnLMS an lidl AnnOpen (Just txt)
markAnnOpen an (SourceText txt) _ = markEpAnnLMS an lidl AnnOpen (Just txt)

markAnnOpen' :: (Monad m, Monoid w)
  => Maybe EpaLocation -> SourceText -> String -> EP w m (Maybe EpaLocation)
markAnnOpen' ms NoSourceText txt   = printStringAtMLoc' ms txt
markAnnOpen' ms (SourceText txt) _ = printStringAtMLoc' ms txt

markAnnOpen'' :: (Monad m, Monoid w)
  => EpaLocation -> SourceText -> String -> EP w m EpaLocation
markAnnOpen'' el NoSourceText txt   = printStringAtAA el txt
markAnnOpen'' el (SourceText txt) _ = printStringAtAA el txt

-- ---------------------------------------------------------------------
{-
data AnnParen
  = AnnParen {
      ap_adornment :: ParenType,
      ap_open      :: EpaLocation,
      ap_close     :: EpaLocation
      } deriving (Data)
-}
markOpeningParen, markClosingParen :: (Monad m, Monoid w) => EpAnn AnnParen -> EP w m (EpAnn AnnParen)
markOpeningParen an = markParen an lfst
markClosingParen an = markParen an lsnd

markParen :: (Monad m, Monoid w) => EpAnn AnnParen -> (forall a. Lens (a,a) a) -> EP w m (EpAnn AnnParen)
markParen EpAnnNotUsed _ = return (EpAnnNotUsed)
markParen (EpAnn anc (AnnParen pt o c) cs) l = do
  loc' <- markKwA (view l $ kw pt) (view l (o, c))
  let (o',c') = set l loc' (o,c)
  return (EpAnn anc (AnnParen pt o' c') cs)
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

-- data AnnsModule
--   = AnnsModule {
--     am_main :: [AddEpAnn],
--     am_decls :: AnnList
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

lal_trailing :: Lens AnnList [TrailingAnn]
lal_trailing k parent = fmap (\new -> parent { al_trailing = new })
                           (k (al_trailing parent))

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
  => EpAnn a -> Lens a AddEpAnn -> EP w m (EpAnn a)
markLensKwA EpAnnNotUsed  _    = return EpAnnNotUsed
markLensKwA (EpAnn anc a cs) l = do
  loc <- markKw (view l a)
  return (EpAnn anc (set l loc a) cs)

markLensKw :: (Monad m, Monoid w)
  => EpAnn a -> Lens a EpaLocation -> AnnKeywordId -> EP w m (EpAnn a)
markLensKw EpAnnNotUsed  _ _  = return EpAnnNotUsed
markLensKw (EpAnn anc a cs) l kw = do
  loc <- markKwA kw (view l a)
  return (EpAnn anc (set l loc a) cs)

markAnnKwL :: (Monad m, Monoid w)
  => EpAnn a -> Lens a EpaLocation -> AnnKeywordId -> EP w m (EpAnn a)
markAnnKwL = markLensKw

markAnnKwAllL :: (Monad m, Monoid w)
  => EpAnn a -> Lens a [EpaLocation] -> AnnKeywordId -> EP w m (EpAnn a)
markAnnKwAllL EpAnnNotUsed  _ _  = return EpAnnNotUsed
markAnnKwAllL (EpAnn anc a cs) l kw = do
  anns <- mapM (markKwA kw) (view l a)
  return (EpAnn anc (set l anns a) cs)

markLensKwM :: (Monad m, Monoid w)
  => EpAnn a -> Lens a (Maybe EpaLocation) -> AnnKeywordId -> EP w m (EpAnn a)
markLensKwM EpAnnNotUsed  _ _ = return EpAnnNotUsed
markLensKwM (EpAnn anc a cs) l kw = do
  new <- go (view l a)
  return (EpAnn anc (set l new a) cs)
  where
    go Nothing = return Nothing
    go (Just s) = Just <$> markKwA kw s

-- ---------------------------------------------------------------------

markALocatedA :: (Monad m, Monoid w) => EpAnn AnnListItem -> EP w m (EpAnn AnnListItem)
markALocatedA EpAnnNotUsed  = return EpAnnNotUsed
markALocatedA (EpAnn anc a cs) = do
  t <- markTrailing (lann_trailing a)
  return (EpAnn anc (a { lann_trailing = t }) cs)

markEpAnnL :: (Monad m, Monoid w)
  => EpAnn ann -> Lens ann [AddEpAnn] -> AnnKeywordId -> EP w m (EpAnn ann)
markEpAnnL EpAnnNotUsed _ _ = return EpAnnNotUsed
markEpAnnL (EpAnn anc a cs) l kw = do
  anns <- mark' (view l a) kw
  return (EpAnn anc (set l anns a) cs)

markEpAnnAllL :: (Monad m, Monoid w)
  => EpAnn ann -> Lens ann [AddEpAnn] -> AnnKeywordId -> EP w m (EpAnn ann)
markEpAnnAllL EpAnnNotUsed _ _ = return EpAnnNotUsed
markEpAnnAllL (EpAnn anc a cs) l kw = do
  anns <- mapM doit (view l a)
  return (EpAnn anc (set l anns a) cs)
  where
    doit an@(AddEpAnn ka _)
      = if ka == kw
          then markKw an
          else return an

markAddEpAnn :: (Monad m, Monoid w) => AddEpAnn -> EP w m AddEpAnn
markAddEpAnn a@(AddEpAnn kw _) = do
  r <- mark' [a] kw
  case r of
    [a'] -> return a'
    _ -> error "Should not happen: markAddEpAnn"

mark' :: (Monad m, Monoid w) => [AddEpAnn] -> AnnKeywordId -> EP w m [AddEpAnn]
mark' anns kw = do
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

-- ---------------------------------------------------------------------

markAnnList :: (Monad m, Monoid w)
  => Bool -> EpAnn AnnList -> EP w m a -> EP w m (EpAnn AnnList, a)
markAnnList reallyTrail ann action = do
  markAnnListA reallyTrail ann $ \a -> do
    r <- action
    return (a,r)

markAnnListA :: (Monad m, Monoid w)
  => Bool -> EpAnn AnnList
  -> (EpAnn AnnList -> EP w m (EpAnn AnnList, a))
  -> EP w m (EpAnn AnnList, a)
markAnnListA _ EpAnnNotUsed action = do
  action EpAnnNotUsed
markAnnListA reallyTrail an action = do
  debugM $ "markAnnListA: an=" ++ showAst an
  an0 <- markLensMAA an lal_open
  an1 <- if (not reallyTrail)
           then markTrailingL an0 lal_trailing
           else return an0
  an2 <- markEpAnnAllL an1 lal_rest AnnSemi
  (an3, r) <- action an2
  an4 <- markLensMAA an3 lal_close
  an5 <- if reallyTrail
           then markTrailingL an4 lal_trailing
           else return an4
  debugM $ "markAnnListA: an5=" ++ showAst an
  return (an5, r)


markAnnList' :: (Monad m, Monoid w)
  => Bool -> EpAnn AnnList -> EP w m a -> EP w m (EpAnn AnnList, a)
markAnnList' reallyTrail an action = do
  p <- getPosP
  debugM $ "markAnnList : " ++ showPprUnsafe (p, an)
  an0 <- markLensMAA an lal_open
  an1 <- if (not reallyTrail)
           then markTrailingL an0 lal_trailing
           else return an0
  an2 <- markEpAnnAllL an1 lal_rest AnnSemi
  r <- action
  an3 <- markLensMAA an2 lal_close
  an4 <- if reallyTrail
           then markTrailingL an3 lal_trailing
           else return an3
  return (an4, r)

-- ---------------------------------------------------------------------

printComments :: (Monad m, Monoid w) => RealSrcSpan -> EP w m ()
printComments ss = do
  cs <- commentAllocation ss
  debugM $ "printComments: (ss): " ++ showPprUnsafe (rs2range ss)
  -- debugM $ "printComments: (ss,comment locations): " ++ showPprUnsafe (rs2range ss,map commentAnchor cs)
  mapM_ printOneComment cs

-- ---------------------------------------------------------------------

printOneComment :: (Monad m, Monoid w) => Comment -> EP w m ()
printOneComment c@(Comment _str loc _r _mo) = do
  debugM $ "printOneComment:c=" ++ showGhc c
  dp <-case anchor_op loc of
    MovedAnchor dp -> return dp
    _ -> do
        pe <- getPriorEndD
        let dp = ss2delta pe (anchor loc)
        debugM $ "printOneComment:(dp,pe,anchor loc)=" ++ showGhc (dp,pe,ss2pos $ anchor loc)
        adjustDeltaForOffsetM dp
  mep <- getExtraDP
  dp' <- case mep of
    Just (Anchor _ (MovedAnchor edp)) -> do
      debugM $ "printOneComment:edp=" ++ show edp
      ddd <- fmap unTweakDelta $ adjustDeltaForOffsetM edp
      debugM $ "printOneComment:ddd=" ++ show ddd
      fmap unTweakDelta $ adjustDeltaForOffsetM edp
    _ -> return dp
  -- Start of debug printing
  -- LayoutStartCol dOff <- getLayoutOffsetD
  -- debugM $ "printOneComment:(dp,dp',dOff)=" ++ showGhc (dp,dp',dOff)
  -- End of debug printing
  -- setPriorEndD (ss2posEnd (anchor loc))
  updateAndApplyComment c dp'
  printQueuedComment (anchor loc) c dp'

-- | For comment-related deltas starting on a new line we have an
-- off-by-one problem. Adjust
unTweakDelta :: DeltaPos  -> DeltaPos
unTweakDelta (SameLine d) = SameLine d
unTweakDelta (DifferentLine l d) = DifferentLine l (d+1)


updateAndApplyComment :: (Monad m, Monoid w) => Comment -> DeltaPos -> EP w m ()
updateAndApplyComment (Comment str anc pp mo) dp = do
  -- debugM $ "updateAndApplyComment: (dp,anc',co)=" ++ showAst (dp,anc',co)
  applyComment (Comment str anc' pp mo)
  where
    anc' = anc { anchor_op = op}

    (r,c) = ss2posEnd pp
    la = anchor anc
    dp'' = if r == 0
           then (ss2delta (r,c+0) la)
           else (ss2delta (r,c)   la)
    dp' = if pp == anchor anc
             then dp
             else dp''
    op' = case dp' of
            SameLine n -> if n >= 0
                            then MovedAnchor dp'
                            else MovedAnchor dp
            _ -> MovedAnchor dp'
    op = if str == "" && op' == MovedAnchor (SameLine 0) -- EOF comment
           then MovedAnchor dp
           -- else op'
           else MovedAnchor dp

-- ---------------------------------------------------------------------

commentAllocation :: (Monad m, Monoid w) => RealSrcSpan -> EP w m [Comment]
commentAllocation ss = do
  cs <- getUnallocatedComments
  -- Note: The CPP comment injection may change the file name in the
  -- RealSrcSpan, which affects comparison, as the Ord instance for
  -- RealSrcSpan compares the file first. So we sort via ss2pos
  -- TODO: this is inefficient, use Pos all the way through
  let (earlier,later) = partition (\(Comment _str loc _r _mo) -> (ss2pos $ anchor loc) <= (ss2pos ss)) cs
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
    _ -> Entry (hackSrcSpanToAnchor l) emptyComments NoFlushComments CanUpdateAnchorOnly

  setAnnotationAnchor (L _ a) anc _cs = (L (hackAnchorToSrcSpan anc) a)
                 `debug` ("setAnnotationAnchor(Located):" ++ showAst anc)

  exact (L l a) = L l <$> markAnnotated a

instance (ExactPrint a) => ExactPrint (LocatedA a) where
  getAnnotationEntry = entryFromLocatedA
  setAnnotationAnchor la anc cs = setAnchorAn la anc cs
  exact (L la a) = do
    debugM $ "LocatedA a:la loc=" ++ show (ss2range $ locA la)
    a' <- markAnnotated a
    ann' <- markALocatedA (ann la)
    return (L (la { ann = ann'}) a')

instance (ExactPrint a) => ExactPrint (LocatedAn NoEpAnns a) where
  getAnnotationEntry = entryFromLocatedA
  setAnnotationAnchor la anc cs = setAnchorAn la anc cs
  exact (L la a) = do
    a' <- markAnnotated a
    return (L la a')

instance (ExactPrint a) => ExactPrint [a] where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor ls _ _ = ls
  exact ls = mapM markAnnotated ls

instance (ExactPrint a) => ExactPrint (Maybe a) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor ma _ _ = ma
  exact ma = mapM markAnnotated ma

-- ---------------------------------------------------------------------

-- | 'Located (HsModule GhcPs)' corresponds to 'ParsedSource'
instance ExactPrint (HsModule GhcPs) where
  getAnnotationEntry hsmod = fromAnn' (hsmodAnn $ hsmodExt hsmod)
  -- A bit pointless actually changing anything here
  setAnnotationAnchor hsmod anc cs = setAnchorHsModule hsmod anc cs
                   `debug` ("setAnnotationAnchor hsmod called" ++ showAst (anc,cs))

  exact hsmod@(HsModule {hsmodExt = XModulePs { hsmodAnn = EpAnnNotUsed }}) = withPpr hsmod >> return hsmod
  exact (HsModule (XModulePs an lo mdeprec mbDoc) mmn mexports imports decls) = do

    mbDoc' <- markAnnotated mbDoc

    (an0, mmn' , mdeprec', mexports') <-
      case mmn of
        Nothing -> return (an, mmn, mdeprec, mexports)
        Just m -> do
          an0 <- markEpAnnL an lam_main AnnModule
          m' <- markAnnotated m

          mdeprec' <- setLayoutTopLevelP $ markAnnotated mdeprec

          mexports' <- setLayoutTopLevelP $ markAnnotated mexports

          an1 <- setLayoutTopLevelP $ markEpAnnL an0 lam_main AnnWhere

          return (an1, Just m', mdeprec', mexports')

    let ann_decls = EpAnn (entry an) (am_decls $ anns an0) emptyComments
    (ann_decls', (decls', imports')) <- markAnnList' False ann_decls $ do
      imports' <- markTopLevelList imports
      decls' <- markTopLevelList decls
      return (decls', imports')
    let am_decls' = case ann_decls' of
          EpAnnNotUsed -> (am_decls $ anns an0)
          EpAnn _ r _ -> r

    let anf = an0 { anns = (anns an0) { am_decls = am_decls' }}
    debugM $ "HsModule, anf=" ++ showAst anf

    return (HsModule (XModulePs anf lo mdeprec' mbDoc') mmn' mexports' imports' decls')

-- ---------------------------------------------------------------------

instance ExactPrint ModuleName where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor n _anc cs = n
     `debug` ("ModuleName.setAnnotationAnchor:cs=" ++ showAst cs)
  exact n = do
    debugM $ "ModuleName: " ++ showPprUnsafe n
    withPpr n

-- ---------------------------------------------------------------------

instance ExactPrint (LocatedP (WarningTxt GhcPs)) where
  getAnnotationEntry = entryFromLocatedA
  setAnnotationAnchor = setAnchorAn

  exact (L (SrcSpanAnn an l) (WarningTxt (L la src) ws)) = do
    an0 <- markAnnOpenP an src "{-# WARNING"
    an1 <- markEpAnnL an0 lapr_rest AnnOpenS
    ws' <- markAnnotated ws
    an2 <- markEpAnnL an1 lapr_rest AnnCloseS
    an3 <- markAnnCloseP an2
    return (L (SrcSpanAnn an3 l) (WarningTxt (L la src) ws'))

  exact (L (SrcSpanAnn an l) (DeprecatedTxt (L ls src) ws)) = do
    an0 <- markAnnOpenP an src "{-# DEPRECATED"
    an1 <- markEpAnnL an0 lapr_rest AnnOpenS
    ws' <- markAnnotated ws
    an2 <- markEpAnnL an1 lapr_rest AnnCloseS
    an3 <- markAnnCloseP an2
    return (L (SrcSpanAnn an3 l) (DeprecatedTxt (L ls src) ws'))

-- ---------------------------------------------------------------------

instance ExactPrint (ImportDecl GhcPs) where
  getAnnotationEntry idecl = fromAnn (ideclAnn $ ideclExt idecl)
  setAnnotationAnchor idecl anc cs = idecl { ideclExt
                    = (ideclExt idecl) { ideclAnn = setAnchorEpa (ideclAnn $ ideclExt idecl) anc cs} }

  exact x@(ImportDecl{ ideclExt = XImportDeclPass{ ideclAnn = EpAnnNotUsed } }) = withPpr x
  exact (ImportDecl (XImportDeclPass ann msrc impl)
                     modname mpkg src safeflag qualFlag mAs hiding) = do

    ann0 <- markLensKw ann limportDeclAnnImport AnnImport
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
  setAnnotationAnchor a _ _ = a
  exact ds = do
    (printStringAdvance . exactPrintHsDocString) ds
    return ds

instance ExactPrint a => ExactPrint (WithHsDocIdentifiers a GhcPs) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ = a
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
  setAnnotationAnchor d _ _ = d

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

  setAnnotationAnchor d _ _ = d


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
    { _dc_a :: EpAnn [AddEpAnn]
    , _dc_f :: TopLevelFlag
    , dc_d :: DataFamInstDecl GhcPs
    }

instance ExactPrint DataFamInstDeclWithContext where
  getAnnotationEntry (DataFamInstDeclWithContext _ _ (DataFamInstDecl (FamEqn { feqn_ext = an})))
    = fromAnn an
  setAnnotationAnchor (DataFamInstDeclWithContext a c (DataFamInstDecl fe)) anc cs
    = (DataFamInstDeclWithContext a c (DataFamInstDecl (fe { feqn_ext = (setAnchorEpa (feqn_ext fe) anc cs)})))
  exact (DataFamInstDeclWithContext an c d) = do
    debugM $ "starting DataFamInstDeclWithContext:an=" ++ showAst an
    (an', d') <- exactDataFamInstDecl an c d
    return (DataFamInstDeclWithContext an' c d')

-- ---------------------------------------------------------------------

exactDataFamInstDecl :: (Monad m, Monoid w)
                     => EpAnn [AddEpAnn] -> TopLevelFlag -> DataFamInstDecl GhcPs
                     -> EP w m (EpAnn [AddEpAnn], DataFamInstDecl GhcPs)
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
           -> EP w m ( EpAnn [AddEpAnn]
                     , LocatedN RdrName
                     , HsOuterTyVarBndrs () GhcPs
                     , HsTyPats GhcPs
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
  getAnnotationEntry (DerivDecl {deriv_ext = an} ) = fromAnn an
  setAnnotationAnchor dd anc cs = dd { deriv_ext = setAnchorEpa (deriv_ext dd) anc cs }
  exact (DerivDecl an typ ms mov) = do
    an0 <- markEpAnnL an lidl AnnDeriving
    ms' <- mapM markAnnotated ms
    an1 <- markEpAnnL an0 lidl AnnInstance
    mov' <- mapM markAnnotated mov
    typ' <- markAnnotated typ
    return (DerivDecl an1 typ' ms' mov')

-- ---------------------------------------------------------------------

instance ExactPrint (ForeignDecl GhcPs) where
  getAnnotationEntry (ForeignImport an _ _  _) = fromAnn an
  getAnnotationEntry (ForeignExport an _ _  _) = fromAnn an

  setAnnotationAnchor (ForeignImport an a b c) anc cs = ForeignImport (setAnchorEpa an anc cs) a b c
  setAnnotationAnchor (ForeignExport an a b c) anc cs = ForeignExport (setAnchorEpa an anc cs) a b c

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
  setAnnotationAnchor a _ _ = a
  exact (CImport (L ls src) cconv safety@(L ll _) mh imp) = do
    cconv' <- markAnnotated cconv
    unless (ll == noSrcSpan) $ markAnnotated safety >> return ()
    unless (ls == noSrcSpan) $ markExternalSourceText ls src "" >> return ()
    return (CImport (L ls src) cconv' safety mh imp)

-- ---------------------------------------------------------------------

instance ExactPrint (ForeignExport GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a
  exact (CExport (L ls src) spec) = do
    debugM $ "CExport starting"
    spec' <- markAnnotated spec
    unless (ls == noSrcSpan) $ markExternalSourceText ls src ""
    return (CExport (L ls src) spec')

-- ---------------------------------------------------------------------

instance ExactPrint CExportSpec where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a
  exact (CExportStatic st lbl cconv) = do
    debugM $ "CExportStatic starting"
    cconv' <- markAnnotated cconv
    return (CExportStatic st lbl cconv')

-- ---------------------------------------------------------------------

instance ExactPrint Safety where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a
  exact = withPpr

-- ---------------------------------------------------------------------

instance ExactPrint CCallConv where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a
  exact = withPpr

-- ---------------------------------------------------------------------

instance ExactPrint (WarnDecls GhcPs) where
  getAnnotationEntry (Warnings (an,_) _) = fromAnn an
  setAnnotationAnchor (Warnings (an,a) b) anc cs = Warnings ((setAnchorEpa an anc cs),a) b

  exact (Warnings (an,src) warns) = do
    an0 <- markAnnOpen an src "{-# WARNING" -- Note: might be {-# DEPRECATED
    warns' <- markAnnotated warns
    an1 <- markEpAnnLMS an0 lidl AnnClose (Just "#-}")
    return (Warnings (an1,src) warns')

-- ---------------------------------------------------------------------

instance ExactPrint (WarnDecl GhcPs) where
  getAnnotationEntry (Warning an _ _) = fromAnn an
  setAnnotationAnchor (Warning an a b) anc cs = Warning (setAnchorEpa an anc cs) a b

  exact (Warning an lns txt) = do
    lns' <- markAnnotated lns
    an0 <- markEpAnnL an lidl AnnOpenS -- "["
    txt' <-
      case txt of
        WarningTxt    src ls -> do
          ls' <- markAnnotated ls
          return (WarningTxt    src ls')
        DeprecatedTxt src ls -> do
          ls' <- markAnnotated ls
          return (DeprecatedTxt src ls')
    an1 <- markEpAnnL an0 lidl AnnCloseS -- "]"
    return (Warning an1 lns' txt')

-- ---------------------------------------------------------------------

instance ExactPrint StringLiteral where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a

  exact l@(StringLiteral src fs mcomma) = do
    printSourceText src (show (unpackFS fs))
    mapM_ (\r -> printStringAtRs r ",") mcomma
    return l

-- ---------------------------------------------------------------------

instance ExactPrint FastString where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a

  -- TODO: https://ghc.haskell.org/trac/ghc/ticket/10313 applies.
  -- exact fs = printStringAdvance (show (unpackFS fs))
  exact fs = printStringAdvance (unpackFS fs) >> return fs


-- ---------------------------------------------------------------------

instance ExactPrint (RuleDecls GhcPs) where
  getAnnotationEntry (HsRules (an,_) _) = fromAnn an
  setAnnotationAnchor (HsRules (an,a) b) anc cs = HsRules ((setAnchorEpa an anc cs),a) b
  exact (HsRules (an, src) rules) = do
    an0 <-
      case src of
        NoSourceText      -> markEpAnnLMS an lidl AnnOpen  (Just "{-# RULES")
        SourceText srcTxt -> markEpAnnLMS an lidl AnnOpen  (Just srcTxt)
    rules' <- markAnnotated rules
    an1 <- markEpAnnLMS an0 lidl AnnClose (Just "#-}")
    return (HsRules (an1,src) rules')

-- ---------------------------------------------------------------------

instance ExactPrint (RuleDecl GhcPs) where
  getAnnotationEntry (HsRule {rd_ext = (an,_)}) = fromAnn an
  setAnnotationAnchor r@(HsRule {rd_ext = (an,a)}) anc cs
    = r { rd_ext = (setAnchorEpa an anc cs, a)}
  exact (HsRule (an,nsrc) (L ln n) act mtybndrs termbndrs lhs rhs) = do
    debugM "HsRule entered"
    (L ln' _) <- markAnnotated (L ln (nsrc, n))
    debugM "HsRule after ln"
    an0 <- markActivation an lra_rest act
    debugM "HsRule after act"
    (an1, mtybndrs') <-
      case mtybndrs of
        Nothing -> return (an0, Nothing)
        Just bndrs -> do
          an1 <-  markLensMAA an0 lra_tyanns_fst  -- AnnForall
          bndrs' <- mapM markAnnotated bndrs
          an2 <- markLensMAA an1 lra_tyanns_snd  -- AnnDot
          return (an2, Just bndrs')

    an2 <- markLensMAA an1 lra_tmanns_fst  -- AnnForall
    termbndrs' <- mapM markAnnotated termbndrs
    an3 <- markLensMAA an2 lra_tmanns_snd  -- AnnDot

    lhs' <- markAnnotated lhs
    an4 <- markEpAnnL an3 lra_rest AnnEqual
    rhs' <- markAnnotated rhs
    return (HsRule (an4,nsrc) (L ln' n) act mtybndrs' termbndrs' lhs' rhs')

markActivation :: (Monad m, Monoid w)
  => EpAnn a -> Lens a [AddEpAnn] -> Activation -> EP w m (EpAnn a)
markActivation an l act = do
  case act of
    ActiveBefore src phase -> do
      an0 <- markEpAnnL an l AnnOpenS --  '['
      an1 <- markEpAnnL an0 l AnnTilde -- ~
      an2 <- markEpAnnLMS an1 l AnnVal (Just (toSourceTextWithSuffix src (show phase) ""))
      an3 <- markEpAnnL an2 l AnnCloseS -- ']'
      return an3
    ActiveAfter src phase -> do
      an0 <- markEpAnnL an l AnnOpenS --  '['
      an1 <- markEpAnnLMS an0 l AnnVal (Just (toSourceTextWithSuffix src (show phase) ""))
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
  setAnnotationAnchor a _ _ = a

  exact (SpliceDecl x splice flag) = do
    splice' <- markAnnotated splice
    return (SpliceDecl x splice' flag)

-- ---------------------------------------------------------------------

instance ExactPrint (DocDecl GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a

  exact v = case v of
    (DocCommentNext ds)    -> DocCommentNext <$> exact ds
    (DocCommentPrev ds)    -> DocCommentPrev <$> exact ds
    (DocCommentNamed s ds) -> DocCommentNamed s <$> exact ds
    (DocGroup i ds)        -> DocGroup i <$> exact ds

-- ---------------------------------------------------------------------

instance ExactPrint (RoleAnnotDecl GhcPs) where
  getAnnotationEntry (RoleAnnotDecl an _ _) = fromAnn an
  setAnnotationAnchor (RoleAnnotDecl an a b) anc cs = RoleAnnotDecl (setAnchorEpa an anc cs) a b
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
  setAnnotationAnchor a _ _ = a
  exact = withPpr

-- ---------------------------------------------------------------------

instance ExactPrint (RuleBndr GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a

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
  getAnnotationEntry (FamEqn { feqn_ext = an}) = fromAnn an
  setAnnotationAnchor fe anc cs = fe {feqn_ext = setAnchorEpa (feqn_ext fe) anc cs}
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
   => EpAnn [AddEpAnn]
   -> LocatedN RdrName
   -> HsOuterTyVarBndrs () GhcPs
   -> HsTyPats GhcPs
   -> LexicalFixity
   -> Maybe (LHsContext GhcPs)
   -> EP w m ( EpAnn [AddEpAnn]
             , LocatedN RdrName
             , HsOuterTyVarBndrs () GhcPs
             , HsTyPats GhcPs, Maybe (LHsContext GhcPs))
exactHsFamInstLHS an thing bndrs typats fixity mb_ctxt = do
  an0 <- markEpAnnL an lidl AnnForall
  bndrs' <- markAnnotated bndrs
  an1 <- markEpAnnL an0 lidl AnnDot
  mb_ctxt' <- mapM markAnnotated mb_ctxt
  (an2, thing', typats') <- exact_pats an1 typats
  return (an2, thing', bndrs', typats', mb_ctxt')
  where
    exact_pats :: (Monad m, Monoid w)
      => EpAnn [AddEpAnn] -> HsTyPats GhcPs -> EP w m (EpAnn [AddEpAnn], LocatedN RdrName, HsTyPats GhcPs)
    exact_pats an' (patl:patr:pats)
      | Infix <- fixity
      = let exact_op_app = do
              an0 <- markEpAnnAllL an' lidl AnnOpenP
              patl' <- markAnnotated patl
              thing' <- markAnnotated thing
              patr' <- markAnnotated patr
              an1 <- markEpAnnAllL an0 lidl AnnCloseP
              return (an1, thing', [patl',patr'])
        in case pats of
             [] -> exact_op_app
             _  -> do
               (an0, thing', p) <- exact_op_app
               pats' <- mapM markAnnotated pats
               return (an0, thing', p++pats')

    exact_pats an' pats = do
      an0 <- markEpAnnAllL an' lidl AnnOpenP
      thing' <- markAnnotated thing
      pats' <- markAnnotated pats
      an1 <- markEpAnnAllL an0 lidl AnnCloseP
      return (an1, thing', pats')

-- ---------------------------------------------------------------------

instance (ExactPrint tm, ExactPrint ty, Outputable tm, Outputable ty)
     =>  ExactPrint (HsArg tm ty) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a

  exact a@(HsValArg tm)    = markAnnotated tm >> return a
  exact a@(HsTypeArg ss ty) = printStringAtSs ss "@" >> markAnnotated ty >> return a
  exact x@(HsArgPar _sp)   = withPpr x -- Does not appear in original source

-- ---------------------------------------------------------------------

instance ExactPrint (ClsInstDecl GhcPs) where
  getAnnotationEntry cid = fromAnn (fst $ cid_ext cid)
  setAnnotationAnchor cid anc cs
    = cid { cid_ext = (setAnchorEpa (fst $ cid_ext cid) anc cs, (snd $ cid_ext cid)) }

  exact (ClsInstDecl { cid_ext = (an, sortKey)
                     , cid_poly_ty = inst_ty, cid_binds = binds
                     , cid_sigs = sigs, cid_tyfam_insts = ats
                     , cid_overlap_mode = mbOverlap
                     , cid_datafam_insts = adts })
      = do
          (an0, mbOverlap', inst_ty') <- top_matter
          an1 <- markEpAnnL an0 lidl AnnOpenC
          an2 <- markEpAnnAllL an1 lid AnnSemi
          ds <- withSortKey sortKey
                               (prepareListAnnotationA ats
                             ++ prepareListAnnotationF an adts
                             ++ prepareListAnnotationA (bagToList binds)
                             ++ prepareListAnnotationA sigs
                               )
          an3 <- markEpAnnL an2 lidl AnnCloseC -- '}'
          let
            ats'   = undynamic ds
            adts'  = undynamic ds
            binds' = listToBag $ undynamic ds
            sigs'  = undynamic ds
          return (ClsInstDecl { cid_ext = (an3, sortKey)
                              , cid_poly_ty = inst_ty', cid_binds = binds'
                              , cid_sigs = sigs', cid_tyfam_insts = ats'
                              , cid_overlap_mode = mbOverlap'
                              , cid_datafam_insts = adts' })

      where
        top_matter = do
          an0 <- markEpAnnL an lidl AnnInstance
          mo <- mapM markAnnotated mbOverlap
          it <- markAnnotated inst_ty
          an1 <- markEpAnnL an0 lidl AnnWhere -- Optional
          return (an1, mo,it)

-- ---------------------------------------------------------------------

instance ExactPrint (TyFamInstDecl GhcPs) where
  getAnnotationEntry (TyFamInstDecl an _) = fromAnn an
  setAnnotationAnchor (TyFamInstDecl an a) anc cs = TyFamInstDecl (setAnchorEpa an anc cs) a

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
  exact (L (SrcSpanAnn an l) (NoOverlap src)) = do
    an0 <- markAnnOpenP an src "{-# NO_OVERLAP"
    an1 <- markAnnCloseP an0
    return (L (SrcSpanAnn an1 l) (NoOverlap src))

  exact (L (SrcSpanAnn an l) (Overlappable src)) = do
    an0 <- markAnnOpenP an src "{-# OVERLAPPABLE"
    an1 <- markAnnCloseP an0
    return (L (SrcSpanAnn an1 l) (Overlappable src))

  exact (L (SrcSpanAnn an l) (Overlapping src)) = do
    an0 <- markAnnOpenP an src "{-# OVERLAPPING"
    an1 <- markAnnCloseP an0
    return (L (SrcSpanAnn an1 l) (Overlapping src))

  exact (L (SrcSpanAnn an l) (Overlaps src)) = do
    an0 <- markAnnOpenP an src "{-# OVERLAPS"
    an1 <- markAnnCloseP an0
    return (L (SrcSpanAnn an1 l) (Overlaps src))

  exact (L (SrcSpanAnn an l) (Incoherent src)) = do
    an0 <- markAnnOpenP an src "{-# INCOHERENT"
    an1 <- markAnnCloseP an0
    return (L (SrcSpanAnn an1 l) (Incoherent src))

-- ---------------------------------------------------------------------

instance ExactPrint (HsBind GhcPs) where
  getAnnotationEntry FunBind{} = NoEntryVal
  getAnnotationEntry PatBind{pat_ext=an} = fromAnn an
  getAnnotationEntry VarBind{} = NoEntryVal
  getAnnotationEntry PatSynBind{} = NoEntryVal

  setAnnotationAnchor pb@PatBind{} anc cs = pb { pat_ext = setAnchorEpa (pat_ext pb) anc cs}
  setAnnotationAnchor a _ _ = a

  exact (FunBind x fid matches) = do
    matches' <- markAnnotated matches
    let
      fun_id' = case unLoc (mg_alts matches') of
        [] -> fid
        (L _ m:_) -> case m_ctxt m of
          FunRhs f _ _ -> f
          _ -> fid
    return (FunBind x fun_id' matches')

  exact (PatBind x pat grhss) = do
    pat' <- markAnnotated pat
    grhss' <- markAnnotated grhss
    return (PatBind x pat' grhss')
  exact (PatSynBind x bind) = do
    bind' <- markAnnotated bind
    return (PatSynBind x bind')

  exact x = error $ "HsBind: exact for " ++ showAst x

-- ---------------------------------------------------------------------

instance ExactPrint (PatSynBind GhcPs GhcPs) where
  getAnnotationEntry (PSB { psb_ext = an}) = fromAnn an
  setAnnotationAnchor p anc cs = p { psb_ext = setAnchorEpa (psb_ext p) anc cs}

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
  setAnnotationAnchor a _ _ = a
  exact r@(RecordPatSynField { recordPatSynField = v }) = markAnnotated v
        >> return r

-- ---------------------------------------------------------------------

instance ExactPrint (Match GhcPs (LocatedA (HsCmd GhcPs))) where
  getAnnotationEntry (Match ann _ _ _) = fromAnn ann
  setAnnotationAnchor (Match an a b c) anc cs = Match (setAnchorEpa an anc cs) a b c

  exact (Match an mctxt pats grhss) =
    exactMatch (Match an mctxt pats grhss)

-- -------------------------------------

instance ExactPrint (Match GhcPs (LocatedA (HsExpr GhcPs))) where
  getAnnotationEntry (Match ann _ _ _) = fromAnn ann
  setAnnotationAnchor (Match an a b c) anc cs = Match (setAnchorEpa an anc cs) a b c

  exact (Match an mctxt pats grhss) =
    exactMatch (Match an mctxt pats grhss)

-- ---------------------------------------------------------------------

exactMatch :: (Monad m, Monoid w) => (ExactPrint (GRHSs GhcPs body)) => (Match GhcPs body) -> EP w m (Match GhcPs body)
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
      LambdaExpr -> do
        an0' <- markEpAnnL an lidl AnnLam
        pats' <- markAnnotated pats
        return (an0', LambdaExpr, pats')
      CaseAlt -> do
        pats' <- markAnnotated pats
        return (an, CaseAlt, pats')
      LamCaseAlt v -> do
        pats' <- markAnnotated pats
        return (an, LamCaseAlt v, pats')
      _ -> do
        mctxt' <- withPpr mctxt
        return (an, mctxt', pats)

  grhss' <- markAnnotated grhss

  return (Match an0 mctxt' pats' grhss')

-- ---------------------------------------------------------------------

instance ExactPrint (GRHSs GhcPs (LocatedA (HsExpr GhcPs))) where
  getAnnotationEntry (GRHSs _ _ _) = NoEntryVal
  setAnnotationAnchor a _ _ = a

  exact (GRHSs cs grhss binds) = do
    addCommentsA $ priorComments cs
    addCommentsA $ getFollowingComments cs
    grhss' <- markAnnotated grhss
    binds' <- markAnnotated binds
    -- The comments will be added back as they are printed
    return (GRHSs emptyComments grhss' binds')


instance ExactPrint (GRHSs GhcPs (LocatedA (HsCmd GhcPs))) where
  getAnnotationEntry (GRHSs _ _ _) = NoEntryVal
  setAnnotationAnchor a _ _ = a

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

  setAnnotationAnchor (HsValBinds an a) anc cs = HsValBinds (setAnchorEpaL an anc cs) a
  setAnnotationAnchor a _ _ = a

  exact (HsValBinds an valbinds) = do
    debugM $ "exact HsValBinds: an=" ++ showAst an
    an0 <- markEpAnnL an lal_rest AnnWhere

    let manc = case an of
                 EpAnnNotUsed -> Nothing
                 _ -> al_anchor $ anns an

    case manc of
      Just anc -> do
        when (not $ isEmptyValBinds valbinds) $ setExtraDP (Just anc)
      _ -> return ()

    (an1, valbinds') <- markAnnList False an0 $ markAnnotatedWithLayout valbinds
    debugM $ "exact HsValBinds: an1=" ++ showAst an1
    return (HsValBinds an1 valbinds')

  exact (HsIPBinds an bs) = do
    (as, ipb) <- markAnnList True an (markEpAnnL an lal_rest AnnWhere
                           >> markAnnotated bs
                           >>= \bs' -> return (HsIPBinds an bs'::HsLocalBinds GhcPs))
    case ipb of
      HsIPBinds _ bs' -> return (HsIPBinds as bs'::HsLocalBinds GhcPs)
      _ -> error "should not happen HsIPBinds"
  exact b@(EmptyLocalBinds _) = return b


-- ---------------------------------------------------------------------
instance ExactPrint (HsValBindsLR GhcPs GhcPs) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ = a

  exact (ValBinds sortKey binds sigs) = do
    ds <- setLayoutBoth $ withSortKey sortKey
       (prepareListAnnotationA (bagToList binds)
     ++ prepareListAnnotationA sigs
       )
    let
      binds' = listToBag $ undynamic ds
      sigs'  = undynamic ds
    return (ValBinds sortKey binds' sigs')
  exact (XValBindsLR _) = panic "XValBindsLR"

undynamic :: Typeable a => [Dynamic] -> [a]
undynamic ds = mapMaybe fromDynamic ds

-- ---------------------------------------------------------------------

instance ExactPrint (HsIPBinds GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a

  exact b@(IPBinds _ binds) = setLayoutBoth $ markAnnotated binds >> return b

-- ---------------------------------------------------------------------

instance ExactPrint (IPBind GhcPs) where
  getAnnotationEntry (IPBind an _ _) = fromAnn an
  setAnnotationAnchor (IPBind an a b) anc cs = IPBind (setAnchorEpa an anc cs) a b

  exact (IPBind an lr rhs) = do
    lr' <- markAnnotated lr
    an0 <- markEpAnnL an lidl AnnEqual
    rhs' <- markAnnotated rhs
    return (IPBind an0 lr' rhs')


-- ---------------------------------------------------------------------

instance ExactPrint HsIPName where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a

  exact i@(HsIPName fs) = printStringAdvance ("?" ++ (unpackFS fs)) >> return i

-- ---------------------------------------------------------------------
-- Managing lists which have been separated, e.g. Sigs and Binds

prepareListAnnotationF :: (Monad m, Monoid w) =>
  EpAnn [AddEpAnn] -> [LDataFamInstDecl GhcPs] -> [(RealSrcSpan,EP w m Dynamic)]
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

withSortKey :: (Monad m, Monoid w) => AnnSortKey -> [(RealSrcSpan, EP w m Dynamic)] -> EP w m [Dynamic]
withSortKey annSortKey xs = do
  debugM $ "withSortKey:annSortKey=" ++ showAst annSortKey
  let ordered = case annSortKey of
                  NoAnnSortKey -> sortBy orderByFst xs
                  -- Just keys -> error $ "withSortKey: keys" ++ show keys
                  AnnSortKey keys -> orderByKey xs keys
                                -- `debug` ("withSortKey:" ++
                                --          showPprUnsafe (map fst (sortBy (comparing (flip elemIndex keys . fst)) xs),
                                --                  map fst xs,
                                --                  keys)
                                --          )
  mapM snd ordered

orderByFst :: Ord a => (a, b1) -> (a, b2) -> Ordering
orderByFst (a,_) (b,_) = compare a b

-- ---------------------------------------------------------------------

instance ExactPrint (Sig GhcPs) where
  getAnnotationEntry (TypeSig a _ _)  = fromAnn a
  getAnnotationEntry (PatSynSig a _ _) = fromAnn a
  getAnnotationEntry (ClassOpSig a _ _ _) = fromAnn a
  getAnnotationEntry (FixSig a _) = fromAnn a
  getAnnotationEntry (InlineSig a _ _) = fromAnn a
  getAnnotationEntry (SpecSig a _ _ _) = fromAnn a
  getAnnotationEntry (SpecInstSig (a, _) _) = fromAnn a
  getAnnotationEntry (MinimalSig (a, _) _) = fromAnn a
  getAnnotationEntry (SCCFunSig (a, _) _ _) = fromAnn a
  getAnnotationEntry (CompleteMatchSig (a, _) _ _) = fromAnn a

  setAnnotationAnchor (TypeSig a x y)  anc           cs = (TypeSig (setAnchorEpa a anc cs) x y)
  setAnnotationAnchor (PatSynSig a x y) anc          cs = (PatSynSig (setAnchorEpa a anc cs) x y)
  setAnnotationAnchor (ClassOpSig a x y z) anc       cs = (ClassOpSig (setAnchorEpa a anc cs) x y z)
  setAnnotationAnchor (FixSig a x) anc               cs = (FixSig (setAnchorEpa a anc cs) x)
  setAnnotationAnchor (InlineSig a x y) anc          cs = (InlineSig (setAnchorEpa a anc cs) x y)
  setAnnotationAnchor (SpecSig a x y z) anc          cs = (SpecSig (setAnchorEpa a anc cs) x y z)
  setAnnotationAnchor (SpecInstSig (a,x) y) anc      cs = (SpecInstSig ((setAnchorEpa a anc cs),x) y)
  setAnnotationAnchor (MinimalSig (a,x) y) anc       cs = (MinimalSig ((setAnchorEpa a anc cs),x) y)
  setAnnotationAnchor (SCCFunSig (a,x) y z) anc      cs = (SCCFunSig ((setAnchorEpa a anc cs),x) y z)
  setAnnotationAnchor (CompleteMatchSig (a,x) y z) anc cs = (CompleteMatchSig ((setAnchorEpa a anc cs),x) y z)

  exact (TypeSig an vars ty)  = do
    (an', vars', ty') <- exactVarSig an vars ty
    return (TypeSig an' vars' ty')

  exact (PatSynSig an lns typ) = do
    an0 <- markEpAnnL an lasRest AnnPattern
    lns' <- markAnnotated lns
    an1 <- markLensAA an0 lasDcolon
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
    an0 <- markEpAnnLMS an  lidl AnnInfix (Just fixstr)
    an1 <- markEpAnnLMS an0 lidl AnnVal (Just (sourceTextToString src (show v)))
    names' <- markAnnotated names
    return (FixSig an1 (FixitySig x names' (Fixity src v fdir)))

  exact (InlineSig an ln inl) = do
    an0 <- markAnnOpen an (inl_src inl) "{-# INLINE"
    an1 <- markActivation an0 id (inl_act inl)
    ln' <- markAnnotated ln
    debugM $ "InlineSig:an=" ++ showAst an
    p <- getPosP
    debugM $ "InlineSig: p=" ++ show p
    an2 <- markEpAnnLMS an1 lidl AnnClose (Just "#-}")
    debugM $ "InlineSig:done"
    return (InlineSig an2 ln' inl)

  exact (SpecSig an ln typs inl) = do
    an0 <- markAnnOpen an (inl_src inl) "{-# SPECIALISE" -- Note: may be {-# SPECIALISE_INLINE
    an1 <- markActivation an0 lidl (inl_act inl)
    ln' <- markAnnotated ln
    an2 <- markEpAnnL an1 lidl AnnDcolon
    typs' <- markAnnotated typs
    an3 <- markEpAnnLMS an2 lidl AnnClose (Just "#-}")
    return (SpecSig an3 ln' typs' inl)

  exact (SpecInstSig (an,src) typ) = do
    an0 <- markAnnOpen an src "{-# SPECIALISE"
    an1 <- markEpAnnL an0 lidl AnnInstance
    typ' <- markAnnotated typ
    an2 <- markEpAnnLMS an1 lidl AnnClose (Just "#-}")
    return (SpecInstSig (an2,src) typ')

  exact (MinimalSig (an,src) formula) = do
    an0 <- markAnnOpen an src "{-# MINIMAL"
    formula' <- markAnnotated formula
    an1 <- markEpAnnLMS an0 lidl AnnClose (Just "#-}")
    return (MinimalSig (an1,src) formula')

  exact (SCCFunSig (an,src) ln ml) = do
    an0 <- markAnnOpen an src "{-# SCC"
    ln' <- markAnnotated ln
    ml' <- markAnnotated ml
    an1 <- markEpAnnLMS an0 lidl AnnClose (Just "#-}")
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
    an2 <- markEpAnnLMS an1 lidl AnnClose (Just "#-}")
    return (CompleteMatchSig (an2,src) cs' mty')

-- ---------------------------------------------------------------------

exactVarSig :: (Monad m, Monoid w, ExactPrint a)
  => EpAnn AnnSig -> [LocatedN RdrName] -> a -> EP w m (EpAnn AnnSig, [LocatedN RdrName], a)
exactVarSig an vars ty = do
  vars' <- mapM markAnnotated vars
  an0 <- markLensAA an lasDcolon
  ty' <- markAnnotated ty
  return (an0, vars', ty')

-- ---------------------------------------------------------------------

instance ExactPrint (StandaloneKindSig GhcPs) where
  getAnnotationEntry (StandaloneKindSig an _ _) = fromAnn an
  setAnnotationAnchor (StandaloneKindSig an a b) anc cs = StandaloneKindSig (setAnchorEpa an anc cs) a b

  exact (StandaloneKindSig an vars sig) = do
    an0 <- markEpAnnL an lidl AnnType
    vars' <- markAnnotated vars
    an1 <- markEpAnnL an0 lidl AnnDcolon
    sig' <- markAnnotated sig
    return (StandaloneKindSig an1 vars' sig')

-- ---------------------------------------------------------------------

instance ExactPrint (DefaultDecl GhcPs) where
  getAnnotationEntry (DefaultDecl an _) = fromAnn an
  setAnnotationAnchor (DefaultDecl an a) anc cs = DefaultDecl (setAnchorEpa an anc cs) a

  exact (DefaultDecl an tys) = do
    an0 <- markEpAnnL an lidl AnnDefault
    an1 <- markEpAnnL an0 lidl AnnOpenP
    tys' <- markAnnotated tys
    an2 <- markEpAnnL an1 lidl AnnCloseP
    return (DefaultDecl an2 tys')

-- ---------------------------------------------------------------------

instance ExactPrint (AnnDecl GhcPs) where
  getAnnotationEntry (HsAnnotation (an, _) _ _) = fromAnn an
  setAnnotationAnchor (HsAnnotation (an,a) b c) anc cs = HsAnnotation ((setAnchorEpa an anc cs),a) b c

  exact (HsAnnotation (an, src) prov e) = do
    an0 <- markAnnOpenP an src "{-# ANN"
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
    an2 <- markAnnCloseP an1
    return (HsAnnotation (an2,src) prov' e')

-- ---------------------------------------------------------------------

instance ExactPrint (BF.BooleanFormula (LocatedN RdrName)) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a

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
  setAnnotationAnchor a _ _ = a
  exact (HsWC x ty) = do
    ty' <- markAnnotated ty
    return (HsWC x ty')

-- ---------------------------------------------------------------------

instance ExactPrint (GRHS GhcPs (LocatedA (HsExpr GhcPs))) where
  getAnnotationEntry (GRHS an _ _) = fromAnn an
  setAnnotationAnchor (GRHS an a b) anc cs = GRHS (setAnchorEpa an anc cs) a b

  exact (GRHS an guards expr) = do
    debugM $ "GRHS comments:" ++ showGhc (comments an)
    an0 <- if null guards
             then return an
             else markLensKwM an lga_vbar AnnVbar
    guards' <- markAnnotated guards
    debugM $ "GRHS before matchSeparator"
    an1 <- markLensAA an0 lga_sep -- Mark the matchSeparator for these GRHSs
    debugM $ "GRHS after matchSeparator"
    expr' <- markAnnotated expr
    return (GRHS an1 guards' expr')

instance ExactPrint (GRHS GhcPs (LocatedA (HsCmd GhcPs))) where
  getAnnotationEntry (GRHS ann _ _) = fromAnn ann
  setAnnotationAnchor (GRHS an a b) anc cs = GRHS (setAnchorEpa an anc cs) a b

  exact (GRHS an guards expr) = do
    an0 <- markLensKwM an lga_vbar AnnVbar
    guards' <- markAnnotated guards
    an1 <- markLensAA an0 lga_sep -- Mark the matchSeparator for these GRHSs
    expr' <- markAnnotated expr
    return (GRHS an1 guards' expr')

-- ---------------------------------------------------------------------

instance ExactPrint (HsExpr GhcPs) where
  getAnnotationEntry (HsVar{})                    = NoEntryVal
  getAnnotationEntry (HsUnboundVar an _)          = fromAnn an
  getAnnotationEntry (HsRecSel{})                 = NoEntryVal
  getAnnotationEntry (HsOverLabel an _)           = fromAnn an
  getAnnotationEntry (HsIPVar an _)               = fromAnn an
  getAnnotationEntry (HsOverLit an _)             = fromAnn an
  getAnnotationEntry (HsLit an _)                 = fromAnn an
  getAnnotationEntry (HsLam _ _)                  = NoEntryVal
  getAnnotationEntry (HsLamCase an _ _)           = fromAnn an
  getAnnotationEntry (HsApp an _ _)               = fromAnn an
  getAnnotationEntry (HsAppType _ _ _ _)          = NoEntryVal
  getAnnotationEntry (OpApp an _ _ _)             = fromAnn an
  getAnnotationEntry (NegApp an _ _)              = fromAnn an
  getAnnotationEntry (HsPar an _ _ _)             = fromAnn an
  getAnnotationEntry (SectionL an _ _)            = fromAnn an
  getAnnotationEntry (SectionR an _ _)            = fromAnn an
  getAnnotationEntry (ExplicitTuple an _ _)       = fromAnn an
  getAnnotationEntry (ExplicitSum an _ _ _)       = fromAnn an
  getAnnotationEntry (HsCase an _ _)              = fromAnn an
  getAnnotationEntry (HsIf an _ _ _)              = fromAnn an
  getAnnotationEntry (HsMultiIf an _)             = fromAnn an
  getAnnotationEntry (HsLet an _ _ _ _)           = fromAnn an
  getAnnotationEntry (HsDo an _ _)                = fromAnn an
  getAnnotationEntry (ExplicitList an _)          = fromAnn an
  getAnnotationEntry (RecordCon an _ _)           = fromAnn an
  getAnnotationEntry (RecordUpd an _ _)           = fromAnn an
  getAnnotationEntry (HsGetField an _ _)          = fromAnn an
  getAnnotationEntry (HsProjection an _)          = fromAnn an
  getAnnotationEntry (ExprWithTySig an _ _)       = fromAnn an
  getAnnotationEntry (ArithSeq an _ _)            = fromAnn an
  getAnnotationEntry (HsTypedBracket an _)        = fromAnn an
  getAnnotationEntry (HsUntypedBracket an _)      = fromAnn an
  getAnnotationEntry (HsTypedSplice (_, an) _)    = fromAnn an
  getAnnotationEntry (HsUntypedSplice an _)       = fromAnn an
  getAnnotationEntry (HsProc an _ _)              = fromAnn an
  getAnnotationEntry (HsStatic an _)              = fromAnn an
  getAnnotationEntry (HsPragE{})                  = NoEntryVal

  setAnnotationAnchor a@(HsVar{})              _ _s = a
  setAnnotationAnchor (HsUnboundVar an a)    anc cs = (HsUnboundVar (setAnchorEpa an anc cs) a)
  setAnnotationAnchor a@(HsRecSel{})           _ _s  = a
  setAnnotationAnchor (HsOverLabel an a)     anc cs = (HsOverLabel (setAnchorEpa an anc cs) a)
  setAnnotationAnchor (HsIPVar an a)         anc cs = (HsIPVar (setAnchorEpa an anc cs) a)
  setAnnotationAnchor (HsOverLit an a)       anc cs = (HsOverLit (setAnchorEpa an anc cs) a)
  setAnnotationAnchor (HsLit an a)           anc cs = (HsLit (setAnchorEpa an anc cs) a)
  setAnnotationAnchor a@(HsLam _ _)            _ _s = a
  setAnnotationAnchor (HsLamCase an a b)     anc cs = (HsLamCase (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (HsApp an a b)         anc cs = (HsApp (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor a@(HsAppType {})      _ _s = a
  setAnnotationAnchor (OpApp an a b c)       anc cs = (OpApp (setAnchorEpa an anc cs) a b c)
  setAnnotationAnchor (NegApp an a b)        anc cs = (NegApp (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (HsPar an a b c)       anc cs = (HsPar (setAnchorEpa an anc cs) a b c)
  setAnnotationAnchor (SectionL an a b)      anc cs = (SectionL (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (SectionR an a b)      anc cs = (SectionR (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (ExplicitTuple an a b) anc cs = (ExplicitTuple (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (ExplicitSum an a b c) anc cs = (ExplicitSum (setAnchorEpa an anc cs) a b c)
  setAnnotationAnchor (HsCase an a b)        anc cs = (HsCase (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (HsIf an a b c)        anc cs = (HsIf (setAnchorEpa an anc cs) a b c)
  setAnnotationAnchor (HsMultiIf an a)       anc cs = (HsMultiIf (setAnchorEpa an anc cs) a)
  setAnnotationAnchor (HsLet an a b c d)     anc cs = (HsLet (setAnchorEpa an anc cs) a b c d)
  setAnnotationAnchor (HsDo an a b)          anc cs = (HsDo (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (ExplicitList an a)    anc cs = (ExplicitList (setAnchorEpa an anc cs) a)
  setAnnotationAnchor (RecordCon an a b)     anc cs = (RecordCon (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (RecordUpd an a b)     anc cs = (RecordUpd (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (HsGetField an a b)    anc cs = (HsGetField (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (HsProjection an a)    anc cs = (HsProjection (setAnchorEpa an anc cs) a)
  setAnnotationAnchor (ExprWithTySig an a b) anc cs = (ExprWithTySig (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (ArithSeq an a b)      anc cs = (ArithSeq (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (HsTypedBracket an a)   anc cs = (HsTypedBracket (setAnchorEpa an anc cs) a)
  setAnnotationAnchor (HsUntypedBracket an a) anc cs = (HsUntypedBracket (setAnchorEpa an anc cs) a)
  setAnnotationAnchor (HsTypedSplice (x,an) e) anc cs = (HsTypedSplice (x,(setAnchorEpa an anc cs)) e)
  setAnnotationAnchor (HsUntypedSplice an e)  anc cs = (HsUntypedSplice (setAnchorEpa an anc cs) e)
  setAnnotationAnchor (HsProc an a b)         anc cs = (HsProc (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (HsStatic an a)         anc cs = (HsStatic (setAnchorEpa an anc cs) a)
  setAnnotationAnchor a@(HsPragE{})            _ _s = a

  exact (HsVar x n) = do
    n' <- markAnnotated n
    return (HsVar x n')
  exact x@(HsUnboundVar an _) = do
    case an of
      EpAnnNotUsed -> withPpr x
      EpAnn _ (EpAnnUnboundVar (ob,cb) l) _ -> do
        printStringAtAA ob "`" >> return ()
        printStringAtAA l  "_" >> return ()
        printStringAtAA cb "`" >> return ()
        return x
  exact x@(HsOverLabel _ _) = withPpr x

  exact x@(HsIPVar _ (HsIPName n))
    = printStringAdvance ("?" ++ unpackFS n) >> return x

  exact x@(HsOverLit _an ol) = do
    let str = case ol_val ol of
                HsIntegral   (IL src _ _) -> src
                HsFractional (FL { fl_text = src }) -> src
                HsIsString src _          -> src
    case str of
      SourceText s -> printStringAdvance s >> return ()
      NoSourceText -> withPpr x >> return ()
    return x

  exact (HsLit an lit) = do
    lit' <- withPpr lit
    return (HsLit an lit')
  exact (HsLam x mg) = do
    mg' <- markAnnotated mg
    return (HsLam x mg')

  exact (HsLamCase an lc_variant mg) = do
    an0 <- markEpAnnL an lidl AnnLam
    an1 <- markEpAnnL an0 lidl (case lc_variant of LamCase -> AnnCase
                                                   LamCases -> AnnCases)
    mg' <- markAnnotated mg
    return (HsLamCase an1 lc_variant mg')

  exact (HsApp an e1 e2) = do
    p <- getPosP
    debugM $ "HsApp entered. p=" ++ show p
    e1' <- markAnnotated e1
    e2' <- markAnnotated e2
    return (HsApp an e1' e2')
  exact (HsAppType ss fun at arg) = do
    fun' <- markAnnotated fun
    at' <- markToken at
    arg' <- markAnnotated arg
    return (HsAppType ss fun' at' arg')
  exact (OpApp an e1 e2 e3) = do
    e1' <- markAnnotated e1
    e2' <- markAnnotated e2
    e3' <- markAnnotated e3
    return (OpApp an e1' e2' e3')

  exact (NegApp an e s) = do
    an0 <- markEpAnnL an lidl AnnMinus
    e' <- markAnnotated e
    return (NegApp an0 e' s)

  exact (HsPar an lpar e rpar) = do
    lpar' <- markToken lpar
    e' <- markAnnotated e
    debugM $ "HsPar closing paren"
    rpar' <- markToken rpar
    debugM $ "HsPar done"
    return (HsPar an lpar' e' rpar')

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
    an0 <- markAnnKwL an lhsCaseAnnCase AnnCase
    e' <- markAnnotated e
    an1 <- markAnnKwL an0 lhsCaseAnnOf AnnOf
    an2 <- markEpAnnL an1 lhsCaseAnnsRest AnnOpenC
    an3 <- markEpAnnAllL an2 lhsCaseAnnsRest AnnSemi
    alts' <- setLayoutBoth $ markAnnotated alts
    an4 <- markEpAnnL an3 lhsCaseAnnsRest AnnCloseC
    return (HsCase an4 e' alts')

  exact (HsIf an e1 e2 e3) = do
    an0 <- markAnnKwL an laiIf AnnIf
    e1' <- markAnnotated e1
    an1 <- markLensKwM an0 laiThenSemi AnnSemi
    an2 <- markAnnKwL an1 laiThen AnnThen
    e2' <- markAnnotated e2
    an3 <- markLensKwM an2 laiElseSemi AnnSemi
    an4 <- markAnnKwL an3 laiElse AnnElse
    e3' <- markAnnotated e3
    return (HsIf an4 e1' e2' e3')

  exact (HsMultiIf an mg) = do
    an0 <- markEpAnnL an lidl AnnIf
    an1 <- markEpAnnL an0 lidl AnnOpenC -- optional
    mg' <- markAnnotated mg
    an2 <- markEpAnnL an1 lidl AnnCloseC -- optional
    return (HsMultiIf an2 mg')

  exact (HsLet an tkLet binds tkIn e) = do
    setLayoutBoth $ do -- Make sure the 'in' gets indented too
      tkLet' <- markToken tkLet
      debugM $ "HSlet:binds coming"
      binds' <- setLayoutBoth $ markAnnotated binds
      debugM $ "HSlet:binds done"
      tkIn' <- markToken tkIn
      debugM $ "HSlet:expr coming"
      e' <- markAnnotated e
      return (HsLet an tkLet' binds' tkIn' e')

  exact (HsDo an do_or_list_comp stmts) = do
    debugM $ "HsDo"
    (an',stmts') <- markAnnListA True an $ \a -> exactDo a do_or_list_comp stmts
    return (HsDo an' do_or_list_comp stmts')

  exact (ExplicitList an es) = do
    debugM $ "ExplicitList start"
    an0 <- markLensMAA an lal_open
    es' <- markAnnotated es
    an1 <- markLensMAA an0 lal_close
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
    an0 <- markAnnKwL an lapOpen AnnOpenP
    flds' <- mapM markAnnotated flds
    an1 <- markAnnKwL an0 lapClose AnnCloseP
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
    an0 <- markEpAnnLMS an lidl AnnOpen (Just "[||")
    an1 <- markEpAnnLMS an0 lidl AnnOpenE (Just "[e||")
    e' <- markAnnotated e
    an2 <- markEpAnnLMS an1 lidl AnnClose (Just "||]")
    return (HsTypedBracket an2 e')

  exact (HsUntypedBracket an (ExpBr a e)) = do
    an0 <- markEpAnnL an  lidl AnnOpenEQ -- "[|"
    an1 <- markEpAnnL an0 lidl AnnOpenE  -- "[e|" -- optional
    e' <- markAnnotated e
    an2 <- markEpAnnL an1 lidl AnnCloseQ -- "|]"
    return (HsUntypedBracket an2 (ExpBr a e'))

  exact (HsUntypedBracket an (PatBr a e)) = do
    an0 <- markEpAnnLMS an lidl AnnOpen (Just "[p|")
    e' <- markAnnotated e
    an1 <- markEpAnnL an0 lidl AnnCloseQ -- "|]"
    return (HsUntypedBracket an1 (PatBr a e'))

  exact (HsUntypedBracket an (DecBrL a e)) = do
    an0 <- markEpAnnLMS an lidl AnnOpen (Just "[d|")
    an1 <- markEpAnnL an lidl AnnOpenC
    e' <- markAnnotated e
    an2 <- markEpAnnL an1 lidl AnnCloseC
    an3 <- markEpAnnL an2 lidl AnnCloseQ -- "|]"
    return (HsUntypedBracket an3 (DecBrL a e'))

  exact (HsUntypedBracket an (TypBr a e)) = do
    an0 <- markEpAnnLMS an lidl AnnOpen (Just "[t|")
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

  exact (HsTypedSplice (x,an) s)   = do
    an0 <- markEpAnnL an lidl AnnDollarDollar
    s' <- exact s
    return (HsTypedSplice (x,an0) s')
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
  exact x = error $ "exact HsExpr for:" ++ showAst x

-- ---------------------------------------------------------------------

exactDo :: (Monad m, Monoid w, ExactPrint (LocatedAn an a))
        => EpAnn AnnList -> HsDoFlavour -> LocatedAn an a
        -> EP w m (EpAnn AnnList, LocatedAn an a)
exactDo an (DoExpr m)    stmts = exactMdo an m AnnDo          >>= \an0 -> markMaybeDodgyStmts an0 stmts
exactDo an GhciStmtCtxt  stmts = markEpAnnL an lal_rest AnnDo >>= \an0 -> markMaybeDodgyStmts an0 stmts
exactDo an (MDoExpr m)   stmts = exactMdo an m AnnMdo         >>= \an0 -> markMaybeDodgyStmts an0 stmts
exactDo an ListComp      stmts = markMaybeDodgyStmts an stmts
exactDo an MonadComp     stmts = markMaybeDodgyStmts an stmts

exactMdo :: (Monad m, Monoid w)
  => EpAnn AnnList -> Maybe ModuleName -> AnnKeywordId -> EP w m (EpAnn AnnList)
exactMdo an Nothing            kw = markEpAnnL   an lal_rest kw
exactMdo an (Just module_name) kw = markEpAnnLMS an lal_rest kw (Just n)
    where
      n = (moduleNameString module_name) ++ "." ++ (keywordToString kw)

markMaybeDodgyStmts :: (Monad m, Monoid w, ExactPrint (LocatedAn an a))
  => EpAnn AnnList -> LocatedAn an a -> EP w m (EpAnn AnnList, LocatedAn an a)
markMaybeDodgyStmts an stmts =
  if isGoodSrcSpan (getLocA stmts)
    then do
      r <- markAnnotatedWithLayout stmts
      return (an, r)
    else return (an, stmts)

-- ---------------------------------------------------------------------
instance ExactPrint (HsPragE GhcPs) where
  getAnnotationEntry HsPragSCC{}  = NoEntryVal
  setAnnotationAnchor a _ _ = a

  exact (HsPragSCC (an,st) sl) = do
    an0 <- markAnnOpenP an st "{-# SCC"
    let txt = sourceTextToString (sl_st sl) (unpackFS $ sl_fs sl)
    an1 <- markEpAnnLMS an0 lapr_rest AnnVal    (Just txt) -- optional
    an2 <- markEpAnnLMS an1 lapr_rest AnnValStr (Just txt) -- optional
    an3 <- markAnnCloseP an2
    return (HsPragSCC (an3,st) sl)


-- ---------------------------------------------------------------------

instance ExactPrint (HsUntypedSplice GhcPs) where
  getAnnotationEntry (HsUntypedSpliceExpr an _) = fromAnn an
  getAnnotationEntry (HsQuasiQuote _ _ _)       = NoEntryVal

  setAnnotationAnchor (HsUntypedSpliceExpr an e) anc cs = HsUntypedSpliceExpr (setAnchorEpa an anc cs) e
  setAnnotationAnchor a@HsQuasiQuote {}         _ _ = a

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
  setAnnotationAnchor a _ _ = a
  exact (MG x matches) = do
    -- TODO:AZ use SortKey, in MG ann.
    matches' <- if isGoodSrcSpan (getLocA matches)
      then markAnnotated matches
      else return matches
    return (MG x matches')

instance ExactPrint (MatchGroup GhcPs (LocatedA (HsCmd GhcPs))) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a
  exact (MG x matches) = do
    -- TODO:AZ use SortKey, in MG ann.
    matches' <- if isGoodSrcSpan (getLocA matches)
      then markAnnotated matches
      else return matches
    return (MG x matches')

-- ---------------------------------------------------------------------

instance (ExactPrint body) => ExactPrint (HsRecFields GhcPs body) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a
  exact (HsRecFields fields mdot) = do
    fields' <- markAnnotated fields
    case mdot of
      Nothing -> return ()
      Just (L ss _) ->
        printStringAtSs ss ".." >> return ()
      -- Note: mdot contains the SrcSpan where the ".." appears, if present
    return (HsRecFields fields' mdot)

-- ---------------------------------------------------------------------

instance (ExactPrint body)
    => ExactPrint (HsFieldBind (LocatedAn NoEpAnns (FieldOcc GhcPs)) body) where
  getAnnotationEntry x = fromAnn (hfbAnn x)
  setAnnotationAnchor (HsFieldBind an f arg isPun) anc cs = (HsFieldBind (setAnchorEpa an anc cs) f arg isPun)
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
  getAnnotationEntry x = fromAnn (hfbAnn x)
  setAnnotationAnchor (HsFieldBind an f arg isPun) anc cs = (HsFieldBind (setAnchorEpa an anc cs) f arg isPun)

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
    => ExactPrint (HsFieldBind (LocatedAn NoEpAnns (AmbiguousFieldOcc GhcPs)) (LocatedA body)) where
  getAnnotationEntry x = fromAnn (hfbAnn x)
  setAnnotationAnchor (HsFieldBind an f arg isPun) anc cs = (HsFieldBind (setAnchorEpa an anc cs) f arg isPun)
  exact (HsFieldBind an f arg isPun) = do
    debugM $ "HsRecUpdField"
    f' <- markAnnotated f
    an0 <- if isPun then return an
             else markEpAnnL an lidl AnnEqual
    arg' <- if ((locA $ getLoc arg) == noSrcSpan )
              then return arg
              else markAnnotated arg
    return (HsFieldBind an0 f' arg' isPun)

-- ---------------------------------------------------------------------
instance
    (ExactPrint (HsFieldBind (LocatedAn NoEpAnns (a GhcPs)) body),
     ExactPrint (HsFieldBind (LocatedAn NoEpAnns (b GhcPs)) body))
    => ExactPrint
         (Either [LocatedA (HsFieldBind (LocatedAn NoEpAnns (a GhcPs)) body)]
                 [LocatedA (HsFieldBind (LocatedAn NoEpAnns (b GhcPs)) body)]) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a

  exact (Left rbinds) = Left <$> markAnnotated rbinds
  exact (Right pbinds) = Right <$> markAnnotated pbinds

-- ---------------------------------------------------------------------

instance ExactPrint (FieldLabelStrings GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a
  exact (FieldLabelStrings fs) = FieldLabelStrings <$> markAnnotated fs

-- ---------------------------------------------------------------------

instance ExactPrint (DotFieldOcc GhcPs) where
  getAnnotationEntry (DotFieldOcc an _) = fromAnn an

  setAnnotationAnchor (DotFieldOcc an a) anc cs = DotFieldOcc (setAnchorEpa an anc cs) a

  exact (DotFieldOcc an (L loc (FieldLabelString fs))) = do
    an0 <- markLensKwM an lafDot  AnnDot
    -- The field name has a SrcSpanAnnN, print it as a
    -- LocatedN RdrName
    L loc' _ <- markAnnotated (L loc (mkVarUnqual fs))
    return (DotFieldOcc an0 (L loc' (FieldLabelString fs)))

-- ---------------------------------------------------------------------

instance ExactPrint (HsTupArg GhcPs) where
  getAnnotationEntry (Present an _) = fromAnn an
  getAnnotationEntry (Missing an)   = fromAnn an

  setAnnotationAnchor (Present an a) anc cs = Present (setAnchorEpa an anc cs) a
  setAnnotationAnchor (Missing an)   anc cs = Missing (setAnchorEpa an anc cs)

  exact (Present a e) = Present a <$> markAnnotated e

  exact a@(Missing EpAnnNotUsed) = return a
  exact a@(Missing _) = printStringAdvance "," >> return a

-- ---------------------------------------------------------------------

instance ExactPrint (HsCmdTop GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a
  exact (HsCmdTop a cmd) = HsCmdTop a <$> markAnnotated cmd

-- ---------------------------------------------------------------------

instance ExactPrint (HsCmd GhcPs) where
  getAnnotationEntry (HsCmdArrApp an _ _ _ _)   = fromAnn an
  getAnnotationEntry (HsCmdArrForm an _ _ _ _ ) = fromAnn an
  getAnnotationEntry (HsCmdApp an _ _ )         = fromAnn an
  getAnnotationEntry (HsCmdLam {})              = NoEntryVal
  getAnnotationEntry (HsCmdPar an _ _ _)        = fromAnn an
  getAnnotationEntry (HsCmdCase an _ _)         = fromAnn an
  getAnnotationEntry (HsCmdLamCase an _ _)      = fromAnn an
  getAnnotationEntry (HsCmdIf an _ _ _ _)       = fromAnn an
  getAnnotationEntry (HsCmdLet an _ _ _ _)      = fromAnn an
  getAnnotationEntry (HsCmdDo an _)             = fromAnn an

  setAnnotationAnchor (HsCmdArrApp an a b c d)   anc cs = (HsCmdArrApp (setAnchorEpa an anc cs) a b c d)
  setAnnotationAnchor (HsCmdArrForm an a b c d ) anc cs = (HsCmdArrForm (setAnchorEpa an anc cs) a b c d )
  setAnnotationAnchor (HsCmdApp an a b )         anc cs = (HsCmdApp (setAnchorEpa an anc cs) a b )
  setAnnotationAnchor a@(HsCmdLam {})              _ _s = a
  setAnnotationAnchor (HsCmdPar an a b c)        anc cs = (HsCmdPar (setAnchorEpa an anc cs) a b c)
  setAnnotationAnchor (HsCmdCase an a b)         anc cs = (HsCmdCase (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (HsCmdLamCase an a b)      anc cs = (HsCmdLamCase (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (HsCmdIf an a b c d)       anc cs = (HsCmdIf (setAnchorEpa an anc cs) a b c d)
  setAnnotationAnchor (HsCmdLet an a b c d)      anc cs = (HsCmdLet (setAnchorEpa an anc cs) a b c d)
  setAnnotationAnchor (HsCmdDo an a)             anc cs = (HsCmdDo (setAnchorEpa an anc cs) a)

  exact (HsCmdArrApp an arr arg o isRightToLeft) = do
    if isRightToLeft
      then do
        arr' <- markAnnotated arr
        an0 <- markKw (anns an)
        arg' <- markAnnotated arg
        let an1 = an{anns = an0}
        return (HsCmdArrApp an1 arr' arg' o isRightToLeft)
      else do
        arg' <- markAnnotated arg
        an0 <- markKw (anns an)
        arr' <- markAnnotated arr
        let an1 = an {anns = an0}
        return (HsCmdArrApp an1 arr' arg' o isRightToLeft)

  exact (HsCmdArrForm an e fixity mf cs) = do
    an0 <- markLensMAA an lal_open
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
    an1 <- markLensMAA an0 lal_close
    return (HsCmdArrForm an1 e' fixity mf cs')

  exact (HsCmdApp an e1 e2) = do
    e1' <- markAnnotated e1
    e2' <- markAnnotated e2
    return (HsCmdApp an e1' e2')

  exact (HsCmdLam a match) = do
    match' <- markAnnotated match
    return (HsCmdLam a match')

  exact (HsCmdPar an lpar e rpar) = do
    lpar' <- markToken lpar
    e' <- markAnnotated e
    rpar' <- markToken rpar
    return (HsCmdPar an lpar' e' rpar')

  exact (HsCmdCase an e alts) = do
    an0 <- markLensKw an lhsCaseAnnCase AnnCase
    e' <- markAnnotated e
    an1 <- markLensKw an0 lhsCaseAnnOf AnnOf
    an2 <- markEpAnnL an1 lhsCaseAnnsRest AnnOpenC
    an3 <- markEpAnnAllL an2 lhsCaseAnnsRest AnnSemi
    alts' <- markAnnotated alts
    an4 <- markEpAnnL an3 lhsCaseAnnsRest AnnCloseC
    return (HsCmdCase an4 e' alts')

  exact (HsCmdLamCase an lc_variant matches) = do
    an0 <- markEpAnnL an lidl AnnLam
    an1 <- markEpAnnL an0 lidl (case lc_variant of LamCase -> AnnCase
                                                   LamCases -> AnnCases)
    matches' <- markAnnotated matches
    return (HsCmdLamCase an1 lc_variant matches')

  exact (HsCmdIf an a e1 e2 e3) = do
    an0 <- markLensKw an laiIf AnnIf
    e1' <- markAnnotated e1
    an1 <- markLensKwM an0 laiThenSemi AnnSemi
    an2 <- markLensKw an1 laiThen AnnThen
    e2' <- markAnnotated e2
    an3 <- markLensKwM an2 laiElseSemi AnnSemi
    an4 <- markLensKw an3 laiElse AnnElse
    e3' <- markAnnotated e3
    return (HsCmdIf an4 a e1' e2' e3')

  exact (HsCmdLet an tkLet binds tkIn e) = do
    setLayoutBoth $ do -- Make sure the 'in' gets indented too
      tkLet' <- markToken tkLet
      binds' <- setLayoutBoth $ markAnnotated binds
      tkIn' <- markToken tkIn
      e' <- markAnnotated e
      return (HsCmdLet an tkLet' binds' tkIn' e')

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
  getAnnotationEntry (LastStmt _ _ _ _)             = NoEntryVal
  getAnnotationEntry (BindStmt an _ _)              = fromAnn an
  getAnnotationEntry (ApplicativeStmt _ _ _)        = NoEntryVal
  getAnnotationEntry (BodyStmt _ _ _ _)             = NoEntryVal
  getAnnotationEntry (LetStmt an _)                 = fromAnn an
  getAnnotationEntry (ParStmt _ _ _ _)              = NoEntryVal
  getAnnotationEntry (TransStmt an _ _ _ _ _ _ _ _) = fromAnn an
  getAnnotationEntry (RecStmt an _ _ _ _ _ _)       = fromAnn an

  -----------------------------------------------------------------

  setAnnotationAnchor a@(LastStmt _ _ _ _)             _ _s = a
  setAnnotationAnchor (BindStmt an a b)              anc cs = (BindStmt (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor a@(ApplicativeStmt _ _ _)        _ _s = a
  setAnnotationAnchor a@(BodyStmt _ _ _ _)             _ _s = a
  setAnnotationAnchor (LetStmt an a)                 anc cs = (LetStmt (setAnchorEpa an anc cs) a)
  setAnnotationAnchor a@(ParStmt _ _ _ _)              _ _s = a
  setAnnotationAnchor (TransStmt an a b c d e f g h) anc cs = (TransStmt (setAnchorEpa an anc cs) a b c d e f g h)
  setAnnotationAnchor (RecStmt an a b c d e f)       anc cs = (RecStmt (setAnchorEpa an anc cs) a b c d e f)

  -----------------------------------------------------------------

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
    (an1, stmts') <- markAnnList True an0 (markAnnotated stmts)
    return (RecStmt an1 stmts' a b c d e)

-- ---------------------------------------------------------------------

instance ExactPrint (ParStmtBlock GhcPs GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a
  exact (ParStmtBlock a stmts b c) = do
    stmts' <- markAnnotated stmts
    return (ParStmtBlock a stmts' b c)

exactTransStmt :: (Monad m, Monoid w)
  => EpAnn [AddEpAnn] -> Maybe (LHsExpr GhcPs) -> (LHsExpr GhcPs) -> TransForm
  -> EP w m (EpAnn [AddEpAnn], Maybe (LHsExpr GhcPs), (LHsExpr GhcPs))
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
  getAnnotationEntry (FamDecl   { })                   = NoEntryVal
  getAnnotationEntry (SynDecl   { tcdSExt = an })      = fromAnn an
  getAnnotationEntry (DataDecl  { tcdDExt = an })      = fromAnn an
  getAnnotationEntry (ClassDecl { tcdCExt = (an, _) }) = fromAnn an

  setAnnotationAnchor a@FamDecl{}     _ _s = a
  setAnnotationAnchor x@SynDecl{}   anc cs = x { tcdSExt = setAnchorEpa (tcdSExt x) anc cs }
  setAnnotationAnchor x@DataDecl{}  anc cs = x { tcdDExt = setAnchorEpa (tcdDExt x) anc cs }
  setAnnotationAnchor x@ClassDecl{} anc cs = x { tcdCExt = (setAnchorEpa an anc cs, a) }
    where
      (an,a) = tcdCExt x

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

  exact (ClassDecl {tcdCExt = (an, sortKey),
                    tcdLayout = lo,
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
          return (ClassDecl {tcdCExt = (an2, sortKey),
                             tcdLayout = lo,
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
          an2 <- markEpAnnAllL an1 lidl AnnSemi
          ds <- withSortKey sortKey
                               (prepareListAnnotationA sigs
                             ++ prepareListAnnotationA (bagToList methods)
                             ++ prepareListAnnotationA ats
                             ++ prepareListAnnotationA at_defs
                             -- ++ prepareListAnnotation docs
                               )
          an3 <- markEpAnnL an2 lidl AnnCloseC
          let
            sigs'    = undynamic ds
            methods' = listToBag $ undynamic ds
            ats'     = undynamic ds
            at_defs' = undynamic ds
          return (ClassDecl {tcdCExt = (an3, sortKey),
                             tcdLayout = lo,
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
  getAnnotationEntry (FunDep an _ _) = fromAnn an
  setAnnotationAnchor (FunDep an a b) anc cs = FunDep (setAnchorEpa an anc cs) a b

  exact (FunDep an ls rs') = do
    ls' <- markAnnotated ls
    an0 <- markEpAnnL an lidl AnnRarrow
    rs'' <- markAnnotated rs'
    return (FunDep an0 ls' rs'')

-- ---------------------------------------------------------------------

instance ExactPrint (FamilyDecl GhcPs) where
  getAnnotationEntry (FamilyDecl { fdExt = an }) = fromAnn an
  setAnnotationAnchor x anc cs = x { fdExt = setAnchorEpa (fdExt x) anc cs}

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


exactFlavour :: (Monad m, Monoid w) => EpAnn [AddEpAnn] -> FamilyInfo GhcPs -> EP w m (EpAnn [AddEpAnn])
exactFlavour an DataFamily            = markEpAnnL an lidl AnnData
exactFlavour an OpenTypeFamily        = markEpAnnL an lidl AnnType
exactFlavour an (ClosedTypeFamily {}) = markEpAnnL an lidl AnnType

-- ---------------------------------------------------------------------

exactDataDefn
  :: (Monad m, Monoid w)
  => EpAnn [AddEpAnn]
  -> (Maybe (LHsContext GhcPs) -> EP w m (EpAnn [AddEpAnn]
                                         , LocatedN RdrName
                                         , a
                                         , b
                                         , Maybe (LHsContext GhcPs))) -- Printing the header
  -> HsDataDefn GhcPs
  -> EP w m ( EpAnn [AddEpAnn] -- ^ from exactHdr
            , EpAnn [AddEpAnn] -- ^ updated one passed in
            , LocatedN RdrName, a, b, Maybe (LHsContext GhcPs), HsDataDefn GhcPs)
exactDataDefn an exactHdr
                 (HsDataDefn { dd_ext = x, dd_ctxt = context
                             , dd_cType = mb_ct
                             , dd_kindSig = mb_sig
                             , dd_cons = condecls, dd_derivs = derivings }) = do

  an' <- annotationsToComments an lidl [AnnOpenP, AnnCloseP]

  an0 <- markEpAnnL an' lidl $ case condecls of
    DataTypeCons _ _ -> AnnData
    NewTypeCon   _ -> AnnNewtype

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
                     -> EP w m ( EpAnn [AddEpAnn]
                               , LocatedN RdrName
                               , LHsQTyVars GhcPs
                               , (), Maybe (LHsContext GhcPs))
exactVanillaDeclHead thing tvs@(HsQTvs { hsq_explicit = tyvars }) fixity context = do
  let
    exact_tyvars (varl:varsr)
      | fixity == Infix && length varsr > 1 = do
          varl' <- markAnnotated varl
          thing' <- markAnnotated thing
          hvarsr <- markAnnotated (head varsr)
          tvarsr <- markAnnotated (tail varsr)
          return (thing', varl':hvarsr:tvarsr)
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
  return (EpAnnNotUsed, thing', tvs { hsq_explicit = tyvars' }, (), context')

-- ---------------------------------------------------------------------

instance ExactPrint (InjectivityAnn GhcPs) where
  getAnnotationEntry (InjectivityAnn an _ _) = fromAnn an
  setAnnotationAnchor (InjectivityAnn an a b) anc cs = InjectivityAnn (setAnchorEpa an anc cs) a b
  exact (InjectivityAnn an lhs rhs) = do
    an0 <- markEpAnnL an lidl AnnVbar
    lhs' <- markAnnotated lhs
    an1 <- markEpAnnL an0 lidl AnnRarrow
    rhs' <- mapM markAnnotated rhs
    return (InjectivityAnn an1 lhs' rhs')

-- ---------------------------------------------------------------------

class Typeable flag => ExactPrintTVFlag flag where
  exactTVDelimiters :: (Monad m, Monoid w)
    => EpAnn [AddEpAnn] -> flag -> EP w m (HsTyVarBndr flag GhcPs)
    -> EP w m (EpAnn [AddEpAnn], (HsTyVarBndr flag GhcPs))

instance ExactPrintTVFlag () where
  exactTVDelimiters an _ thing_inside = do
    an0 <- markEpAnnAllL an lid AnnOpenP
    r <- thing_inside
    an1 <- markEpAnnAllL an0 lid AnnCloseP
    return (an1, r)

instance ExactPrintTVFlag Specificity where
  exactTVDelimiters an s thing_inside = do
    an0 <- markEpAnnAllL an lid open
    r <- thing_inside
    an1 <- markEpAnnAllL an0 lid close
    return (an1, r)
    where
      (open, close) = case s of
        SpecifiedSpec -> (AnnOpenP, AnnCloseP)
        InferredSpec  -> (AnnOpenC, AnnCloseC)

instance ExactPrintTVFlag flag => ExactPrint (HsTyVarBndr flag GhcPs) where
  getAnnotationEntry (UserTyVar an _ _)     = fromAnn an
  getAnnotationEntry (KindedTyVar an _ _ _) = fromAnn an

  setAnnotationAnchor (UserTyVar an a b)     anc cs = UserTyVar (setAnchorEpa an anc cs) a b
  setAnnotationAnchor (KindedTyVar an a b c) anc cs = KindedTyVar (setAnchorEpa an anc cs) a b c

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
  getAnnotationEntry (HsForAllTy _ _ _)        = NoEntryVal
  getAnnotationEntry (HsQualTy _ _ _)          = NoEntryVal
  getAnnotationEntry (HsTyVar an _ _)          = fromAnn an
  getAnnotationEntry (HsAppTy _ _ _)           = NoEntryVal
  getAnnotationEntry (HsAppKindTy _ _ _)       = NoEntryVal
  getAnnotationEntry (HsFunTy an _ _ _)        = fromAnn an
  getAnnotationEntry (HsListTy an _)           = fromAnn an
  getAnnotationEntry (HsTupleTy an _ _)        = fromAnn an
  getAnnotationEntry (HsSumTy an _)            = fromAnn an
  getAnnotationEntry (HsOpTy an _ _ _ _)       = fromAnn an
  getAnnotationEntry (HsParTy an _)            = fromAnn an
  getAnnotationEntry (HsIParamTy an _ _)       = fromAnn an
  getAnnotationEntry (HsStarTy _ _)            = NoEntryVal
  getAnnotationEntry (HsKindSig an _ _)        = fromAnn an
  getAnnotationEntry (HsSpliceTy _ _)          = NoEntryVal
  getAnnotationEntry (HsDocTy an _ _)          = fromAnn an
  getAnnotationEntry (HsBangTy an _ _)         = fromAnn an
  getAnnotationEntry (HsRecTy an _)            = fromAnn an
  getAnnotationEntry (HsExplicitListTy an _ _) = fromAnn an
  getAnnotationEntry (HsExplicitTupleTy an _)  = fromAnn an
  getAnnotationEntry (HsTyLit _ _)             = NoEntryVal
  getAnnotationEntry (HsWildCardTy _)          = NoEntryVal
  getAnnotationEntry (XHsType _)               = NoEntryVal

  setAnnotationAnchor a@(HsForAllTy _ _ _)        _ _s = a
  setAnnotationAnchor a@(HsQualTy _ _ _)          _ _s = a
  setAnnotationAnchor (HsTyVar an a b)          anc cs = (HsTyVar (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor a@(HsAppTy _ _ _)           _ _s = a
  setAnnotationAnchor a@(HsAppKindTy _ _ _)       _ _s = a
  setAnnotationAnchor (HsFunTy an a b c)        anc cs = (HsFunTy (setAnchorEpa an anc cs) a b c)
  setAnnotationAnchor (HsListTy an a)           anc cs = (HsListTy (setAnchorEpa an anc cs) a)
  setAnnotationAnchor (HsTupleTy an a b)        anc cs = (HsTupleTy (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (HsSumTy an a)            anc cs = (HsSumTy (setAnchorEpa an anc cs) a)
  setAnnotationAnchor a@(HsOpTy _ _ _ _ _)        _ _s = a
  setAnnotationAnchor (HsParTy an a)            anc cs = (HsParTy (setAnchorEpa an anc cs) a)
  setAnnotationAnchor (HsIParamTy an a b)       anc cs = (HsIParamTy (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor a@(HsStarTy _ _)            _ _s = a
  setAnnotationAnchor (HsKindSig an a b)        anc cs = (HsKindSig (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor a@(HsSpliceTy _ _)          _ _s = a
  setAnnotationAnchor (HsDocTy an a b)          anc cs = (HsDocTy (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (HsBangTy an a b)         anc cs = (HsBangTy (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (HsRecTy an a)            anc cs = (HsRecTy (setAnchorEpa an anc cs) a)
  setAnnotationAnchor (HsExplicitListTy an a b) anc cs = (HsExplicitListTy (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (HsExplicitTupleTy an a)  anc cs = (HsExplicitTupleTy (setAnchorEpa an anc cs) a)
  setAnnotationAnchor a@(HsTyLit _ _)             _ _s = a
  setAnnotationAnchor a@(HsWildCardTy _)          _ _s = a
  setAnnotationAnchor a@(XHsType _)               _ _s = a

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
  exact (HsAppKindTy ss ty ki) = do
    ty' <- markAnnotated ty
    printStringAtSs ss "@"
    ki' <- markAnnotated ki
    return (HsAppKindTy ss ty' ki')
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
  exact (HsBangTy an (HsSrcBang mt up str) ty) = do
    an0 <-
      case mt of
        NoSourceText -> return an
        SourceText src -> do
          debugM $ "HsBangTy: src=" ++ showAst src
          an0 <- markEpAnnLMS an lid AnnOpen  (Just src)
          an1 <- markEpAnnLMS an0 lid AnnClose (Just "#-}")
          debugM $ "HsBangTy: done unpackedness"
          return an1
    an1 <-
      case str of
        SrcLazy     -> markEpAnnL an0 lidl AnnTilde
        SrcStrict   -> markEpAnnL an0 lidl AnnBang
        NoSrcStrict -> return an0
    ty' <- markAnnotated ty
    return (HsBangTy an1 (HsSrcBang mt up str) ty')
  exact (HsExplicitListTy an prom tys) = do
    an0 <- if (isPromoted prom)
             then markEpAnnL an lidl AnnSimpleQuote
             else return an
    an1 <- markEpAnnL an0 lidl AnnOpenS
    tys' <- markAnnotated tys
    an2 <- markEpAnnL an1 lidl AnnCloseS
    return (HsExplicitListTy an2 prom tys')
  exact (HsExplicitTupleTy an tys) = do
    an0 <- markEpAnnL an lidl AnnSimpleQuote
    an1 <- markEpAnnL an0 lidl AnnOpenP
    tys' <- markAnnotated tys
    an2 <- markEpAnnL an1 lidl AnnCloseP
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

  setAnnotationAnchor (HsForAllVis an a) anc cs = HsForAllVis (setAnchorEpa an anc cs) a
  setAnnotationAnchor (HsForAllInvis an a) anc cs = HsForAllInvis (setAnchorEpa an anc cs) a

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
  getAnnotationEntry d@(HsDerivingClause{}) = fromAnn (deriv_clause_ext d)
  setAnnotationAnchor x anc cs = (x { deriv_clause_ext = setAnchorEpa (deriv_clause_ext x) anc cs})
                                   `debug` ("setAnnotationAnchor HsDerivingClause: (anc,cs):" ++ showAst (anc,cs))

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
  getAnnotationEntry (StockStrategy an)    = fromAnn an
  getAnnotationEntry (AnyclassStrategy an) = fromAnn an
  getAnnotationEntry (NewtypeStrategy an)  = fromAnn an
  getAnnotationEntry (ViaStrategy (XViaStrategyPs an  _)) = fromAnn an

  setAnnotationAnchor (StockStrategy an)    anc cs = (StockStrategy (setAnchorEpa an anc cs))
  setAnnotationAnchor (AnyclassStrategy an) anc cs = (AnyclassStrategy (setAnchorEpa an anc cs))
  setAnnotationAnchor (NewtypeStrategy an)  anc cs = (NewtypeStrategy (setAnchorEpa an anc cs))
  setAnnotationAnchor (ViaStrategy (XViaStrategyPs an  a)) anc cs = (ViaStrategy (XViaStrategyPs (setAnchorEpa an anc cs)  a))

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

  exact (L (SrcSpanAnn EpAnnNotUsed l) a) = do
    a' <- markAnnotated a
    return (L (SrcSpanAnn EpAnnNotUsed l) a')
  exact (L (SrcSpanAnn (EpAnn anc (AnnContext ma opens closes) cs) l) a) = do
    opens' <- mapM (markKwA AnnOpenP) opens
    a' <- markAnnotated a
    closes' <- mapM (markKwA AnnCloseP) closes
    ma' <- case ma of
      Just (UnicodeSyntax, r) -> Just . (UnicodeSyntax,) <$> markKwA AnnDarrowU r
      Just (NormalSyntax,  r) -> Just . (NormalSyntax,) <$> markKwA AnnDarrow  r
      Nothing -> pure Nothing
    return (L (SrcSpanAnn (EpAnn anc (AnnContext ma' opens' closes') cs) l) a')

-- ---------------------------------------------------------------------

instance ExactPrint (DerivClauseTys GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a

  exact (DctSingle x ty) = do
    ty' <- markAnnotated ty
    return (DctSingle x ty')
  exact (DctMulti x tys) = do
    tys' <- markAnnotated tys
    return (DctMulti x tys')

-- ---------------------------------------------------------------------

instance ExactPrint (HsSigType GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a

  exact (HsSig a bndrs ty) = do
    bndrs' <- markAnnotated bndrs
    ty' <- markAnnotated ty
    return (HsSig a bndrs' ty')

-- ---------------------------------------------------------------------

instance ExactPrint (LocatedN RdrName) where
  getAnnotationEntry (L sann _) = fromAnn sann
  setAnnotationAnchor = setAnchorAn

  exact x@(L (SrcSpanAnn EpAnnNotUsed l) n) = do
    _ <- printUnicode (spanAsAnchor l) n
    return x
  exact (L (SrcSpanAnn (EpAnn anc ann cs) ll) n) = do
    ann' <-
      case ann of
        NameAnn a o l c t -> do
          mn <- markName a o (Just (l,n)) c
          case mn of
            (o', (Just (l',_n)), c') -> do -- (o', (Just (l',n')), c')
              t' <- markTrailing t
              return (NameAnn a o' l' c' t')
            _ -> error "ExactPrint (LocatedN RdrName)"
        NameAnnCommas a o commas c t -> do
          let (kwo,kwc) = adornments a
          (AddEpAnn _ o') <- markKwC NoCaptureComments (AddEpAnn kwo o)
          commas' <- forM commas (\loc -> locFromAdd <$> markKwC NoCaptureComments (AddEpAnn AnnComma loc))
          (AddEpAnn _ c') <- markKwC NoCaptureComments (AddEpAnn kwc c)
          t' <- markTrailing t
          return (NameAnnCommas a o' commas' c' t')
        NameAnnBars a o bars c t -> do
          let (kwo,kwc) = adornments a
          (AddEpAnn _ o') <- markKwC NoCaptureComments (AddEpAnn kwo o)
          bars' <- forM bars (\loc -> locFromAdd <$> markKwC NoCaptureComments (AddEpAnn AnnVbar loc))
          (AddEpAnn _ c') <- markKwC NoCaptureComments (AddEpAnn kwc c)
          t' <- markTrailing t
          return (NameAnnBars a o' bars' c' t')
        NameAnnOnly a o c t -> do
          (o',_,c') <- markName a o Nothing c
          t' <- markTrailing t
          return (NameAnnOnly a o' c' t')
        NameAnnRArrow nl t -> do
          (AddEpAnn _ nl') <- markKwC NoCaptureComments (AddEpAnn AnnRarrow nl)
          t' <- markTrailing t
          return (NameAnnRArrow nl' t')
        NameAnnQuote q name t -> do
          debugM $ "NameAnnQuote"
          (AddEpAnn _ q') <- markKwC NoCaptureComments (AddEpAnn AnnSimpleQuote q)
          (L name' _) <- markAnnotated (L name n)
          t' <- markTrailing t
          return (NameAnnQuote q' name' t')
        NameAnnTrailing t -> do
          _anc' <- printUnicode anc n
          t' <- markTrailing t
          return (NameAnnTrailing t')
    return (L (SrcSpanAnn (EpAnn anc ann' cs) ll) n)

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
    EpaSpan _ _ -> return anc
    EpaDelta dp [] -> return anc { anchor_op = MovedAnchor dp }
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


markTrailingL :: (Monad m, Monoid w) => EpAnn a -> Lens a [TrailingAnn] -> EP w m (EpAnn a)
markTrailingL EpAnnNotUsed _ = return EpAnnNotUsed
markTrailingL (EpAnn anc an cs) l = do
  ts <- mapM markKwT (view l an)
  return (EpAnn anc (set l ts an) cs)

markTrailing :: (Monad m, Monoid w) => [TrailingAnn] -> EP w m [TrailingAnn]
markTrailing ts = do
  p <- getPosP
  debugM $ "markTrailing:" ++ showPprUnsafe (p,ts)
  mapM markKwT ts

-- ---------------------------------------------------------------------

-- based on pp_condecls in Decls.hs
exact_condecls :: (Monad m, Monoid w)
  => EpAnn [AddEpAnn] -> [LConDecl GhcPs] -> EP w m (EpAnn [AddEpAnn],[LConDecl GhcPs])
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
  getAnnotationEntry x@(ConDeclGADT{}) = fromAnn (con_g_ext x)
  getAnnotationEntry x@(ConDeclH98{})  = fromAnn (con_ext x)

  setAnnotationAnchor x@ConDeclGADT{} anc cs = x { con_g_ext = setAnchorEpa (con_g_ext x) anc cs}
  setAnnotationAnchor x@ConDeclH98{}  anc cs = x { con_ext   = setAnchorEpa (con_ext x) anc cs}

-- based on pprConDecl
  exact (ConDeclH98 { con_ext = an
                    , con_name = con
                    , con_forall = has_forall
                    , con_ex_tvs = ex_tvs
                    , con_mb_cxt = mcxt
                    , con_args = args
                    , con_doc = doc }) = do
    doc' <- mapM markAnnotated doc
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
                       , con_doc = doc' })

    where
    --   -- In ppr_details: let's not print the multiplicities (they are always 1, by
    --   -- definition) as they do not appear in an actual declaration.
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

  exact (ConDeclGADT { con_g_ext = an
                     , con_names = cons
                     , con_dcolon = dcol
                     , con_bndrs = bndrs
                     , con_mb_cxt = mcxt, con_g_args = args
                     , con_res_ty = res_ty, con_doc = doc }) = do
    doc' <- mapM markAnnotated doc
    cons' <- mapM markAnnotated cons
    dcol' <- markUniToken dcol
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
          (PrefixConGADT args0) -> do
            args0' <- mapM markAnnotated args0
            return (PrefixConGADT args0')
          (RecConGADT fields rarr) -> do
            fields' <- markAnnotated fields
            rarr' <- markUniToken rarr
            return (RecConGADT fields' rarr')
    res_ty' <- markAnnotated res_ty
    return (ConDeclGADT { con_g_ext = an2
                        , con_names = cons'
                        , con_dcolon = dcol'
                        , con_bndrs = bndrs'
                        , con_mb_cxt = mcxt', con_g_args = args'
                        , con_res_ty = res_ty', con_doc = doc' })

-- ---------------------------------------------------------------------

instance ExactPrint Void where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a
  exact x = return x

-- ---------------------------------------------------------------------

instance ExactPrintTVFlag flag => ExactPrint (HsOuterTyVarBndrs flag GhcPs) where
  getAnnotationEntry (HsOuterImplicit _) = NoEntryVal
  getAnnotationEntry (HsOuterExplicit an _) = fromAnn an

  setAnnotationAnchor (HsOuterImplicit a) _ _ = HsOuterImplicit a
  setAnnotationAnchor (HsOuterExplicit an a) anc cs = HsOuterExplicit (setAnchorEpa an anc cs) a

  exact b@(HsOuterImplicit _) = pure b
  exact (HsOuterExplicit an bndrs) = do
    an0 <- markLensAA an lfst -- "forall"
    bndrs' <- markAnnotated bndrs
    an1 <- markLensAA an0 lsnd -- "."
    return (HsOuterExplicit an1 bndrs')

-- ---------------------------------------------------------------------

instance ExactPrint (ConDeclField GhcPs) where
  getAnnotationEntry f@(ConDeclField{}) = fromAnn (cd_fld_ext f)

  setAnnotationAnchor x anc cs = x { cd_fld_ext = setAnchorEpa (cd_fld_ext x) anc cs}

  exact (ConDeclField an names ftype mdoc) = do
    names' <- markAnnotated names
    an0 <- markEpAnnL an lidl AnnDcolon
    ftype' <- markAnnotated ftype
    mdoc' <- mapM markAnnotated mdoc
    return (ConDeclField an0 names' ftype' mdoc')

-- ---------------------------------------------------------------------

instance ExactPrint (FieldOcc GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a
  exact f@(FieldOcc _ n) = markAnnotated n >> return f

-- ---------------------------------------------------------------------

instance ExactPrint (AmbiguousFieldOcc GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a
  exact f@(Unambiguous _ n) = markAnnotated n >> return f
  exact f@(Ambiguous   _ n) = markAnnotated n >> return f

-- ---------------------------------------------------------------------

instance (ExactPrint a) => ExactPrint (HsScaled GhcPs a) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a
  exact (HsScaled arr t) = do
    t' <- markAnnotated t
    arr' <- markArrow arr
    return (HsScaled arr' t')

-- ---------------------------------------------------------------------

instance ExactPrint (LocatedP CType) where
  getAnnotationEntry = entryFromLocatedA
  setAnnotationAnchor = setAnchorAn

  exact x@(L (SrcSpanAnn EpAnnNotUsed _) ct) = withPpr ct >> return x
  exact (L (SrcSpanAnn an ll)
         (CType stp mh (stct,ct))) = do
    an0 <- markAnnOpenP an stp "{-# CTYPE"
    an1 <- case mh of
             Nothing -> return an0
             Just (Header srcH _h) ->
               markEpAnnLMS an0 lapr_rest AnnHeader (Just (toSourceTextWithSuffix srcH "" ""))
    an2 <- markEpAnnLMS an1 lapr_rest AnnVal (Just (toSourceTextWithSuffix stct (unpackFS ct) ""))
    an3 <- markAnnCloseP an2
    return (L (SrcSpanAnn an3 ll)
         (CType stp mh (stct,ct)))

-- ---------------------------------------------------------------------

instance ExactPrint (SourceText, RuleName) where
  -- We end up at the right place from the Located wrapper
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a

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

  exact (L (SrcSpanAnn an l) ies) = do
    debugM $ "LocatedL [LIE"
    an0 <- markEpAnnL an lal_rest AnnHiding
    p <- getPosP
    debugM $ "LocatedL [LIE:p=" ++ showPprUnsafe p
    (an1, ies') <- markAnnList True an0 (markAnnotated ies)
    return (L (SrcSpanAnn an1 l) ies')

instance (ExactPrint (Match GhcPs (LocatedA body)))
   => ExactPrint (LocatedL [LocatedA (Match GhcPs (LocatedA body))]) where
  getAnnotationEntry = entryFromLocatedA
  setAnnotationAnchor = setAnchorAn
  exact (L la a) = do
    let an = ann la
    debugM $ "LocatedL [LMatch"
    -- TODO: markAnnList?
    an0 <- markEpAnnAllL an lal_rest AnnWhere
    an1 <- markLensMAA an0 lal_open
    an2 <- markEpAnnAllL an1 lal_rest AnnSemi
    a' <- markAnnotated a
    an3 <- markLensMAA an2 lal_close
    return (L (la { ann = an3}) a')

instance ExactPrint (LocatedL [LocatedA (StmtLR GhcPs GhcPs (LocatedA (HsExpr GhcPs)))]) where
  getAnnotationEntry = entryFromLocatedA
  setAnnotationAnchor = setAnchorAn
  exact (L (SrcSpanAnn an l) stmts) = do
    debugM $ "LocatedL [ExprLStmt"
    (an'', stmts') <- markAnnList True an $ do
      case snocView stmts of
        Just (initStmts, ls@(L _ (LastStmt _ _body _ _))) -> do
          debugM $ "LocatedL [ExprLStmt: snocView"
          ls' <- markAnnotated ls
          initStmts' <- markAnnotated initStmts
          return (initStmts' ++ [ls'])
        _ -> do
          markAnnotated stmts
    return (L (SrcSpanAnn an'' l) stmts')

-- instance ExactPrint (LocatedL [CmdLStmt GhcPs]) where
instance ExactPrint (LocatedL [LocatedA (StmtLR GhcPs GhcPs (LocatedA (HsCmd GhcPs)))]) where
  getAnnotationEntry = entryFromLocatedA
  setAnnotationAnchor = setAnchorAn
  exact (L (SrcSpanAnn ann l) es) = do
    debugM $ "LocatedL [CmdLStmt"
    an0 <- markLensMAA ann lal_open
    es' <- mapM markAnnotated es
    an1 <- markLensMAA an0 lal_close
    return (L (SrcSpanAnn an1 l) es')

instance ExactPrint (LocatedL [LocatedA (ConDeclField GhcPs)]) where
  getAnnotationEntry = entryFromLocatedA
  setAnnotationAnchor = setAnchorAn
  exact (L (SrcSpanAnn an l) fs) = do
    debugM $ "LocatedL [LConDeclField"
    (an', fs') <- markAnnList True an (markAnnotated fs)
    return (L (SrcSpanAnn an' l) fs')

instance ExactPrint (LocatedL (BF.BooleanFormula (LocatedN RdrName))) where
  getAnnotationEntry = entryFromLocatedA
  setAnnotationAnchor = setAnchorAn
  exact (L (SrcSpanAnn an l) bf) = do
    debugM $ "LocatedL [LBooleanFormula"
    (an', bf') <- markAnnList True an (markAnnotated bf)
    return (L (SrcSpanAnn an' l) bf')

-- ---------------------------------------------------------------------
-- LocatedL instances end --
-- =====================================================================

instance ExactPrint (IE GhcPs) where
  getAnnotationEntry (IEVar _ _)            = NoEntryVal
  getAnnotationEntry (IEThingAbs an _)      = fromAnn an
  getAnnotationEntry (IEThingAll an _)      = fromAnn an
  getAnnotationEntry (IEThingWith an _ _ _) = fromAnn an
  getAnnotationEntry (IEModuleContents an _)= fromAnn an
  getAnnotationEntry (IEGroup _ _ _)        = NoEntryVal
  getAnnotationEntry (IEDoc _ _)            = NoEntryVal
  getAnnotationEntry (IEDocNamed _ _)       = NoEntryVal

  setAnnotationAnchor a@(IEVar _ _)             _ _s = a
  setAnnotationAnchor (IEThingAbs an a)       anc cs = (IEThingAbs (setAnchorEpa an anc cs) a)
  setAnnotationAnchor (IEThingAll an a)       anc cs = (IEThingAll (setAnchorEpa an anc cs) a)
  setAnnotationAnchor (IEThingWith an a b c)  anc cs = (IEThingWith (setAnchorEpa an anc cs) a b c)
  setAnnotationAnchor (IEModuleContents an a) anc cs = (IEModuleContents (setAnchorEpa an anc cs) a)
  setAnnotationAnchor a@(IEGroup _ _ _)         _ _s = a
  setAnnotationAnchor a@(IEDoc _ _)             _ _s = a
  setAnnotationAnchor a@(IEDocNamed _ _)        _ _s = a

  exact (IEVar x ln) = do
    ln' <- markAnnotated ln
    return (IEVar x ln')
  exact (IEThingAbs x thing) = do
    thing' <- markAnnotated thing
    return (IEThingAbs x thing')
  exact (IEThingAll an thing) = do
    thing' <- markAnnotated thing
    an0 <- markEpAnnL an  lidl AnnOpenP
    an1 <- markEpAnnL an0 lidl AnnDotdot
    an2 <- markEpAnnL an1 lidl AnnCloseP
    return (IEThingAll an2 thing')

  exact (IEThingWith an thing wc withs) = do
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
    return (IEThingWith an2 thing' wc' withs')

  exact (IEModuleContents an m) = do
    an0 <- markEpAnnL an lidl AnnModule
    m' <- markAnnotated m
    return (IEModuleContents an0 m')

  exact x = error $ "missing match for IE:" ++ showAst x

-- ---------------------------------------------------------------------

instance ExactPrint (IEWrappedName GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a

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
  getAnnotationEntry (WildPat _)              = NoEntryVal
  getAnnotationEntry (VarPat _ _)             = NoEntryVal
  getAnnotationEntry (LazyPat an _)           = fromAnn an
  getAnnotationEntry (AsPat an _ _ _)         = fromAnn an
  getAnnotationEntry (ParPat an _ _ _)        = fromAnn an
  getAnnotationEntry (BangPat an _)           = fromAnn an
  getAnnotationEntry (ListPat an _)           = fromAnn an
  getAnnotationEntry (TuplePat an _ _)        = fromAnn an
  getAnnotationEntry (SumPat an _ _ _)        = fromAnn an
  getAnnotationEntry (ConPat an _ _)          = fromAnn an
  getAnnotationEntry (ViewPat an _ _)         = fromAnn an
  getAnnotationEntry (SplicePat _ _)          = NoEntryVal
  getAnnotationEntry (LitPat _ _)             = NoEntryVal
  getAnnotationEntry (NPat an _ _ _)          = fromAnn an
  getAnnotationEntry (NPlusKPat an _ _ _ _ _) = fromAnn an
  getAnnotationEntry (SigPat an _ _)          = fromAnn an

  setAnnotationAnchor a@(WildPat _)              _ _s = a
  setAnnotationAnchor a@(VarPat _ _)             _ _s = a
  setAnnotationAnchor (LazyPat an a)            anc cs = (LazyPat (setAnchorEpa an anc cs) a)
  setAnnotationAnchor (AsPat an a at b)         anc cs = (AsPat (setAnchorEpa an anc cs) a at b)
  setAnnotationAnchor (ParPat an a b c)         anc cs = (ParPat (setAnchorEpa an anc cs) a b c)
  setAnnotationAnchor (BangPat an a)            anc cs = (BangPat (setAnchorEpa an anc cs) a)
  setAnnotationAnchor (ListPat an a)            anc cs = (ListPat (setAnchorEpa an anc cs) a)
  setAnnotationAnchor (TuplePat an a b)         anc cs = (TuplePat (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (SumPat an a b c)         anc cs = (SumPat (setAnchorEpa an anc cs) a b c)
  setAnnotationAnchor (ConPat an a b)           anc cs = (ConPat (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (ViewPat an a b)          anc cs = (ViewPat (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor a@(SplicePat _ _)           _ _s = a
  setAnnotationAnchor a@(LitPat _ _)              _ _s = a
  setAnnotationAnchor (NPat an a b c)          anc cs = (NPat (setAnchorEpa an anc cs) a b c)
  setAnnotationAnchor (NPlusKPat an a b c d e) anc cs = (NPlusKPat (setAnchorEpa an anc cs) a b c d e)
  setAnnotationAnchor (SigPat an a b)          anc cs = (SigPat (setAnchorEpa an anc cs) a b)

  exact (WildPat w) = do
    anchor <- getAnchorU
    debugM $ "WildPat:anchor=" ++ show anchor
    _ <- printStringAtRs anchor "_"
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
  exact (AsPat an n at pat) = do
    n' <- markAnnotated n
    at' <- markToken at
    pat' <- markAnnotated pat
    return (AsPat an n' at' pat')
  exact (ParPat an lpar pat rpar) = do
    lpar' <- markToken lpar
    pat' <- markAnnotated pat
    rpar' <- markToken rpar
    return (ParPat an lpar' pat' rpar')

  exact (BangPat an pat) = do
    an0 <- markEpAnnL an lidl AnnBang
    pat' <- markAnnotated pat
    return (BangPat an0 pat')

  exact (ListPat an pats) = do
    (an', pats') <- markAnnList True an (markAnnotated pats)
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

  -- | ConPat an con args)
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

  -- | NPlusKPat an n lit1 lit2 _ _)
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

-- ---------------------------------------------------------------------

instance ExactPrint (HsPatSigType GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a

  exact (HsPS an ty) = do
    ty' <- markAnnotated ty
    return (HsPS an ty')

-- ---------------------------------------------------------------------

instance ExactPrint (HsOverLit GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a

  exact ol =
    let str = case ol_val ol of
                HsIntegral   (IL src _ _) -> src
                HsFractional (FL{ fl_text = src }) -> src
                HsIsString src _ -> src
    in
      case str of
        SourceText s -> printStringAdvance s >> return ol
        NoSourceText -> return ol

-- ---------------------------------------------------------------------

hsLit2String :: HsLit GhcPs -> String
hsLit2String lit =
  case lit of
    HsChar       src v   -> toSourceTextWithSuffix src v ""
    -- It should be included here
    -- https://github.com/ghc/ghc/blob/master/compiler/parser/Lexer.x#L1471
    HsCharPrim   src p   -> toSourceTextWithSuffix src p "#"
    HsString     src v   -> toSourceTextWithSuffix src v ""
    HsStringPrim src v   -> toSourceTextWithSuffix src v ""
    HsInt        _ (IL src _ v)   -> toSourceTextWithSuffix src v ""
    HsIntPrim    src v   -> toSourceTextWithSuffix src v ""
    HsWordPrim   src v   -> toSourceTextWithSuffix src v ""
    HsInt64Prim  src v   -> toSourceTextWithSuffix src v ""
    HsWord64Prim src v   -> toSourceTextWithSuffix src v ""
    HsInteger    src v _ -> toSourceTextWithSuffix src v ""
    HsRat        _ fl@(FL{fl_text = src }) _ -> toSourceTextWithSuffix src fl ""
    HsFloatPrim  _ fl@(FL{fl_text = src })   -> toSourceTextWithSuffix src fl "#"
    HsDoublePrim _ fl@(FL{fl_text = src })   -> toSourceTextWithSuffix src fl "##"

toSourceTextWithSuffix :: (Show a) => SourceText -> a -> String -> String
toSourceTextWithSuffix (NoSourceText)    alt suffix = show alt ++ suffix
toSourceTextWithSuffix (SourceText txt) _alt suffix = txt ++ suffix

sourceTextToString :: SourceText -> String -> String
sourceTextToString NoSourceText alt   = alt
sourceTextToString (SourceText txt) _ = txt

-- ---------------------------------------------------------------------

exactUserCon :: (Monad m, Monoid w, ExactPrint con)
  => EpAnn [AddEpAnn] -> con -> HsConPatDetails GhcPs
  -> EP w m (EpAnn [AddEpAnn], con, HsConPatDetails GhcPs)
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
  setAnnotationAnchor a _ _ = a
  exact (HsConPatTyArg at tyarg) = do
    at' <- markToken at
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

entryFromLocatedA :: LocatedAn ann a -> Entry
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
        -- `debug` ("printStringAtLsDelta:(pos,s):" ++ show (undelta p cl colOffset,s))
      p' <- getPosP
      d <- getPriorEndD
      debugM $ "printStringAtLsDelta:(pos,p',d,s):" ++ show (undelta p cl colOffset,p',d,s)
    else return () `debug` ("printStringAtLsDelta:bad delta for (mc,s):" ++ show (cl,s))

-- ---------------------------------------------------------------------

isGoodDeltaWithOffset :: DeltaPos -> LayoutStartCol -> Bool
isGoodDeltaWithOffset dp colOffset = isGoodDelta (deltaPos l c)
  where (l,c) = undelta (0,0) dp colOffset

-- | Print a comment, using the current layout offset to convert the
-- @DeltaPos@ to an absolute position.
printQueuedComment :: (Monad m, Monoid w) => RealSrcSpan -> Comment -> DeltaPos -> EP w m ()
printQueuedComment _loc Comment{commentContents} dp = do
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
                  , pLHS = 1} )
  r <- k
  debugM $ "setLayoutTopLevelP:resetting"
  setLayoutOffsetP oldAnchorOffset
  return r

------------------------------------------------------------------------

getPosP :: (Monad m, Monoid w) => EP w m Pos
getPosP = gets epPos

setPosP :: (Monad m, Monoid w) => Pos -> EP w m ()
setPosP l = do
  -- debugM $ "setPosP:" ++ show l
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
