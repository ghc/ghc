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
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances  #-} -- For the (StmtLR GhcPs GhcPs (LocatedA (body GhcPs))) ExactPrint instance

module ExactPrint
  (
    ExactPrint(..)
  , exactPrint
  -- , exactPrintWithOptions
  ) where

import GHC
import GHC.Core.Coercion.Axiom (Role(..))
import GHC.Data.Bag
import qualified GHC.Data.BooleanFormula as BF
import GHC.Data.FastString
import GHC.Types.Basic hiding (EP)
import GHC.Types.Fixity
import GHC.Types.ForeignCall
import GHC.Types.SourceText
import GHC.Types.PkgQual
import GHC.Types.Var
import GHC.Utils.Outputable hiding ( (<>) )
import GHC.Unit.Module.Warnings
import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.TypeLits

import Control.Monad.Identity
import Control.Monad.RWS
import Data.Data ( Data )
import Data.Foldable
import Data.Typeable
import Data.List ( partition, sortBy)
import Data.List.NonEmpty ( NonEmpty )
import Data.Maybe ( isJust )

import Data.Void

import Lookup
import Utils
import Types
import Data.Ord

-- import Debug.Trace

-- ---------------------------------------------------------------------

exactPrint :: ExactPrint ast => Located ast -> String
exactPrint ast = runIdentity (runEP stringOptions (markAnnotated ast))

type EP w m a = RWST (PrintOptions m w) (EPWriter w) EPState m a
type EPP a = EP String Identity a

runEP :: PrintOptions Identity String
      -> Annotated () -> Identity String
runEP epReader action =
  fmap (output . snd) .
    (\next -> execRWST next epReader defaultEPState)
    . xx $ action

xx :: Annotated () -> EP String Identity ()
-- xx :: Annotated() -> RWST (PrintOptions m w) (EPWriter w) EPState m ()
xx = id

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
             , epComments = []
             }


-- ---------------------------------------------------------------------
-- The EP monad and basic combinators

-- | The R part of RWS. The environment. Updated via 'local' as we
-- enter a new AST element, having a different anchor point.
data PrintOptions m a = PrintOptions
            {
              epAnn :: !Annotation
            , epAstPrint :: forall ast . Data ast => GHC.Located ast -> a -> m a
            , epTokenPrint :: String -> m a
            , epWhitespacePrint :: String -> m a
            , epRigidity :: Rigidity
            }

-- | Helper to create a 'PrintOptions'
printOptions ::
      (forall ast . Data ast => GHC.Located ast -> a -> m a)
      -> (String -> m a)
      -> (String -> m a)
      -> Rigidity
      -> PrintOptions m a
printOptions astPrint tokenPrint wsPrint rigidity = PrintOptions
             {
               epAnn = annNone
             , epAstPrint = astPrint
             , epWhitespacePrint = wsPrint
             , epTokenPrint = tokenPrint
             , epRigidity = rigidity
             }

-- | Options which can be used to print as a normal String.
stringOptions :: PrintOptions Identity String
stringOptions = printOptions (\_ b -> return b) return return NormalLayout

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
             }

-- ---------------------------------------------------------------------

-- AZ:TODO: this can just be a function :: (EpAnn a) -> Entry
class HasEntry ast where
  fromAnn :: ast -> Entry

-- ---------------------------------------------------------------------

-- type Annotated = FreeT AnnotationF Identity
type Annotated a = EP String Identity a

-- ---------------------------------------------------------------------

-- | Key entry point.  Switches to an independent AST element with its
-- own annotation, calculating new offsets, etc
markAnnotated :: ExactPrint a => a -> Annotated ()
markAnnotated a = enterAnn (getAnnotationEntry a) a

data Entry = Entry Anchor EpAnnComments
           | NoEntryVal

instance (HasEntry (EpAnn an)) =>  HasEntry (SrcSpanAnn' (EpAnn an)) where
  fromAnn (SrcSpanAnn EpAnnNotUsed ss) = Entry (spanAsAnchor ss) emptyComments
  fromAnn (SrcSpanAnn an _) = fromAnn an

instance HasEntry (EpAnn a) where
  fromAnn (EpAnn anchor _ cs) = Entry anchor cs
  fromAnn EpAnnNotUsed = NoEntryVal

-- ---------------------------------------------------------------------

astId :: (Typeable a) => a -> String
astId a = show (typeOf a)

-- | "Enter" an annotation, by using the associated 'anchor' field as
-- the new reference point for calculating all DeltaPos positions.
--
-- This is combination of the ghc=exactprint Delta.withAST and
-- Print.exactPC functions and effectively does the delta processing
-- immediately followed by the print processing.  JIT ghc-exactprint.
enterAnn :: (ExactPrint a) => Entry -> a -> Annotated ()
enterAnn NoEntryVal a = do
  p <- getPosP
  debugM $ "enterAnn:NO ANN:(p,a) =" ++ show (p, astId a) ++ " starting"
  exact a
  debugM $ "enterAnn:NO ANN:p =" ++ show (p, astId a) ++ " done"
enterAnn (Entry anchor' cs) a = do
  p <- getPosP
  debugM $ "enterAnn:(p,a) =" ++ show (p, astId a) ++ " starting"
  let curAnchor = anchor anchor' -- As a base for the current AST element
  debugM $ "enterAnn:(curAnchor):=" ++ show (rs2range curAnchor)
  addCommentsA (priorComments cs)
  printComments curAnchor
  -- -------------------------
  case anchor_op anchor' of
    MovedAnchor dp -> do
      debugM $ "enterAnn: MovedAnchor:" ++ show dp
      -- Set the original anchor as prior end, so the rest of this AST
      -- fragment has a reference
      -- BUT: this means the entry DP can be calculated incorrectly too,
      -- for immediately nested items.
      setPriorEndNoLayoutD (ss2pos curAnchor)
    _ -> do
      return ()
  -- -------------------------
  setAnchorU curAnchor
  -- -------------------------------------------------------------------
  -- The first part corresponds to the delta phase, so should only use
  -- delta phase variables
  -- -----------------------------------
  -- Calculate offset required to get to the start of the SrcSPan
  off <- gets dLHS
  let spanStart = ss2pos curAnchor
  priorEndAfterComments <- getPriorEndD
  let edp' = adjustDeltaForOffset 0
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
            dp = adjustDeltaForOffset 0
                   off (ss2delta priorEndAfterComments r)
  when (isJust med) $ debugM $ "enterAnn:(med,edp)=" ++ show (med,edp)
  -- ---------------------------------------------
  -- Preparation complete, perform the action
  when (priorEndAfterComments < spanStart) (do
    debugM $ "enterAnn.dPriorEndPosition:spanStart=" ++ show spanStart
    modify (\s -> s { dPriorEndPosition    = spanStart } ))

  debugM $ "enterAnn: (anchor_op, curAnchor):" ++ show (anchor_op anchor', rs2range curAnchor)
  debugM $ "enterAnn: (dLHS,spanStart,pec,edp)=" ++ show (off,spanStart,priorEndAfterComments,edp)

  -- end of delta phase processing
  -- -------------------------------------------------------------------
  -- start of print phase processing

  let
    st = annNone { annEntryDelta = edp }
  withOffset st (advance edp >> exact a)

  when ((getFollowingComments cs) /= []) $ do
    debugM $ "starting trailing comments:" ++ showAst (getFollowingComments cs)
    mapM_ printOneComment (map tokComment $ getFollowingComments cs)
    debugM $ "ending trailing comments"

-- ---------------------------------------------------------------------

addCommentsA :: [LEpaComment] -> EPP ()
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
addComments :: [Comment] -> EPP ()
addComments csNew = do
  debugM $ "addComments:" ++ show csNew
  cs <- getUnallocatedComments
  -- Must compare without span filenames, for CPP injected comments with fake filename
  let cmp (Comment _ l1 _) (Comment _ l2 _) = compare (ss2pos $ anchor l1) (ss2pos $ anchor l2)
  -- AZ:TODO: sortedlist?
  putUnallocatedComments (sortBy cmp $ csNew ++ cs)

-- ---------------------------------------------------------------------

-- | Just before we print out the EOF comments, flush the remaining
-- ones in the state.
flushComments :: EPP ()
flushComments = do
  cs <- getUnallocatedComments
  -- Must compare without span filenames, for CPP injected comments with fake filename
  let cmp (Comment _ l1 _) (Comment _ l2 _) = compare (ss2pos $ anchor l1) (ss2pos $ anchor l2)
  mapM_ printOneComment (sortBy cmp cs)

-- ---------------------------------------------------------------------

-- |In order to interleave annotations into the stream, we turn them into
-- comments.
annotationsToComments :: [AddEpAnn] -> [AnnKeywordId] -> EPP ()
annotationsToComments ans kws = do
  let
    getSpans _ [] = []
    getSpans k1 (AddEpAnn k2 ss:as)
      | k1 == k2 = ss : getSpans k1 as
      | otherwise = getSpans k1 as
    doOne :: AnnKeywordId -> EPP [Comment]
    doOne kw = do
      let sps =getSpans kw ans
      return $ concatMap (mkKWComment kw ) sps
    -- TODO:AZ make sure these are sorted/merged properly when the invariant for
    -- allocateComments is re-established.
  newComments <- mapM doOne kws
  addComments (concat newComments)

annotationsToCommentsA :: EpAnn [AddEpAnn] -> [AnnKeywordId] -> EPP ()
annotationsToCommentsA EpAnnNotUsed _ = return ()
annotationsToCommentsA an kws = annotationsToComments (anns an) kws

-- ---------------------------------------------------------------------

-- Temporary function to simply reproduce the "normal" pretty printer output
withPpr :: (Outputable a) => a -> Annotated ()
withPpr a = do
  ss <- getAnchorU
  debugM $ "withPpr: ss=" ++ show ss
  printStringAtKw' ss (showPprUnsafe a)

-- ---------------------------------------------------------------------
-- Modeled on Outputable

-- | An AST fragment with an annotation must be able to return the
-- requirements for nesting another one, captured in an 'Entry', and
-- to be able to use the rest of the exactprint machinery to print the
-- element.  In the analogy to Outputable, 'exact' plays the role of
-- 'ppr'.
class (Typeable a) => ExactPrint a where
  getAnnotationEntry :: a -> Entry
  exact :: a -> Annotated ()

-- ---------------------------------------------------------------------

-- | Bare Located elements are simply stripped off without further
-- processing.
instance (ExactPrint a) => ExactPrint (Located a) where
  getAnnotationEntry (L l _) = Entry (spanAsAnchor l) emptyComments
  exact (L _ a) = markAnnotated a

instance (ExactPrint a) => ExactPrint (LocatedA a) where
  getAnnotationEntry = entryFromLocatedA
  exact (L la a) = do
    debugM $ "LocatedA a:la loc=" ++ show (ss2range $ locA la)
    markAnnotated a
    markALocatedA (ann la)

instance (ExactPrint a) => ExactPrint (LocatedAn NoEpAnns a) where
  getAnnotationEntry = entryFromLocatedA
  exact (L _ a) = markAnnotated a

instance (ExactPrint a) => ExactPrint [a] where
  getAnnotationEntry = const NoEntryVal
  exact ls = mapM_ markAnnotated ls

instance (ExactPrint a) => ExactPrint (NonEmpty a) where
  getAnnotationEntry = const NoEntryVal
  exact ls = mapM_ markAnnotated ls

instance (ExactPrint a) => ExactPrint (Maybe a) where
  getAnnotationEntry = const NoEntryVal
  exact Nothing = return ()
  exact (Just a) = markAnnotated a

-- ---------------------------------------------------------------------

-- | 'Located (HsModule GhcPs)' corresponds to 'ParsedSource'
instance ExactPrint HsModule where
  getAnnotationEntry hsmod = fromAnn (hsmodAnn hsmod)

  exact hsmod@(HsModule EpAnnNotUsed _ _ _ _ _ _ _) = withPpr hsmod
  exact (HsModule an _lo mmn mexports imports decls mdeprec mbDoc) = do

    markAnnotated mbDoc

    case mmn of
      Nothing -> return ()
      Just (L ln mn) -> do
        markEpAnn' an am_main AnnModule
        markAnnotated (L ln mn)

        -- forM_ mdeprec markLocated
        setLayoutTopLevelP $ markAnnotated mdeprec

        setLayoutTopLevelP $ markAnnotated mexports

        debugM $ "HsModule.AnnWhere coming"
        setLayoutTopLevelP $ markEpAnn' an am_main AnnWhere

    markAnnList' False (am_decls $ anns an) $ do
      markTopLevelList imports
      markTopLevelList decls

    -- In the weird case of an empty file with comments, make sure
    -- they print
    flushComments

-- ---------------------------------------------------------------------

-- TODO:AZ: do we *need* the following, or can we capture it in the AST?
-- | We can have a list with its own entry point defined. Create a
-- data structure to capture this, for defining an ExactPrint instance
data AnnotatedList a = AnnotatedList (Maybe Anchor) a
                     deriving (Eq,Show)

instance (ExactPrint a) => ExactPrint (AnnotatedList a) where
  getAnnotationEntry (AnnotatedList (Just anc) _) = Entry anc (EpaComments [])
  getAnnotationEntry (AnnotatedList Nothing    _) = NoEntryVal

  exact (AnnotatedList an ls) = do
    debugM $ "AnnotatedList:an=" ++ show an
    markAnnotatedWithLayout ls


-- ---------------------------------------------------------------------
-- Start of utility functions
-- ---------------------------------------------------------------------

printSourceText :: SourceText -> String -> EPP ()
printSourceText NoSourceText txt   =  printStringAdvance txt
printSourceText (SourceText txt) _ =  printStringAdvance txt

-- ---------------------------------------------------------------------

printStringAtRs :: RealSrcSpan -> String -> EPP ()
printStringAtRs ss str = printStringAtKw' ss str

printStringAtSs :: SrcSpan -> String -> EPP ()
printStringAtSs ss str = printStringAtKw' (realSrcSpan ss) str

-- ---------------------------------------------------------------------

-- AZ:TODO get rid of this
printStringAtMkw :: Maybe EpaLocation -> String -> EPP ()
printStringAtMkw (Just aa) s = printStringAtAA aa s
printStringAtMkw Nothing s = printStringAtLsDelta (SameLine 1) s

printStringAtAnn :: EpAnn a -> (a -> EpaLocation) -> String -> EPP ()
printStringAtAnn EpAnnNotUsed  _ _  = return ()
printStringAtAnn (EpAnn _ a _) f str = printStringAtAA (f a) str

printStringAtAA :: EpaLocation -> String -> EPP ()
printStringAtAA (EpaSpan r) s = printStringAtKw' r s
printStringAtAA (EpaDelta d cs) s = do
  mapM_ (printOneComment . tokComment) cs
  pe <- getPriorEndD
  p1 <- getPosP
  printStringAtLsDelta d s
  p2 <- getPosP
  debugM $ "printStringAtAA:(pe,p1,p2)=" ++ show (pe,p1,p2)
  setPriorEndASTPD True (p1,p2)

-- Based on Delta.addAnnotationWorker
printStringAtKw' :: RealSrcSpan -> String -> EPP ()
printStringAtKw' pa str = do
  printComments pa
  pe <- getPriorEndD
  debugM $ "printStringAtKw':pe=" ++ show pe
  let p = ss2delta pe pa
  p' <- adjustDeltaForOffsetM p
  printStringAtLsDelta p' str
  setPriorEndASTD True pa

-- ---------------------------------------------------------------------

markExternalSourceText :: SrcSpan -> SourceText -> String -> EPP ()
markExternalSourceText l NoSourceText txt   = printStringAtKw' (realSrcSpan l) txt
markExternalSourceText l (SourceText txt) _ = printStringAtKw' (realSrcSpan l) txt

-- ---------------------------------------------------------------------

markAddEpAnn :: AddEpAnn -> EPP ()
markAddEpAnn a@(AddEpAnn kw _) = mark [a] kw

markLocatedMAA :: EpAnn a -> (a -> Maybe AddEpAnn) -> EPP ()
markLocatedMAA EpAnnNotUsed  _  = return ()
markLocatedMAA (EpAnn _ a _) f =
  case f a of
    Nothing -> return ()
    Just aa -> markAddEpAnn aa

markLocatedAA :: EpAnn a -> (a -> AddEpAnn) -> EPP ()
markLocatedAA EpAnnNotUsed  _  = return ()
markLocatedAA (EpAnn _ a _) f = markKw (f a)

markLocatedAAL :: EpAnn a -> (a -> [AddEpAnn]) -> AnnKeywordId -> EPP ()
markLocatedAAL EpAnnNotUsed  _ _ = return ()
markLocatedAAL (EpAnn _ a _) f kw = go (f a)
  where
    go [] = return ()
    go (aa@(AddEpAnn kw' _):as)
      | kw' == kw = mark [aa] kw
      | otherwise = go as

markLocatedAALS :: EpAnn a -> (a -> [AddEpAnn]) -> AnnKeywordId -> Maybe String -> EPP ()
markLocatedAALS an f kw Nothing = markLocatedAAL an f kw
markLocatedAALS EpAnnNotUsed  _ _ _ = return ()
markLocatedAALS (EpAnn _ a _) f kw (Just str) = go (f a)
  where
    go [] = return ()
    go (AddEpAnn kw' r:as)
      | kw' == kw = printStringAtAA r str
      | otherwise = go as

-- ---------------------------------------------------------------------

markArrow :: HsArrow GhcPs -> EPP ()
markArrow (HsUnrestrictedArrow arr) = do
  markUniToken arr
markArrow (HsLinearArrow (HsPct1 pct1 arr)) = do
  markToken pct1
  markUniToken arr
markArrow (HsLinearArrow (HsLolly arr)) = do
  markToken arr
markArrow (HsExplicitMult pct t arr) = do
  markToken pct
  markAnnotated t
  markUniToken arr

-- ---------------------------------------------------------------------

markAnnCloseP :: EpAnn AnnPragma -> EPP ()
markAnnCloseP an = markLocatedAALS an (pure . apr_close) AnnClose (Just "#-}")

markAnnOpenP :: EpAnn AnnPragma -> SourceText -> String -> EPP ()
markAnnOpenP an NoSourceText txt   = markLocatedAALS an (pure . apr_open) AnnOpen (Just txt)
markAnnOpenP an (SourceText txt) _ = markLocatedAALS an (pure . apr_open) AnnOpen (Just txt)

markAnnOpen :: EpAnn [AddEpAnn] -> SourceText -> String -> EPP ()
markAnnOpen an NoSourceText txt   = markLocatedAALS an id AnnOpen (Just txt)
markAnnOpen an (SourceText txt) _ = markLocatedAALS an id AnnOpen (Just txt)

markAnnOpen' :: Maybe EpaLocation -> SourceText -> String -> EPP ()
markAnnOpen' ms NoSourceText txt   = printStringAtMkw ms txt
markAnnOpen' ms (SourceText txt) _ = printStringAtMkw ms txt

-- ---------------------------------------------------------------------

markOpeningParen, markClosingParen :: EpAnn AnnParen -> EPP ()
markOpeningParen an = markParen an fst
markClosingParen an = markParen an snd

markParen :: EpAnn AnnParen -> (forall a. (a,a) -> a) -> EPP ()
markParen EpAnnNotUsed _ = return ()
markParen (EpAnn _ (AnnParen pt o c) _) f = markKwA (f $ kw pt) (f (o, c))
  where
    kw AnnParens       = (AnnOpenP,  AnnCloseP)
    kw AnnParensHash   = (AnnOpenPH, AnnClosePH)
    kw AnnParensSquare = (AnnOpenS, AnnCloseS)


markAnnKw :: EpAnn a -> (a -> EpaLocation) -> AnnKeywordId -> EPP ()
markAnnKw EpAnnNotUsed  _ _  = return ()
markAnnKw (EpAnn _ a _) f kw = markKwA kw (f a)

markAnnKwAll :: EpAnn a -> (a -> [EpaLocation]) -> AnnKeywordId -> EPP ()
markAnnKwAll EpAnnNotUsed  _ _  = return ()
markAnnKwAll (EpAnn _ a _) f kw = mapM_ (markKwA kw) (sortBy (comparing unsafeGetEpaLoc) (f a))

markAnnKwM :: EpAnn a -> (a -> Maybe EpaLocation) -> AnnKeywordId -> EPP ()
markAnnKwM EpAnnNotUsed  _ _ = return ()
markAnnKwM (EpAnn _ a _) f kw = go (f a)
  where
    go Nothing = return ()
    go (Just s) = markKwA kw s

markALocatedA :: EpAnn AnnListItem -> EPP ()
markALocatedA EpAnnNotUsed  = return ()
markALocatedA (EpAnn _ a _) = markTrailing (lann_trailing a)

markEpAnn :: EpAnn [AddEpAnn] -> AnnKeywordId -> EPP ()
markEpAnn EpAnnNotUsed _ = return ()
markEpAnn (EpAnn _ a _) kw = mark a kw

markEpAnn' :: EpAnn ann -> (ann -> [AddEpAnn]) -> AnnKeywordId -> EPP ()
markEpAnn' EpAnnNotUsed _ _ = return ()
markEpAnn' (EpAnn _ a _) f kw = mark (f a) kw

markEpAnnAll :: EpAnn ann -> (ann -> [AddEpAnn]) -> AnnKeywordId -> EPP ()
markEpAnnAll EpAnnNotUsed _ _ = return ()
markEpAnnAll (EpAnn _ a _) f kw = mapM_ markKw (sortBy (comparing unsafeGetEpAnnLoc) anns)
  where
    anns = filter (\(AddEpAnn ka _) -> ka == kw) (f a)

unsafeGetEpAnnLoc :: AddEpAnn -> RealSrcSpan
unsafeGetEpAnnLoc (AddEpAnn _ ss) = unsafeGetEpaLoc ss


unsafeGetEpaLoc :: EpaLocation -> RealSrcSpan
unsafeGetEpaLoc (EpaSpan real) = real
unsafeGetEpaLoc (EpaDelta _ _) = error "DELTA"

markAnnAll :: [AddEpAnn] -> AnnKeywordId -> EPP ()
markAnnAll a kw = mapM_ markKw (sortBy (comparing unsafeGetEpAnnLoc) anns)
  where
    anns = filter (\(AddEpAnn ka _) -> ka == kw) a

mark :: [AddEpAnn] -> AnnKeywordId -> EPP ()
mark anns kw = do
  case find (\(AddEpAnn k _) -> k == kw) anns of
    Just aa -> markKw aa
    Nothing -> case find (\(AddEpAnn k _) -> k == (unicodeAnn kw)) anns of
      Just aau -> markKw aau
      Nothing -> return ()

markKwT :: TrailingAnn -> EPP ()
markKwT (AddSemiAnn ss)    = markKwA AnnSemi ss
markKwT (AddCommaAnn ss)   = markKwA AnnComma ss
markKwT (AddVbarAnn ss)    = markKwA AnnVbar ss

markKw :: AddEpAnn -> EPP ()
markKw (AddEpAnn kw ss) = markKwA kw ss

-- | This should be the main driver of the process, managing comments
markKwA :: AnnKeywordId -> EpaLocation -> EPP ()
markKwA kw aa = printStringAtAA aa (keywordToString (G kw))

markToken :: forall tok. KnownSymbol tok => LHsToken tok GhcPs -> EPP ()
markToken (L NoTokenLoc _) = return ()
markToken (L (TokenLoc aa) _) = printStringAtAA aa (symbolVal (Proxy @tok))

markUniToken :: forall tok utok. (KnownSymbol tok, KnownSymbol utok) => LHsUniToken tok utok GhcPs -> EPP ()
markUniToken (L l HsNormalTok)  = markToken (L l (HsTok @tok))
markUniToken (L l HsUnicodeTok) = markToken (L l (HsTok @utok))

-- ---------------------------------------------------------------------

markAnnList :: Bool -> EpAnn AnnList -> EPP () -> EPP ()
markAnnList _ EpAnnNotUsed action = action
markAnnList reallyTrail (EpAnn _ ann _) action = markAnnList' reallyTrail ann action

markAnnList' :: Bool -> AnnList -> EPP () -> EPP ()
markAnnList' reallyTrail ann action = do
  p <- getPosP
  debugM $ "markAnnList : " ++ showPprUnsafe (p, ann)
  mapM_ markAddEpAnn (al_open ann)
  unless reallyTrail $ markTrailing (al_trailing ann) -- Only makes sense for HsModule.
  markAnnAll (sortBy (comparing unsafeGetEpAnnLoc) $ al_rest ann) AnnSemi
  action
  mapM_ markAddEpAnn (al_close ann)
  debugM $ "markAnnList: calling markTrailing with:" ++ showPprUnsafe (al_trailing ann)
  when reallyTrail $ markTrailing (al_trailing ann) -- normal case

-- ---------------------------------------------------------------------

printComments :: RealSrcSpan -> EPP ()
printComments ss = do
  cs <- commentAllocation ss
  -- debugM $ "printComments: (ss,comment locations): " ++ showPprUnsafe (rs2range ss,map commentAnchor cs)
  mapM_ printOneComment cs

-- ---------------------------------------------------------------------

printOneComment :: Comment -> EPP ()
printOneComment c@(Comment _str loc _mo) = do
  debugM $ "printOneComment:c=" ++ showGhc c
  dp <-case anchor_op loc of
    MovedAnchor dp -> return dp
    _ -> do
        pe <- getPriorEndD
        let dp = ss2delta pe (anchor loc)
        debugM $ "printOneComment:(dp,pe,anchor loc)=" ++ showGhc (dp,pe,ss2pos $ anchor loc)
        return dp
  dp'' <- adjustDeltaForOffsetM dp
  mep <- getExtraDP
  dp' <- case mep of
    Just (Anchor _ (MovedAnchor edp)) -> do
      debugM $ "printOneComment:edp=" ++ show edp
      return edp
    _ -> return dp''
  LayoutStartCol dOff <- gets dLHS
  debugM $ "printOneComment:(dp,dp',dp'',dOff)=" ++ showGhc (dp,dp',dp'',dOff)
  setPriorEndD (ss2posEnd (anchor loc))
  printQueuedComment (anchor loc) c dp'

-- ---------------------------------------------------------------------

commentAllocation :: RealSrcSpan -> EPP [Comment]
commentAllocation ss = do
  cs <- getUnallocatedComments
  -- Note: The CPP comment injection may change the file name in the
  -- RealSrcSpan, which affects comparison, as the Ord instance for
  -- RealSrcSpan compares the file first. So we sort via ss2pos
  -- TODO: this is inefficient, use Pos all the way through
  let (earlier,later) = partition (\(Comment _str loc _mo) -> (ss2pos $ anchor loc) <= (ss2pos ss)) cs
  putUnallocatedComments later
  -- debugM $ "commentAllocation:(ss,earlier,later)" ++ show (rs2range ss,earlier,later)
  return earlier

-- ---------------------------------------------------------------------


markAnnotatedWithLayout :: ExactPrint ast => ast -> EPP ()
markAnnotatedWithLayout a = setLayoutBoth $ markAnnotated a

-- ---------------------------------------------------------------------

markTopLevelList :: ExactPrint ast => [ast] -> EPP ()
markTopLevelList ls = mapM_ (\a -> setLayoutTopLevelP $ markAnnotated a) ls

-- ---------------------------------------------------------------------

instance ExactPrint ModuleName where
  getAnnotationEntry _ = NoEntryVal
  exact n = do
    debugM $ "ModuleName: " ++ showPprUnsafe n
    withPpr n

-- ---------------------------------------------------------------------

instance ExactPrint (LocatedP (WarningTxt GhcPs)) where
  getAnnotationEntry = entryFromLocatedA
  exact (L (SrcSpanAnn an _) (WarningTxt (L _ src) ws)) = do
    markAnnOpenP an src "{-# WARNING"
    markLocatedAAL an apr_rest AnnOpenS
    markAnnotated ws
    markLocatedAAL an apr_rest AnnCloseS
    markAnnCloseP an

  exact (L (SrcSpanAnn an _) (DeprecatedTxt (L _ src) ws)) = do
    markAnnOpenP an src "{-# DEPRECATED"
    markLocatedAAL an apr_rest AnnOpenS
    markAnnotated ws
    markLocatedAAL an apr_rest AnnCloseS
    markAnnCloseP an

-- ---------------------------------------------------------------------

instance ExactPrint (ImportDecl GhcPs) where
  getAnnotationEntry idecl = fromAnn (ideclExt idecl)
  exact x@(ImportDecl EpAnnNotUsed _ _ _ _ _ _ _ _ _) = withPpr x
  exact (ImportDecl ann@(EpAnn _ an _) msrc modname mpkg _src safeflag qualFlag _impl mAs hiding) = do

    markAnnKw ann importDeclAnnImport AnnImport

    -- "{-# SOURCE" and "#-}"
    case msrc of
      SourceText _txt -> do
        debugM $ "ImportDecl sourcetext"
        let mo = fmap fst $ importDeclAnnPragma an
        let mc = fmap snd $ importDeclAnnPragma an
        markAnnOpen' mo msrc "{-# SOURCE"
        printStringAtMkw mc "#-}"
      NoSourceText -> return ()
    when safeflag (markAnnKwM ann importDeclAnnSafe AnnSafe)
    case qualFlag of
      QualifiedPre  -- 'qualified' appears in prepositive position.
        -> printStringAtMkw (importDeclAnnQualified an) "qualified"
      _ -> return ()
    case mpkg of
     RawPkgQual (StringLiteral src v _) ->
       printStringAtMkw (importDeclAnnPackage an) (sourceTextToString src (show v))
     _ -> return ()

    markAnnotated modname

    case qualFlag of
      QualifiedPost  -- 'qualified' appears in postpositive position.
        -> printStringAtMkw (importDeclAnnQualified an) "qualified"
      _ -> return ()

    case mAs of
      Nothing -> return ()
      Just mn -> do
        printStringAtMkw (importDeclAnnAs an) "as"
        markAnnotated mn

    case hiding of
      Nothing -> return ()
      Just (_isHiding,lie) -> exact lie
 --   markTrailingSemi


-- ---------------------------------------------------------------------

instance ExactPrint HsDocString where
  getAnnotationEntry _ = NoEntryVal
  exact = printStringAdvance . exactPrintHsDocString

instance ExactPrint a => ExactPrint (WithHsDocIdentifiers a GhcPs) where
  getAnnotationEntry _ = NoEntryVal
  exact = exact . hsDocString

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

  exact (TyClD       _ d) = markAnnotated d
  exact (InstD       _ d) = markAnnotated d
  exact (DerivD      _ d) = markAnnotated d
  exact (ValD        _ d) = markAnnotated d
  exact (SigD        _ d) = markAnnotated d
  exact (KindSigD    _ d) = markAnnotated d
  exact (DefD        _ d) = markAnnotated d
  exact (ForD        _ d) = markAnnotated d
  exact (WarningD    _ d) = markAnnotated d
  exact (AnnD        _ d) = markAnnotated d
  exact (RuleD       _ d) = markAnnotated d
  exact (SpliceD     _ d) = markAnnotated d
  exact (DocD        _ d) = markAnnotated d
  exact (RoleAnnotD  _ d) = markAnnotated d

-- ---------------------------------------------------------------------

instance ExactPrint (InstDecl GhcPs) where
  getAnnotationEntry (ClsInstD     _  _) = NoEntryVal
  getAnnotationEntry (DataFamInstD an _) = fromAnn an
  getAnnotationEntry (TyFamInstD   _  _) = NoEntryVal


  exact (ClsInstD     _  cid) = markAnnotated cid
  exact (DataFamInstD an decl) = do
    exactDataFamInstDecl an TopLevel decl
  exact (TyFamInstD _ eqn) = do
    -- exactTyFamInstDecl an TopLevel eqn
    markAnnotated eqn

-- ---------------------------------------------------------------------

exactDataFamInstDecl :: EpAnn [AddEpAnn] -> TopLevelFlag -> (DataFamInstDecl GhcPs) -> EPP ()
exactDataFamInstDecl an top_lvl
  (DataFamInstDecl ( FamEqn { feqn_ext    = an2
                            , feqn_tycon  = tycon
                            , feqn_bndrs  = bndrs
                            , feqn_pats   = pats
                            , feqn_fixity = fixity
                            , feqn_rhs    = defn }))
  = exactDataDefn an2 pp_hdr defn -- See Note [an and an2 in exactDataFamInstDecl]
  where
    pp_hdr mctxt = do
      case top_lvl of
        TopLevel -> markEpAnn an AnnInstance -- TODO: maybe in toplevel
        NotTopLevel -> return ()
      exactHsFamInstLHS an tycon bndrs pats fixity mctxt

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

exactTyFamInstDecl :: TopLevelFlag -> (TyFamInstDecl GhcPs) -> EPP ()
exactTyFamInstDecl top_lvl (TyFamInstDecl { tfid_xtn = an, tfid_eqn = eqn }) = do
  markEpAnn an AnnType
  case top_lvl of
    TopLevel -> markEpAnn an AnnInstance
    NotTopLevel -> return ()
  markAnnotated eqn

-- ---------------------------------------------------------------------

instance ExactPrint (DerivDecl GhcPs) where
  getAnnotationEntry (DerivDecl {deriv_ext = an} ) = fromAnn an
  exact (DerivDecl an typ ms mov) = do
    markEpAnn an AnnDeriving
    mapM_ markAnnotated ms
    markEpAnn an AnnInstance
    mapM_ markAnnotated mov
    markAnnotated typ
  -- markAST _ (GHC.DerivDecl _ (GHC.HsWC _ (GHC.HsIB _ typ)) ms mov) = do
  --   mark GHC.AnnDeriving
  --   markMaybe ms
  --   mark GHC.AnnInstance
  --   markMaybe mov
  --   markLocated typ
  --   markTrailingSemi

-- ---------------------------------------------------------------------

instance ExactPrint (ForeignDecl GhcPs) where
  getAnnotationEntry (ForeignImport an _ _  _) = fromAnn an
  getAnnotationEntry (ForeignExport an _ _  _) = fromAnn an

  exact (ForeignImport an n ty fimport) = do
    markEpAnn an AnnForeign
    markEpAnn an AnnImport

    markAnnotated fimport

    markAnnotated n
    markEpAnn an AnnDcolon
    markAnnotated ty

  exact (ForeignExport an n ty fexport) = do
    markEpAnn an AnnForeign
    markEpAnn an AnnExport
    markAnnotated fexport
    markAnnotated n
    markEpAnn an AnnDcolon
    markAnnotated ty

-- ---------------------------------------------------------------------

instance ExactPrint ForeignImport where
  getAnnotationEntry = const NoEntryVal
  exact (CImport cconv safety@(L ll _) _mh _imp (L ls src)) = do
    markAnnotated cconv
    unless (ll == noSrcSpan) $ markAnnotated safety
    unless (ls == noSrcSpan) $ markExternalSourceText ls src ""

-- ---------------------------------------------------------------------

instance ExactPrint ForeignExport where
  getAnnotationEntry = const NoEntryVal
  exact (CExport spec (L ls src)) = do
    debugM $ "CExport starting"
    markAnnotated spec
    unless (ls == noSrcSpan) $ markExternalSourceText ls src ""

-- ---------------------------------------------------------------------

instance ExactPrint CExportSpec where
  getAnnotationEntry = const NoEntryVal
  exact (CExportStatic _st _lbl cconv) = do
    debugM $ "CExportStatic starting"
    markAnnotated cconv

-- ---------------------------------------------------------------------

instance ExactPrint Safety where
  getAnnotationEntry = const NoEntryVal
  exact = withPpr

-- ---------------------------------------------------------------------

instance ExactPrint CCallConv where
  getAnnotationEntry = const NoEntryVal
  exact = withPpr

-- ---------------------------------------------------------------------

instance ExactPrint (WarnDecls GhcPs) where
  getAnnotationEntry (Warnings an _ _) = fromAnn an
  exact (Warnings an src warns) = do
    markAnnOpen an src "{-# WARNING" -- Note: might be {-# DEPRECATED
    markAnnotated warns
    markLocatedAALS an id AnnClose (Just "#-}")

-- ---------------------------------------------------------------------

instance ExactPrint (WarnDecl GhcPs) where
  getAnnotationEntry (Warning an _ _) = fromAnn an

  exact (Warning an lns txt) = do
    markAnnotated lns
    markEpAnn an AnnOpenS -- "["
    case txt of
      WarningTxt    _src ls -> markAnnotated ls
      DeprecatedTxt _src ls -> markAnnotated ls
    markEpAnn an AnnCloseS -- "]"

-- ---------------------------------------------------------------------

instance ExactPrint StringLiteral where
  getAnnotationEntry = const NoEntryVal

  exact (StringLiteral src fs mcomma) = do
    printSourceText src (show (unpackFS fs))
    mapM_ (\r -> printStringAtKw' r ",") mcomma

-- ---------------------------------------------------------------------

instance ExactPrint FastString where
  getAnnotationEntry = const NoEntryVal

  -- TODO: https://ghc.haskell.org/trac/ghc/ticket/10313 applies.
  -- exact fs = printStringAdvance (show (unpackFS fs))
  exact fs = printStringAdvance (unpackFS fs)


-- ---------------------------------------------------------------------

instance ExactPrint (RuleDecls GhcPs) where
  getAnnotationEntry (HsRules an _ _) = fromAnn an
  exact (HsRules an src rules) = do
    case src of
      NoSourceText      -> markLocatedAALS an id AnnOpen  (Just "{-# RULES")
      SourceText srcTxt -> markLocatedAALS an id AnnOpen  (Just srcTxt)
    markAnnotated rules
    markLocatedAALS an id AnnClose (Just "#-}")
    -- markTrailingSemi

-- ---------------------------------------------------------------------

instance ExactPrint (RuleDecl GhcPs) where
  getAnnotationEntry (HsRule {rd_ext = an}) = fromAnn an
  exact (HsRule an ln act mtybndrs termbndrs lhs rhs) = do
    debugM "HsRule entered"
    markAnnotated ln
    debugM "HsRule after ln"
    markActivation an ra_rest act
    debugM "HsRule after act"
    case mtybndrs of
      Nothing -> return ()
      Just bndrs -> do
        markLocatedMAA an (\a -> fmap fst (ra_tyanns a))  -- AnnForall
        mapM_ markAnnotated bndrs
        markLocatedMAA an (\a -> fmap snd (ra_tyanns a))  -- AnnDot

    markLocatedMAA an (\a -> fmap fst (ra_tmanns a))  -- AnnForall
    mapM_ markAnnotated termbndrs
    markLocatedMAA an (\a -> fmap snd (ra_tmanns a))  -- AnnDot

    markAnnotated lhs
    markEpAnn' an ra_rest AnnEqual
    markAnnotated rhs
  -- markAST l (GHC.HsRule _ ln act mtybndrs termbndrs lhs rhs) = do
  --   markLocated ln
  --   setContext (Set.singleton ExplicitNeverActive) $ markActivation l act


  --   mark GHC.AnnForall
  --   mapM_ markLocated termbndrs
  --   mark GHC.AnnDot

  --   markLocated lhs
  --   mark GHC.AnnEqual
  --   markLocated rhs
  --   inContext (Set.singleton Intercalate) $ mark GHC.AnnSemi
  --   markTrailingSemi

markActivation :: EpAnn a -> (a -> [AddEpAnn]) -> Activation -> Annotated ()
markActivation an fn act = do
  case act of
    ActiveBefore src phase -> do
      markEpAnn' an fn AnnOpenS --  '['
      markEpAnn' an fn AnnTilde -- ~
      markLocatedAALS an fn AnnVal (Just (toSourceTextWithSuffix src (show phase) ""))
      markEpAnn' an fn AnnCloseS -- ']'
    ActiveAfter src phase -> do
      markEpAnn' an fn AnnOpenS --  '['
      markLocatedAALS an fn AnnVal (Just (toSourceTextWithSuffix src (show phase) ""))
      markEpAnn' an fn AnnCloseS -- ']'
    NeverActive -> do
      markEpAnn' an fn AnnOpenS --  '['
      markEpAnn' an fn AnnTilde -- ~
      markEpAnn' an fn AnnCloseS -- ']'
    _ -> return ()

-- ---------------------------------------------------------------------

instance ExactPrint (SpliceDecl GhcPs) where
  getAnnotationEntry = const NoEntryVal

  exact (SpliceDecl _ splice _flag) = do
    markAnnotated splice

-- ---------------------------------------------------------------------

instance ExactPrint (DocDecl GhcPs) where
  getAnnotationEntry = const NoEntryVal

  exact v = case v of
    (DocCommentNext ds)     -> exact ds
    (DocCommentPrev ds)     -> exact ds
    (DocCommentNamed _s ds) -> exact ds
    (DocGroup _i ds)        -> exact ds

-- ---------------------------------------------------------------------

instance ExactPrint (RoleAnnotDecl GhcPs) where
  getAnnotationEntry (RoleAnnotDecl an _ _) = fromAnn an
  exact (RoleAnnotDecl an ltycon roles) = do
    markEpAnn an AnnType
    markEpAnn an AnnRole
    markAnnotated ltycon
    let markRole (L l (Just r)) = markAnnotated (L (locA l) r)
        markRole (L l Nothing) = printStringAtSs (locA l) "_"
    mapM_ markRole roles

-- ---------------------------------------------------------------------

instance ExactPrint Role where
  getAnnotationEntry = const NoEntryVal
  exact = withPpr

-- ---------------------------------------------------------------------

instance ExactPrint (RuleBndr GhcPs) where
  getAnnotationEntry = const NoEntryVal

{-
  = RuleBndr (XCRuleBndr pass)  (Located (IdP pass))
  | RuleBndrSig (XRuleBndrSig pass) (Located (IdP pass)) (HsPatSigType pass)
-}
  exact (RuleBndr _ ln) = markAnnotated ln
  exact (RuleBndrSig an ln (HsPS _ ty)) = do
    markEpAnn an AnnOpenP -- "("
    markAnnotated ln
    markEpAnn an AnnDcolon
    markAnnotated ty
    markEpAnn an AnnCloseP -- ")"

-- ---------------------------------------------------------------------

-- instance ExactPrint (TyFamInstEqn GhcPs) where
-- instance (ExactPrint body) => ExactPrint (FamInstEqn GhcPs body) where
--   getAnnotationEntry = const NoEntryVal
--   exact (HsIB { hsib_body = FamEqn { feqn_ext = an
--                                    , feqn_tycon  = tycon
--                                    , feqn_bndrs  = bndrs
--                                    , feqn_pats   = pats
--                                    , feqn_fixity = fixity
--                                    , feqn_rhs    = rhs }}) = do
--     exactHsFamInstLHS an tycon bndrs pats fixity Nothing
--     markEpAnn an AnnEqual
--     markAnnotated rhs

instance (ExactPrint body) => ExactPrint (FamEqn GhcPs body) where
  getAnnotationEntry (FamEqn { feqn_ext = an}) = fromAnn an
  exact (FamEqn { feqn_ext = an
                , feqn_tycon  = tycon
                , feqn_bndrs  = bndrs
                , feqn_pats   = pats
                , feqn_fixity = fixity
                , feqn_rhs    = rhs }) = do
    exactHsFamInstLHS an tycon bndrs pats fixity Nothing
    markEpAnn an AnnEqual
    markAnnotated rhs

-- ---------------------------------------------------------------------

exactHsFamInstLHS ::
      EpAnn [AddEpAnn]
   -> LocatedN RdrName
   -- -> Maybe [LHsTyVarBndr () GhcPs]
   -> HsOuterTyVarBndrs () GhcPs
   -> HsTyPats GhcPs
   -> LexicalFixity
   -> Maybe (LHsContext GhcPs)
   -> EPP ()
exactHsFamInstLHS an thing bndrs typats fixity mb_ctxt = do
  markEpAnn an AnnForall
  markAnnotated bndrs
  markEpAnn an AnnDot
  mapM_ markAnnotated mb_ctxt
  exact_pats typats
  where
    exact_pats :: HsTyPats GhcPs -> EPP ()
    exact_pats (patl:patr:pats)
      | Infix <- fixity
      = let exact_op_app = do
              markAnnotated patl
              markAnnotated thing
              markAnnotated patr
        in case pats of
             [] -> exact_op_app
             _  -> do
               markEpAnn an AnnOpenP
               exact_op_app
               markEpAnn an AnnCloseP
               mapM_ markAnnotated pats

    exact_pats pats = do
      markAnnAll (epAnnAnns an) AnnOpenP
      markAnnotated thing
      markAnnotated pats
      markAnnAll (epAnnAnns an) AnnCloseP

-- ---------------------------------------------------------------------

-- instance ExactPrint (LHsTypeArg GhcPs) where
instance (ExactPrint tm, ExactPrint ty, Outputable tm, Outputable ty)
     =>  ExactPrint (HsArg tm ty) where
  getAnnotationEntry = const NoEntryVal

  exact (HsValArg tm)    = markAnnotated tm
  exact (HsTypeArg ss ty) = printStringAtSs ss "@" >> markAnnotated ty
  exact x@(HsArgPar _sp)   = withPpr x -- Does not appear in original source

-- ---------------------------------------------------------------------

instance ExactPrint (ClsInstDecl GhcPs) where
  getAnnotationEntry cid = fromAnn (fst $ cid_ext cid)

  exact (ClsInstDecl { cid_ext = (an, sortKey)
                     , cid_poly_ty = inst_ty, cid_binds = binds
                     , cid_sigs = sigs, cid_tyfam_insts = ats
                     , cid_overlap_mode = mbOverlap
                     , cid_datafam_insts = adts })
      | null sigs, null ats, null adts, isEmptyBag binds  -- No "where" part
      = top_matter

      | otherwise       -- Laid out
      = do
          top_matter
          markEpAnn an AnnWhere
          markEpAnn an AnnOpenC
          markEpAnnAll an id AnnSemi
      -- = vcat [ top_matter <+> text "where"
      --        , nest 2 $ pprDeclList $
      --          map (pprTyFamInstDecl NotTopLevel . unLoc)   ats ++
      --          map (pprDataFamInstDecl NotTopLevel . unLoc) adts ++
      --          pprLHsBindsForUser binds sigs ]
          withSortKey sortKey
                               (prepareListAnnotationA ats
                             ++ prepareListAnnotationF (exactDataFamInstDecl an NotTopLevel ) adts
                             ++ prepareListAnnotationA (bagToList binds)
                             ++ prepareListAnnotationA sigs
                               )
          markEpAnn an AnnCloseC -- '}'

      where
        top_matter = do
          markEpAnn an AnnInstance
          mapM_ markAnnotated mbOverlap
          markAnnotated inst_ty
          markEpAnn an AnnWhere -- Optional
          -- text "instance" <+> ppOverlapPragma mbOverlap
          --                                    <+> ppr inst_ty

-- ---------------------------------------------------------------------

instance ExactPrint (TyFamInstDecl GhcPs) where
  getAnnotationEntry (TyFamInstDecl an _) = fromAnn an
  exact d@(TyFamInstDecl _an _eqn) =
    exactTyFamInstDecl TopLevel d

-- ---------------------------------------------------------------------

-- instance (ExactPrint body) => ExactPrint (HsImplicitBndrs GhcPs body) where
--   getAnnotationEntry (HsIB an _) = fromAnn an
--   exact (HsIB an t) = markAnnotated t

-- ---------------------------------------------------------------------

instance ExactPrint (LocatedP OverlapMode) where
  getAnnotationEntry = entryFromLocatedA

  -- NOTE: NoOverlap is only used in the typechecker
  exact (L (SrcSpanAnn an _) (NoOverlap src)) = do
    markAnnOpenP an src "{-# NO_OVERLAP"
    markAnnCloseP an

  exact (L (SrcSpanAnn an _) (Overlappable src)) = do
    markAnnOpenP an src "{-# OVERLAPPABLE"
    markAnnCloseP an

  exact (L (SrcSpanAnn an _) (Overlapping src)) = do
    markAnnOpenP an src "{-# OVERLAPPING"
    markAnnCloseP an

  exact (L (SrcSpanAnn an _) (Overlaps src)) = do
    markAnnOpenP an src "{-# OVERLAPS"
    markAnnCloseP an

  exact (L (SrcSpanAnn an _) (Incoherent src)) = do
    markAnnOpenP an src "{-# INCOHERENT"
    markAnnCloseP an

-- ---------------------------------------------------------------------

instance ExactPrint (HsBind GhcPs) where
  getAnnotationEntry FunBind{} = NoEntryVal
  getAnnotationEntry PatBind{} = NoEntryVal
  getAnnotationEntry VarBind{} = NoEntryVal
  getAnnotationEntry PatSynBind{} = NoEntryVal

  exact (FunBind _ _ matches _) = do
    markAnnotated matches
  exact (PatBind _ pat grhss _) = do
    markAnnotated pat
    markAnnotated grhss
  exact (PatSynBind _ bind) = markAnnotated bind

  exact x = error $ "HsBind: exact for " ++ showAst x

-- ---------------------------------------------------------------------

instance ExactPrint (PatSynBind GhcPs GhcPs) where
  getAnnotationEntry (PSB { psb_ext = an}) = fromAnn an

  exact (PSB{ psb_ext = an
            , psb_id = psyn, psb_args = details
            , psb_def = pat
            , psb_dir = dir }) = do
    markEpAnn an AnnPattern
    case details of
      InfixCon v1 v2 -> do
        markAnnotated v1
        markAnnotated psyn
        markAnnotated v2
      PrefixCon tvs vs -> do
        markAnnotated psyn
        markAnnotated tvs
        markAnnotated vs
      RecCon vs -> do
        markAnnotated psyn
        markEpAnn an AnnOpenC  -- '{'
        markAnnotated vs
        markEpAnn an AnnCloseC -- '}'

    case dir of
      Unidirectional           -> do
        markEpAnn an AnnLarrow
        markAnnotated pat
      ImplicitBidirectional    -> do
        markEpAnn an AnnEqual
        markAnnotated pat
      ExplicitBidirectional mg -> do
        markEpAnn an AnnLarrow
        markAnnotated pat
        markEpAnn an AnnWhere
        markAnnotated mg

    -- case dir of
    --   GHC.ImplicitBidirectional -> mark GHC.AnnEqual
    --   _                         -> mark GHC.AnnLarrow

    -- markLocated def
    -- case dir of
    --   GHC.Unidirectional           -> return ()
    --   GHC.ImplicitBidirectional    -> return ()
    --   GHC.ExplicitBidirectional mg -> do
    --     mark GHC.AnnWhere
    --     mark GHC.AnnOpenC  -- '{'
    --     markMatchGroup l mg
    --     mark GHC.AnnCloseC -- '}'

    -- markTrailingSemi


-- ---------------------------------------------------------------------

instance ExactPrint (RecordPatSynField GhcPs) where
  getAnnotationEntry = const NoEntryVal
  exact (RecordPatSynField { recordPatSynField = v }) = markAnnotated v

-- ---------------------------------------------------------------------

instance ExactPrint (Match GhcPs (LocatedA (HsCmd GhcPs))) where
  getAnnotationEntry (Match ann _ _ _) = fromAnn ann

  -- exact match@(Match EpAnnNotUsed _ _ _) = withPpr match
  exact (Match an mctxt pats grhss) = do
    exactMatch (Match an mctxt pats grhss)

-- -------------------------------------

instance ExactPrint (Match GhcPs (LocatedA (HsExpr GhcPs))) where
  getAnnotationEntry (Match ann _ _ _) = fromAnn ann

  -- exact match@(Match EpAnnNotUsed _ _ _) = withPpr match
  exact (Match an mctxt pats grhss) = do
    exactMatch (Match an mctxt pats grhss)
  -- -- Based on Expr.pprMatch

  --   debugM $ "exact Match entered"

  --   -- herald
  --   case mctxt of
  --     FunRhs fun fixity strictness -> do
  --       debugM $ "exact Match FunRhs:" ++ showPprUnsafe fun
  --       case strictness of
  --         SrcStrict -> markEpAnn an AnnBang
  --         _ -> pure ()
  --       case fixity of
  --         Prefix -> do
  --           markAnnotated fun
  --           mapM_ markAnnotated pats
  --         Infix ->
  --           case pats of
  --             (p1:p2:rest)
  --               | null rest -> do
  --                   markAnnotated p1
  --                   markAnnotated fun
  --                   markAnnotated p2
  --               | otherwise -> do
  --                   markEpAnn an AnnOpenP
  --                   markAnnotated p1
  --                   markAnnotated fun
  --                   markAnnotated p2
  --                   markEpAnn an AnnCloseP
  --                   mapM_ markAnnotated rest
  --     LambdaExpr -> do
  --       markEpAnn an AnnLam
  --       mapM_ markAnnotated pats
  --     GHC.CaseAlt -> do
  --       mapM_ markAnnotated pats
  --     _ -> withPpr mctxt

  --   markAnnotated grhss

-- ---------------------------------------------------------------------

exactMatch :: (ExactPrint (GRHSs GhcPs body)) => (Match GhcPs body) -> Annotated ()
exactMatch (Match an mctxt pats grhss) = do
-- Based on Expr.pprMatch

  debugM $ "exact Match entered"

  -- herald
  case mctxt of
    FunRhs fun fixity strictness -> do
      debugM $ "exact Match FunRhs:" ++ showPprUnsafe fun
      case strictness of
        SrcStrict -> markEpAnn an AnnBang
        _ -> pure ()
      case fixity of
        Prefix -> do
          annotationsToCommentsA an [AnnOpenP,AnnCloseP]
          markAnnotated fun
          markAnnotated pats
        Infix ->
          case pats of
            (p1:p2:rest)
              | null rest -> do
                  markAnnotated p1
                  markAnnotated fun
                  markAnnotated p2
              | otherwise -> do
                  markEpAnn an AnnOpenP
                  markAnnotated p1
                  markAnnotated fun
                  markAnnotated p2
                  markEpAnn an AnnCloseP
                  mapM_ markAnnotated rest
            _ -> panic "FunRhs"
    LambdaExpr -> do
      markEpAnn an AnnLam
      markAnnotated pats
    GHC.CaseAlt -> do
      markAnnotated pats
    _ -> withPpr mctxt

  markAnnotated grhss

-- ---------------------------------------------------------------------

instance ExactPrint (GRHSs GhcPs (LocatedA (HsExpr GhcPs))) where
  getAnnotationEntry (GRHSs _ _ _) = NoEntryVal

  exact (GRHSs _ grhss binds) = do
    markAnnotated grhss
    markAnnotated binds


instance ExactPrint (GRHSs GhcPs (LocatedA (HsCmd GhcPs))) where
  getAnnotationEntry (GRHSs _ _ _) = NoEntryVal

  exact (GRHSs _an grhss binds) = do
    markAnnotated grhss
    markAnnotated binds

-- ---------------------------------------------------------------------

instance ExactPrint (HsLocalBinds GhcPs) where
  getAnnotationEntry (HsValBinds an _) = fromAnn an
  getAnnotationEntry (HsIPBinds{}) = NoEntryVal
  getAnnotationEntry (EmptyLocalBinds{}) = NoEntryVal

  exact (HsValBinds an valbinds) = do
    markLocatedAAL an al_rest AnnWhere
    let manc = case an of
                 EpAnnNotUsed -> Nothing
                 _ -> al_anchor $ anns an

    case manc of
      Just anc -> do
        when (not $ isEmptyValBinds valbinds) $ setExtraDP (Just anc)
      _ -> return ()

    markAnnList False an $ markAnnotatedWithLayout valbinds

  exact (HsIPBinds an bs)
    = markAnnList True an (markLocatedAAL an al_rest AnnWhere >> markAnnotated bs)
  exact (EmptyLocalBinds _) = return ()


-- ---------------------------------------------------------------------
instance ExactPrint (HsValBindsLR GhcPs GhcPs) where
  getAnnotationEntry _ = NoEntryVal

  exact (ValBinds sortKey binds sigs) = do
    setLayoutBoth $ withSortKey sortKey
       (prepareListAnnotationA (bagToList binds)
     ++ prepareListAnnotationA sigs
       )
  exact (XValBindsLR _) = panic "XValBindsLR"

-- ---------------------------------------------------------------------

instance ExactPrint (HsIPBinds GhcPs) where
  getAnnotationEntry = const NoEntryVal

  exact (IPBinds _ binds) = setLayoutBoth $ markAnnotated binds

-- ---------------------------------------------------------------------

instance ExactPrint (IPBind GhcPs) where
  getAnnotationEntry (IPBind an _ _) = fromAnn an

  exact (IPBind an lr rhs) = do
    markAnnotated lr
    markEpAnn an AnnEqual
    markAnnotated rhs

-- ---------------------------------------------------------------------

instance ExactPrint HsIPName where
  getAnnotationEntry = const NoEntryVal

  exact (HsIPName fs) = printStringAdvance ("?" ++ (unpackFS fs))

-- ---------------------------------------------------------------------
-- Managing lists which have been separated, e.g. Sigs and Binds

prepareListAnnotationF :: (a -> EPP ()) -> [LocatedAn an a] -> [(RealSrcSpan,EPP ())]
prepareListAnnotationF f ls
  = map (\b -> (realSrcSpan $ getLocA b, f (unLoc b))) ls

prepareListAnnotationA :: ExactPrint (LocatedAn an a)
  => [LocatedAn an a] -> [(RealSrcSpan,EPP ())]
prepareListAnnotationA ls = map (\b -> (realSrcSpan $ getLocA b,markAnnotated b)) ls

withSortKey :: AnnSortKey -> [(RealSrcSpan, EPP ())] -> EPP ()
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
  mapM_ snd ordered

orderByFst :: Ord a => (a, b1) -> (a, b2) -> Ordering
orderByFst (a,_) (b,_) = compare a b

-- ---------------------------------------------------------------------

instance ExactPrint (Sig GhcPs) where
  getAnnotationEntry (TypeSig a _ _)  = fromAnn a
  getAnnotationEntry (PatSynSig a _ _) = fromAnn a
  getAnnotationEntry (ClassOpSig a _ _ _) = fromAnn a
  getAnnotationEntry (IdSig {}) = NoEntryVal
  getAnnotationEntry (FixSig a _) = fromAnn a
  getAnnotationEntry (InlineSig a _ _) = fromAnn a
  getAnnotationEntry (SpecSig a _ _ _) = fromAnn a
  getAnnotationEntry (SpecInstSig a _ _) = fromAnn a
  getAnnotationEntry (MinimalSig a _ _) = fromAnn a
  getAnnotationEntry (SCCFunSig a _ _ _) = fromAnn a
  getAnnotationEntry (CompleteMatchSig a _ _ _) = fromAnn a

-- instance Annotate (Sig GhcPs) where

  exact (TypeSig an vars ty)  = exactVarSig an vars ty

  exact (PatSynSig an lns typ) = do
    markLocatedAAL an asRest AnnPattern
    markAnnotated lns
    markLocatedAA an asDcolon
    markAnnotated typ

  exact (ClassOpSig an is_deflt vars ty)
    | is_deflt  = markLocatedAAL an asRest AnnDefault >> exactVarSig an vars ty
    | otherwise = exactVarSig an vars ty

--   markAST _ (IdSig {}) =
--     traceM "warning: Introduced after renaming"

  exact (FixSig an (FixitySig _ names (Fixity src v fdir))) = do
    let fixstr = case fdir of
         InfixL -> "infixl"
         InfixR -> "infixr"
         InfixN -> "infix"
    markLocatedAALS an id AnnInfix (Just fixstr)
--     markSourceText src (show v)
    markLocatedAALS an id AnnVal (Just (sourceTextToString src (show v)))
    markAnnotated names


  exact (InlineSig an ln inl) = do
    markAnnOpen an (inl_src inl) "{-# INLINE"
    -- markActivation l (inl_act inl)
    markActivation an id (inl_act inl)
    markAnnotated ln
    -- markWithString AnnClose "#-}" -- '#-}'
    debugM $ "InlineSig:an=" ++ showAst an
    p <- getPosP
    debugM $ "InlineSig: p=" ++ show p
    markLocatedAALS an id AnnClose (Just "#-}")
    debugM $ "InlineSig:done"

  exact (SpecSig an ln typs inl) = do
    markAnnOpen an (inl_src inl) "{-# SPECIALISE" -- Note: may be {-# SPECIALISE_INLINE
    markActivation an id (inl_act inl)
    markAnnotated ln
    markEpAnn an AnnDcolon
    markAnnotated typs
    markLocatedAALS an id AnnClose (Just "#-}")

  exact (SpecInstSig an src typ) = do
    markAnnOpen an src "{-# SPECIALISE"
    markEpAnn an AnnInstance
    markAnnotated typ
    markLocatedAALS an id AnnClose (Just "#-}")

--   markAST _ (SpecInstSig _ src typ) = do
--     markAnnOpen src "{-# SPECIALISE"
--     mark AnnInstance
--     markLHsSigType typ
--     markWithString AnnClose "#-}" -- '#-}'
--     markTrailingSemi

  exact (MinimalSig an src formula) = do
    markAnnOpen an src "{-# MINIMAL"
    markAnnotated formula
    markLocatedAALS an id AnnClose (Just "#-}")

--   markAST _ (MinimalSig _ src formula) = do
--     markAnnOpen src "{-# MINIMAL"
--     markLocated formula
--     markWithString AnnClose "#-}"
--     markTrailingSemi

  exact (SCCFunSig an src ln ml) = do
    markAnnOpen an src "{-# SCC"
    markAnnotated ln
    markAnnotated ml
    markLocatedAALS an id AnnClose (Just "#-}")

  exact (CompleteMatchSig an src cs mty) = do
    markAnnOpen an src "{-# COMPLETE"
    markAnnotated cs
    case mty of
      Nothing -> return ()
      Just ty -> do
        markEpAnn an AnnDcolon
        markAnnotated ty
    markLocatedAALS an id AnnClose (Just "#-}")

  exact x = error $ "exact Sig for:" ++ showAst x

-- ---------------------------------------------------------------------

exactVarSig :: (ExactPrint a) => EpAnn AnnSig -> [LocatedN RdrName] -> a -> EPP ()
exactVarSig an vars ty = do
  mapM_ markAnnotated vars
  markLocatedAA an asDcolon
  markAnnotated ty

-- ---------------------------------------------------------------------

-- instance ExactPrint (FixitySig GhcPs) where
--   getAnnotationEntry = const NoEntryVal

--   exact (FixitySig an names (Fixity src v fdir)) = do
--     let fixstr = case fdir of
--          InfixL -> "infixl"
--          InfixR -> "infixr"
--          InfixN -> "infix"
--     markAnnotated names
--     markLocatedAALS an id AnnInfix (Just fixstr)
-- --   markAST _ (FixSig _ (FixitySig _ lns (Fixity src v fdir))) = do
-- --     let fixstr = case fdir of
-- --          InfixL -> "infixl"
-- --          InfixR -> "infixr"
-- --          InfixN -> "infix"
-- --     markWithString AnnInfix fixstr
-- --     markSourceText src (show v)
-- --     setContext (Set.singleton InfixOp) $ markListIntercalate lns
-- --     markTrailingSemi
-- ---------------------------------------------------------------------

instance ExactPrint (StandaloneKindSig GhcPs) where
  getAnnotationEntry (StandaloneKindSig an _ _) = fromAnn an

  exact (StandaloneKindSig an vars sig) = do
    markEpAnn an AnnType
    markAnnotated vars
    markEpAnn an AnnDcolon
    markAnnotated sig

-- ---------------------------------------------------------------------

instance ExactPrint (DefaultDecl GhcPs) where
  getAnnotationEntry (DefaultDecl an _) = fromAnn an

  exact (DefaultDecl an tys) = do
    markEpAnn an AnnDefault
    markEpAnn an AnnOpenP
    markAnnotated tys
    markEpAnn an AnnCloseP

-- ---------------------------------------------------------------------

instance ExactPrint (AnnDecl GhcPs) where
  getAnnotationEntry (HsAnnotation an _ _ _) = fromAnn an

  exact (HsAnnotation an src prov e) = do
    markAnnOpenP an src "{-# ANN"
    case prov of
      (ValueAnnProvenance n) -> markAnnotated n
      (TypeAnnProvenance n) -> do
        markLocatedAAL an apr_rest AnnType
        markAnnotated n
      ModuleAnnProvenance -> markLocatedAAL an apr_rest AnnModule

    markAnnotated e
    markAnnCloseP an

-- ---------------------------------------------------------------------

instance ExactPrint (BF.BooleanFormula (LocatedN RdrName)) where
  getAnnotationEntry = const NoEntryVal

  exact (BF.Var x)  = do
    markAnnotated x
  exact (BF.Or ls)  = markAnnotated ls
  exact (BF.And ls) = do
    markAnnotated ls
  exact (BF.Parens x)  = do
    -- mark AnnOpenP -- '('
    markAnnotated x
    -- mark AnnCloseP -- ')'

-- instance  (Annotate name) => Annotate (GHC.BooleanFormula (GHC.Located name)) where
--   markAST _ (GHC.Var x)  = do
--     setContext (Set.singleton PrefixOp) $ markLocated x
--     inContext (Set.fromList [AddVbar]) $ mark GHC.AnnVbar
--     inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma
--   markAST _ (GHC.Or ls)  = markListIntercalateWithFunLevelCtx markLocated 2 AddVbar ls
--   markAST _ (GHC.And ls) = do
--     markListIntercalateWithFunLevel markLocated 2 ls
--     inContext (Set.fromList [AddVbar]) $ mark GHC.AnnVbar
--     inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma
--   markAST _ (GHC.Parens x)  = do
--     mark GHC.AnnOpenP -- '('
--     markLocated x
--     mark GHC.AnnCloseP -- ')'
--     inContext (Set.fromList [AddVbar]) $ mark GHC.AnnVbar
--     inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma

-- ---------------------------------------------------------------------

-- instance ExactPrint (LHsSigWcType GhcPs) where
-- instance ExactPrint (HsWildCardBndrs GhcPs (LHsSigType GhcPs)) where
instance (ExactPrint body) => ExactPrint (HsWildCardBndrs GhcPs body) where
  getAnnotationEntry = const NoEntryVal
  exact (HsWC _ ty) = markAnnotated ty

-- ---------------------------------------------------------------------

instance ExactPrint (GRHS GhcPs (LocatedA (HsExpr GhcPs))) where
  getAnnotationEntry (GRHS an _ _) = fromAnn an

  exact (GRHS an guards expr) = do
    debugM $ "GRHS comments:" ++ showGhc (comments an)
    markAnnKwM an ga_vbar AnnVbar
    markAnnotated guards
    debugM $ "GRHS before matchSeparator"
    markLocatedAA an ga_sep -- Mark the matchSeparator for these GRHSs
    debugM $ "GRHS after matchSeparator"
    markAnnotated expr
    -- markLocatedAA an ga_sep

instance ExactPrint (GRHS GhcPs (LocatedA (HsCmd GhcPs))) where
  getAnnotationEntry (GRHS ann _ _) = fromAnn ann

  exact (GRHS an guards expr) = do
    markAnnKwM an ga_vbar AnnVbar
    markAnnotated guards
    markLocatedAA an ga_sep -- Mark the matchSeparator for these GRHSs
    markAnnotated expr

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
  getAnnotationEntry (HsLamCase an _)             = fromAnn an
  getAnnotationEntry (HsApp an _ _)               = fromAnn an
  getAnnotationEntry (HsAppType _ _ _)            = NoEntryVal
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
  getAnnotationEntry (HsSpliceE an _)             = fromAnn an
  getAnnotationEntry (HsProc an _ _)              = fromAnn an
  getAnnotationEntry (HsStatic an _)              = fromAnn an
  getAnnotationEntry (HsPragE{})                  = NoEntryVal


  exact (HsVar _ n) = markAnnotated n
  exact x@(HsUnboundVar an _v) = do
    case an of
      EpAnnNotUsed -> withPpr x
      EpAnn _ (EpAnnUnboundVar (ob,cb) l) _ -> do
        printStringAtAA ob "`"
        printStringAtAA l  "_"
        printStringAtAA cb "`"
  -- exact x@(HsRecSel{})                 = withPpr x
  exact x@(HsOverLabel _ _) = withPpr x

  exact (HsIPVar _ (HsIPName n))
    = printStringAdvance ("?" ++ unpackFS n)

  exact x@(HsOverLit _an ol) = do
    let str = case ol_val ol of
                HsIntegral   (IL src _ _) -> src
                HsFractional (FL { fl_text = src }) -> src
                HsIsString src _          -> src
    -- markExternalSourceText l str ""
    case str of
      SourceText s -> printStringAdvance s
      NoSourceText -> withPpr x

  exact (HsLit _an lit) = withPpr lit
  exact (HsLam _ (MG _ (L _ [match]) _)) = do
    markAnnotated match
      -- markExpr _ (HsLam _ (MG _ (L _ [match]) _)) = do
      --   setContext (Set.singleton LambdaExpr) $ do
      --   -- TODO: Change this, HsLam binds do not need obey layout rules.
      --   --       And will only ever have a single match
      --     markLocated match
      -- markExpr _ (HsLam _ _) = error $ "HsLam with other than one match"
  exact (HsLam _ _) = error $ "HsLam with other than one match"

  exact (HsLamCase an mg) = do
    markEpAnn an AnnLam
    markEpAnn an AnnCase
    markAnnotated mg

  exact (HsApp _an e1 e2) = do
    p <- getPosP
    debugM $ "HsApp entered. p=" ++ show p
    markAnnotated e1
    markAnnotated e2
  exact (HsAppType ss fun arg) = do
    markAnnotated fun
    printStringAtSs ss "@"
    markAnnotated arg
  exact (OpApp _an e1 e2 e3) = do
    markAnnotated e1
    markAnnotated e2
    markAnnotated e3

  exact (NegApp an e _) = do
    markEpAnn an AnnMinus
    markAnnotated e

  exact (HsPar _an lpar e rpar) = do
    markToken lpar
    markAnnotated e
    debugM $ "HsPar closing paren"
    markToken rpar
    debugM $ "HsPar done"

  exact (SectionL _an expr op) = do
    markAnnotated expr
    markAnnotated op

  exact (SectionR _an op expr) = do
    markAnnotated op
    markAnnotated expr

  exact (ExplicitTuple an args b) = do
    if b == Boxed then markEpAnn an AnnOpenP
                  else markEpAnn an AnnOpenPH

    mapM_ markAnnotated args

    if b == Boxed then markEpAnn an AnnCloseP
                  else markEpAnn an AnnClosePH
    debugM $ "ExplicitTuple done"

  exact (ExplicitSum an _alt _arity expr) = do
    -- markEpAnn an AnnOpenPH
    markAnnKw an aesOpen AnnOpenPH
    markAnnKwAll an aesBarsBefore AnnVbar
    markAnnotated expr
    markAnnKwAll an aesBarsAfter AnnVbar
    markAnnKw an aesClose AnnClosePH

  exact (HsCase an e alts) = do
    markAnnKw an hsCaseAnnCase AnnCase
    markAnnotated e
    markAnnKw an hsCaseAnnOf AnnOf
    markEpAnn' an hsCaseAnnsRest AnnOpenC
    markEpAnnAll an hsCaseAnnsRest AnnSemi
    setLayoutBoth $ markAnnotated alts
    markEpAnn' an hsCaseAnnsRest AnnCloseC

  -- exact x@(HsCase EpAnnNotUsed   _ _) = withPpr x
  exact (HsIf an e1 e2 e3) = do
    markAnnKw an aiIf AnnIf
    markAnnotated e1
    markAnnKwM an aiThenSemi AnnSemi
    markAnnKw an aiThen AnnThen
    markAnnotated e2
    markAnnKwM an aiElseSemi AnnSemi
    markAnnKw an aiElse AnnElse
    markAnnotated e3

  exact (HsMultiIf an mg) = do
    markEpAnn an AnnIf
    markEpAnn an AnnOpenC -- optional
    markAnnotated mg
    markEpAnn an AnnCloseC -- optional

  exact (HsLet _an tkLet binds tkIn e) = do
    setLayoutBoth $ do -- Make sure the 'in' gets indented too
      markToken tkLet
      debugM $ "HSlet:binds coming"
      setLayoutBoth $ markAnnotated binds
      debugM $ "HSlet:binds done"
      markToken tkIn
      debugM $ "HSlet:expr coming"
      markAnnotated e

  exact (HsDo an do_or_list_comp stmts) = do
    debugM $ "HsDo"
    markAnnList True an $ exactDo an do_or_list_comp stmts

  exact (ExplicitList an es) = do
    debugM $ "ExplicitList start"
    markLocatedMAA an al_open
    markAnnotated es
    markLocatedMAA an al_close
    debugM $ "ExplicitList end"
  exact (RecordCon an con_id binds) = do
    markAnnotated con_id
    markEpAnn an AnnOpenC
    markAnnotated binds
    markEpAnn an AnnCloseC
  exact (RecordUpd an expr fields) = do
    markAnnotated expr
    markEpAnn an AnnOpenC
    markAnnotated fields
    markEpAnn an AnnCloseC
  exact (HsGetField _an expr field) = do
    markAnnotated expr
    markAnnotated field
  exact (HsProjection an flds) = do
    markAnnKw an apOpen AnnOpenP
    markAnnotated flds
    markAnnKw an apClose AnnCloseP
  exact (ExprWithTySig an expr sig) = do
    markAnnotated expr
    markEpAnn an AnnDcolon
    markAnnotated sig
  exact (ArithSeq an _ seqInfo) = do
    markEpAnn an AnnOpenS -- '['
    case seqInfo of
        From e -> do
          markAnnotated e
          markEpAnn an AnnDotdot
        FromTo e1 e2 -> do
          markAnnotated e1
          markEpAnn an AnnDotdot
          markAnnotated e2
        FromThen e1 e2 -> do
          markAnnotated e1
          markEpAnn an AnnComma
          markAnnotated e2
          markEpAnn an AnnDotdot
        FromThenTo e1 e2 e3 -> do
          markAnnotated e1
          markEpAnn an AnnComma
          markAnnotated e2
          markEpAnn an AnnDotdot
          markAnnotated e3
    markEpAnn an AnnCloseS -- ']'


  exact (HsTypedBracket an e) = do
    markLocatedAALS an id AnnOpen (Just "[||")
    markLocatedAALS an id AnnOpenE (Just "[e||")
    markAnnotated e
    markLocatedAALS an id AnnClose (Just "||]")

  exact (HsUntypedBracket an (ExpBr _ e)) = do
    markEpAnn an AnnOpenEQ -- "[|"
    markEpAnn an AnnOpenE  -- "[e|" -- optional
    markAnnotated e
    markEpAnn an AnnCloseQ -- "|]"
  exact (HsUntypedBracket an (PatBr _ e)) = do
    markLocatedAALS an id AnnOpen (Just "[p|")
    markAnnotated e
    markEpAnn an AnnCloseQ -- "|]"
  exact (HsUntypedBracket an (DecBrL _ e)) = do
    markLocatedAALS an id AnnOpen (Just "[d|")
    markAnnotated e
    markEpAnn an AnnCloseQ -- "|]"
  -- -- exact (HsUntypedBracket an (DecBrG _ _)) =
  -- --   traceM "warning: DecBrG introduced after renamer"
  exact (HsUntypedBracket an (TypBr _ e)) = do
    markLocatedAALS an id AnnOpen (Just "[t|")
    markAnnotated e
    markEpAnn an AnnCloseQ -- "|]"
  exact (HsUntypedBracket an (VarBr _ b e)) = do
    if b
      then do
        markEpAnn an AnnSimpleQuote
        markAnnotated e
      else do
        markEpAnn an AnnThTyQuote
        markAnnotated e


  exact (HsSpliceE _ sp) = markAnnotated sp

  exact (HsProc an p c) = do
    debugM $ "HsProc start"
    markEpAnn an AnnProc
    markAnnotated p
    markEpAnn an AnnRarrow
    debugM $ "HsProc after AnnRarrow"
    markAnnotated c

  exact (HsStatic an e) = do
    markEpAnn an AnnStatic
    markAnnotated e

  exact (HsPragE _ prag e) = do
    markAnnotated prag
    markAnnotated e
  exact x = error $ "exact HsExpr for:" ++ showAst x

-- ---------------------------------------------------------------------

exactDo :: (ExactPrint body)
        => EpAnn AnnList -> HsDoFlavour -> body -> EPP ()
exactDo an (DoExpr m)    stmts = exactMdo an m AnnDo             >> markAnnotatedWithLayout stmts
exactDo an GhciStmtCtxt  stmts = markLocatedAAL an al_rest AnnDo >> markAnnotatedWithLayout stmts
exactDo an (MDoExpr m)   stmts = exactMdo an m AnnMdo            >> markAnnotatedWithLayout stmts
exactDo _  ListComp      stmts = markAnnotatedWithLayout stmts
exactDo _  MonadComp     stmts = markAnnotatedWithLayout stmts

exactMdo :: EpAnn AnnList -> Maybe ModuleName -> AnnKeywordId -> EPP ()
exactMdo an Nothing            kw = markLocatedAAL  an al_rest kw
exactMdo an (Just module_name) kw = markLocatedAALS an al_rest kw (Just n)
    where
      n = (moduleNameString module_name) ++ "." ++ (keywordToString (G kw))


-- ---------------------------------------------------------------------
instance ExactPrint (HsPragE GhcPs) where
  getAnnotationEntry HsPragSCC{}  = NoEntryVal

  exact (HsPragSCC an st sl) = do
    markAnnOpenP an st "{-# SCC"
    let txt = sourceTextToString (sl_st sl) (unpackFS $ sl_fs sl)
    markLocatedAALS an apr_rest AnnVal    (Just txt) -- optional
    markLocatedAALS an apr_rest AnnValStr (Just txt) -- optional
    markAnnCloseP an

      -- markExpr _ (GHC.HsPragE _ prag e) = do
      --   case prag of
      --     (GHC.HsPragSCC _ src csFStr) -> do
      --       markAnnOpen src "{-# SCC"
      --       let txt = sourceTextToString (GHC.sl_st csFStr) (GHC.unpackFS $ GHC.sl_fs csFStr)
      --       markWithStringOptional GHC.AnnVal    txt
      --       markWithString         GHC.AnnValStr txt
      --       markWithString GHC.AnnClose "#-}"
      --       markLocated e

      --     (GHC.HsPragTick _ src (str,(v1,v2),(v3,v4)) ((s1,s2),(s3,s4))) -> do
      --       -- '{-# GENERATED' STRING INTEGER ':' INTEGER '-' INTEGER ':' INTEGER '#-}'
      --       markAnnOpen src  "{-# GENERATED"
      --       markOffsetWithString GHC.AnnVal 0 (stringLiteralToString str) -- STRING

      --       let
      --         markOne n  v GHC.NoSourceText   = markOffsetWithString GHC.AnnVal n (show v)
      --         markOne n _v (GHC.SourceText s) = markOffsetWithString GHC.AnnVal n s

      --       markOne  1 v1 s1 -- INTEGER
      --       markOffset GHC.AnnColon 0 -- ':'
      --       markOne  2 v2 s2 -- INTEGER
      --       mark   GHC.AnnMinus   -- '-'
      --       markOne  3 v3 s3 -- INTEGER
      --       markOffset GHC.AnnColon 1 -- ':'
      --       markOne  4 v4 s4 -- INTEGER
      --       markWithString   GHC.AnnClose  "#-}"
      --       markLocated e

-- ---------------------------------------------------------------------

instance ExactPrint (HsSplice GhcPs) where
  getAnnotationEntry (HsTypedSplice an _ _ _)   = fromAnn an
  getAnnotationEntry (HsUntypedSplice an _ _ _) = fromAnn an
  getAnnotationEntry (HsQuasiQuote _ _ _ _ _)   = NoEntryVal
  getAnnotationEntry (HsSpliced _ _ _)          = NoEntryVal

  exact (HsTypedSplice an DollarSplice _n e) = do
    markEpAnn an AnnDollarDollar
    markAnnotated e

  -- = ppr_splice (text "$$") n e empty
  -- exact (HsTypedSplice _ BareSplice _ _ )
  -- = panic "Bare typed splice"  -- impossible
  exact (HsUntypedSplice an decoration _n b) = do
    when (decoration == DollarSplice) $ markEpAnn an AnnDollar
    markAnnotated b

  -- exact (HsUntypedSplice _ DollarSplice n e)
  -- = ppr_splice (text "$")  n e empty
  -- exact (HsUntypedSplice _ BareSplice n e)
  -- = ppr_splice empty  n e empty

  exact (HsQuasiQuote _ _ q ss fs) = do
    -- The quasiquote string does not honour layout offsets. Store
    -- the colOffset for now.
    -- TODO: use local?
    oldOffset <- getLayoutOffsetP
    setLayoutOffsetP 0
    printStringAdvance
            -- Note: Lexer.x does not provide unicode alternative. 2017-02-26
            ("[" ++ (showPprUnsafe q) ++ "|" ++ (unpackFS fs) ++ "|]")
    setLayoutOffsetP oldOffset
    p <- getPosP
    debugM $ "HsQuasiQuote:after:(p,ss)=" ++ show (p,ss2range ss)

  -- exact (HsSpliced _ _ thing)         = ppr thing
  -- exact (XSplice x)                   = case ghcPass @p of
  exact x = error $ "exact HsSplice for:" ++ showAst x

-- ---------------------------------------------------------------------

-- TODO:AZ: combine these instances
instance ExactPrint (MatchGroup GhcPs (LocatedA (HsExpr GhcPs))) where
  getAnnotationEntry = const NoEntryVal
  exact (MG _ matches _) = do
    -- TODO:AZ use SortKey, in MG ann.
    markAnnotated matches

instance ExactPrint (MatchGroup GhcPs (LocatedA (HsCmd GhcPs))) where
  getAnnotationEntry = const NoEntryVal
  exact (MG _ matches _) = do
    -- TODO:AZ use SortKey, in MG ann.
    markAnnotated matches

-- ---------------------------------------------------------------------

instance (ExactPrint body) => ExactPrint (HsRecFields GhcPs body) where
  getAnnotationEntry = const NoEntryVal
  exact (HsRecFields fields mdot) = do
    markAnnotated fields
    case mdot of
      Nothing -> return ()
      Just (L ss _) ->
        printStringAtSs ss ".."
      -- Note: mdot contains the SrcSpan where the ".." appears, if present

-- ---------------------------------------------------------------------

-- instance (ExactPrint body) => ExactPrint (HsRecField GhcPs body) where
instance (ExactPrint body)
    => ExactPrint (HsFieldBind (LocatedAn NoEpAnns (FieldOcc GhcPs)) body) where
  getAnnotationEntry x = fromAnn (hfbAnn x)
  exact (HsFieldBind an f arg isPun) = do
    debugM $ "HsFieldBind"
    markAnnotated f
    if isPun then return ()
             else do
      markEpAnn an AnnEqual
      markAnnotated arg

-- ---------------------------------------------------------------------

instance (ExactPrint body)
    => ExactPrint (HsFieldBind (LocatedAn NoEpAnns (FieldLabelStrings GhcPs)) body) where
  getAnnotationEntry x = fromAnn (hfbAnn x)
  exact (HsFieldBind an f arg isPun) = do
    debugM $ "HsFieldBind FieldLabelStrings"
    markAnnotated f
    if isPun then return ()
             else do
      markEpAnn an AnnEqual
      markAnnotated arg

-- ---------------------------------------------------------------------

-- instance ExactPrint (HsRecUpdField GhcPs ) where
instance (ExactPrint (LocatedA body))
    => ExactPrint (HsFieldBind (LocatedAn NoEpAnns (AmbiguousFieldOcc GhcPs)) (LocatedA body)) where
-- instance (ExactPrint body)
    -- => ExactPrint (HsFieldBind (AmbiguousFieldOcc GhcPs) body) where
  getAnnotationEntry x = fromAnn (hfbAnn x)
  exact (HsFieldBind an f arg isPun) = do
    debugM $ "HsRecUpdField"
    markAnnotated f
    if isPun then return ()
             else markEpAnn an AnnEqual
    unless ((locA $ getLoc arg) == noSrcSpan ) $ markAnnotated arg

-- ---------------------------------------------------------------------
instance
    (ExactPrint (HsFieldBind (LocatedAn NoEpAnns (a GhcPs)) body),
     ExactPrint (HsFieldBind (LocatedAn NoEpAnns (b GhcPs)) body))
    => ExactPrint
         (Either [LocatedA (HsFieldBind (LocatedAn NoEpAnns (a GhcPs)) body)]
                 [LocatedA (HsFieldBind (LocatedAn NoEpAnns (b GhcPs)) body)]) where
  getAnnotationEntry = const NoEntryVal
  exact (Left rbinds) = markAnnotated rbinds
  exact (Right pbinds) = markAnnotated pbinds

-- ---------------------------------------------------------------------

instance ExactPrint (FieldLabelStrings GhcPs) where
  getAnnotationEntry = const NoEntryVal
  exact (FieldLabelStrings fs) = markAnnotated fs

-- ---------------------------------------------------------------------

instance ExactPrint (DotFieldOcc GhcPs) where
  getAnnotationEntry (DotFieldOcc an _) = fromAnn an

  exact (DotFieldOcc an fs) = do
    markAnnKwM an afDot  AnnDot
    markAnnotated fs

-- ---------------------------------------------------------------------

instance ExactPrint (HsTupArg GhcPs) where
  getAnnotationEntry (Present an _) = fromAnn an
  getAnnotationEntry (Missing an)   = fromAnn an

  exact (Present _ e) = markAnnotated e

  exact (Missing EpAnnNotUsed) = return ()
  exact (Missing _) = printStringAdvance ","

-- ---------------------------------------------------------------------

instance ExactPrint (HsCmdTop GhcPs) where
  getAnnotationEntry = const NoEntryVal
  exact (HsCmdTop _ cmd) = markAnnotated cmd

-- ---------------------------------------------------------------------

instance ExactPrint (HsCmd GhcPs) where
  getAnnotationEntry (HsCmdArrApp an _ _ _ _)   = fromAnn an
  getAnnotationEntry (HsCmdArrForm an _ _ _ _ ) = fromAnn an
  getAnnotationEntry (HsCmdApp an _ _ )         = fromAnn an
  getAnnotationEntry (HsCmdLam {})              = NoEntryVal
  getAnnotationEntry (HsCmdPar an _ _ _)        = fromAnn an
  getAnnotationEntry (HsCmdCase an _ _)         = fromAnn an
  getAnnotationEntry (HsCmdLamCase an _)        = fromAnn an
  getAnnotationEntry (HsCmdIf an _ _ _ _)       = fromAnn an
  getAnnotationEntry (HsCmdLet an _ _ _ _)      = fromAnn an
  getAnnotationEntry (HsCmdDo an _)             = fromAnn an



  exact (HsCmdArrApp an arr arg _o isRightToLeft) = do
    if isRightToLeft
      then do
        markAnnotated arr
        markKw (anns an)
        markAnnotated arg
      else do
        markAnnotated arg
        markKw (anns an)
        markAnnotated arr

  exact (HsCmdArrForm an e fixity _mf cs) = do
    markLocatedMAA an al_open
    case (fixity, cs) of
      (Infix, (arg1:argrest)) -> do
        markAnnotated arg1
        markAnnotated e
        markAnnotated argrest
      (Prefix, _) -> do
        markAnnotated e
        markAnnotated cs
      (Infix, []) -> error "Not possible"
    markLocatedMAA an al_close

  exact (HsCmdApp _an e1 e2) = do
    markAnnotated e1
    markAnnotated e2

  exact (HsCmdLam _ match) = markAnnotated match

  exact (HsCmdPar _an lpar e rpar) = do
    markToken lpar
    markAnnotated e
    markToken rpar

  exact (HsCmdCase an e alts) = do
    markAnnKw an hsCaseAnnCase AnnCase
    markAnnotated e
    markAnnKw an hsCaseAnnOf AnnOf
    markEpAnn' an hsCaseAnnsRest AnnOpenC
    markEpAnnAll an hsCaseAnnsRest AnnSemi
    markAnnotated alts
    markEpAnn' an hsCaseAnnsRest AnnCloseC

  exact (HsCmdLamCase an matches) = do
    markEpAnn an AnnLam
    markEpAnn an AnnCase
    markAnnotated matches

  exact (HsCmdIf an _ e1 e2 e3) = do
    markAnnKw an aiIf AnnIf
    markAnnotated e1
    markAnnKwM an aiThenSemi AnnSemi
    markAnnKw an aiThen AnnThen
    markAnnotated e2
    markAnnKwM an aiElseSemi AnnSemi
    markAnnKw an aiElse AnnElse
    markAnnotated e3

--   markAST _ (GHC.HsCmdLet _ (GHC.L _ binds) e) = do
--     mark GHC.AnnLet
--     markOptional GHC.AnnOpenC
--     markLocalBindsWithLayout binds
--     markOptional GHC.AnnCloseC
--     mark GHC.AnnIn
--     markLocated e

  exact (HsCmdDo an es) = do
    debugM $ "HsCmdDo"
    markEpAnn' an al_rest AnnDo
    markAnnotated es

--   markAST _ (GHC.HsCmdDo _ (GHC.L _ es)) = do
--     mark GHC.AnnDo
--     markOptional GHC.AnnOpenC
--     markListWithLayout es
--     markOptional GHC.AnnCloseC

--   markAST _ (GHC.HsCmdWrap {}) =
--     traceM "warning: HsCmdWrap introduced after renaming"

--   markAST _ (GHC.XCmd x) = error $ "got XCmd for:" ++ showPprUnsafe x

  exact x = error $ "exact HsCmd for:" ++ showAst x

-- ---------------------------------------------------------------------

-- instance ExactPrint (CmdLStmt GhcPs) where
--   getAnnotationEntry = const NoEntryVal
--   exact (L _ a) = markAnnotated a

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

  exact (LastStmt _ body _ _) = do
    debugM $ "LastStmt"
    markAnnotated body

  exact (BindStmt an pat body) = do
    debugM $ "BindStmt"
    markAnnotated pat
    markEpAnn an AnnLarrow
    markAnnotated body

  exact (ApplicativeStmt _ _body _) = do
    debugM $ "ApplicativeStmt"
    -- TODO: ApplicativeStmt
    -- markAnnotated body
    error $ "need to complete ApplicativeStmt"

  exact (BodyStmt _ body _ _) = do
    debugM $ "BodyStmt"
    markAnnotated body

  exact (LetStmt an binds) = do
    debugM $ "LetStmt"
    markEpAnn an AnnLet
    markAnnotated binds

  exact (ParStmt _ pbs _ _) = do
    debugM $ "ParStmt"
    markAnnotated pbs

  -- markAST l (GHC.ParStmt _ pbs _ _) = do
  --   -- Within a given parallel list comprehension,one of the sections to be done
  --   -- in parallel. It is a normal list comprehension, so has a list of
  --   -- ParStmtBlock, one for each part of the sub- list comprehension


  --   ifInContext (Set.singleton Intercalate)
  --     (

  --     unsetContext Intercalate $
  --       markListWithContextsFunction
  --         (LC (Set.singleton Intercalate)  -- only
  --             Set.empty -- first
  --             Set.empty -- middle
  --             (Set.singleton Intercalate) -- last
  --         ) (markAST l) pbs
  --        )
  --     (
  --     unsetContext Intercalate $
  --       markListWithContextsFunction
  --         (LC Set.empty -- only
  --             (Set.fromList [AddVbar]) -- first
  --             (Set.fromList [AddVbar]) -- middle
  --             Set.empty                -- last
  --         ) (markAST l) pbs
  --      )
  --   markTrailingSemi


-- pprStmt (TransStmt { trS_stmts = stmts, trS_by = by
--                    , trS_using = using, trS_form = form })
--   = sep $ punctuate comma (map ppr stmts ++ [pprTransStmt by using form])

  exact (TransStmt an form stmts _b using by _ _ _) = do
    debugM $ "TransStmt"
    markAnnotated stmts
    exactTransStmt an by using form

  -- markAST _ (GHC.TransStmt _ form stmts _b using by _ _ _) = do
  --   setContext (Set.singleton Intercalate) $ mapM_ markLocated stmts
  --   case form of
  --     GHC.ThenForm -> do
  --       mark GHC.AnnThen
  --       unsetContext Intercalate $ markLocated using
  --       case by of
  --         Just b -> do
  --           mark GHC.AnnBy
  --           unsetContext Intercalate $ markLocated b
  --         Nothing -> return ()
  --     GHC.GroupForm -> do
  --       mark GHC.AnnThen
  --       mark GHC.AnnGroup
  --       case by of
  --         Just b -> mark GHC.AnnBy >> markLocated b
  --         Nothing -> return ()
  --       mark GHC.AnnUsing
  --       markLocated using
  --   inContext (Set.singleton AddVbar)     $ mark GHC.AnnVbar
  --   inContext (Set.singleton Intercalate) $ mark GHC.AnnComma
  --   markTrailingSemi

  exact (RecStmt an stmts _ _ _ _ _) = do
    debugM $ "RecStmt"
    markLocatedAAL an al_rest AnnRec
    markAnnList True an (markAnnotated stmts)

  -- markAST _ (GHC.RecStmt _ stmts _ _ _ _ _) = do
  --   mark GHC.AnnRec
  --   markOptional GHC.AnnOpenC
  --   markInside GHC.AnnSemi
  --   markListWithLayout stmts
  --   markOptional GHC.AnnCloseC
  --   inContext (Set.singleton AddVbar)     $ mark GHC.AnnVbar
  --   inContext (Set.singleton Intercalate) $ mark GHC.AnnComma
  --   markTrailingSemi

  -- exact x = error $ "exact CmdLStmt for:" ++ showAst x
  -- exact x = error $ "exact CmdLStmt for:"


-- ---------------------------------------------------------------------

instance ExactPrint (ParStmtBlock GhcPs GhcPs) where
  getAnnotationEntry = const NoEntryVal
  exact (ParStmtBlock _ stmts _ _) = markAnnotated stmts

exactTransStmt :: EpAnn [AddEpAnn] -> Maybe (LHsExpr GhcPs) -> (LHsExpr GhcPs) -> TransForm -> EPP ()
exactTransStmt an by using ThenForm = do
  debugM $ "exactTransStmt:ThenForm"
  markEpAnn an AnnThen
  markAnnotated using
  case by of
    Nothing -> return ()
    Just b -> do
      markEpAnn an AnnBy
      markAnnotated b
exactTransStmt an by using GroupForm = do
  debugM $ "exactTransStmt:GroupForm"
  markEpAnn an AnnThen
  markEpAnn an AnnGroup
  case by of
    Just b -> do
      markEpAnn an AnnBy
      markAnnotated b
    Nothing -> return ()
  markEpAnn an AnnUsing
  markAnnotated using

-- ---------------------------------------------------------------------

instance ExactPrint (TyClDecl GhcPs) where
  getAnnotationEntry (FamDecl   { })                      = NoEntryVal
  getAnnotationEntry (SynDecl   { tcdSExt = an })         = fromAnn an
  getAnnotationEntry (DataDecl  { tcdDExt = an })         = fromAnn an
  getAnnotationEntry (ClassDecl { tcdCExt = (an, _, _) }) = fromAnn an

  exact (FamDecl _ decl) = do
    markAnnotated decl

  exact (SynDecl { tcdSExt = an
                 , tcdLName = ltycon, tcdTyVars = tyvars, tcdFixity = fixity
                 , tcdRhs = rhs }) = do
    -- There may be arbitrary parens around parts of the constructor that are
    -- infix.
    -- Turn these into comments so that they feed into the right place automatically
    annotationsToComments (epAnnAnns an) [AnnOpenP,AnnCloseP]
    markEpAnn an AnnType

    -- markTyClass Nothing fixity ln tyvars
    exactVanillaDeclHead ltycon tyvars fixity Nothing
    markEpAnn an AnnEqual
    markAnnotated rhs

    -- ppr (SynDecl { tcdLName = ltycon, tcdTyVars = tyvars, tcdFixity = fixity
    --              , tcdRhs = rhs })
    --   = hang (text "type" <+>
    --           pp_vanilla_decl_head ltycon tyvars fixity Nothing <+> equals)
    --       4 (ppr rhs)
-- {-
--     SynDecl { tcdSExt   :: XSynDecl pass          -- ^ Post renameer, FVs
--             , tcdLName  :: Located (IdP pass)     -- ^ Type constructor
--             , tcdTyVars :: LHsQTyVars pass        -- ^ Type variables; for an
--                                                   -- associated type these
--                                                   -- include outer binders
--             , tcdFixity :: LexicalFixity    -- ^ Fixity used in the declaration
--             , tcdRhs    :: LHsType pass }         -- ^ RHS of type declaration

-- -}
--   markAST _ (GHC.SynDecl _ ln (GHC.HsQTvs _ tyvars) fixity typ) = do
--     -- There may be arbitrary parens around parts of the constructor that are
--     -- infix.
--     -- Turn these into comments so that they feed into the right place automatically
--     -- annotationsToComments [GHC.AnnOpenP,GHC.AnnCloseP]
--     mark GHC.AnnType

--     markTyClass Nothing fixity ln tyvars
--     mark GHC.AnnEqual
--     markLocated typ
--     markTrailingSemi

  exact (DataDecl { tcdDExt = an, tcdLName = ltycon, tcdTyVars = tyvars
                  , tcdFixity = fixity, tcdDataDefn = defn }) =
    exactDataDefn an (exactVanillaDeclHead ltycon tyvars fixity) defn

  -- -----------------------------------

  exact (ClassDecl {tcdCExt = (an, sortKey, _),
                    tcdCtxt = context, tcdLName = lclas, tcdTyVars = tyvars,
                    tcdFixity = fixity,
                    tcdFDs  = fds,
                    tcdSigs = sigs, tcdMeths = methods,
                    tcdATs = ats, tcdATDefs = at_defs,
                    tcdDocs = _docs})
      -- TODO: add a test that demonstrates tcdDocs
      | null sigs && isEmptyBag methods && null ats && null at_defs -- No "where" part
      = do
          top_matter
          markEpAnn an AnnOpenC
          markEpAnn an AnnCloseC

      | otherwise       -- Laid out
      = do
          top_matter
          markEpAnn an AnnOpenC
          markEpAnnAll an id AnnSemi
          withSortKey sortKey
                               (prepareListAnnotationA sigs
                             ++ prepareListAnnotationA (bagToList methods)
                             ++ prepareListAnnotationA ats
                             ++ prepareListAnnotationA at_defs
                             -- ++ prepareListAnnotation docs
                               )
          markEpAnn an AnnCloseC
      where
        top_matter = do
          annotationsToComments (epAnnAnns an)  [AnnOpenP, AnnCloseP]
          markEpAnn an AnnClass
          exactVanillaDeclHead lclas tyvars fixity context
          unless (null fds) $ do
            markEpAnn an AnnVbar
            markAnnotated fds
          markEpAnn an AnnWhere

-- ---------------------------------------------------------------------

instance ExactPrint (FunDep GhcPs) where
  getAnnotationEntry (FunDep an _ _) = fromAnn an

  exact (FunDep an ls rs') = do
    markAnnotated ls
    markEpAnn an AnnRarrow
    markAnnotated rs'

-- ---------------------------------------------------------------------

instance ExactPrint (FamilyDecl GhcPs) where
  getAnnotationEntry (FamilyDecl { fdExt = an }) = fromAnn an

  exact (FamilyDecl { fdExt = an
                    , fdInfo = info
                    , fdTopLevel = top_level
                    , fdLName = ltycon
                    , fdTyVars = tyvars
                    , fdFixity = fixity
                    , fdResultSig = L _ result
                    , fdInjectivityAnn = mb_inj }) = do
    -- = vcat [ pprFlavour info <+> pp_top_level <+>
    --          pp_vanilla_decl_head ltycon tyvars fixity Nothing <+>
    --          pp_kind <+> pp_inj <+> pp_where
    --        , nest 2 $ pp_eqns ]
    exactFlavour an info
    exact_top_level
    annotationsToCommentsA an [AnnOpenP,AnnCloseP]
    exactVanillaDeclHead ltycon tyvars fixity Nothing
    exact_kind
    case mb_inj of
      Nothing -> return ()
      Just inj -> do
        markEpAnn an AnnVbar
        markAnnotated inj
    case info of
      ClosedTypeFamily mb_eqns -> do
        markEpAnn an AnnWhere
        markEpAnn an AnnOpenC
        case mb_eqns of
          Nothing -> markEpAnn an AnnDotdot
          Just eqns -> markAnnotated eqns
        markEpAnn an AnnCloseC
      _ -> return ()
    where
      exact_top_level = case top_level of
                          TopLevel    -> markEpAnn an AnnFamily
                          NotTopLevel -> do
                            -- It seems that in some kind of legacy
                            -- mode the 'family' keyword is still
                            -- accepted.
                            markEpAnn an AnnFamily
                            return ()

      exact_kind = case result of
                     NoSig    _         -> return ()
                     KindSig  _ kind    -> markEpAnn an AnnDcolon >> markAnnotated kind
                     TyVarSig _ tv_bndr -> markEpAnn an AnnEqual >> markAnnotated tv_bndr

      -- exact_inj = case mb_inj of
      --               Just (L _ (InjectivityAnn _ lhs rhs)) ->
      --                 hsep [ vbar, ppr lhs, text "->", hsep (map ppr rhs) ]
      --               Nothing -> empty
      -- (pp_where, pp_eqns) = case info of
      --   ClosedTypeFamily mb_eqns ->
      --     ( text "where"
      --     , case mb_eqns of
      --         Nothing   -> text ".."
      --         Just eqns -> vcat $ map (ppr_fam_inst_eqn . unLoc) eqns )
      --   _ -> (empty, empty)

exactFlavour :: EpAnn [AddEpAnn] -> FamilyInfo GhcPs -> EPP ()
exactFlavour an DataFamily            = markEpAnn an AnnData
exactFlavour an OpenTypeFamily        = markEpAnn an AnnType
exactFlavour an (ClosedTypeFamily {}) = markEpAnn an AnnType

-- instance Outputable (FamilyInfo pass) where
--   ppr info = pprFlavour info <+> text "family"

-- ---------------------------------------------------------------------

exactDataDefn :: EpAnn [AddEpAnn]
              -> (Maybe (LHsContext GhcPs) -> EPP ()) -- Printing the header
              -> HsDataDefn GhcPs
              -> EPP ()
exactDataDefn an exactHdr
                 (HsDataDefn { dd_ND = new_or_data, dd_ctxt = context
                             , dd_cType = mb_ct
                             , dd_kindSig = mb_sig
                             , dd_cons = condecls, dd_derivs = derivings }) = do
  annotationsToComments (epAnnAnns an) [AnnOpenP, AnnCloseP]
  if new_or_data == DataType
    then markEpAnn an AnnData
    else markEpAnn an AnnNewtype
  markEpAnn an AnnInstance -- optional
  mapM_ markAnnotated mb_ct
  exactHdr context
  case mb_sig of
    Nothing -> return ()
    Just kind -> do
      markEpAnn an AnnDcolon
      markAnnotated kind
  when (isGadt condecls) $ markEpAnn an AnnWhere
  markEpAnn an AnnOpenC
  exact_condecls an condecls
  markEpAnn an AnnCloseC
  mapM_ markAnnotated derivings
  return ()

exactVanillaDeclHead :: LocatedN RdrName
                     -> LHsQTyVars GhcPs
                     -> LexicalFixity
                     -> Maybe (LHsContext GhcPs)
                     -> EPP ()
exactVanillaDeclHead thing (HsQTvs { hsq_explicit = tyvars }) fixity context = do
  let
    exact_tyvars :: [LHsTyVarBndr () GhcPs] -> EPP ()
    exact_tyvars (varl:varsr)
      | fixity == Infix && length varsr > 1 = do
         -- = hsep [char '(',ppr (unLoc varl), pprInfixOcc (unLoc thing)
         --        , (ppr.unLoc) (head varsr), char ')'
         --        , hsep (map (ppr.unLoc) (tail vaprsr))]
          markAnnotated varl
          markAnnotated thing
          markAnnotated (head varsr)
          markAnnotated (tail varsr)
          return ()
      | fixity == Infix = do
         -- = hsep [ppr (unLoc varl), pprInfixOcc (unLoc thing)
         -- , hsep (map (ppr.unLoc) varsr)]
          markAnnotated varl
          markAnnotated thing
          markAnnotated varsr
          return ()
      | otherwise = do
          -- hsep [ pprPrefixOcc (unLoc thing)
          --      , hsep (map (ppr.unLoc) (varl:varsr))]
          markAnnotated thing
          mapM_ markAnnotated (varl:varsr)
          return ()
    exact_tyvars [] = do
      -- pprPrefixOcc (unLoc thing)
      markAnnotated thing
  mapM_ markAnnotated context
  exact_tyvars tyvars

-- ---------------------------------------------------------------------

instance ExactPrint (InjectivityAnn GhcPs) where
  getAnnotationEntry (InjectivityAnn an _ _) = fromAnn an
  exact (InjectivityAnn an lhs rhs) = do
    markEpAnn an AnnVbar
    markAnnotated lhs
    markEpAnn an AnnRarrow
    mapM_ markAnnotated rhs
    --               Just (L _ (InjectivityAnn _ lhs rhs)) ->
    --                 hsep [ vbar, ppr lhs, text "->", hsep (map ppr rhs) ]
    --               Nothing -> empty

-- ---------------------------------------------------------------------

class Typeable flag => ExactPrintTVFlag flag where
  exactTVDelimiters :: EpAnn [AddEpAnn] -> flag -> Annotated () -> Annotated ()

instance ExactPrintTVFlag () where
  exactTVDelimiters an _ thing_inside = do
    markEpAnnAll an id AnnOpenP
    thing_inside
    markEpAnnAll an id AnnCloseP

instance ExactPrintTVFlag Specificity where
  exactTVDelimiters an s thing_inside = do
    markEpAnnAll an id open
    thing_inside
    markEpAnnAll an id close
    where
      (open, close) = case s of
        SpecifiedSpec -> (AnnOpenP, AnnCloseP)
        InferredSpec  -> (AnnOpenC, AnnCloseC)

instance ExactPrintTVFlag flag => ExactPrint (HsTyVarBndr flag GhcPs) where
  getAnnotationEntry (UserTyVar an _ _)     = fromAnn an
  getAnnotationEntry (KindedTyVar an _ _ _) = fromAnn an

  exact (UserTyVar an flag n) =
    exactTVDelimiters an flag $ markAnnotated n
  exact (KindedTyVar an flag n k) = exactTVDelimiters an flag $ do
    markAnnotated n
    markEpAnn an AnnDcolon
    markAnnotated k

-- ---------------------------------------------------------------------

-- NOTE: this is also an alias for LHsKind
-- instance ExactPrint (LHsType GhcPs) where
--   getAnnotationEntry = entryFromLocatedA
--   exact (L _ a) = markAnnotated a

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


  exact (HsForAllTy { hst_xforall = _an
                    , hst_tele = tele, hst_body = ty }) = do
    markAnnotated tele
    markAnnotated ty

  exact (HsQualTy _ ctxt ty) = do
    markAnnotated ctxt
    -- markEpAnn an AnnDarrow
    markAnnotated ty
  exact (HsTyVar an promoted name) = do
    when (promoted == IsPromoted) $ markEpAnn an AnnSimpleQuote
    markAnnotated name

  exact (HsAppTy _ t1 t2) = markAnnotated t1 >> markAnnotated t2
  exact (HsAppKindTy ss ty ki) = do
    markAnnotated ty
    printStringAtSs ss "@"
    markAnnotated ki
  exact (HsFunTy _an mult ty1 ty2) = do
    markAnnotated ty1
    markArrow mult
    markAnnotated ty2
  exact (HsListTy an tys) = do
    markOpeningParen an
    markAnnotated tys
    markClosingParen an
  exact (HsTupleTy an _con tys) = do
    markOpeningParen an
    markAnnotated tys
    markClosingParen an
  exact (HsSumTy an tys) = do
    markOpeningParen an
    markAnnotated tys
    markClosingParen an
  exact (HsOpTy an promoted t1 lo t2) = do
    when (isPromoted promoted) $ markEpAnn an AnnSimpleQuote
    markAnnotated t1
    markAnnotated lo
    markAnnotated t2
  exact (HsParTy an ty) = do
    markOpeningParen an
    markAnnotated ty
    markClosingParen an
  exact (HsIParamTy an n t) = do
      markAnnotated n
      markEpAnn an AnnDcolon
      markAnnotated t
  exact (HsStarTy _an isUnicode)
    = if isUnicode
        then printStringAdvance "\x2605" -- Unicode star
        else printStringAdvance "*"
  exact (HsKindSig an ty k) = do
    markAnnotated ty
    markEpAnn an AnnDcolon
    markAnnotated k
  exact (HsSpliceTy _ splice) = do
    markAnnotated splice
  -- exact x@(HsDocTy an _ _)          = withPpr x
  exact (HsBangTy an (HsSrcBang mt _up str) ty) = do
    case mt of
      NoSourceText -> return ()
      SourceText src -> do
        debugM $ "HsBangTy: src=" ++ showAst src
        markLocatedAALS an id AnnOpen  (Just src)
        markLocatedAALS an id AnnClose (Just "#-}")
        debugM $ "HsBangTy: done unpackedness"
    case str of
      SrcLazy     -> markEpAnn an AnnTilde
      SrcStrict   -> markEpAnn an AnnBang
      NoSrcStrict -> return ()
    markAnnotated ty
  -- exact x@(HsRecTy an _)            = withPpr x
  exact (HsExplicitListTy an prom tys) = do
    when (isPromoted prom) $ markEpAnn an AnnSimpleQuote
    markEpAnn an AnnOpenS
    markAnnotated tys
    markEpAnn an AnnCloseS
  exact (HsExplicitTupleTy an tys) = do
    markEpAnn an AnnSimpleQuote
    markEpAnn an AnnOpenP
    markAnnotated tys
    markEpAnn an AnnCloseP
  exact (HsTyLit _ lit) = do
    case lit of
      (HsNumTy src v) -> printSourceText src (show v)
      (HsStrTy src v) -> printSourceText src (show v)
      (HsCharTy src v) -> printSourceText src (show v)
  exact (HsWildCardTy _) = printStringAdvance "_"
  exact x = error $ "missing match for HsType:" ++ showAst x

-- ---------------------------------------------------------------------

instance ExactPrint (HsForAllTelescope GhcPs) where
  getAnnotationEntry (HsForAllVis an _)   = fromAnn an
  getAnnotationEntry (HsForAllInvis an _) = fromAnn an

  exact (HsForAllVis an bndrs)   = do
    markLocatedAA an fst -- AnnForall
    markAnnotated bndrs
    markLocatedAA an snd -- AnnRarrow

  exact (HsForAllInvis an bndrs) = do
    markLocatedAA an fst -- AnnForall
    markAnnotated bndrs
    markLocatedAA an snd -- AnnDot

-- ---------------------------------------------------------------------

instance ExactPrint (HsDerivingClause GhcPs) where
  getAnnotationEntry d@(HsDerivingClause{}) = fromAnn (deriv_clause_ext d)

  exact (HsDerivingClause { deriv_clause_ext      = an
                          , deriv_clause_strategy = dcs
                          , deriv_clause_tys      = dct }) = do
    -- = hsep [ text "deriving"
    --        , pp_strat_before
    --        , pp_dct dct
    --        , pp_strat_after ]
    markEpAnn an AnnDeriving
    exact_strat_before
    markAnnotated dct
    exact_strat_after
      where
        -- -- This complexity is to distinguish between
        -- --    deriving Show
        -- --    deriving (Show)
        -- pp_dct [HsIB { hsib_body = ty }]
        --          = ppr (parenthesizeHsType appPrec ty)
        -- pp_dct _ = parens (interpp'SP dct)

        -- @via@ is unique in that in comes /after/ the class being derived,
        -- so we must special-case it.
        (exact_strat_before, exact_strat_after) =
          case dcs of
            Just v@(L _ ViaStrategy{}) -> (pure (), markAnnotated v)
            _                          -> (mapM_ markAnnotated dcs, pure ())

-- ---------------------------------------------------------------------

instance ExactPrint (DerivStrategy GhcPs) where
  getAnnotationEntry (StockStrategy an)    = fromAnn an
  getAnnotationEntry (AnyclassStrategy an) = fromAnn an
  getAnnotationEntry (NewtypeStrategy an)  = fromAnn an
  getAnnotationEntry (ViaStrategy (XViaStrategyPs an  _)) = fromAnn an

  exact (StockStrategy an)    = markEpAnn an AnnStock
  exact (AnyclassStrategy an) = markEpAnn an AnnAnyclass
  exact (NewtypeStrategy an)  = markEpAnn an AnnNewtype
  exact (ViaStrategy (XViaStrategyPs an ty))
    = markEpAnn an AnnVia >> markAnnotated ty

-- ---------------------------------------------------------------------

instance (ExactPrint a) => ExactPrint (LocatedC a) where
  getAnnotationEntry (L sann _) = fromAnn sann

  exact (L (SrcSpanAnn EpAnnNotUsed _) a) = markAnnotated a
  exact (L (SrcSpanAnn (EpAnn _ (AnnContext ma opens closes) _) _) a) = do
    -- case ma of
    --   Just (UnicodeSyntax, rs) -> markKw' AnnDarrowU rs
    --   Just (NormalSyntax,  rs) -> markKw' AnnDarrow  rs
    --   Nothing -> pure ()
    mapM_ (markKwA AnnOpenP) (sortBy (comparing unsafeGetEpaLoc) opens)
    markAnnotated a
    mapM_ (markKwA AnnCloseP) (sortBy (comparing unsafeGetEpaLoc) closes)
    case ma of
      Just (UnicodeSyntax, r) -> markKwA AnnDarrowU r
      Just (NormalSyntax,  r) -> markKwA AnnDarrow  r
      Nothing -> pure ()

-- ---------------------------------------------------------------------

instance ExactPrint (DerivClauseTys GhcPs) where
  getAnnotationEntry = const NoEntryVal

  exact (DctSingle _ ty) = markAnnotated ty
  exact (DctMulti _ tys) = do
    -- parens (interpp'SP tys)
    markAnnotated tys

-- ---------------------------------------------------------------------

instance ExactPrint (HsSigType GhcPs) where
  getAnnotationEntry = const NoEntryVal

  exact (HsSig _ bndrs ty) = do
    markAnnotated bndrs
    markAnnotated ty

-- ---------------------------------------------------------------------

instance ExactPrint (LocatedN RdrName) where
  getAnnotationEntry (L sann _) = fromAnn sann

  exact (L (SrcSpanAnn EpAnnNotUsed l) n) = do
    p <- getPosP
    debugM $ "LocatedN RdrName:NOANN: (p,l,str)=" ++ show (p,ss2range l, showPprUnsafe n)
    let str = case (showPprUnsafe n) of
              -- TODO: unicode support?
                "forall" -> if spanLength (realSrcSpan l) == 1 then "∀" else "forall"
                s -> s
    printStringAtSs l str
  exact (L (SrcSpanAnn (EpAnn _anchor ann _cs) _ll) n) = do
    case ann of
      NameAnn a o l c t -> do
        markName a o (Just (l,n)) c
        markTrailing t
      NameAnnCommas a o cs c t -> do
        let (kwo,kwc) = adornments a
        markKw (AddEpAnn kwo o)
        forM_ cs (\loc -> markKw (AddEpAnn AnnComma loc))
        markKw (AddEpAnn kwc c)
        markTrailing t
      NameAnnBars a o bs c t -> do
        let (kwo,kwc) = adornments a
        markKw (AddEpAnn kwo o)
        forM_ bs (\loc -> markKw (AddEpAnn AnnVbar loc))
        markKw (AddEpAnn kwc c)
        markTrailing t
      NameAnnOnly a o c t -> do
        markName a o Nothing c
        markTrailing t
      NameAnnRArrow nl t -> do
        markKw (AddEpAnn AnnRarrow nl)
        markTrailing t
      NameAnnQuote q name t -> do
        debugM $ "NameAnnQuote"
        markKw (AddEpAnn AnnSimpleQuote q)
        markAnnotated (L name n)
        markTrailing t
      NameAnnTrailing t -> do
        printStringAdvance (showPprUnsafe n)
        markTrailing t

markName :: NameAdornment
         -> EpaLocation -> Maybe (EpaLocation,RdrName) -> EpaLocation -> EPP ()
markName adorn open mname close = do
  let (kwo,kwc) = adornments adorn
  markKw (AddEpAnn kwo open)
  case mname of
    Nothing -> return ()
    Just (name, a) -> printStringAtAA name (showPprUnsafe a)
  markKw (AddEpAnn kwc close)

adornments :: NameAdornment -> (AnnKeywordId, AnnKeywordId)
adornments NameParens     = (AnnOpenP, AnnCloseP)
adornments NameParensHash = (AnnOpenPH, AnnClosePH)
adornments NameBackquotes = (AnnBackquote, AnnBackquote)
adornments NameSquare     = (AnnOpenS, AnnCloseS)

markTrailing :: [TrailingAnn] -> EPP ()
markTrailing ts = do
  p <- getPosP
  debugM $ "markTrailing:" ++ showPprUnsafe (p,ts)
  mapM_ markKwT (sortBy (comparing (unsafeGetEpaLoc . k)) ts)
  where
    k (AddSemiAnn l) = l
    k (AddCommaAnn l) = l
    k (AddVbarAnn l) = l

-- ---------------------------------------------------------------------

-- based on pp_condecls in Decls.hs
exact_condecls :: EpAnn [AddEpAnn] -> [LConDecl GhcPs] -> EPP ()
exact_condecls an cs
  | gadt_syntax                  -- In GADT syntax
  -- = hang (text "where") 2 (vcat (map ppr cs))
  = do
      -- printStringAdvance "exact_condecls:gadt"
      mapM_ markAnnotated cs
  | otherwise                    -- In H98 syntax
  -- = equals <+> sep (punctuate (text " |") (map ppr cs))
  = do
      -- printStringAdvance "exact_condecls:not gadt"
      markEpAnn an AnnEqual
      mapM_ markAnnotated cs
  where
    gadt_syntax = case cs of
      []                      -> False
      (L _ ConDeclH98{}  : _) -> False
      (L _ ConDeclGADT{} : _) -> True

-- ---------------------------------------------------------------------

instance ExactPrint (ConDecl GhcPs) where
  getAnnotationEntry x@(ConDeclGADT{}) = fromAnn (con_g_ext x)
  getAnnotationEntry x@(ConDeclH98{})  = fromAnn (con_ext x)

-- based on pprConDecl
  exact (ConDeclH98 { con_ext = an
                    , con_name = con
                    , con_forall = has_forall
                    , con_ex_tvs = ex_tvs
                    , con_mb_cxt = mcxt
                    , con_args = args
                    , con_doc = doc }) = do
    -- = sep [ ppr_mbDoc doc
    --       , pprHsForAll (mkHsForAllInvisTele ex_tvs) mcxt
    --       , ppr_details args ]
    mapM_ markAnnotated doc
    when has_forall $ markEpAnn an AnnForall
    mapM_ markAnnotated ex_tvs
    when has_forall $ markEpAnn an AnnDot
    -- exactHsForall (mkHsForAllInvisTele ex_tvs) mcxt
    mapM_ markAnnotated mcxt
    when (isJust mcxt) $ markEpAnn an AnnDarrow

    exact_details args

    -- case args of
    --   InfixCon _ _ -> return ()
    --   _ -> markAnnotated con
    where
    --   -- In ppr_details: let's not print the multiplicities (they are always 1, by
    --   -- definition) as they do not appear in an actual declaration.
      exact_details (InfixCon t1 t2) = do
        markAnnotated t1
        markAnnotated con
        markAnnotated t2
      exact_details (PrefixCon tyargs tys) = do
        markAnnotated con
        markAnnotated tyargs
        markAnnotated tys
      exact_details (RecCon fields) = do
        markAnnotated con
        markAnnotated fields

  -- -----------------------------------

  exact (ConDeclGADT { con_g_ext = an
                     , con_names = cons
                     , con_bndrs = bndrs
                     , con_mb_cxt = mcxt, con_g_args = args
                     , con_res_ty = res_ty, con_doc = doc }) = do
    mapM_ markAnnotated doc
    mapM_ markAnnotated cons
    markEpAnn an AnnDcolon
    annotationsToComments (epAnnAnns an)  [AnnOpenP, AnnCloseP]
    -- when has_forall $ markEpAnn an AnnForall
    markAnnotated bndrs
    -- mapM_ markAnnotated qvars
    -- when has_forall $ markEpAnn an AnnDot
    mapM_ markAnnotated mcxt
    when (isJust mcxt) $ markEpAnn an AnnDarrow
    -- mapM_ markAnnotated args
    case args of
        PrefixConGADT args' -> mapM_ markAnnotated args'
        RecConGADT fields arr -> do
          markAnnotated fields
          markUniToken arr
          -- mapM_ markAnnotated (unLoc fields)
    markAnnotated res_ty
  -- markAST _ (GHC.ConDeclGADT _ lns (GHC.L l forall) qvars mbCxt args typ _) = do
  --   setContext (Set.singleton PrefixOp) $ markListIntercalate lns
  --   mark GHC.AnnDcolon
  --   annotationsToComments [GHC.AnnOpenP]
  --   markLocated (GHC.L l (ResTyGADTHook forall qvars))
  --   markMaybe mbCxt
  --   markHsConDeclDetails False True lns args
  --   markLocated typ
  --   markManyOptional GHC.AnnCloseP
  --   markTrailingSemi

-- pprConDecl (ConDeclGADT { con_names = cons, con_qvars = qvars
--                         , con_mb_cxt = mcxt, con_args = args
--                         , con_res_ty = res_ty, con_doc = doc })
--   = ppr_mbDoc doc <+> ppr_con_names cons <+> dcolon
--     <+> (sep [pprHsForAll (mkHsForAllInvisTele qvars) mcxt,
--               ppr_arrow_chain (get_args args ++ [ppr res_ty]) ])
--   where
--     get_args (PrefixCon args) = map ppr args
--     get_args (RecCon fields)  = [pprConDeclFields (unLoc fields)]
--     get_args (InfixCon {})    = pprPanic "pprConDecl:GADT" (ppr_con_names cons)

--     ppr_arrow_chain (a:as) = sep (a : map (arrow <+>) as)
--     ppr_arrow_chain []     = empty

-- ppr_con_names :: (OutputableBndr a) => [GenLocated l a] -> SDoc
-- ppr_con_names = pprWithCommas (pprPrefixOcc . unLoc)


-- ---------------------------------------------------------------------

-- exactHsForall :: HsForAllTelescope GhcPs
--               -> Maybe (LHsContext GhcPs) -> EPP ()
-- exactHsForall = exactHsForAllExtra False

-- exactHsForAllExtra :: Bool
--                    -> HsForAllTelescope GhcPs
--                    -> Maybe (LHsContext GhcPs) -> EPP ()
-- exactHsForAllExtra show_extra Nothing = return ()
-- exactHsForAllExtra show_extra lctxt@(Just ctxt)
--   | not show_extra = markAnnotated ctxt
--   -- | null ctxt      = char '_' <+> darrow
--   | null ctxt      = return ()
--   | otherwise      = parens (sep (punctuate comma ctxt')) <+> darrow
--   where
--     ctxt' = map ppr ctxt ++ [char '_']

-- ---------------------------------------------------------------------

instance ExactPrint Void where
  getAnnotationEntry = const NoEntryVal
  exact _ = return ()

-- ---------------------------------------------------------------------

instance ExactPrintTVFlag flag => ExactPrint (HsOuterTyVarBndrs flag GhcPs) where
  getAnnotationEntry (HsOuterImplicit _) = NoEntryVal
  getAnnotationEntry (HsOuterExplicit an _) = fromAnn an

  exact (HsOuterImplicit _) = pure ()
  exact (HsOuterExplicit an bndrs) = do
    markLocatedAA an fst -- "forall"
    markAnnotated bndrs
    markLocatedAA an snd -- "."

-- ---------------------------------------------------------------------

instance ExactPrint (ConDeclField GhcPs) where
  getAnnotationEntry f@(ConDeclField{}) = fromAnn (cd_fld_ext f)

  exact (ConDeclField an names ftype mdoc) = do
    markAnnotated names
    markEpAnn an AnnDcolon
    markAnnotated ftype
    mapM_ markAnnotated mdoc

-- ---------------------------------------------------------------------

instance ExactPrint (FieldOcc GhcPs) where
  getAnnotationEntry = const NoEntryVal
  exact (FieldOcc _ n) = markAnnotated n

-- ---------------------------------------------------------------------

instance ExactPrint (AmbiguousFieldOcc GhcPs) where
  getAnnotationEntry = const NoEntryVal
  exact (Unambiguous _ n) = markAnnotated n
  exact (Ambiguous   _ n) = markAnnotated n

-- ---------------------------------------------------------------------

instance (ExactPrint a) => ExactPrint (HsScaled GhcPs a) where
  getAnnotationEntry = const NoEntryVal
  exact (HsScaled arr t) = do
    markAnnotated t
    markArrow arr

-- ---------------------------------------------------------------------

-- instance ExactPrint (LHsContext GhcPs) where
--   getAnnotationEntry (L (SrcSpanAnn ann _) _) = fromAnn ann
--   exact = withPpr

-- ---------------------------------------------------------------------

instance ExactPrint (LocatedP CType) where
  getAnnotationEntry = entryFromLocatedA

  exact (L (SrcSpanAnn EpAnnNotUsed _) ct) = withPpr ct
  exact (L (SrcSpanAnn an _ll)
         (CType stp mh (stct,ct))) = do
    markAnnOpenP an stp "{-# CTYPE"
    case mh of
      Nothing -> return ()
      Just (Header srcH _h) ->
         markLocatedAALS an apr_rest AnnHeader (Just (toSourceTextWithSuffix srcH "" ""))
    markLocatedAALS an apr_rest AnnVal (Just (toSourceTextWithSuffix stct (unpackFS ct) ""))
    markAnnCloseP an

-- ---------------------------------------------------------------------

instance ExactPrint (SourceText, RuleName) where
  -- We end up at the right place from the Located wrapper
  getAnnotationEntry = const NoEntryVal

  exact (st, rn)
    = printStringAdvance (toSourceTextWithSuffix st (unpackFS rn) "")


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

-- instance (ExactPrint body) => ExactPrint (LocatedL body) where
--   getAnnotationEntry = entryFromLocatedA
--   exact (L (SrcSpanAnn an _) b) = do
--     markLocatedMAA an al_open
--     markEpAnnAll an al_rest AnnSemi
--     markAnnotated b
--     markLocatedMAA an al_close

instance ExactPrint (LocatedL [LocatedA (IE GhcPs)]) where
  getAnnotationEntry = entryFromLocatedA

  exact (L (SrcSpanAnn ann _) ies) = do
    debugM $ "LocatedL [LIE"
    markLocatedAAL ann al_rest AnnHiding
    p <- getPosP
    debugM $ "LocatedL [LIE:p=" ++ showPprUnsafe p
    markAnnList True ann (markAnnotated ies)

-- instance (ExactPrint (LocatedA body), (ExactPrint (Match GhcPs (LocatedA body)))) => ExactPrint (LocatedL [LocatedA (Match GhcPs (LocatedA body))]) where
instance (ExactPrint (Match GhcPs (LocatedA body)))
   => ExactPrint (LocatedL [LocatedA (Match GhcPs (LocatedA body))]) where
  getAnnotationEntry = entryFromLocatedA
  exact (L la a) = do
    debugM $ "LocatedL [LMatch"
    -- TODO: markAnnList?
    markEpAnnAll (ann la) al_rest AnnWhere
    markLocatedMAA (ann la) al_open
    markEpAnnAll (ann la) al_rest AnnSemi
    markAnnotated a
    markLocatedMAA (ann la) al_close

{-
-- AZ:TODO: combine with next instance
instance ExactPrint (LocatedL [LocatedA (Match GhcPs (LocatedA (HsExpr GhcPs)))]) where
  getAnnotationEntry = entryFromLocatedA
  exact (L la a) = do
    debugM $ "LocatedL [LMatch"
    -- TODO: markAnnList?
    markEpAnnAll (ann la) al_rest AnnWhere
    markLocatedMAA (ann la) al_open
    markEpAnnAll (ann la) al_rest AnnSemi
    markAnnotated a
    markLocatedMAA (ann la) al_close

instance ExactPrint (LocatedL [LocatedA (Match GhcPs (LocatedA (HsCmd GhcPs)))]) where
  getAnnotationEntry = entryFromLocatedA
  exact (L la a) = do
    debugM $ "LocatedL [LMatch"
    -- TODO: markAnnList?
    markEpAnnAll (ann la) al_rest AnnWhere
    markLocatedMAA (ann la) al_open
    markEpAnnAll (ann la) al_rest AnnSemi
    markAnnotated a
    markLocatedMAA (ann la) al_close
-}

-- instance ExactPrint (LocatedL [ExprLStmt GhcPs]) where
instance ExactPrint (LocatedL [LocatedA (StmtLR GhcPs GhcPs (LocatedA (HsExpr GhcPs)))]) where
  getAnnotationEntry = entryFromLocatedA
  exact (L (SrcSpanAnn an _) stmts) = do
    debugM $ "LocatedL [ExprLStmt"
    markAnnList True an $ do
      -- markLocatedMAA an al_open
      case snocView stmts of
        Just (initStmts, ls@(L _ (LastStmt _ _body _ _))) -> do
          debugM $ "LocatedL [ExprLStmt: snocView"
          markAnnotated ls
          markAnnotated initStmts
        _ -> markAnnotated stmts
        -- x -> error $ "pprDo:ListComp" ++ showAst x
      -- markLocatedMAA an al_close

-- instance ExactPrint (LocatedL [CmdLStmt GhcPs]) where
instance ExactPrint (LocatedL [LocatedA (StmtLR GhcPs GhcPs (LocatedA (HsCmd GhcPs)))]) where
  getAnnotationEntry = entryFromLocatedA
  exact (L (SrcSpanAnn ann _) es) = do
    debugM $ "LocatedL [CmdLStmt"
    markLocatedMAA ann al_open
    mapM_ markAnnotated es
    markLocatedMAA ann al_close

instance ExactPrint (LocatedL [LocatedA (ConDeclField GhcPs)]) where
  getAnnotationEntry = entryFromLocatedA
  exact (L (SrcSpanAnn an _) fs) = do
    debugM $ "LocatedL [LConDeclField"
    markAnnList True an (mapM_ markAnnotated fs) -- AZ:TODO get rid of mapM_

instance ExactPrint (LocatedL (BF.BooleanFormula (LocatedN RdrName))) where
  getAnnotationEntry = entryFromLocatedA
  exact (L (SrcSpanAnn an _) bf) = do
    debugM $ "LocatedL [LBooleanFormula"
    markAnnList True an (markAnnotated bf)

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

  exact (IEVar _ ln) = markAnnotated ln
  exact (IEThingAbs _ thing) = markAnnotated thing
  exact (IEThingAll an thing) = do
    markAnnotated thing
    markEpAnn an AnnOpenP
    markEpAnn an AnnDotdot
    markEpAnn an AnnCloseP

  exact (IEThingWith an thing wc withs) = do
    markAnnotated thing
    markEpAnn an AnnOpenP
    case wc of
      NoIEWildcard -> markAnnotated withs
      IEWildcard pos -> do
        let (bs, as) = splitAt pos withs
        markAnnotated bs
        markEpAnn an AnnDotdot
        markEpAnn an AnnComma
        markAnnotated as
    markEpAnn an AnnCloseP

  exact (IEModuleContents an mn) = do
    markEpAnn an AnnModule
    markAnnotated mn

  -- exact (IEGroup _ _ _)          = NoEntryVal
  -- exact (IEDoc _ _)              = NoEntryVal
  -- exact (IEDocNamed _ _)         = NoEntryVal
  exact x = error $ "missing match for IE:" ++ showAst x

-- ---------------------------------------------------------------------

instance ExactPrint (IEWrappedName RdrName) where
  getAnnotationEntry = const NoEntryVal

  exact (IEName n) = markAnnotated n
  exact (IEPattern r n) = do
    printStringAtAA r "pattern"
    markAnnotated n
  exact (IEType r n) = do
    printStringAtAA r "type"
    markAnnotated n

-- markIEWrapped :: EpAnn -> LIEWrappedName RdrName -> EPP ()
-- markIEWrapped an (L _ (IEName n))
--   = markAnnotated n
-- markIEWrapped an (L _ (IEPattern n))
--   = markEpAnn an AnnPattern >> markAnnotated n
-- markIEWrapped an (L _ (IEType n))
--   = markEpAnn an AnnType    >> markAnnotated n

-- ---------------------------------------------------------------------

-- instance ExactPrint (LocatedA (Pat GhcPs)) where
--   -- getAnnotationEntry (L (SrcSpanAnn ann _) _) = fromAnn ann
--   getAnnotationEntry = entryFromLocatedA
--   exact (L _ a) = do
--     debugM $ "exact:LPat:" ++ showPprUnsafe a
--     markAnnotated a

instance ExactPrint (Pat GhcPs) where
  getAnnotationEntry (WildPat _)              = NoEntryVal
  getAnnotationEntry (VarPat _ _)             = NoEntryVal
  getAnnotationEntry (LazyPat an _)           = fromAnn an
  getAnnotationEntry (AsPat an _ _)           = fromAnn an
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

  exact (WildPat _) = do
    anchor <- getAnchorU
    debugM $ "WildPat:anchor=" ++ show anchor
    printStringAtRs anchor "_"
  exact (VarPat _ n) = do
        -- The parser inserts a placeholder value for a record pun rhs. This must be
        -- filtered.
        let pun_RDR = "pun-right-hand-side"
        when (showPprUnsafe n /= pun_RDR) $ markAnnotated n
  exact (LazyPat an pat) = do
    markEpAnn an AnnTilde
    markAnnotated pat
  exact (AsPat an n pat) = do
    markAnnotated n
    markEpAnn an AnnAt
    markAnnotated pat
  exact (ParPat _an lpar pat rpar) = do
    markToken lpar
    markAnnotated pat
    markToken rpar

  exact (BangPat an pat) = do
    markEpAnn an AnnBang
    markAnnotated pat

  exact (ListPat an pats) = markAnnList True an (markAnnotated pats)

  exact (TuplePat an pats boxity) = do
    case boxity of
      Boxed   -> markEpAnn an AnnOpenP
      Unboxed -> markEpAnn an AnnOpenPH
    markAnnotated pats
    case boxity of
      Boxed   -> markEpAnn an AnnCloseP
      Unboxed -> markEpAnn an AnnClosePH

  exact (SumPat an pat _alt _arity) = do
    markLocatedAAL an sumPatParens AnnOpenPH
    markAnnKwAll an sumPatVbarsBefore AnnVbar
    markAnnotated pat
    markAnnKwAll an sumPatVbarsAfter AnnVbar
    markLocatedAAL an sumPatParens AnnClosePH

  -- | ConPat an con args)
  exact (ConPat an con details) = exactUserCon an con details
  exact (ViewPat an expr pat) = do
    markAnnotated expr
    markEpAnn an AnnRarrow
    markAnnotated pat
  exact (SplicePat _ splice) = markAnnotated splice
  exact (LitPat _ lit) = printStringAdvance (hsLit2String lit)
  exact (NPat an ol mn _) = do
    when (isJust mn) $ markEpAnn an AnnMinus
    markAnnotated ol

  -- | NPlusKPat an n lit1 lit2 _ _)
  exact (NPlusKPat an n k _lit2 _ _) = do
    markAnnotated n
    printStringAtAnn an id "+"
    markAnnotated k


  exact (SigPat an pat sig) = do
    markAnnotated pat
    markEpAnn an AnnDcolon
    markAnnotated sig
  -- exact x = error $ "missing match for Pat:" ++ showAst x

-- ---------------------------------------------------------------------

instance ExactPrint (HsPatSigType GhcPs) where
  getAnnotationEntry = const NoEntryVal

  exact (HsPS an ty) = do
    markAnnKw an id AnnAt
    markAnnotated ty

-- ---------------------------------------------------------------------

instance ExactPrint (HsOverLit GhcPs) where
  getAnnotationEntry = const NoEntryVal

  exact ol =
    let str = case ol_val ol of
                HsIntegral   (IL src _ _) -> src
                HsFractional (FL{ fl_text = src }) -> src
                HsIsString src _ -> src
    in
      case str of
        SourceText s -> printStringAdvance s
        NoSourceText -> return ()

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
    -- (XLit x) -> error $ "got XLit for:" ++ showPprUnsafe x

toSourceTextWithSuffix :: (Show a) => SourceText -> a -> String -> String
toSourceTextWithSuffix (NoSourceText)    alt suffix = show alt ++ suffix
toSourceTextWithSuffix (SourceText txt) _alt suffix = txt ++ suffix

sourceTextToString :: SourceText -> String -> String
sourceTextToString NoSourceText alt   = alt
sourceTextToString (SourceText txt) _ = txt

-- ---------------------------------------------------------------------

exactUserCon :: (ExactPrint con) => EpAnn [AddEpAnn] -> con -> HsConPatDetails GhcPs -> EPP ()
exactUserCon _  c (InfixCon p1 p2) = markAnnotated p1 >> markAnnotated c >> markAnnotated p2
exactUserCon an c details          = do
  markAnnotated c
  markEpAnn an AnnOpenC
  exactConArgs details
  markEpAnn an AnnCloseC


exactConArgs ::HsConPatDetails GhcPs -> EPP ()
exactConArgs (PrefixCon tyargs pats) = markAnnotated tyargs >> markAnnotated pats
exactConArgs (InfixCon p1 p2) = markAnnotated p1 >> markAnnotated p2
exactConArgs (RecCon rpats)   = markAnnotated rpats

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
        `debug` ("printStringAtLsDelta:(pos,s):" ++ show (undelta p cl colOffset,s))
    else return () `debug` ("printStringAtLsDelta:bad delta for (mc,s):" ++ show (cl,s))

-- ---------------------------------------------------------------------

isGoodDeltaWithOffset :: DeltaPos -> LayoutStartCol -> Bool
isGoodDeltaWithOffset dp colOffset = isGoodDelta (deltaPos l c)
  where (l,c) = undelta (0,0) dp colOffset

printQueuedComment :: (Monad m, Monoid w) => RealSrcSpan -> Comment -> DeltaPos -> EP w m ()
printQueuedComment loc Comment{commentContents} dp = do
  p <- getPosP
  colOffset <- getLayoutOffsetP
  let (dr,dc) = undelta (0,0) dp colOffset
  -- do not lose comments against the left margin
  when (isGoodDelta (deltaPos dr (max 0 dc))) $ do
    printCommentAt (undelta p dp colOffset) commentContents
    setPriorEndASTD False loc
  p' <- getPosP
  debugM $ "printQueuedComment: (p,p',dp,colOffset,undelta)=" ++ show (p,p',dp,colOffset,undelta p dp colOffset)

{-
-- Print version
printQueuedComment :: (Monad m, Monoid w) => Comment -> DeltaPos -> EP w m ()
printQueuedComment Comment{commentContents} dp = do
  p <- getPos
  colOffset <- getLayoutOffset
  let (dr,dc) = undelta (0,0) dp colOffset
  -- do not lose comments against the left margin
  when (isGoodDelta (DP (dr,max 0 dc))) $
    printCommentAt (undelta p dp colOffset) commentContents

-}

-- ---------------------------------------------------------------------

-- withContext :: (Monad m, Monoid w)
--             => [(KeywordId, DeltaPos)]
--             -> Annotation
--             -> EP w m a -> EP w m a
-- withContext kds an x = withKds kds (withOffset an x)

-- ---------------------------------------------------------------------
--
-- | Given an annotation associated with a specific SrcSpan,
-- determines a new offset relative to the previous offset
--
withOffset :: (Monad m, Monoid w) => Annotation -> (EP w m a -> EP w m a)
withOffset a =
  local (\s -> s { epAnn = a })

------------------------------------------------------------------------

setLayoutBoth :: (Monad m, Monoid w) => EP w m () -> EP w m ()
setLayoutBoth k = do
  oldLHS <- gets dLHS
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
setLayoutTopLevelP :: (Monad m, Monoid w) => EP w m () -> EP w m ()
setLayoutTopLevelP k = do
  debugM $ "setLayoutTopLevelP entered"
  oldAnchorOffset <- getLayoutOffsetP
  modify (\a -> a { pMarkLayout = False
                  , pLHS = 0} )
  k
  debugM $ "setLayoutTopLevelP:resetting"
  setLayoutOffsetP oldAnchorOffset

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

setPriorEndD :: (Monad m, Monoid w) => Pos -> EP w m ()
setPriorEndD pe = do
  -- setLayoutStartIfNeededD (snd pe)
  setPriorEndNoLayoutD pe

setPriorEndNoLayoutD :: (Monad m, Monoid w) => Pos -> EP w m ()
setPriorEndNoLayoutD pe = do
  debugM $ "setPriorEndNoLayout:pe=" ++ show pe
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

setAnchorU :: (Monad m, Monoid w) => RealSrcSpan -> EP w m ()
setAnchorU rss = do
  debugM $ "setAnchorU:" ++ show (rs2range rss)
  modify (\s -> s { uAnchorSpan = rss })

getUnallocatedComments :: (Monad m, Monoid w) => EP w m [Comment]
getUnallocatedComments = gets epComments

putUnallocatedComments :: (Monad m, Monoid w) => [Comment] -> EP w m ()
putUnallocatedComments cs = modify (\s -> s { epComments = cs } )

getLayoutOffsetP :: (Monad m, Monoid w) => EP w m LayoutStartCol
getLayoutOffsetP = gets pLHS

setLayoutOffsetP :: (Monad m, Monoid w) => LayoutStartCol -> EP w m ()
setLayoutOffsetP c = do
  debugM $ "setLayoutOffsetP:" ++ show c
  modify (\s -> s { pLHS = c })


-- ---------------------------------------------------------------------
-------------------------------------------------------------------------
-- |First move to the given location, then call exactP
-- exactPC :: (Data ast, Monad m, Monoid w) => GHC.Located ast -> EP w m a -> EP w m a
-- exactPC :: (Data ast, Data (GHC.SrcSpanLess ast), GHC.HasSrcSpan ast, Monad m, Monoid w)
-- exactPC :: (Data ast, Monad m, Monoid w) => GHC.Located ast -> EP w m a -> EP w m a
-- exactPC ast action =
--     do
--       return () `debug` ("exactPC entered for:" ++ show (mkAnnKey ast))
--       ma <- getAndRemoveAnnotation ast
--       let an@Ann{ annEntryDelta=edp
--                 , annPriorComments=comments
--                 , annFollowingComments=fcomments
--                 , annsDP=kds
--                 } = fromMaybe annNone ma
--       PrintOptions{epAstPrint} <- ask
--       r <- withContext kds an
--        (mapM_ (uncurry printQueuedComment) comments
--        >> advance edp
--        >> censorM (epAstPrint ast) action
--        <* mapM_ (uncurry printQueuedComment) fcomments)
--       return r `debug` ("leaving exactPCfor:" ++ show (mkAnnKey ast))

-- censorM :: (Monoid w, Monad m) => (w -> m w) -> EP w m a -> EP w m a
-- censorM f m = passM (liftM (\x -> (x,f)) m)

-- passM :: (Monad m) => EP w m (a, w -> m w) -> EP w m a
-- passM m = RWST $ \r s -> do
--       ~((a, f),s', EPWriter w) <- runRWST m r s
--       w' <- f w
--       return (a, s', EPWriter w')

advance :: (Monad m, Monoid w) => DeltaPos -> EP w m ()
advance dp = do
  p <- getPosP
  colOffset <- getLayoutOffsetP
  debugM $ "advance:(p,dp,colOffset,ws)=" ++ show (p,dp,colOffset,undelta p dp colOffset)
  printWhitespace (undelta p dp colOffset)

{-
Version from Print.advance
advance :: (Monad m, Monoid w) => DeltaPos -> EP w m ()
advance cl = do
  p <- getPos
  colOffset <- getLayoutOffset
  printWhitespace (undelta p cl colOffset)
-}

-- ---------------------------------------------------------------------

adjustDeltaForOffsetM :: DeltaPos -> EPP DeltaPos
adjustDeltaForOffsetM dp = do
  colOffset <- gets dLHS
  return (adjustDeltaForOffset 0 colOffset dp)

-- ---------------------------------------------------------------------
-- Printing functions

printString :: (Monad m, Monoid w) => Bool -> String -> EP w m ()
printString layout str = do
  EPState{epPos = (_,c), pMarkLayout} <- get
  PrintOptions{epTokenPrint, epWhitespacePrint} <- ask
  when (pMarkLayout && layout) $ do
    debugM $ "printString: setting pLHS to " ++ show c
    modify (\s -> s { pLHS = LayoutStartCol c, pMarkLayout = False } )

  -- Advance position, taking care of any newlines in the string
  let strDP = dpFromString str
      cr = getDeltaLine strDP
  p <- getPosP
  colOffset <- getLayoutOffsetP
  -- debugM $ "printString:(p,colOffset,strDP,cr)="  ++ show (p,colOffset,strDP,cr)
  if cr == 0
    then setPosP (undelta p strDP colOffset)
    else setPosP (undelta p strDP 1)

  -- Debug stuff
  -- pp <- getPosP
  -- debugM $ "printString: (p,pp,str)" ++ show (p,pp,str)
  -- Debug end

  --
  if not layout && c == 0
    then lift (epWhitespacePrint str) >>= \s -> tell EPWriter { output = s}
    else lift (epTokenPrint      str) >>= \s -> tell EPWriter { output = s}


{-

-- Print.printString
printString :: (Monad m, Monoid w) => Bool -> String -> EP w m ()
printString layout str = do
  EPState{epPos = (_,c), epMarkLayout} <- get
  PrintOptions{epTokenPrint, epWhitespacePrint} <- ask
  when (epMarkLayout && layout) $
    modify (\s -> s { epLHS = LayoutStartCol c, epMarkLayout = False } )

  -- Advance position, taking care of any newlines in the string
  let strDP@(DP (cr,_cc)) = dpFromString str
  p <- getPos
  colOffset <- getLayoutOffset
  if cr == 0
    then setPos (undelta p strDP colOffset)
    else setPos (undelta p strDP 1)

  --
  if not layout && c == 0
    then lift (epWhitespacePrint str) >>= \s -> tell EPWriter { output = s}
    else lift (epTokenPrint      str) >>= \s -> tell EPWriter { output = s}

-}

--------------------------------------------------------

printStringAdvance :: String -> EPP ()
printStringAdvance str = do
  ss <- getAnchorU
  printStringAtKw' ss str

--------------------------------------------------------

newLine :: (Monad m, Monoid w) => EP w m ()
newLine = do
    (l,_) <- getPosP
    printString False "\n"
    setPosP (l+1,1)

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
