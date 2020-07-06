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
import GHC.Types.ForeignCall
import GHC.Types.SrcLoc
import GHC.Utils.Outputable hiding ( (<>) )
import GHC.Driver.Ppr
import GHC.Utils.Misc
import GHC.Utils.Panic

import Control.Monad.Identity
import Control.Monad.RWS
import Data.Data ( Data )
import Data.Foldable
import Data.List ( partition, intercalate, sort, sortBy)
import Data.Maybe (fromMaybe, isJust, maybeToList)
-- import Data.Ord (comparing)

import qualified Data.Map as Map
import qualified Data.Set as Set

-- import qualified GHC

import Lookup
import Utils
import Types

-- import Debug.Trace

-- ---------------------------------------------------------------------

exactPrint :: ExactPrint ast => Located ast -> ApiAnns -> String
exactPrint ast anns = runIdentity (runEP anns stringOptions (exact ast))

type EP w m a = RWST (PrintOptions m w) (EPWriter w) EPState m a
type EPP a = EP String Identity a

runEP :: ApiAnns -> PrintOptions Identity String
      -> Annotated () -> Identity String
runEP anns epReader action =
  fmap (output . snd) .
    (\next -> execRWST next epReader (defaultEPState anns))
    . xx $ action

xx :: Annotated () -> EP String Identity ()
-- xx :: Annotated() -> RWST (PrintOptions m w) (EPWriter w) EPState m ()
xx = id

-- ---------------------------------------------------------------------

defaultEPState :: ApiAnns -> EPState
defaultEPState as = EPState
             { epPos    = (1,1)
             , epAnns   = Map.empty
             , epApiAnns = as
             , epAnnKds = []
             , epLHS    = 0
             , epMarkLayout = False
             , priorEndPosition = (1,1)
             , epComments = rogueComments as
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
            , epContext :: !AstContextSet
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
             , epContext = defaultACS
             }

-- | Options which can be used to print as a normal String.
stringOptions :: PrintOptions Identity String
stringOptions = printOptions (\_ b -> return b) return return NormalLayout

data EPWriter a = EPWriter
              { output :: !a }

instance Monoid w => Semigroup (EPWriter w) where
  (<>) = mappend

instance Monoid w => Monoid (EPWriter w) where
  mempty = EPWriter mempty
  (EPWriter a) `mappend` (EPWriter b) = EPWriter (a <> b)

data EPState = EPState
             { epPos    :: !Pos -- ^ Current output position
             , epAnns   :: !Anns
             , epApiAnns :: !ApiAnns
             , epAnnKds :: ![[(KeywordId, DeltaPos)]] -- MP: Could this be moved to the local statE w mith suitable refactoring?
             , epMarkLayout :: Bool
             , epLHS :: LayoutStartCol
             , priorEndPosition :: !Pos -- ^ Position reached when
                                        -- processing the last element
             , epComments :: ![Comment]
             }

-- ---------------------------------------------------------------------

-- AZ:TODO: this can just be a function :: (ApiAnn' a) -> Entry
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

data Entry = Entry RealSrcSpan [RealLocated AnnotationComment]
           | NoEntryVal

instance (HasEntry (ApiAnn' an)) =>  HasEntry (SrcSpanAnn' (ApiAnn' an)) where
  fromAnn (SrcSpanAnn ApiAnnNotUsed ss) = Entry (realSrcSpan ss) []
  fromAnn (SrcSpanAnn an _) = fromAnn an

instance HasEntry (ApiAnn' a) where
  fromAnn (ApiAnn anchor _ cs) = Entry anchor cs
  fromAnn ApiAnnNotUsed = NoEntryVal

-- | "Enter" an annotation, by using the associated 'anchor' field as
-- the new reference point for calculating all DeltaPos positions.
enterAnn :: (ExactPrint a) => Entry -> a -> Annotated ()
enterAnn NoEntryVal a = do
  p <- getPos
  debugM $ "enterAnn:NO ANN:p =" ++ show p
  exact a
enterAnn (Entry anchor cs) a = do
  addCommentsA cs
  printComments anchor
  p <- getPos
  debugM $ "enterAnn:(anchor(pos),p)=" ++ show (ss2pos(anchor),p)
  -- do all the machinery of advancing to the anchor, with a local etc
  -- modelled on exactpc (which is normally called via withast

  -- First thing is to calculate the entry DeltaPos.  This is based on
  -- the current position, and the anchor.
  -- off <- gets apLayoutStart
  off <- gets epLHS
  priorEndAfterComments <- getPos
  let ss = anchor
  let edp = adjustDeltaForOffset
              -- Use the propagated offset if one is set
              -- Note that we need to use the new offset if it has
              -- changed.
              off (ss2delta priorEndAfterComments ss)

  let
    st = annNone { annEntryDelta = edp }
  withOffset st (advance edp >> exact a)

-- ---------------------------------------------------------------------

addCommentsA :: [RealLocated AnnotationComment] -> EPP ()
addCommentsA csNew = addComments (map tokComment csNew)
  -- cs <- getUnallocatedComments
  -- -- AZ:TODO: sortedlist?
  -- putUnallocatedComments (sort $ (map tokComment csNew) ++ cs)

addComments :: [Comment] -> EPP ()
addComments csNew = do
  cs <- getUnallocatedComments
  let cmp (Comment _ l1 _) (Comment _ l2 _) = compare l1 l2
  -- AZ:TODO: sortedlist?
  putUnallocatedComments (sortBy cmp $ csNew ++ cs)

-- ---------------------------------------------------------------------

-- |In order to interleave annotations into the stream, we turn them into
-- comments.
annotationsToComments :: [AddApiAnn] -> [AnnKeywordId] -> EPP ()
annotationsToComments ans kws = do
  let
    getSpans _ [] = []
    getSpans k1 (AddApiAnn k2 ss:as)
      | k1 == k2 = ss : getSpans k1 as
      | otherwise = getSpans k1 as
    doOne :: AnnKeywordId -> EPP [Comment]
    doOne kw = do
      let spans =getSpans kw ans
      return $ map (mkKWComment kw ) spans
    -- TODO:AZ make sure these are sorted/merged properly when the invariant for
    -- allocateComments is re-established.
  newComments <- mapM doOne kws
  addComments (concat newComments)


sr :: RealSrcSpan -> SrcSpan
sr s = RealSrcSpan s Nothing

-- ---------------------------------------------------------------------

-- Temporary function to simply reproduce the "normal" pretty printer output
withPpr :: (Outputable a) => a -> Annotated ()
withPpr a = printString False (showPprUnsafe a)

-- ---------------------------------------------------------------------
-- Modeled on Outputable

-- | An AST fragment with an annotation must be able to return the
-- requirements for nesting another one, captured in an 'Entry', and
-- to be able to use the rest of the exactprint machinery to print the
-- element.  In the analogy to Outputable, 'exact' plays the role of
-- 'ppr'.
class ExactPrint a where
  getAnnotationEntry :: a -> Entry
  exact :: a -> Annotated ()

-- ---------------------------------------------------------------------

-- | Bare Located elements are simply stripped off without further
-- processing.
instance (ExactPrint a) => ExactPrint (Located a) where
  getAnnotationEntry (L l _) = Entry (realSrcSpan l) []
  exact (L _ a) = markAnnotated a

instance (ExactPrint a) => ExactPrint (LocatedA a) where
  getAnnotationEntry = entryFromLocatedA
  exact (L la a) = do
    markAnnotated a
    markALocatedA (ann la)

instance (ExactPrint a) => ExactPrint [a] where
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

  exact hsmod@(HsModule ApiAnnNotUsed _ _ _ _ _ _ _) = withPpr hsmod
  exact (HsModule an _lo mmn mexports imports decls mdeprec mbDoc) = do

    markAnnotated mbDoc

    case mmn of
      Nothing -> return ()
      Just (L ln mn) -> do
        markApiAnn' an am_main AnnModule
        -- debugM $ "HsModule name: (ss,ln)=" ++ show (ss2pos ss,ss2pos (realSrcSpan ln))
        printStringAtSs ln (moduleNameString mn)

        -- forM_ mdeprec markLocated
        markAnnotated mdeprec

        markAnnotated mexports

        markApiAnn' an am_main AnnWhere
        -- markApiAnn (am_main anns) AnnWhere

    mapM_ markAddApiAnn (al_open $ am_decls $ anns an)

    -- markOptional GHC.AnnOpenC -- Possible '{'
    -- markManyOptional GHC.AnnSemi -- possible leading semis
    -- setContextLevel (Set.singleton TopLevel) 2 $ markListWithLayout imports
    markListWithLayout imports

    -- setContextLevel (Set.singleton TopLevel) 2 $ markListWithLayout decls
    markListWithLayout decls

    mapM_ markAddApiAnn (al_close $ am_decls $ anns an)
    -- markOptional GHC.AnnCloseC -- Possible '}'

    -- markEOF
    eof <- getEofPos
    printStringAtKw' eof ""


-- ---------------------------------------------------------------------
-- Start of utility functions
-- ---------------------------------------------------------------------

printSourceText :: SourceText -> String -> EPP ()
printSourceText NoSourceText txt   =  printString False txt
printSourceText (SourceText txt) _ =  printString False txt

-- ---------------------------------------------------------------------

printStringAtSs :: SrcSpan -> String -> EPP ()
printStringAtSs ss str = printStringAtKw' (realSrcSpan ss) str

-- ---------------------------------------------------------------------

-- printStringAtKw :: ApiAnn' ann -> AnnKeywordId -> String -> EPP ()
-- printStringAtKw ApiAnnNotUsed _ str = printString True str
-- printStringAtKw (ApiAnn anchor anns _cs) kw str = do
--   case find (\(AddApiAnn k _) -> k == kw) anns of
--     Nothing -> printString True str
--     Just (AddApiAnn _ ss) -> printStringAtKw' ss str

-- AZ:TODO get rid of this
printStringAtMkw :: Maybe RealSrcSpan -> String -> EPP ()
printStringAtMkw (Just r) s = printStringAtKw' r s
printStringAtMkw Nothing s = printStringAtLsDelta [] (DP (0,1)) s

printStringAtKw' :: RealSrcSpan -> String -> EPP ()
printStringAtKw' ss str = do
  printComments ss
  dp <- nextDP ss
  p <- getPos
  debugM $ "printStringAtKw': (dp,p) = " ++ show (dp,p)
  printStringAtLsDelta [] dp str

-- ---------------------------------------------------------------------

markExternalSourceText :: SrcSpan -> SourceText -> String -> EPP ()
markExternalSourceText l NoSourceText txt   = printStringAtKw' (realSrcSpan l) txt
markExternalSourceText l (SourceText txt) _ = printStringAtKw' (realSrcSpan l) txt

-- ---------------------------------------------------------------------

markAddApiAnn :: AddApiAnn -> EPP ()
markAddApiAnn a@(AddApiAnn kw _) = mark [a] kw

markLocatedMAA :: ApiAnn' a -> (a -> Maybe AddApiAnn) -> EPP ()
markLocatedMAA ApiAnnNotUsed  _  = return ()
markLocatedMAA (ApiAnn _ a _) f =
  case f a of
    Nothing -> return ()
    Just aa -> markAddApiAnn aa

markLocatedAA :: ApiAnn' a -> (a -> AddApiAnn) -> EPP ()
markLocatedAA ApiAnnNotUsed  _  = return ()
markLocatedAA (ApiAnn _ a _) f = markKw (f a)
  where
    AddApiAnn kw _ = f a

markLocatedAAL :: ApiAnn' a -> (a -> [AddApiAnn]) -> AnnKeywordId -> EPP ()
markLocatedAAL ApiAnnNotUsed  _ _ = return ()
markLocatedAAL (ApiAnn _ a _) f kw = go (f a)
  where
    go [] = return ()
    go (a@(AddApiAnn kw' _):as)
      | kw' == kw = mark [a] kw
      | otherwise = go as
    go (_:as) = go as

markLocatedAALS :: ApiAnn' a -> (a -> [AddApiAnn]) -> AnnKeywordId -> Maybe String -> EPP ()
markLocatedAALS an f kw Nothing = markLocatedAAL an f kw
markLocatedAALS ApiAnnNotUsed  _ _ _ = return ()
markLocatedAALS (ApiAnn _ a _) f kw (Just str) = go (f a)
  where
    go [] = return ()
    go (a@(AddApiAnn kw' rs):as)
      | kw' == kw = printStringAtKw' rs str
      | otherwise = go as

-- ---------------------------------------------------------------------

-- markLocatedMaybe :: a -> (a -> Maybe RealSrcSpan) -> AnnKeywordId -> EPP ()
-- markLocatedMaybe a

-- ---------------------------------------------------------------------

markArrow :: ApiAnn' TrailingAnn -> (HsArrow GhcPs) -> EPP ()
markArrow ApiAnnNotUsed _ = pure ()
markArrow an mult = markKwT (anns an)
  -- = case mult of
  --     HsLinearArrow ->  markApiAnn an AnnLolly
  --     HsUnrestrictedArrow -> markApiAnn an AnnRarrow
  --     HsExplicitMult p -> do
  --       printString False "#"
  --       markAnnotated p
  --       markApiAnn an AnnRarrow

-- ---------------------------------------------------------------------

markAnnCloseP :: ApiAnn' AnnPragma -> EPP ()
markAnnCloseP an = markLocatedAALS an (pure . apr_close) AnnClose (Just "#-}")
markAnnCloseP an = markLocatedAALS an (pure . apr_close) AnnClose (Just "#-}")

markAnnOpenP :: ApiAnn' AnnPragma -> SourceText -> String -> EPP ()
markAnnOpenP an NoSourceText txt   = markLocatedAALS an (pure . apr_open) AnnOpen (Just txt)
markAnnOpenP an (SourceText txt) _ = markLocatedAALS an (pure . apr_open) AnnOpen (Just txt)

markAnnOpen :: ApiAnn -> SourceText -> String -> EPP ()
markAnnOpen an NoSourceText txt   = markLocatedAALS an id AnnOpen (Just txt)
markAnnOpen an (SourceText txt) _ = markLocatedAALS an id AnnOpen (Just txt)

markAnnOpen' :: Maybe RealSrcSpan -> SourceText -> String -> EPP ()
markAnnOpen' ms NoSourceText txt   = printStringAtMkw ms txt
markAnnOpen' ms (SourceText txt) _ = printStringAtMkw ms txt

-- ---------------------------------------------------------------------

markOpeningParen, markClosingParen :: ApiAnn' AnnParen -> EPP ()
markOpeningParen an = markParen an fst
markClosingParen an = markParen an snd

markParen :: ApiAnn' AnnParen -> (forall a. (a,a) -> a) -> EPP ()
markParen ApiAnnNotUsed _ = return ()
markParen (ApiAnn _ (AnnParen pt o c) _) f = markKw' (f $ kw pt) (f (o, c))
  where
    kw AnnParens       = (AnnOpenP,  AnnCloseP)
    kw AnnParensHash   = (AnnOpenPH, AnnClosePH)
    kw AnnParensSquare = (AnnOpenS, AnnCloseS)


markAnnKw :: ApiAnn' a -> (a -> RealSrcSpan) -> AnnKeywordId -> EPP ()
markAnnKw ApiAnnNotUsed  _ _  = return ()
markAnnKw (ApiAnn _ a _) f kw = markKw' kw (f a)

markAnnKwAll :: ApiAnn' a -> (a -> [RealSrcSpan]) -> AnnKeywordId -> EPP ()
markAnnKwAll ApiAnnNotUsed  _ _  = return ()
markAnnKwAll (ApiAnn _ a _) f kw = mapM_ (markKw' kw) (sort (f a))

markAnnKwM :: ApiAnn' a -> (a -> Maybe RealSrcSpan) -> AnnKeywordId -> EPP ()
markAnnKwM ApiAnnNotUsed  _ _ = return ()
markAnnKwM (ApiAnn _ a _) f kw = go (f a)
  where
    go Nothing = return ()
    go (Just s) = markKw' kw s

markALocatedA :: ApiAnn' AnnListItem -> EPP ()
markALocatedA ApiAnnNotUsed  = return ()
markALocatedA (ApiAnn _ a _) = markTrailing (lann_trailing a)

markApiAnn :: ApiAnn -> AnnKeywordId -> EPP ()
markApiAnn ApiAnnNotUsed _ = return ()
markApiAnn (ApiAnn _ a _) kw = mark a kw

markApiAnn' :: ApiAnn' ann -> (ann -> [AddApiAnn]) -> AnnKeywordId -> EPP ()
markApiAnn' ApiAnnNotUsed _ _ = return ()
markApiAnn' (ApiAnn _ a _) f kw = mark (f a) kw

markApiAnnAll :: ApiAnn' ann -> (ann -> [AddApiAnn]) -> AnnKeywordId -> EPP ()
markApiAnnAll ApiAnnNotUsed _ _ = return ()
markApiAnnAll (ApiAnn _ a _) f kw = mapM_ markKw (sort anns)
  where
    anns = filter (\(AddApiAnn ka _) -> ka == kw) (f a)

mark :: [AddApiAnn] -> AnnKeywordId -> EPP ()
mark anns kw = do
  case find (\(AddApiAnn k _) -> k == kw) anns of
    Just aa -> markKw aa
    Nothing -> case find (\(AddApiAnn k _) -> k == (unicodeAnn kw)) anns of
      Just aau -> markKw aau
      Nothing -> return ()

markKwT :: TrailingAnn -> EPP ()
markKwT (AddSemiAnn ss)    = markKw' AnnSemi ss
markKwT (AddCommaAnn ss)   = markKw' AnnComma ss
markKwT (AddVbarAnn ss)    = markKw' AnnVbar ss
markKwT (AddRarrowAnn ss)  = markKw' AnnRarrow ss
markKwT (AddRarrowAnnU ss) = markKw' AnnRarrowU ss
-- markKwT (AddLollyAnn ss)   = markKw' AnnLolly ss
markKwT (AddLollyAnnU ss)  = markKw' AnnLollyU ss

markKw :: AddApiAnn -> EPP ()
markKw (AddApiAnn kw ss) = markKw' kw ss

-- | This should be the main driver of the process, managing comments
markKw' :: AnnKeywordId -> RealSrcSpan -> EPP ()
markKw' kw ss = do
  p' <- getPos
  printComments ss
  dp <- nextDP ss
  p <- getPos
  debugM $ "markKw: (dp,p,p') = " ++ show (dp,p,p')
  printStringAtLsDelta [] dp (keywordToString (G kw))

-- ---------------------------------------------------------------------

markAnnList :: ApiAnn' AnnList -> EPP () -> EPP ()
markAnnList ApiAnnNotUsed action = action
markAnnList an@(ApiAnn _ ann _) action = do
  p <- getPos
  debugM $ "markAnnList : " ++ showPprUnsafe (p, an)
  markLocatedMAA an al_open
  action
  markLocatedMAA an al_close
  debugM $ "markAnnList: calling markTrailing with:" ++ showPprUnsafe (al_trailing ann)
  markTrailing (al_trailing ann)

-- ---------------------------------------------------------------------

-- printTrailingComments :: EPP ()
-- printTrailingComments = do
--   cs <- getUnallocatedComments
--   mapM_ printOneComment cs

-- ---------------------------------------------------------------------

printComments :: RealSrcSpan -> EPP ()
printComments ss = do
  cs <- commentAllocation ss
  debugM $ "printComments: (ss,comment locations): " ++ showPprUnsafe (ss,map commentIdentifier cs)
  mapM_ printOneComment cs

-- ---------------------------------------------------------------------

printOneComment :: Comment -> EPP ()
printOneComment c@(Comment _str loc _mo) = do
  p <- getPos
  let dp = ss2delta p loc
  printQueuedComment c dp

-- ---------------------------------------------------------------------

commentAllocation :: RealSrcSpan -> EPP [Comment]
commentAllocation ss = do
  cs <- getUnallocatedComments
  let (earlier,later) = partition (\(Comment _str loc _mo) -> loc <= ss) cs
  putUnallocatedComments later
  return earlier

-- ---------------------------------------------------------------------

-- commentAllocation :: (Comment -> Bool)
--                   -> EPP a
-- commentAllocation p = do
--   cs <- getUnallocatedComments
--   let (allocated,cs') = allocateComments p cs
--   putUnallocatedComments cs'
--   mapM makeDeltaComment (sortBy (comparing commentIdentifier) allocated)

-- makeDeltaComment :: Comment -> EPP (Comment, DeltaPos)
-- makeDeltaComment c = do
--   let pa = commentIdentifier c
--   pe <- getPriorEnd
--   let p = ss2delta pe pa
--   p' <- adjustDeltaForOffsetM p
--   setPriorEnd (ss2posEnd pa)
--   return (c, p')


-- ---------------------------------------------------------------------

nextDP :: RealSrcSpan -> EPP DeltaPos
nextDP ss = do
  p <- getPos
  return $ pos2delta p (ss2pos ss)

-- ---------------------------------------------------------------------

markListWithLayout :: ExactPrint (LocatedA ast) => [LocatedA ast] -> EPP ()
markListWithLayout ls =
  setLayout $ markListA ls

-- ---------------------------------------------------------------------

markList :: ExactPrint ast => [Located ast] -> EPP ()
markList ls =
  -- setContext (Set.singleton NoPrecedingSpace)
  --  $ markListWithContexts' listContexts' ls
  mapM_ markAnnotated ls

markListA :: ExactPrint (LocatedA ast) => [LocatedA ast] -> EPP ()
markListA ls =
  -- setContext (Set.singleton NoPrecedingSpace)
  --  $ markListWithContexts' listContexts' ls
  mapM_ markAnnotated ls

-- ---------------------------------------------------------------------


------------------------------
{-
instance Annotate (GHC.HsModule GHC.GhcPs) where
  markAST _ (GHC.HsModule mmn mexp imps decs mdepr _haddock) = do

    case mmn of
      Nothing -> return ()
      Just (GHC.L ln mn) -> do
        mark GHC.AnnModule
        markExternal ln GHC.AnnVal (GHC.moduleNameString mn)

        forM_ mdepr markLocated
        forM_ mexp markLocated

        mark GHC.AnnWhere

    markOptional GHC.AnnOpenC -- Possible '{'
    markManyOptional GHC.AnnSemi -- possible leading semis
    setContextLevel (Set.singleton TopLevel) 2 $ markListWithLayout imps

    setContextLevel (Set.singleton TopLevel) 2 $ markListWithLayout decs

    markOptional GHC.AnnCloseC -- Possible '}'

    markEOF
-}
------------------------------

    -- exact (HsModule anns Nothing _ imports decls _ mbDoc)
    --   = pp_mb mbDoc $$ pp_nonnull imports
    --                 $$ pp_nonnull decls

    -- exact (HsModule _ (Just name) exports imports decls deprec mbDoc)
    --   = vcat [
    --         pp_mb mbDoc,
    --         case exports of
    --           Nothing -> pp_header (text "where")
    --           Just es -> vcat [
    --                        pp_header lparen,
    --                        nest 8 (fsep (punctuate comma (map exact (unLoc es)))),
    --                        nest 4 (text ") where")
    --                       ],
    --         pp_nonnull imports,
    --         pp_nonnull decls
    --       ]
    --   where
    --     pp_header rest = case deprec of
    --        Nothing -> pp_modname <+> rest
    --        Just d -> vcat [ pp_modname, exact d, rest ]

    --     pp_modname = text "module" <+> exact name

-- ---------------------------------------------------------------------

-- pp_mb :: ExactPrint t => Maybe t -> SDoc
-- pp_mb (Just x) = ppr x
-- pp_mb Nothing  = empty

-- pp_nonnull :: ExactPrint t => [t] -> SDoc
-- pp_nonnull [] = empty
-- pp_nonnull xs = vcat (map ppr xs)

-- ---------------------------------------------------------------------

instance ExactPrint ModuleName where
  getAnnotationEntry _ = NoEntryVal
  exact = withPpr

-- ---------------------------------------------------------------------

instance ExactPrint (LocatedP WarningTxt) where
  getAnnotationEntry = entryFromLocatedA
  exact (L (SrcSpanAnn an l) (WarningTxt src ws)) = do
    markAnnOpenP an src "{-# WARNING"
    markLocatedAAL an apr_rest AnnOpenS
    markAnnotated ws
    markLocatedAAL an apr_rest AnnCloseS
    markAnnCloseP an

  exact (L (SrcSpanAnn an l) (DeprecatedTxt src ws)) = do
    markAnnOpenP an src "{-# DEPRECATED"
    markLocatedAAL an apr_rest AnnOpenS
    markAnnotated ws
    markLocatedAAL an apr_rest AnnCloseS
    markAnnCloseP an

-- ---------------------------------------------------------------------

instance ExactPrint (ImportDecl GhcPs) where
  getAnnotationEntry idecl = fromAnn (ideclExt idecl)
  exact x@(ImportDecl ApiAnnNotUsed _ _ _ _ _ _ _ _ _) = withPpr x
  exact (ImportDecl ann@(ApiAnn _ an _) msrc (L lm modname) mpkg _src safeflag qualFlag _impl mAs hiding) = do

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
     Just (StringLiteral src v _) ->
       printStringAtMkw (importDeclAnnPackage an) (sourceTextToString src (show v))
     _ -> return ()

    printStringAtKw' (realSrcSpan lm) (moduleNameString modname)

    case qualFlag of
      QualifiedPost  -- 'qualified' appears in postpositive position.
        -> printStringAtMkw (importDeclAnnQualified an) "qualified"
      _ -> return ()

    case mAs of
      Nothing -> return ()
      Just (L l mn) -> do
        printStringAtMkw (importDeclAnnAs an) "as"
        printStringAtKw' (realSrcSpan l) (moduleNameString mn)

    case hiding of
      Nothing -> return ()
      Just (_isHiding,lie) -> exact lie
 --   markTrailingSemi


-- ---------------------------------------------------------------------

instance ExactPrint HsDocString where
  getAnnotationEntry _ = NoEntryVal
  exact = withPpr -- TODO:AZ use annotations

-- ---------------------------------------------------------------------

instance ExactPrint (HsDecl GhcPs) where
  getAnnotationEntry (TyClD      _ d) = NoEntryVal
  getAnnotationEntry (InstD      _ d) = NoEntryVal
  getAnnotationEntry (DerivD     _ d) = NoEntryVal
  getAnnotationEntry (ValD       _ d) = NoEntryVal
  getAnnotationEntry (SigD       _ d) = NoEntryVal
  getAnnotationEntry (KindSigD   _ d) = NoEntryVal
  getAnnotationEntry (DefD       _ d) = NoEntryVal
  getAnnotationEntry (ForD       _ d) = NoEntryVal
  getAnnotationEntry (WarningD   _ d) = NoEntryVal
  getAnnotationEntry (AnnD       _ d) = NoEntryVal
  getAnnotationEntry (RuleD      _ d) = NoEntryVal
  getAnnotationEntry (SpliceD    _ d) = NoEntryVal
  getAnnotationEntry (DocD       _ d) = NoEntryVal
  getAnnotationEntry (RoleAnnotD _ d) = NoEntryVal

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

-- instance Annotate (GHC.InstDecl GHC.GhcPs) where

--   markAST l (GHC.ClsInstD     _  cid) = markAST l  cid
--   markAST l (GHC.DataFamInstD _ dfid) = markAST l dfid
--   markAST l (GHC.TyFamInstD   _ tfid) = markAST l tfid
--   markAST _ (GHC.XInstDecl x) = error $ "got XInstDecl for:" ++ showPprUnsafe x

  exact (ClsInstD     _  cid) = markAnnotated cid
  exact (DataFamInstD an decl) = do
    exactDataFamInstDecl an TopLevel decl
  exact (TyFamInstD _ eqn) = do
    -- exactTyFamInstDecl an TopLevel eqn
    markAnnotated eqn
  exact x = error $ "InstDecl: exact for " ++ showAst x

-- ---------------------------------------------------------------------

exactDataFamInstDecl :: ApiAnn -> TopLevelFlag -> (DataFamInstDecl GhcPs) -> EPP ()
exactDataFamInstDecl an top_lvl
  (DataFamInstDecl { dfid_eqn = HsIB { hsib_body =
                             FamEqn { feqn_tycon  = tycon
                                    , feqn_bndrs  = bndrs
                                    , feqn_pats   = pats
                                    , feqn_fixity = fixity
                                    , feqn_rhs    = defn }}})
  = exactDataDefn an pp_hdr defn
  where
    pp_hdr mctxt = do
      case top_lvl of
        TopLevel -> markApiAnn an AnnInstance -- TODO: maybe in toplevel
        NotTopLevel -> return ()
      exactHsFamInstLHS an tycon bndrs pats fixity mctxt

-- ---------------------------------------------------------------------

exactTyFamInstDecl :: TopLevelFlag -> (TyFamInstDecl GhcPs) -> EPP ()
exactTyFamInstDecl top_lvl (TyFamInstDecl { tfid_xtn = an, tfid_eqn = eqn }) = do
  markApiAnn an AnnType
  case top_lvl of
    TopLevel -> markApiAnn an AnnInstance
    NotTopLevel -> return ()
  markAnnotated eqn

-- ---------------------------------------------------------------------

instance ExactPrint (DerivDecl GhcPs) where
  getAnnotationEntry (DerivDecl {deriv_ext = an} ) = fromAnn an
  exact (DerivDecl an typ ms mov) = do
    markApiAnn an AnnDeriving
    mapM_ markAnnotated ms
    markApiAnn an AnnInstance
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
    markApiAnn an AnnForeign
    markApiAnn an AnnImport

    markAnnotated fimport

    markAnnotated n
    markApiAnn an AnnDcolon
    markAnnotated ty
  exact x = error $ "ForDecl: exact for " ++ showAst x
{-
  markAST _ (GHC.ForeignImport _ ln (GHC.HsIB _ typ)
               (GHC.CImport cconv safety@(GHC.L ll _) _mh _imp (GHC.L ls src))) = do
    mark GHC.AnnForeign
    mark GHC.AnnImport

    markLocated cconv
    unless (ll == GHC.noSrcSpan) $ markLocated safety
    markExternalSourceText ls src ""

    markLocated ln
    mark GHC.AnnDcolon
    markLocated typ
    markTrailingSemi

-}


-- ---------------------------------------------------------------------

instance ExactPrint ForeignImport where
  getAnnotationEntry = const NoEntryVal
  exact (CImport cconv safety@(L ll _) _mh _imp (L ls src)) = do
    markAnnotated cconv
    unless (ll == noSrcSpan) $ markAnnotated safety
    markExternalSourceText ls src ""

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
    markApiAnn an AnnOpenS -- "["
    case txt of
      WarningTxt    _src ls -> markAnnotated ls
      DeprecatedTxt _src ls -> markAnnotated ls
    markApiAnn an AnnCloseS -- "]"

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
  exact fs = printString False (show (unpackFS fs))


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

-- instance Annotate (RuleDecls GhcPs) where
--   markAST _ (HsRules _ src rules) = do
--     markAnnOpen src "{-# RULES"
--     setLayoutFlag $ markListIntercalateWithFunLevel markLocated 2 rules
--     markWithString AnnClose "#-}"
--     markTrailingSemi

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
    markApiAnn' an ra_rest AnnEqual
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

markActivation :: ApiAnn' a -> (a -> [AddApiAnn]) -> Activation -> Annotated ()
markActivation an fn act = do
  case act of
    ActiveBefore src phase -> do
      markApiAnn' an fn AnnOpenS --  '['
      markApiAnn' an fn AnnTilde -- ~
      markLocatedAALS an fn AnnVal (Just (toSourceTextWithSuffix src (show phase) ""))
      markApiAnn' an fn AnnCloseS -- ']'
    ActiveAfter src phase -> do
      markApiAnn' an fn AnnOpenS --  '['
      markLocatedAALS an fn AnnVal (Just (toSourceTextWithSuffix src (show phase) ""))
      markApiAnn' an fn AnnCloseS -- ']'
    NeverActive -> do
      markApiAnn' an fn AnnOpenS --  '['
      markApiAnn' an fn AnnTilde -- ~
      markApiAnn' an fn AnnCloseS -- ']'
    _ -> return ()

-- ---------------------------------------------------------------------

instance ExactPrint (SpliceDecl GhcPs) where
  getAnnotationEntry = const NoEntryVal

  exact (SpliceDecl _ splice flag) = do
    markAnnotated splice

-- ---------------------------------------------------------------------

instance ExactPrint DocDecl where
  getAnnotationEntry = const NoEntryVal

  exact v =
    let str =
          case v of
            (DocCommentNext ds)     -> unpackHDS ds
            (DocCommentPrev ds)     -> unpackHDS ds
            (DocCommentNamed _s ds) -> unpackHDS ds
            (DocGroup _i ds)        -> unpackHDS ds
    in
      printString False str

-- ---------------------------------------------------------------------

instance ExactPrint (RoleAnnotDecl GhcPs) where
  getAnnotationEntry (RoleAnnotDecl an _ _) = fromAnn an
  exact (RoleAnnotDecl an ltycon roles) = do
    markApiAnn an AnnType
    markApiAnn an AnnRole
    markAnnotated ltycon
    markAnnotated roles

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
    markApiAnn an AnnOpenP -- "("
    markAnnotated ln
    markApiAnn an AnnDcolon
    markAnnotated ty
    markApiAnn an AnnCloseP -- ")"

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
--     markApiAnn an AnnEqual
--     markAnnotated rhs

instance (ExactPrint body) => ExactPrint (FamEqn GhcPs body) where
  getAnnotationEntry = const NoEntryVal
  exact (FamEqn { feqn_ext = an
                , feqn_tycon  = tycon
                , feqn_bndrs  = bndrs
                , feqn_pats   = pats
                , feqn_fixity = fixity
                , feqn_rhs    = rhs }) = do
    exactHsFamInstLHS an tycon bndrs pats fixity Nothing
    markApiAnn an AnnEqual
    markAnnotated rhs

-- ---------------------------------------------------------------------

exactHsFamInstLHS ::
      ApiAnn
   -> LocatedN RdrName
   -> Maybe [LHsTyVarBndr () GhcPs]
   -> HsTyPats GhcPs
   -> LexicalFixity
   -> Maybe (LHsContext GhcPs)
   -> EPP ()
exactHsFamInstLHS an thing bndrs typats fixity mb_ctxt = do
  markApiAnn an AnnForall
  mapM_ markAnnotated bndrs
  markApiAnn an AnnDot
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
               markApiAnn an AnnOpenP
               exact_op_app
               markApiAnn an AnnCloseP
               mapM_ markAnnotated pats

    exact_pats pats = do
      markAnnotated thing
      mapM_ markAnnotated pats

-- ---------------------------------------------------------------------

-- instance ExactPrint (LHsTypeArg GhcPs) where
instance (ExactPrint tm, ExactPrint ty, Outputable tm, Outputable ty)
     =>  ExactPrint (HsArg tm ty) where
  getAnnotationEntry = const NoEntryVal

  exact (HsValArg tm)    = markAnnotated tm
  exact (HsTypeArg ss ty) = printStringAtSs ss "@" >> markAnnotated ty
  exact x@(HsArgPar sp)   = withPpr x -- Does not appear in original source

-- ---------------------------------------------------------------------

-- instance ExactPrint [LHsTyVarBndr () GhcPs] where
--   getAnnotationEntry = const NoEntryVal
--   exact bs = mapM_ markAnnotated bs

-- ---------------------------------------------------------------------

instance ExactPrint (ClsInstDecl GhcPs) where
  getAnnotationEntry cid = fromAnn (cid_ext cid)

  -- exact (ClsInstDecl an poly binds sigs tyfams datafams mov) = do
  --   markApiAnn an AnnInstance
  --   mapM_ markAnnotated mov
  --   markAnnotated poly
  --   markApiAnn an AnnWhere
  --   markApiAnn an AnnOpenC -- '{'

  --   applyListAnnotations (prepareListAnnotationA (bagToList binds)
  --                      ++ prepareListAnnotation sigs
  --                      ++ prepareListAnnotation tyfams
  --                      ++ prepareListAnnotation datafams
  --                        )

  --   markApiAnn an AnnCloseC -- '}'

  exact (ClsInstDecl { cid_ext = an
                     , cid_poly_ty = inst_ty, cid_binds = binds
                     , cid_sigs = sigs, cid_tyfam_insts = ats
                     , cid_overlap_mode = mbOverlap
                     , cid_datafam_insts = adts })
      | null sigs, null ats, null adts, isEmptyBag binds  -- No "where" part
      = top_matter

      | otherwise       -- Laid out
      = do
          top_matter
          markApiAnn an AnnWhere
          markApiAnn an AnnOpenC
      -- = vcat [ top_matter <+> text "where"
      --        , nest 2 $ pprDeclList $
      --          map (pprTyFamInstDecl NotTopLevel . unLoc)   ats ++
      --          map (pprDataFamInstDecl NotTopLevel . unLoc) adts ++
      --          pprLHsBindsForUser binds sigs ]
          applyListAnnotations (prepareListAnnotationA ats
                             ++ prepareListAnnotationF (exactDataFamInstDecl an NotTopLevel ) adts
                             ++ prepareListAnnotationA (bagToList binds)
                             ++ prepareListAnnotationA sigs
                               )
          markApiAnn an AnnCloseC -- '}'

      where
        top_matter = do
          markApiAnn an AnnInstance
          mapM_ markAnnotated mbOverlap
          markAnnotated inst_ty
          markApiAnn an AnnWhere -- Optional
          -- text "instance" <+> ppOverlapPragma mbOverlap
          --                                    <+> ppr inst_ty

-- ---------------------------------------------------------------------

instance ExactPrint (TyFamInstDecl GhcPs) where
  getAnnotationEntry (TyFamInstDecl an _) = fromAnn an
  exact d@(TyFamInstDecl an eqn) =
    exactTyFamInstDecl TopLevel d

-- ---------------------------------------------------------------------

instance (ExactPrint body) => ExactPrint (HsImplicitBndrs GhcPs body) where
  getAnnotationEntry (HsIB an _) = fromAnn an
  exact (HsIB an t) = markAnnotated t

-- ---------------------------------------------------------------------

instance ExactPrint (LocatedP OverlapMode) where
  getAnnotationEntry = entryFromLocatedA

  -- NOTE: NoOverlap is only used in the typechecker
  exact (L (SrcSpanAnn an ll) (NoOverlap src)) = do
    markAnnOpenP an src "{-# NO_OVERLAP"
    markAnnCloseP an

  exact (L (SrcSpanAnn an ll) (Overlappable src)) = do
    markAnnOpenP an src "{-# OVERLAPPABLE"
    markAnnCloseP an

  exact (L (SrcSpanAnn an ll) (Overlapping src)) = do
    markAnnOpenP an src "{-# OVERLAPPING"
    markAnnCloseP an

  exact (L (SrcSpanAnn an ll) (Overlaps src)) = do
    markAnnOpenP an src "{-# OVERLAPS"
    markAnnCloseP an

  exact (L (SrcSpanAnn an ll) (Incoherent src)) = do
    markAnnOpenP an src "{-# INCOHERENT"
    markAnnCloseP an

-- ---------------------------------------------------------------------

instance ExactPrint (HsBind GhcPs) where
  getAnnotationEntry FunBind{} = NoEntryVal
  getAnnotationEntry PatBind{} = NoEntryVal
  getAnnotationEntry VarBind{} = NoEntryVal
  getAnnotationEntry AbsBinds{} = NoEntryVal
  getAnnotationEntry PatSynBind{} = NoEntryVal

  exact (FunBind _ _ matches _) = do
    markAnnotated matches
  exact (PatBind an pat grhss _) = do
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
    markApiAnn an AnnPattern
    case details of
      InfixCon v1 v2 -> do
        markAnnotated v1
        markAnnotated psyn
        markAnnotated v2
      PrefixCon vs -> do
        markAnnotated psyn
        markAnnotated vs
      RecCon vs -> do
        markAnnotated psyn
        markApiAnn an AnnOpenC  -- '{'
        markAnnotated vs
        markApiAnn an AnnCloseC -- '}'

    case dir of
      Unidirectional           -> do
        markApiAnn an AnnLarrow
        markAnnotated pat
      ImplicitBidirectional    -> do
        markApiAnn an AnnEqual
        markAnnotated pat
      ExplicitBidirectional mg -> do
        markApiAnn an AnnLarrow
        markAnnotated pat
        markApiAnn an AnnWhere
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

instance (ExactPrint a) => ExactPrint (RecordPatSynField a) where
  getAnnotationEntry = const NoEntryVal
  exact (RecordPatSynField { recordPatSynSelectorId = v }) = markAnnotated v

-- ---------------------------------------------------------------------

instance ExactPrint (Match GhcPs (LocatedA (HsCmd GhcPs))) where
  getAnnotationEntry (Match ann _ _ _) = fromAnn ann

  exact match@(Match ApiAnnNotUsed _ _ _) = withPpr match
  exact (Match an mctxt pats grhss) = do
    exactMatch (Match an mctxt pats grhss)

-- -------------------------------------

instance ExactPrint (Match GhcPs (LocatedA (HsExpr GhcPs))) where
  getAnnotationEntry (Match ann _ _ _) = fromAnn ann

  exact match@(Match ApiAnnNotUsed _ _ _) = withPpr match
  exact (Match an mctxt pats grhss) = do
    exactMatch (Match an mctxt pats grhss)
  -- -- Based on Expr.pprMatch

  --   debugM $ "exact Match entered"

  --   -- herald
  --   case mctxt of
  --     FunRhs fun fixity strictness -> do
  --       debugM $ "exact Match FunRhs:" ++ showPprUnsafe fun
  --       case strictness of
  --         SrcStrict -> markApiAnn an AnnBang
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
  --                   markApiAnn an AnnOpenP
  --                   markAnnotated p1
  --                   markAnnotated fun
  --                   markAnnotated p2
  --                   markApiAnn an AnnCloseP
  --                   mapM_ markAnnotated rest
  --     LambdaExpr -> do
  --       markApiAnn an AnnLam
  --       mapM_ markAnnotated pats
  --     GHC.CaseAlt -> do
  --       mapM_ markAnnotated pats
  --     _ -> withPpr mctxt

  --   markAnnotated grhss

-- ---------------------------------------------------------------------

exactMatch (Match an mctxt pats grhss) = do
-- Based on Expr.pprMatch

  debugM $ "exact Match entered"

  -- herald
  case mctxt of
    FunRhs fun fixity strictness -> do
      debugM $ "exact Match FunRhs:" ++ showPprUnsafe fun
      case strictness of
        SrcStrict -> markApiAnn an AnnBang
        _ -> pure ()
      case fixity of
        Prefix -> do
          markAnnotated fun
          mapM_ markAnnotated pats
        Infix ->
          case pats of
            (p1:p2:rest)
              | null rest -> do
                  markAnnotated p1
                  markAnnotated fun
                  markAnnotated p2
              | otherwise -> do
                  markApiAnn an AnnOpenP
                  markAnnotated p1
                  markAnnotated fun
                  markAnnotated p2
                  markApiAnn an AnnCloseP
                  mapM_ markAnnotated rest
    LambdaExpr -> do
      markApiAnn an AnnLam
      mapM_ markAnnotated pats
    GHC.CaseAlt -> do
      mapM_ markAnnotated pats
    _ -> withPpr mctxt

  markAnnotated grhss

-- ---------------------------------------------------------------------

instance ExactPrint (GRHSs GhcPs (LocatedA (HsExpr GhcPs))) where
  getAnnotationEntry (GRHSs an _ _) = fromAnn an

  exact (GRHSs an grhss binds) = do
    debugM $ "GRHSs: before matchSeparator"
    markLocatedAA an id -- Mark the matchSeparator for these GRHSs
    debugM $ "GRHSs: after matchSeparator"
    markAnnotated grhss
    markAnnotated binds

instance ExactPrint (GRHSs GhcPs (LocatedA (HsCmd GhcPs))) where
  getAnnotationEntry (GRHSs an _ _) = fromAnn an

  exact (GRHSs an grhss binds) = do
    debugM $ "GRHSs: before matchSeparator"
    markLocatedAA an id -- Mark the matchSeparator for these GRHSs
    debugM $ "GRHSs: after matchSeparator"
    markAnnotated grhss
    markAnnotated binds

-- ---------------------------------------------------------------------

instance ExactPrint (HsLocalBinds GhcPs) where
  getAnnotationEntry (HsValBinds an _) = fromAnn an
  getAnnotationEntry (HsIPBinds{}) = NoEntryVal
  getAnnotationEntry (EmptyLocalBinds{}) = NoEntryVal

  -- exact (HsValBinds _ bs) = markAnnotated bs
  exact (HsValBinds an bs) = do
    markLocatedAAL an al_rest AnnWhere
    markAnnotated bs
    -- applyListAnnotations
    --    (prepareListAnnotation (bagToList binds)
    --  ++ prepareListAnnotation sigs
    --    )

    -- withPpr bs
  exact (HsIPBinds an bs)
    = markAnnList an (markLocatedAAL an al_rest AnnWhere >> markAnnotated bs)
  exact (EmptyLocalBinds _) = return ()


-- ---------------------------------------------------------------------

instance ExactPrint (HsIPBinds GhcPs) where
  getAnnotationEntry = const NoEntryVal

  exact (IPBinds _ binds) = markAnnotated binds

-- ---------------------------------------------------------------------

instance ExactPrint (IPBind GhcPs) where
  getAnnotationEntry (IPBind an _ _) = fromAnn an

  exact (IPBind an (Left lr) rhs) = do
    markAnnotated lr
    markApiAnn an AnnEqual
    markAnnotated rhs

  exact (IPBind _ (Right _) _) = error $ "ExactPrint IPBind: Right only after typechecker"

-- ---------------------------------------------------------------------

instance ExactPrint HsIPName where
  getAnnotationEntry = const NoEntryVal

  exact (HsIPName fs) = printString False ("?" ++ (unpackFS fs))

-- ---------------------------------------------------------------------

instance ExactPrint (HsValBindsLR GhcPs GhcPs) where
  getAnnotationEntry _ = NoEntryVal

  exact (ValBinds sortkey binds sigs) = do
    -- printString False "ValBinds"
    applyListAnnotations
       (prepareListAnnotationA (bagToList binds)
     ++ prepareListAnnotationA sigs
       )
-- ---------------------------------------------------------------------
-- Managing lists which have been separated, e.g. Sigs and Binds


-- AZ:TODO: generalise this, and the next one
-- prepareListAnnotationFamilyD :: [LFamilyDecl GhcPs] -> [(RealSrcSpan,EPP ())]
-- prepareListAnnotationFamilyD ls
--   = map (\b -> (realSrcSpan $ getLocA b,exactFamilyDecl NotTopLevel (unLoc b))) ls

prepareListAnnotationF :: (a -> EPP ()) -> [LocatedAn an a] -> [(RealSrcSpan,EPP ())]
prepareListAnnotationF f ls
  = map (\b -> (realSrcSpan $ getLocA b, f (unLoc b))) ls

prepareListAnnotation :: ExactPrint (Located a)
  => [Located a] -> [(RealSrcSpan,EPP ())]
prepareListAnnotation ls = map (\b -> (realSrcSpan $ getLoc b,markAnnotated b)) ls

prepareListAnnotationA :: ExactPrint (LocatedAn an a)
  => [LocatedAn an a] -> [(RealSrcSpan,EPP ())]
prepareListAnnotationA ls = map (\b -> (realSrcSpan $ getLocA b,markAnnotated b)) ls


applyListAnnotations :: [(RealSrcSpan, EPP ())] -> EPP ()
applyListAnnotations ls = withSortKey ls

withSortKey :: [(RealSrcSpan, EPP ())] -> EPP ()
withSortKey xs = do
  Ann{annSortKey} <- asks epAnn
  let ordered = case annSortKey of
                  Nothing   -> sortBy orderByFst xs
                  Just keys -> orderByKey xs keys
                                -- `debug` ("withSortKey:" ++
                                --          showPprUnsafe (map fst (sortBy (comparing (flip elemIndex keys . fst)) xs),
                                --                  map fst xs,
                                --                  keys)
                                --          )
  mapM_ snd ordered

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
--   markAST _ (TypeSig _ lns st)  = do
--     setContext (Set.singleton PrefixOp) $ markListNoPrecedingSpace True lns
--     mark AnnDcolon
--     markLHsSigWcType st
--     markTrailingSemi
--     tellContext (Set.singleton FollowingLine)

  exact (PatSynSig an lns typ) = do
    markApiAnn an AnnPattern
    markAnnotated lns
    markApiAnn an AnnDcolon
    markAnnotated typ

  exact (ClassOpSig an is_deflt vars ty)
    | is_deflt  = markApiAnn an AnnDefault >> exactVarSig an vars ty
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
    p <- getPos
    debugM $ "InlineSig: p=" ++ show p
    markLocatedAALS an id AnnClose (Just "#-}")
    debugM $ "InlineSig:done"

  exact (SpecSig an ln typs inl) = do
    markAnnOpen an (inl_src inl) "{-# SPECIALISE" -- Note: may be {-# SPECIALISE_INLINE
    markActivation an id (inl_act inl)
    markAnnotated ln
    markApiAnn an AnnDcolon
    markAnnotated typs
    markLocatedAALS an id AnnClose (Just "#-}")

  exact (SpecInstSig an src typ) = do
    markAnnOpen an src "{-# SPECIALISE"
    markApiAnn an AnnInstance
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

--   markAST _ (CompleteMatchSig _ src (L _ ns) mlns) = do
--     markAnnOpen src "{-# COMPLETE"
--     markListIntercalate ns
--     case mlns of
--       Nothing -> return ()
--       Just _ -> do
--         mark AnnDcolon
--         markMaybe mlns
--     markWithString AnnClose "#-}" -- '#-}'
--     markTrailingSemi

  exact x = error $ "exact Sig for:" ++ showAst x

-- ---------------------------------------------------------------------

exactVarSig :: (ExactPrint a) => ApiAnn -> [LocatedN RdrName] -> a -> EPP ()
exactVarSig an vars ty = do
  mapM_ markAnnotated vars
  markApiAnn an AnnDcolon
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
    markApiAnn an AnnType
    markAnnotated vars
    markApiAnn an AnnDcolon
    markAnnotated sig

-- ---------------------------------------------------------------------

instance ExactPrint (DefaultDecl GhcPs) where
  getAnnotationEntry (DefaultDecl an _) = fromAnn an

  exact (DefaultDecl an tys) = do
    markApiAnn an AnnDefault
    markApiAnn an AnnOpenP
    markAnnotated tys
    markApiAnn an AnnCloseP

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
  getAnnotationEntry (GRHS ann _ _) = fromAnn ann

  exact (GRHS an guards expr) = do
    markAnnKwM an ga_vbar AnnVbar
    markAnnotated guards
    markLocatedAA an ga_sep -- Mark the matchSeparator for these GRHSs
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
  getAnnotationEntry (HsConLikeOut{})             = NoEntryVal
  getAnnotationEntry (HsRecFld{})                 = NoEntryVal
  getAnnotationEntry (HsOverLabel an _ _)         = fromAnn an
  getAnnotationEntry (HsIPVar an _)               = fromAnn an
  getAnnotationEntry (HsOverLit an _)             = fromAnn an
  getAnnotationEntry (HsLit an _)                 = fromAnn an
  getAnnotationEntry (HsLam _ _)                  = NoEntryVal
  getAnnotationEntry (HsLamCase an _)             = fromAnn an
  getAnnotationEntry (HsApp an _ _)               = fromAnn an
  getAnnotationEntry (HsAppType an _ _)           = fromAnn an
  getAnnotationEntry (OpApp an _ _ _)             = fromAnn an
  getAnnotationEntry (NegApp an _ _)              = fromAnn an
  getAnnotationEntry (HsPar an _)                 = fromAnn an
  getAnnotationEntry (SectionL an _ _)            = fromAnn an
  getAnnotationEntry (SectionR an _ _)            = fromAnn an
  getAnnotationEntry (ExplicitTuple an _ _)       = fromAnn an
  getAnnotationEntry (ExplicitSum an _ _ _)       = fromAnn an
  getAnnotationEntry (HsCase an _ _)              = fromAnn an
  getAnnotationEntry (HsIf an _ _ _)              = fromAnn an
  getAnnotationEntry (HsMultiIf an _)             = fromAnn an
  getAnnotationEntry (HsLet an _ _)               = fromAnn an
  getAnnotationEntry (HsDo an _ _)                = fromAnn an
  getAnnotationEntry (ExplicitList an _ _)        = fromAnn an
  getAnnotationEntry (RecordCon an _ _)           = fromAnn an
  getAnnotationEntry (RecordUpd an _ _)           = fromAnn an
  getAnnotationEntry (ExprWithTySig an _ _)       = fromAnn an
  getAnnotationEntry (ArithSeq an _ _)            = fromAnn an
  getAnnotationEntry (HsBracket an _)             = fromAnn an
  getAnnotationEntry (HsRnBracketOut{})           = NoEntryVal
  getAnnotationEntry (HsTcBracketOut{})           = NoEntryVal
  getAnnotationEntry (HsSpliceE an _)             = fromAnn an
  getAnnotationEntry (HsProc an _ _)              = fromAnn an
  getAnnotationEntry (HsStatic an _)              = fromAnn an
  getAnnotationEntry (HsTick {})                  = NoEntryVal
  getAnnotationEntry (HsBinTick {})               = NoEntryVal
  getAnnotationEntry (HsPragE{})                  = NoEntryVal


  exact (HsVar _ n) = markAnnotated n
  exact x@(HsUnboundVar an v) = do
    case an of
      ApiAnnNotUsed -> withPpr x
      ApiAnn _ (ApiAnnUnboundVar (ob,cb) l) _ -> do
        printStringAtKw' ob "`"
        printStringAtKw' l  "_"
        printStringAtKw' cb "`"
  -- exact x@(HsConLikeOut{})             = withPpr x
  -- exact x@(HsRecFld{})                 = withPpr x
  -- exact x@(HsOverLabel ann _ _)        = withPpr x
  exact (HsIPVar _ (HsIPName n))
    = printString False ("?" ++ unpackFS n)

  exact x@(HsOverLit ann ol) = do
    let str = case ol_val ol of
                HsIntegral   (IL src _ _) -> src
                HsFractional (FL src _ _) -> src
                HsIsString src _          -> src
    -- markExternalSourceText l str ""
    case str of
      SourceText s -> printString False s
      NoSourceText -> withPpr x

  exact (HsLit ann lit) = withPpr lit
  exact (HsLam an (MG _ (L _ [match]) _)) = do
    markAnnotated match
      -- markExpr _ (HsLam _ (MG _ (L _ [match]) _)) = do
      --   setContext (Set.singleton LambdaExpr) $ do
      --   -- TODO: Change this, HsLam binds do not need obey layout rules.
      --   --       And will only ever have a single match
      --     markLocated match
      -- markExpr _ (HsLam _ _) = error $ "HsLam with other than one match"
  exact (HsLam _ _) = error $ "HsLam with other than one match"

  exact (HsLamCase an mg) = do
    markApiAnn an AnnLam
    markApiAnn an AnnCase
    markAnnotated mg

  exact (HsApp an e1 e2) = do
    p <- getPos
    debugM $ "HsApp entered. p=" ++ show p
    markAnnotated e1
    markAnnotated e2
  exact (HsAppType an fun arg) = do
    markAnnotated fun
    markApiAnn an AnnAt
    markAnnotated arg
  exact x@(OpApp ann e1 e2 e3) = do
    exact e1
    exact e2
    exact e3

  exact x@(NegApp an e _) = do
    markApiAnn an AnnMinus
    markAnnotated e

  exact x@(HsPar an e) = do
    markOpeningParen an
    markAnnotated e
    -- debugM $ "HsPar closing paren"
    markClosingParen an
    -- debugM $ "HsPar done"

  -- exact (SectionL an expr op) = do
  exact (SectionR an op expr) = do
    markAnnotated op
    markAnnotated expr
  exact (ExplicitTuple an args b) = do
    if b == Boxed then markApiAnn an AnnOpenP
                  else markApiAnn an AnnOpenPH

    mapM_ markAnnotated args

    if b == Boxed then markApiAnn an AnnCloseP
                  else markApiAnn an AnnClosePH
    debugM $ "ExplicitTuple done"

  exact (ExplicitSum an alt arity expr) = do
    -- markApiAnn an AnnOpenPH
    markAnnKw an aesOpen AnnOpenPH
    markAnnKwAll an aesBarsBefore AnnVbar
    markAnnotated expr
    markAnnKwAll an aesBarsAfter AnnVbar
    markAnnKw an aesClose AnnClosePH

  exact x@(HsCase an e alts) = do
    markAnnKw an hsCaseAnnCase AnnCase
    markAnnotated e
    markAnnKw an hsCaseAnnOf AnnOf
    markApiAnn' an hsCaseAnnsRest AnnOpenC
    markApiAnnAll an hsCaseAnnsRest AnnSemi
    markAnnotated alts
    markApiAnn' an hsCaseAnnsRest AnnCloseC

  -- exact x@(HsCase ApiAnnNotUsed   _ _) = withPpr x
  exact (HsIf an e1 e2 e3) = do
    markApiAnn an AnnIf
    markAnnotated e1
    markApiAnn an AnnThen
    markAnnotated e2
    markApiAnn an AnnElse
    markAnnotated e3

  exact (HsMultiIf an mg) = do
    markApiAnn an AnnIf
    markApiAnn an AnnOpenC -- optional
    markAnnotated mg
    markApiAnn an AnnCloseC -- optional

  exact (HsLet an binds e) = do
    markApiAnn an AnnLet
    markApiAnn an AnnOpenC -- '{'
    markAnnotated binds
    markApiAnn an AnnCloseC -- '}'
    markApiAnn an AnnIn
    markAnnotated e

  exact (HsDo an do_or_list_comp stmts) = do
    debugM $ "HsDo"
    markAnnList an $ exactDo an do_or_list_comp stmts

  exact (ExplicitList an _ es) = do
    debugM $ "ExplicitList"
    markLocatedMAA an al_open
    markAnnotated es
    markLocatedMAA an al_close
  exact (RecordCon an con_id binds) = do
    markAnnotated con_id
    markApiAnn an AnnOpenC
    markAnnotated binds
    markApiAnn an AnnCloseC
  exact x@(RecordUpd an expr fields) = do
    markAnnotated expr
    markApiAnn an AnnOpenC
    markAnnotated fields
    markApiAnn an AnnCloseC
  exact (ExprWithTySig an expr sig) = do
    markAnnotated expr
    markApiAnn an AnnDcolon
    markAnnotated sig
  exact (ArithSeq an _ seqInfo) = do
    markApiAnn an AnnOpenS -- '['
    case seqInfo of
        From e -> do
          markAnnotated e
          markApiAnn an AnnDotdot
        FromTo e1 e2 -> do
          markAnnotated e1
          markApiAnn an AnnDotdot
          markAnnotated e2
        FromThen e1 e2 -> do
          markAnnotated e1
          markApiAnn an AnnComma
          markAnnotated e2
          markApiAnn an AnnDotdot
        FromThenTo e1 e2 e3 -> do
          markAnnotated e1
          markApiAnn an AnnComma
          markAnnotated e2
          markApiAnn an AnnDotdot
          markAnnotated e3
    markApiAnn an AnnCloseS -- ']'


  exact (HsBracket an (ExpBr _ e)) = do
    markApiAnn an AnnOpenEQ -- "[|"
    markApiAnn an AnnOpenE  -- "[e|" -- optional
    markAnnotated e
    markApiAnn an AnnCloseQ -- "|]"
  exact (HsBracket an (PatBr _ e)) = do
    markLocatedAALS an id AnnOpen (Just "[p|")
    markAnnotated e
    markApiAnn an AnnCloseQ -- "|]"
  exact (HsBracket an (DecBrL _ e)) = do
    markLocatedAALS an id AnnOpen (Just "[d|")
    markAnnotated e
    markApiAnn an AnnCloseQ -- "|]"
  -- -- exact (HsBracket an (DecBrG _ _)) =
  -- --   traceM "warning: DecBrG introduced after renamer"
  exact (HsBracket an (TypBr _ e)) = do
    markLocatedAALS an id AnnOpen (Just "[t|")
    markAnnotated e
    markApiAnn an AnnCloseQ -- "|]"
  exact (HsBracket an (VarBr _ b e)) = do
    if b
      then do
        markApiAnn an AnnSimpleQuote
        markAnnotated e
      else do
        markApiAnn an AnnThTyQuote
        markAnnotated e
  exact (HsBracket an (TExpBr _ e)) = do
    markLocatedAALS an id AnnOpen (Just "[||")
    markLocatedAALS an id AnnOpenE (Just "[e||")
    markAnnotated e
    markLocatedAALS an id AnnClose (Just "||]")


  -- exact x@(HsRnBracketOut{})           = withPpr x
  -- exact x@(HsTcBracketOut{})           = withPpr x
  exact (HsSpliceE an sp) = markAnnotated sp

  exact (HsProc an p c) = do
    markApiAnn an AnnProc
    markAnnotated p
    markApiAnn an AnnRarrow
    markAnnotated c

  exact (HsStatic an e) = do
    markApiAnn an AnnStatic
    markAnnotated e

  -- exact x@(HsTick {})                  = withPpr x
  -- exact x@(HsBinTick {})               = withPpr x
  exact (HsPragE _ prag e) = do
    markAnnotated prag
    markAnnotated e
  exact x = error $ "exact HsExpr for:" ++ showAst x

-- ---------------------------------------------------------------------

exactDo :: (ExactPrint body)
        => ApiAnn' AnnList -> (HsStmtContext any) -> body -> EPP ()
exactDo an (DoExpr m)    stmts = exactMdo an m AnnDo             >> markAnnotated stmts
exactDo an GhciStmtCtxt  stmts = markLocatedAAL an al_rest AnnDo >> markAnnotated stmts
exactDo an ArrowExpr     stmts = markLocatedAAL an al_rest AnnDo >> markAnnotated stmts
exactDo an (MDoExpr m)   stmts = exactMdo an m AnnMdo            >> markAnnotated stmts
exactDo an ListComp      stmts = markAnnotated stmts
exactDo an MonadComp     stmts = markAnnotated stmts
exactDo _  _             _     = panic "pprDo" -- PatGuard, ParStmtCxt

exactMdo :: ApiAnn' AnnList -> Maybe ModuleName -> AnnKeywordId -> EPP ()
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
  getAnnotationEntry (XSplice _)                = NoEntryVal

  exact (HsTypedSplice an DollarSplice n e) = do
    markApiAnn an AnnDollarDollar
    markAnnotated e

  -- = ppr_splice (text "$$") n e empty
  -- exact (HsTypedSplice _ BareSplice _ _ )
  -- = panic "Bare typed splice"  -- impossible
  exact (HsUntypedSplice an decoration _n b) = do
    when (decoration == DollarSplice) $ markApiAnn an AnnDollar
    markAnnotated b

  -- exact (HsUntypedSplice _ DollarSplice n e)
  -- = ppr_splice (text "$")  n e empty
  -- exact (HsUntypedSplice _ BareSplice n e)
  -- = ppr_splice empty  n e empty

  exact (HsQuasiQuote _ _ q _ss fs) = do
        printString False
              -- Note: Lexer.x does not provide unicode alternative. 2017-02-26
              ("[" ++ (showPprUnsafe q) ++ "|" ++ (unpackFS fs) ++ "|]")

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
    => ExactPrint (HsRecField' (FieldOcc GhcPs) body) where
  getAnnotationEntry x = fromAnn (hsRecFieldAnn x)
  exact (HsRecField an f arg isPun) = do
    debugM $ "HsRecField"
    markAnnotated f
    if isPun then return ()
             else markApiAnn an AnnEqual
    markAnnotated arg

-- ---------------------------------------------------------------------

-- instance ExactPrint (HsRecUpdField GhcPs ) where
instance (ExactPrint body)
    => ExactPrint (HsRecField' (AmbiguousFieldOcc GhcPs) body) where
-- instance (ExactPrint body)
    -- => ExactPrint (HsRecField' (AmbiguousFieldOcc GhcPs) body) where
  getAnnotationEntry x = fromAnn (hsRecFieldAnn x)
  exact (HsRecField an f arg isPun) = do
    debugM $ "HsRecUpdField"
    markAnnotated f
    if isPun then return ()
             else markApiAnn an AnnEqual
    markAnnotated arg

-- ---------------------------------------------------------------------

-- instance ExactPrint (LHsTupArg GhcPs) where
--   getAnnotationEntry = entryFromLocatedA
--   exact (L an a) = do
--     exact a
--     markALocatedA (ann an)

-- ---------------------------------------------------------------------

instance ExactPrint (HsTupArg GhcPs) where
  getAnnotationEntry (Present an _) = fromAnn an
  getAnnotationEntry (Missing an)   = fromAnn an

  exact (Present _ e) = markAnnotated e

  exact (Missing ApiAnnNotUsed) = return ()
  exact (Missing _) = printString False ","

-- ---------------------------------------------------------------------

instance ExactPrint (HsCmdTop GhcPs) where
  getAnnotationEntry = const NoEntryVal
  exact (HsCmdTop _ cmd) = markAnnotated cmd

-- ---------------------------------------------------------------------

-- instance ExactPrint (LHsCmd GhcPs) where
--   getAnnotationEntry (L a _) = fromAnn a
--   exact (L an a) = do
--     exact a
--     markALocatedA (ann an)

-- ---------------------------------------------------------------------

instance ExactPrint (HsCmd GhcPs) where
  getAnnotationEntry (HsCmdArrApp {}) = NoEntryVal
  getAnnotationEntry (HsCmdArrForm{}) = NoEntryVal
  getAnnotationEntry (HsCmdApp    {}) = NoEntryVal
  getAnnotationEntry (HsCmdLam    {}) = NoEntryVal
  getAnnotationEntry (HsCmdPar    {}) = NoEntryVal
  getAnnotationEntry (HsCmdCase   {}) = NoEntryVal
  getAnnotationEntry (HsCmdLamCase{}) = NoEntryVal
  getAnnotationEntry (HsCmdIf     {}) = NoEntryVal
  getAnnotationEntry (HsCmdLet    {}) = NoEntryVal
  getAnnotationEntry (HsCmdDo     {}) = NoEntryVal


-- ppr_cmd (HsCmdArrApp _ arrow arg HsFirstOrderApp True)
--   = hsep [ppr_lexpr arrow, larrowt, ppr_lexpr arg]
-- ppr_cmd (HsCmdArrApp _ arrow arg HsFirstOrderApp False)
--   = hsep [ppr_lexpr arg, arrowt, ppr_lexpr arrow]
-- ppr_cmd (HsCmdArrApp _ arrow arg HsHigherOrderApp True)
--   = hsep [ppr_lexpr arrow, larrowtt, ppr_lexpr arg]
-- ppr_cmd (HsCmdArrApp _ arrow arg HsHigherOrderApp False)
--   = hsep [ppr_lexpr arg, arrowtt, ppr_lexpr arrow]

  exact (HsCmdArrApp an arrow arg o isRightToLeft) = do
    if isRightToLeft
      then do
        markAnnotated arrow
        markKw (anns an)
        markAnnotated arg
      else do
        markAnnotated arg
        markKw (anns an)
        markAnnotated arrow
--   markAST _ (GHC.HsCmdArrApp _ e1 e2 o isRightToLeft) = do
--         -- isRightToLeft True  => right-to-left (f -< arg)
--         --               False => left-to-right (arg >- f)
--     if isRightToLeft
--       then do
--         markLocated e1
--         case o of
--           GHC.HsFirstOrderApp  -> mark GHC.Annlarrowtail
--           GHC.HsHigherOrderApp -> mark GHC.AnnLarrowtail
--       else do
--         markLocated e2
--         case o of
--           GHC.HsFirstOrderApp  -> mark GHC.Annrarrowtail
--           GHC.HsHigherOrderApp -> mark GHC.AnnRarrowtail

--     if isRightToLeft
--       then markLocated e2
--       else markLocated e1

  exact (HsCmdArrForm an e fixity _mf [arg1,arg2]) = do
    markLocatedMAA an al_open
    case fixity of
      Infix -> do
        markAnnotated arg1
        markAnnotated e
        markAnnotated arg2
      Prefix -> do
        markAnnotated e
        markAnnotated arg1
        markAnnotated arg2
    markLocatedMAA an al_close
--   markAST _ (GHC.HsCmdArrForm _ e fixity _mf cs) = do
--     -- The AnnOpen should be marked for a prefix usage, not for a postfix one,
--     -- due to the way checkCmd maps both HsArrForm and OpApp to HsCmdArrForm

--     let isPrefixOp = case fixity of
--           GHC.Infix  -> False
--           GHC.Prefix -> True
--     when isPrefixOp $ mark GHC.AnnOpenB -- "(|"

--     -- This may be an infix operation
--     applyListAnnotationsContexts (LC (Set.singleton PrefixOp) (Set.singleton PrefixOp)
--                                      (Set.singleton InfixOp) (Set.singleton InfixOp))
--                        (prepareListAnnotation [e]
--                          ++ prepareListAnnotation cs)
--     when isPrefixOp $ mark GHC.AnnCloseB -- "|)"

--   markAST _ (GHC.HsCmdApp _ e1 e2) = do
--     markLocated e1
--     markLocated e2

  exact (HsCmdLam _ match) = markAnnotated match
--   markAST l (GHC.HsCmdLam _ match) = do
--     setContext (Set.singleton LambdaExpr) $ do markMatchGroup l match

  exact (HsCmdPar an e) = do
    markOpeningParen an
    markAnnotated e
    markClosingParen an

  exact (HsCmdCase an e alts) = do
    markAnnKw an hsCaseAnnCase AnnCase
    markAnnotated e
    markAnnKw an hsCaseAnnOf AnnOf
    markApiAnn' an hsCaseAnnsRest AnnOpenC
    markApiAnnAll an hsCaseAnnsRest AnnSemi
    markAnnotated alts
    markApiAnn' an hsCaseAnnsRest AnnCloseC
    -- markApiAnn an AnnCase
    -- markAnnotated e1
    -- markApiAnn an AnnOf
    -- markApiAnn an AnnOpenC
    -- markAnnotated matches
    -- markApiAnn an AnnCloseC

--   markAST l (GHC.HsCmdCase _ e1 matches) = do
--     mark GHC.AnnCase
--     markLocated e1
--     mark GHC.AnnOf
--     markOptional GHC.AnnOpenC
--     setContext (Set.singleton CaseAlt) $ do
--       markMatchGroup l matches
--     markOptional GHC.AnnCloseC

--   markAST _ (GHC.HsCmdIf _ _ e1 e2 e3) = do
--     mark GHC.AnnIf
--     markLocated e1
--     markOffset GHC.AnnSemi 0
--     mark GHC.AnnThen
--     markLocated e2
--     markOffset GHC.AnnSemi 1
--     mark GHC.AnnElse
--     markLocated e3

--   markAST _ (GHC.HsCmdLet _ (GHC.L _ binds) e) = do
--     mark GHC.AnnLet
--     markOptional GHC.AnnOpenC
--     markLocalBindsWithLayout binds
--     markOptional GHC.AnnCloseC
--     mark GHC.AnnIn
--     markLocated e

  exact (HsCmdDo a es) = do
    markApiAnn' a al_rest AnnDo
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

-- instance ExactPrint (StmtLR GhcPs GhcPs (LHsCmd GhcPs)) where
instance (ExactPrint (LocatedA body))
   => ExactPrint (StmtLR GhcPs GhcPs (LocatedA body)) where
-- instance ExactPrint (StmtLR GhcPs GhcPs (LocatedA (HsCmd GhcPs))) where
  getAnnotationEntry (LastStmt _ _ _ _)            = NoEntryVal
  getAnnotationEntry (BindStmt an _ _)              = fromAnn an
  getAnnotationEntry (ApplicativeStmt an _ _)       = NoEntryVal
  getAnnotationEntry (BodyStmt an _ _ _)            = NoEntryVal
  getAnnotationEntry (LetStmt an _)                 = fromAnn an
  getAnnotationEntry (ParStmt an _ _ _)             = NoEntryVal
  getAnnotationEntry (TransStmt an _ _ _ _ _ _ _ _) = fromAnn an
  getAnnotationEntry (RecStmt an _ _ _ _ _ _)       = fromAnn an

  -----------------------------------------------------------------

  exact (LastStmt _ body _ _) = do
    debugM $ "LastStmt"
    markAnnotated body

  exact (BindStmt an pat body) = do
    debugM $ "BindStmt"
    markAnnotated pat
    markApiAnn an AnnLarrow
    markAnnotated body

  exact (ApplicativeStmt _ body _) = do
    debugM $ "ApplicativeStmt"
    -- markAnnotated body

  exact (BodyStmt _ body _ _) = do
    debugM $ "BodyStmt"
    markAnnotated body

  exact (LetStmt an binds) = do
    debugM $ "LetStmt"
    markApiAnn an AnnLet
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

  exact (RecStmt _ stmts _ _ _ _ _) = do
    debugM $ "RecStmt"

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
  exact x = error $ "exact CmdLStmt for:"


-- ---------------------------------------------------------------------

instance ExactPrint (ParStmtBlock GhcPs GhcPs) where
  getAnnotationEntry = const NoEntryVal
  exact (ParStmtBlock _ stmts _ _) = markAnnotated stmts

exactTransStmt :: ApiAnn -> Maybe (LHsExpr GhcPs) -> (LHsExpr GhcPs) -> TransForm -> EPP ()
exactTransStmt an by using ThenForm = do
  debugM $ "exactTransStmt:ThenForm"
  markApiAnn an AnnThen
  markAnnotated using
  case by of
    Nothing -> return ()
    Just b -> do
      markApiAnn an AnnBy
      markAnnotated b
exactTransStmt an by using GroupForm = do
  debugM $ "exactTransStmt:GroupForm"
  markApiAnn an AnnThen
  markApiAnn an AnnGroup
  case by of
    Just b -> do
      markApiAnn an AnnBy
      markAnnotated b
  markApiAnn an AnnUsing
  markAnnotated using

-- ---------------------------------------------------------------------

instance ExactPrint (TyClDecl GhcPs) where
  getAnnotationEntry (FamDecl   { })                   = NoEntryVal
  getAnnotationEntry (SynDecl   { tcdSExt = an })      = fromAnn an
  getAnnotationEntry (DataDecl  { tcdDExt = an })      = fromAnn an
  getAnnotationEntry (ClassDecl { tcdCExt = (an, _) }) = fromAnn an

  exact (FamDecl _ decl) = do
    markAnnotated decl

  exact (SynDecl { tcdSExt = an
                 , tcdLName = ltycon, tcdTyVars = tyvars, tcdFixity = fixity
                 , tcdRhs = rhs }) = do
    -- There may be arbitrary parens around parts of the constructor that are
    -- infix.
    -- Turn these into comments so that they feed into the right place automatically
    -- annotationsToComments [GHC.AnnOpenP,GHC.AnnCloseP]
    markApiAnn an AnnType

    -- markTyClass Nothing fixity ln tyvars
    exactVanillaDeclHead an ltycon tyvars fixity Nothing
    markApiAnn an AnnEqual
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
    exactDataDefn an (exactVanillaDeclHead an ltycon tyvars fixity) defn

  -- -----------------------------------

  exact (ClassDecl {tcdCExt = (an, _),
                    tcdCtxt = context, tcdLName = lclas, tcdTyVars = tyvars,
                    tcdFixity = fixity,
                    tcdFDs  = fds,
                    tcdSigs = sigs, tcdMeths = methods,
                    tcdATs = ats, tcdATDefs = at_defs,
                    tcdDocs = docs})
      | null sigs && isEmptyBag methods && null ats && null at_defs -- No "where" part
      = top_matter

      | otherwise       -- Laid out
      = do
          top_matter
          -- markApiAnn an AnnWhere
          markApiAnn an AnnOpenC
          applyListAnnotations
                               (prepareListAnnotationA sigs
                             ++ prepareListAnnotationA (bagToList methods)
                             ++ prepareListAnnotationA ats
                             ++ prepareListAnnotationA at_defs
                             -- ++ prepareListAnnotation docs
                               )
          markApiAnn an AnnCloseC
      where
        top_matter = do
          annotationsToComments (apiAnnAnns an)  [AnnOpenP, AnnCloseP]
          markApiAnn an AnnClass
          exactVanillaDeclHead an lclas tyvars fixity context
          -- markAnnotated fundeps
          markApiAnn an AnnWhere

--   -- -----------------------------------

--   markAST _ (GHC.ClassDecl _ ctx ln (GHC.HsQTvs _ tyVars) fixity fds
--                           sigs meths ats atdefs docs) = do
--     mark GHC.AnnClass
--     markLocated ctx

--     markTyClass Nothing fixity ln tyVars

--     unless (null fds) $ do
--       mark GHC.AnnVbar
--       markListIntercalateWithFunLevel markLocated 2 fds
--     mark GHC.AnnWhere
--     markOptional GHC.AnnOpenC -- '{'
--     markInside GHC.AnnSemi
--     -- AZ:TODO: we end up with both the tyVars and the following body of the
--     -- class defn in annSortKey for the class. This could cause problems when
--     -- changing things.
--     setContext (Set.singleton InClassDecl) $
--       applyListAnnotationsLayout
--                            (prepareListAnnotation sigs
--                          ++ prepareListAnnotation (GHC.bagToList meths)
--                          ++ prepareListAnnotation ats
--                          ++ prepareListAnnotation atdefs
--                          ++ prepareListAnnotation docs
--                            )
--     markOptional GHC.AnnCloseC -- '}'
--     markTrailingSemi
-- {-
--   | ClassDecl { tcdCExt    :: XClassDecl pass,         -- ^ Post renamer, FVs
--                 tcdCtxt    :: LHsContext pass,         -- ^ Context...
--                 tcdLName   :: Located (IdP pass),      -- ^ Name of the class
--                 tcdTyVars  :: LHsQTyVars pass,         -- ^ Class type variables
--                 tcdFixity  :: LexicalFixity, -- ^ Fixity used in the declaration
--                 tcdFDs     :: [Located (FunDep (Located (IdP pass)))],
--                                                         -- ^ Functional deps
--                 tcdSigs    :: [LSig pass],              -- ^ Methods' signatures
--                 tcdMeths   :: LHsBinds pass,            -- ^ Default methods
--                 tcdATs     :: [LFamilyDecl pass],       -- ^ Associated types;
--                 tcdATDefs  :: [LTyFamDefltEqn pass],
--                                                    -- ^ Associated type defaults
--                 tcdDocs    :: [LDocDecl]                -- ^ Haddock docs
--     }

-- -}

--   markAST _ (GHC.SynDecl _ _ (GHC.XLHsQTyVars _) _ _)
--     = error "extension hit for TyClDecl"
--   markAST _ (GHC.DataDecl _ _ (GHC.HsQTvs _ _) _ (GHC.XHsDataDefn _))
--     = error "extension hit for TyClDecl"
--   markAST _ (GHC.DataDecl _ _ (GHC.XLHsQTyVars _) _ _)
--     = error "extension hit for TyClDecl"
--   markAST _ (GHC.ClassDecl _ _ _ (GHC.XLHsQTyVars _) _ _ _ _ _ _ _)
--     = error "extension hit for TyClDecl"
--   markAST _ (GHC.XTyClDecl _)
--     = error "extension hit for TyClDecl"
  exact x = error $ "exact TyClDecl for:" ++ showAst x

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
    exactVanillaDeclHead an ltycon tyvars fixity Nothing
    exact_kind
    mapM_ markAnnotated mb_inj
    case info of
      ClosedTypeFamily mb_eqns -> do
        markApiAnn an AnnWhere
        markApiAnn an AnnOpenC
        case mb_eqns of
          Nothing -> printString False ".."
          Just eqns -> markAnnotated eqns
        markApiAnn an AnnCloseC
      _ -> return ()
    where
      exact_top_level = case top_level of
                          TopLevel    -> markApiAnn an AnnFamily
                          NotTopLevel -> return ()

      exact_kind = case result of
                     NoSig    _         -> return ()
                     KindSig  _ kind    -> markApiAnn an AnnDcolon >> markAnnotated kind
                     TyVarSig _ tv_bndr -> markApiAnn an AnnEqual >> markAnnotated tv_bndr

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

exactFlavour :: ApiAnn -> FamilyInfo GhcPs -> EPP ()
exactFlavour an DataFamily            = markApiAnn an AnnData
exactFlavour an OpenTypeFamily        = markApiAnn an AnnType
exactFlavour an (ClosedTypeFamily {}) = markApiAnn an AnnType

-- instance Outputable (FamilyInfo pass) where
--   ppr info = pprFlavour info <+> text "family"

-- ---------------------------------------------------------------------

exactDataDefn :: ApiAnn
              -> (Maybe (LHsContext GhcPs) -> EPP ()) -- Printing the header
              -> HsDataDefn GhcPs
              -> EPP ()
exactDataDefn an exactHdr
                 (HsDataDefn { dd_ext = an2
                             , dd_ND = new_or_data, dd_ctxt = context
                             , dd_cType = mb_ct
                             , dd_kindSig = mb_sig
                             , dd_cons = condecls, dd_derivs = derivings }) = do
  if new_or_data == DataType
    then markApiAnn an2 AnnData
    else markApiAnn an2 AnnNewtype
  mapM_ markAnnotated mb_ct
  exactHdr context
  case mb_sig of
    Nothing -> return ()
    Just kind -> do
      markApiAnn an AnnDcolon
      markAnnotated kind
  when (isGadt condecls) $ markApiAnn an AnnWhere
  exact_condecls an2 condecls
  mapM_ markAnnotated derivings
  return ()

exactVanillaDeclHead :: ApiAnn
                     -> LocatedN RdrName
                     -> LHsQTyVars GhcPs
                     -> LexicalFixity
                     -> Maybe (LHsContext GhcPs)
                     -> EPP ()
exactVanillaDeclHead an thing (HsQTvs { hsq_explicit = tyvars }) fixity context = do
  let
    exact_tyvars :: [LHsTyVarBndr () GhcPs] -> EPP ()
    exact_tyvars (varl:varsr)
      | fixity == Infix && length varsr > 1 = do
         -- = hsep [char '(',ppr (unLoc varl), pprInfixOcc (unLoc thing)
         --        , (ppr.unLoc) (head varsr), char ')'
         --        , hsep (map (ppr.unLoc) (tail vaprsr))]
          markApiAnnAll an id AnnOpenP
          markAnnotated varl
          markAnnotated thing
          markAnnotated (head varsr)
          markApiAnnAll an id AnnCloseP
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
    markApiAnn an AnnVbar
    markAnnotated lhs
    markApiAnn an AnnRarrow
    mapM_ markAnnotated rhs
    --               Just (L _ (InjectivityAnn _ lhs rhs)) ->
    --                 hsep [ vbar, ppr lhs, text "->", hsep (map ppr rhs) ]
    --               Nothing -> empty

-- ---------------------------------------------------------------------

-- instance ExactPrint (HsTyVarBndr () GhcPs) where
--   getAnnotationEntry (UserTyVar an _ _)     = fromAnn an
--   getAnnotationEntry (KindedTyVar an _ _ _) = fromAnn an
--   exact = withPpr

instance ExactPrint (HsTyVarBndr flag GhcPs) where
  getAnnotationEntry (UserTyVar an _ _)     = fromAnn an
  getAnnotationEntry (KindedTyVar an _ _ _) = fromAnn an

  exact (UserTyVar an _ n)     = do
    markApiAnnAll an id AnnOpenP
    markAnnotated n
    markApiAnnAll an id AnnCloseP
  exact (KindedTyVar an _ n k) = do
    markApiAnnAll an id AnnOpenP
    markAnnotated n
    markApiAnn an AnnDcolon
    markAnnotated k
    markApiAnnAll an id AnnCloseP

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
  getAnnotationEntry (HsOpTy _ _ _ _)          = NoEntryVal
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


  exact (HsForAllTy { hst_xforall = an
                    , hst_tele = tele, hst_body = ty }) = do
    markAnnotated tele
    markAnnotated ty

  exact (HsQualTy _ ctxt ty) = do
    markAnnotated ctxt
    -- markApiAnn an AnnDarrow
    markAnnotated ty
  exact (HsTyVar an promoted name) = do
    when (promoted == IsPromoted) $ markApiAnn an AnnSimpleQuote
    markAnnotated name

  exact x@(HsAppTy _ t1 t2)         = markAnnotated t1 >> markAnnotated t2
  exact x@(HsAppKindTy an _ _)      = withPpr x
  exact x@(HsFunTy an mult ty1 ty2) = do
    markAnnotated ty1
    markArrow an mult
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
  exact (HsOpTy an t1 lo t2) = do
    markAnnotated t1
    markAnnotated lo
    markAnnotated t2
  exact (HsParTy an ty) = do
    markOpeningParen an
    markAnnotated ty
    markClosingParen an
  exact x@(HsIParamTy an n t) = do
      markAnnotated n
      markApiAnn an AnnDcolon
      markAnnotated t
  exact (HsStarTy an isUnicode)
    = if isUnicode
        then printString False "\x2605" -- Unicode star
        else printString False "*"
  exact (HsKindSig an ty k) = do
    exact ty
    markApiAnn an AnnDcolon
    exact k
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
      SrcLazy     -> markApiAnn an AnnTilde
      SrcStrict   -> markApiAnn an AnnBang
      NoSrcStrict -> return ()
    markAnnotated ty
  -- exact x@(HsRecTy an _)            = withPpr x
  exact (HsExplicitListTy an prom tys) = do
    when (isPromoted prom) $ markApiAnn an AnnSimpleQuote
    markApiAnn an AnnOpenS
    markAnnotated tys
    markApiAnn an AnnCloseS
  exact (HsExplicitTupleTy an tys) = do
    markApiAnn an AnnSimpleQuote
    markApiAnn an AnnOpenP
    markAnnotated tys
    markApiAnn an AnnCloseP
  exact (HsTyLit _ lit) = do
    case lit of
      (HsNumTy src v) -> printSourceText src (show v)
      (HsStrTy src v) -> printSourceText src (show v)
  exact (HsWildCardTy _) = printString False "_"
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
    markApiAnn an AnnDeriving
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

  exact (StockStrategy an)    = markApiAnn an AnnStock
  exact (AnyclassStrategy an) = markApiAnn an AnnAnyclass
  exact (NewtypeStrategy an)  = markApiAnn an AnnNewtype
  exact (ViaStrategy (XViaStrategyPs an ty))
    = markApiAnn an AnnVia >> markAnnotated ty

-- ---------------------------------------------------------------------

instance (ExactPrint a) => ExactPrint (LocatedC a) where
  getAnnotationEntry (L sann _) = fromAnn sann

  exact (L (SrcSpanAnn ApiAnnNotUsed _) a) = markAnnotated a
  exact (L (SrcSpanAnn (ApiAnn _ (AnnContext ma opens closes) _) _) a) = do
    -- case ma of
    --   Just (UnicodeSyntax, rs) -> markKw' AnnDarrowU rs
    --   Just (NormalSyntax,  rs) -> markKw' AnnDarrow  rs
    --   Nothing -> pure ()
    mapM_ (markKw' AnnOpenP) (sort opens)
    markAnnotated a
    mapM_ (markKw' AnnCloseP) (sort closes)
    case ma of
      Just (UnicodeSyntax, rs) -> markKw' AnnDarrowU rs
      Just (NormalSyntax,  rs) -> markKw' AnnDarrow  rs
      Nothing -> pure ()

-- ---------------------------------------------------------------------

instance ExactPrint (DerivClauseTys GhcPs) where
  getAnnotationEntry = const NoEntryVal

  exact (DctSingle _ ty) = markAnnotated ty
  exact (DctMulti _ tys) = do
    -- parens (interpp'SP tys)
    markAnnotated tys

-- ---------------------------------------------------------------------

instance ExactPrint (LocatedN RdrName) where
  getAnnotationEntry (L sann _) = fromAnn sann

  exact (L (SrcSpanAnn ApiAnnNotUsed _) n) = do
    printString False (showPprUnsafe n)
  exact (L (SrcSpanAnn (ApiAnn _anchor ann _cs) ll) n) = do
    case ann of
      NameAnn a o l c t -> do
        markName a o (Just (l,n)) c
        markTrailing t
      NameAnnCommas a o cs c t -> do
        let (kwo,kwc) = adornments a
        markKw (AddApiAnn kwo o)
        forM_ cs (\loc -> markKw (AddApiAnn AnnComma loc))
        markKw (AddApiAnn kwc c)
        markTrailing t
      NameAnnOnly a o c t -> do
        markName a o Nothing c
        markTrailing t
      NameAnnRArrow nl t -> do
        markKw (AddApiAnn AnnRarrow nl)
        markTrailing t
      NameAnnQuote q name t -> do
        markKw (AddApiAnn AnnSimpleQuote q)
        markAnnotated (L (SrcSpanAnn name ll) n)
        markTrailing t
      NameAnnTrailing t -> do
        printString False (showPprUnsafe n)
        markTrailing t

markName :: NameAdornment
         -> RealSrcSpan -> Maybe (RealSrcSpan,RdrName) -> RealSrcSpan -> EPP ()
markName adorn open mname close = do
  let (kwo,kwc) = adornments adorn
  markKw (AddApiAnn kwo open)
  case mname of
    Nothing -> return ()
    Just (name, a) -> printStringAtKw' name (showPprUnsafe a)
  markKw (AddApiAnn kwc close)

adornments :: NameAdornment -> (AnnKeywordId, AnnKeywordId)
adornments NameParens     = (AnnOpenP, AnnCloseP)
adornments NameParensHash = (AnnOpenPH, AnnClosePH)
adornments NameBackquotes = (AnnBackquote, AnnBackquote)
adornments NameSquare     = (AnnOpenS, AnnCloseS)

markTrailing :: [TrailingAnn] -> EPP ()
markTrailing ts = do
  p <- getPos
  debugM $ "markTrailing:" ++ showPprUnsafe (p,ts)
  mapM_ markKwT (sort ts)

-- ---------------------------------------------------------------------

-- based on pp_condecls in Decls.hs
exact_condecls :: ApiAnn -> [LConDecl GhcPs] -> EPP ()
exact_condecls an cs
  | gadt_syntax                  -- In GADT syntax
  -- = hang (text "where") 2 (vcat (map ppr cs))
  = do
      -- printString False "exact_condecls:gadt"
      mapM_ markAnnotated cs
  | otherwise                    -- In H98 syntax
  -- = equals <+> sep (punctuate (text " |") (map ppr cs))
  = do
      -- printString False "exact_condecls:not gadt"
      markApiAnn an AnnEqual
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
    when has_forall $ markApiAnn an AnnForall
    mapM_ markAnnotated ex_tvs
    when has_forall $ markApiAnn an AnnDot
    -- exactHsForall (mkHsForAllInvisTele ex_tvs) mcxt
    mapM_ markAnnotated mcxt
    when (isJust mcxt) $ markApiAnn an AnnDarrow

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
      exact_details (PrefixCon tys) = do
        markAnnotated con
        mapM_  markAnnotated tys
      exact_details (RecCon fields) = do
        markAnnotated con
        markAnnotated fields

  -- -----------------------------------

  exact (ConDeclGADT { con_g_ext = an
                     , con_names = cons
                     , con_forall = has_forall
                     , con_qvars = qvars
                     , con_mb_cxt = mcxt, con_args = args
                     , con_res_ty = res_ty, con_doc = doc }) = do
    mapM_ markAnnotated doc
    mapM_ markAnnotated cons
    markApiAnn an AnnDcolon
    annotationsToComments (apiAnnAnns an)  [AnnOpenP, AnnCloseP]
    when has_forall $ markApiAnn an AnnForall
    mapM_ markAnnotated qvars
    when has_forall $ markApiAnn an AnnDot
    mapM_ markAnnotated mcxt
    when (isJust mcxt) $ markApiAnn an AnnDarrow
    -- mapM_ markAnnotated args
    case args of
        (PrefixCon args) -> mapM_ markAnnotated args
        (RecCon fields)  -> markAnnotated fields
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

instance ExactPrint (ConDeclField GhcPs) where
  getAnnotationEntry f@(ConDeclField{}) = fromAnn (cd_fld_ext f)

  exact (ConDeclField an names ftype mdoc) = do
    markAnnotated names
    markApiAnn an AnnDcolon
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
  exact (HsScaled _arr t) = markAnnotated t

-- ---------------------------------------------------------------------

-- instance ExactPrint (LHsContext GhcPs) where
--   getAnnotationEntry (L (SrcSpanAnn ann _) _) = fromAnn ann
--   exact = withPpr

-- ---------------------------------------------------------------------

instance ExactPrint (LocatedP CType) where
  getAnnotationEntry = entryFromLocatedA

  exact (L (SrcSpanAnn ApiAnnNotUsed _) ct) = withPpr ct
  exact (L (SrcSpanAnn an ll)
         (CType stp mh (stct,ct))) = do
    markAnnOpenP an stp "{-# CTYPE"
    case mh of
      Nothing -> return ()
      Just (Header srcH _h) ->
         markLocatedAALS an apr_rest AnnHeader (Just (toSourceTextWithSuffix srcH "" ""))
    markLocatedAALS an apr_rest AnnVal (Just (toSourceTextWithSuffix stct (unpackFS ct) ""))
    markAnnCloseP an

-- instance Annotate GHC.CType where
--   markAST _ (GHC.CType src mh f) = do
--     -- markWithString GHC.AnnOpen src
--     markAnnOpen src ""
--     case mh of
--       Nothing -> return ()
--       Just (GHC.Header srcH _h) ->
--          -- markWithString GHC.AnnHeader srcH
--          markWithString GHC.AnnHeader (toSourceTextWithSuffix srcH "" "")
--     -- markWithString GHC.AnnVal (fst f)
--     markSourceText  (fst f) (GHC.unpackFS $ snd f)
--     markWithString GHC.AnnClose "#-}"

-- ---------------------------------------------------------------------

instance ExactPrint (SourceText, RuleName) where
  -- We end up at the right place from the Located wrapper
  getAnnotationEntry = const NoEntryVal

  exact (st, rn)
    = printString False (toSourceTextWithSuffix st (unpackFS rn) "")


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
--     markApiAnnAll an al_rest AnnSemi
--     markAnnotated b
--     markLocatedMAA an al_close

instance ExactPrint (LocatedL [LocatedA (IE GhcPs)]) where
  getAnnotationEntry = entryFromLocatedA

  exact (L (SrcSpanAnn ann _) ies) = do
    debugM $ "LocatedL [LIE"
    markLocatedAAL ann al_rest AnnHiding
    p <- getPos
    debugM $ "LocatedL [LIE:p=" ++ showPprUnsafe p
    markAnnList ann (markAnnotated ies)

-- AZ:TODO: combine with next instance
instance ExactPrint (LocatedL [LocatedA (Match GhcPs (LocatedA (HsExpr GhcPs)))]) where
  getAnnotationEntry = entryFromLocatedA
  exact (L la a) = do
    debugM $ "LocatedL [LMatch"
    -- TODO: markAnnList?
    markApiAnnAll (ann la) al_rest AnnWhere
    markLocatedMAA (ann la) al_open
    markApiAnnAll (ann la) al_rest AnnSemi
    markAnnotated a
    markLocatedMAA (ann la) al_close

instance ExactPrint (LocatedL [LocatedA (Match GhcPs (LocatedA (HsCmd GhcPs)))]) where
  getAnnotationEntry = entryFromLocatedA
  exact (L la a) = do
    debugM $ "LocatedL [LMatch"
    -- TODO: markAnnList?
    markApiAnnAll (ann la) al_rest AnnWhere
    markLocatedMAA (ann la) al_open
    markApiAnnAll (ann la) al_rest AnnSemi
    markAnnotated a
    markLocatedMAA (ann la) al_close

-- instance ExactPrint (LocatedL [ExprLStmt GhcPs]) where
instance ExactPrint (LocatedL [LocatedA (StmtLR GhcPs GhcPs (LocatedA (HsExpr GhcPs)))]) where
  getAnnotationEntry = entryFromLocatedA
  exact (L (SrcSpanAnn an _) stmts) = do
    debugM $ "LocatedL [ExprLStmt"
    markAnnList an $ do
      -- markLocatedMAA an al_open
      case snocView stmts of
        Just (initStmts, ls@(L _ (LastStmt _ body _ _))) -> do
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
    markAnnList an (mapM_ markAnnotated fs) -- AZ:TODO get rid of mapM_

instance ExactPrint (LocatedL (BF.BooleanFormula (LocatedN RdrName))) where
  getAnnotationEntry = entryFromLocatedA
  exact (L (SrcSpanAnn an _) bf) = do
    debugM $ "LocatedL [LBooleanFormula"
    markAnnList an (markAnnotated bf)

-- ---------------------------------------------------------------------
-- LocatedL instances end --
-- =====================================================================

instance ExactPrint (IE GhcPs) where
  getAnnotationEntry (IEVar _ _)              = NoEntryVal
  getAnnotationEntry (IEThingAbs an _)        = fromAnn an
  getAnnotationEntry (IEThingAll an _)        = fromAnn an
  getAnnotationEntry (IEThingWith an _ _ _ _) = fromAnn an
  getAnnotationEntry (IEModuleContents an _)  = fromAnn an
  getAnnotationEntry (IEGroup _ _ _)          = NoEntryVal
  getAnnotationEntry (IEDoc _ _)              = NoEntryVal
  getAnnotationEntry (IEDocNamed _ _)         = NoEntryVal

  exact (IEVar _ ln) = markAnnotated ln
  exact (IEThingAbs _ thing) = markAnnotated thing
  exact (IEThingAll an thing) = do
    markAnnotated thing
    markApiAnn an AnnOpenP
    markApiAnn an AnnDotdot
    markApiAnn an AnnCloseP

  exact (IEThingWith an thing wc withs flds) = do
    markAnnotated thing
    markApiAnn an AnnOpenP
    case wc of
      NoIEWildcard -> markAnnotated withs
      IEWildcard pos -> do
        let (bs, as) = splitAt pos withs
        markAnnotated bs
        markApiAnn an AnnDotdot
        markApiAnn an AnnComma
        markAnnotated as
    markApiAnn an AnnCloseP

  exact (IEModuleContents an (L lm mn)) = do
    markApiAnn an AnnModule
    printStringAtSs lm (moduleNameString mn)

  -- exact (IEGroup _ _ _)          = NoEntryVal
  -- exact (IEDoc _ _)              = NoEntryVal
  -- exact (IEDocNamed _ _)         = NoEntryVal
  exact x = error $ "missing match for IE:" ++ showAst x

-- ---------------------------------------------------------------------

instance ExactPrint (IEWrappedName RdrName) where
  getAnnotationEntry = const NoEntryVal

  exact (IEName n) = markAnnotated n
  exact (IEPattern r n) = do
    printStringAtKw' r "pattern"
    markAnnotated n
  exact (IEType r n) = do
    printStringAtKw' r "type"
    markAnnotated n

-- markIEWrapped :: ApiAnn -> LIEWrappedName RdrName -> EPP ()
-- markIEWrapped an (L _ (IEName n))
--   = markAnnotated n
-- markIEWrapped an (L _ (IEPattern n))
--   = markApiAnn an AnnPattern >> markAnnotated n
-- markIEWrapped an (L _ (IEType n))
--   = markApiAnn an AnnType    >> markAnnotated n

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
  getAnnotationEntry (ParPat an _)            = fromAnn an
  getAnnotationEntry (BangPat an _)           = fromAnn an
  getAnnotationEntry (ListPat an _)           = fromAnn an
  getAnnotationEntry (TuplePat an _ _)        = fromAnn an
  getAnnotationEntry (SumPat an _ _ _)        = fromAnn an
  getAnnotationEntry (ConPat an _ _)          = fromAnn an
  getAnnotationEntry (ViewPat an _ _)         = fromAnn an
  getAnnotationEntry (SplicePat _ _)          = NoEntryVal
  getAnnotationEntry (LitPat _ _)             = NoEntryVal
  getAnnotationEntry (NPat _ _ _ _)           = NoEntryVal
  getAnnotationEntry (NPlusKPat an _ _ _ _ _) = fromAnn an
  getAnnotationEntry (SigPat an _ _)          = fromAnn an


  exact (WildPat _) = printString False "_"
  exact (VarPat _ n) = do
        -- The parser inserts a placeholder value for a record pun rhs. This must be
        -- filtered.
        let pun_RDR = "pun-right-hand-side"
        when (showPprUnsafe n /= pun_RDR) $ markAnnotated n
  -- | LazyPat an pat)
  exact (AsPat an n pat) = do
    markAnnotated n
    markApiAnn an AnnAt
    markAnnotated pat
  exact (ParPat an pat) = do
    markAnnKw an ap_open AnnOpenP
    markAnnotated pat
    markAnnKw an ap_close AnnCloseP

  -- | BangPat an pat)
  exact (ListPat an pats) = markAnnList an (markAnnotated pats)

  exact (TuplePat an pats boxity) = do
    case boxity of
      Boxed   -> markApiAnn an AnnOpenP
      Unboxed -> markApiAnn an AnnOpenPH
    markAnnotated pats
    case boxity of
      Boxed   -> markApiAnn an AnnCloseP
      Unboxed -> markApiAnn an AnnClosePH

  exact (SumPat an pat alt arity) = do
    markLocatedAAL an sumPatParens AnnOpenPH
    markAnnKwAll an sumPatVbarsBefore AnnVbar
    markAnnotated pat
    markAnnKwAll an sumPatVbarsAfter AnnVbar
    markLocatedAAL an sumPatParens AnnClosePH
      -- markPat _ (GHC.SumPat _ pat alt arity) = do
      --   markWithString GHC.AnnOpen "(#"
      --   replicateM_ (alt - 1) $ mark GHC.AnnVbar
      --   markLocated pat
      --   replicateM_ (arity - alt) $ mark GHC.AnnVbar
      --   markWithString GHC.AnnClose "#)"

  -- | ConPat an con args)
  exact (ConPat an con details) = exactUserCon an con details
  exact (ViewPat an expr pat) = do
    markAnnotated expr
    markApiAnn an AnnRarrow
    markAnnotated pat
  exact (SplicePat _ splice) = markAnnotated splice
  exact (LitPat _ lit) = printString False (hsLit2String lit)
  exact (NPat an ol mn _) = do
    when (isJust mn) $ markApiAnn an AnnMinus
    markAnnotated ol

  -- | NPlusKPat an n lit1 lit2 _ _)
  exact (SigPat an pat sig) = do
    markAnnotated pat
    markApiAnn an AnnDcolon
    markAnnotated sig
  -- exact x = withPpr x
  exact x = error $ "missing match for Pat:" ++ showAst x

-- instance Annotate (GHC.Pat GHC.GhcPs) where
--   markAST loc typ = do
--     markPat loc typ
--     inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma `debug` ("AnnComma in Pat")
--     where
--       markPat l (GHC.WildPat _) = markExternal l GHC.AnnVal "_"
--       markPat l (GHC.VarPat _ n) = do
--         -- The parser inserts a placeholder value for a record pun rhs. This must be
--         -- filtered out until https://ghc.haskell.org/trac/ghc/ticket/12224 is
--         -- resolved, particularly for pretty printing where annotations are added.
--         let pun_RDR = "pun-right-hand-side"
--         when (showPprUnsafe n /= pun_RDR) $
--           unsetContext Intercalate $ setContext (Set.singleton PrefixOp) $ markAST l (GHC.unLoc n)
--           -- unsetContext Intercalate $ setContext (Set.singleton PrefixOp) $ markLocated n
--       markPat _ (GHC.LazyPat _ p) = do
--         mark GHC.AnnTilde
--         markLocated p

--       markPat _ (GHC.AsPat _ ln p) = do
--         markLocated ln
--         mark GHC.AnnAt
--         markLocated p

--       markPat _ (GHC.ParPat _ p) = do
--         mark GHC.AnnOpenP
--         markLocated p
--         mark GHC.AnnCloseP

--       markPat _ (GHC.BangPat _ p) = do
--         mark GHC.AnnBang
--         markLocated p

--       markPat _ (GHC.ListPat _ ps) = do
--         mark GHC.AnnOpenS
--         markListIntercalateWithFunLevel markLocated 2 ps
--         mark GHC.AnnCloseS

--       markPat _ (GHC.TuplePat _ pats b) = do
--         if b == GHC.Boxed then mark GHC.AnnOpenP
--                           else markWithString GHC.AnnOpen "(#"
--         markListIntercalateWithFunLevel markLocated 2 pats
--         if b == GHC.Boxed then mark GHC.AnnCloseP
--                           else markWithString GHC.AnnClose "#)"

--       markPat _ (GHC.SumPat _ pat alt arity) = do
--         markWithString GHC.AnnOpen "(#"
--         replicateM_ (alt - 1) $ mark GHC.AnnVbar
--         markLocated pat
--         replicateM_ (arity - alt) $ mark GHC.AnnVbar
--         markWithString GHC.AnnClose "#)"

--       markPat _ (GHC.ConPatIn n dets) = do
--         markHsConPatDetails n dets

--       markPat _ GHC.ConPatOut {} =
--         traceM "warning: ConPatOut Introduced after renaming"

--       markPat _ (GHC.ViewPat _ e pat) = do
--         markLocated e
--         mark GHC.AnnRarrow
--         markLocated pat

--       markPat l (GHC.SplicePat _ s) = do
--         markAST l s

--       markPat l (GHC.LitPat _ lp) = markAST l lp

--       markPat _ (GHC.NPat _ ol mn _) = do
--         when (isJust mn) $ mark GHC.AnnMinus
--         markLocated ol

--       markPat _ (GHC.NPlusKPat _ ln ol _ _ _) = do
--         markLocated ln
--         markWithString GHC.AnnVal "+"  -- "+"
--         markLocated ol


--       markPat _ (GHC.SigPat _ pat ty) = do
--         markLocated pat
--         mark GHC.AnnDcolon
--         markLHsSigWcType ty

--       markPat _ GHC.CoPat {} =
--         traceM "warning: CoPat introduced after renaming"

--       markPat _ (GHC.XPat (GHC.L l p)) = markPat l p
--       -- markPat _ (GHC.XPat x) = error $ "got XPat for:" ++ showPprUnsafe x


-- ---------------------------------------------------------------------

instance ExactPrint (HsPatSigType GhcPs) where
  getAnnotationEntry = const NoEntryVal

  exact (HsPS _ ty) = markAnnotated ty

-- ---------------------------------------------------------------------

instance ExactPrint (HsOverLit GhcPs) where
  getAnnotationEntry = const NoEntryVal

  exact ol =
    let str = case ol_val ol of
                HsIntegral   (IL src _ _) -> src
                HsFractional (FL src _ _) -> src
                HsIsString src _ -> src
    in
      case str of
        SourceText s -> printString False s
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
    HsRat        _ (FL src _ v) _ -> toSourceTextWithSuffix src v ""
    HsFloatPrim  _ (FL src _ v)   -> toSourceTextWithSuffix src v "#"
    HsDoublePrim _ (FL src _ v)   -> toSourceTextWithSuffix src v "##"
    -- (XLit x) -> error $ "got XLit for:" ++ showPprUnsafe x

toSourceTextWithSuffix :: (Show a) => SourceText -> a -> String -> String
toSourceTextWithSuffix (NoSourceText)    alt suffix = show alt ++ suffix
toSourceTextWithSuffix (SourceText txt) _alt suffix = txt ++ suffix

sourceTextToString :: SourceText -> String -> String
sourceTextToString NoSourceText alt   = alt
sourceTextToString (SourceText txt) _ = txt

-- ---------------------------------------------------------------------

exactUserCon :: (ExactPrint con) => ApiAnn -> con -> HsConPatDetails GhcPs -> EPP ()
exactUserCon _  c (InfixCon p1 p2) = markAnnotated p1 >> markAnnotated c >> markAnnotated p2
exactUserCon an c details          = do
  markAnnotated c
  markApiAnn an AnnOpenC
  exactConArgs details
  markApiAnn an AnnCloseC


exactConArgs ::HsConPatDetails GhcPs -> EPP ()
exactConArgs (PrefixCon pats) = markAnnotated pats
exactConArgs (InfixCon p1 p2) = markAnnotated p1 >> markAnnotated p2
exactConArgs (RecCon rpats)   = markAnnotated rpats

-- ---------------------------------------------------------------------

entryFromLocatedA :: LocatedAn ann a -> Entry
entryFromLocatedA (L la _) = fromAnn la


-- =====================================================================
-- Utility stuff
-- annNone :: Annotation
-- annNone = Ann (DP (0,0)) [] [] [] Nothing Nothing

-- -- ---------------------------------------------------------------------
-- -- | Calculates the distance from the start of a string to the end of
-- -- a string.
-- dpFromString ::  String -> DeltaPos
-- dpFromString xs = dpFromString' xs 0 0
--   where
--     dpFromString' "" line col = DP (line, col)
--     dpFromString' ('\n': cs) line _   = dpFromString' cs (line + 1) 0
--     dpFromString' (_:cs)     line col = dpFromString' cs line       (col + 1)

-- ---------------------------------------------------------------------
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------

-- | Put the provided context elements into the existing set with fresh level
-- counts
-- setAcs :: Set.Set AstContext -> AstContextSet -> AstContextSet
-- setAcs ctxt acs = setAcsWithLevel ctxt 3 acs

-- -- | Put the provided context elements into the existing set with given level
-- -- counts
-- -- setAcsWithLevel :: Set.Set AstContext -> Int -> AstContextSet -> AstContextSet
-- -- setAcsWithLevel ctxt level (ACS a) = ACS a'
-- --   where
-- --     upd s (k,v) = Map.insert k v s
-- --     a' = foldl' upd a $ zip (Set.toList ctxt) (repeat level)
-- setAcsWithLevel :: (Ord a) => Set.Set a -> Int -> ACS' a -> ACS' a
-- setAcsWithLevel ctxt level (ACS a) = ACS a'
--   where
--     upd s (k,v) = Map.insert k v s
--     a' = foldl' upd a $ zip (Set.toList ctxt) (repeat level)

-- ---------------------------------------------------------------------
-- | Remove the provided context element from the existing set
-- unsetAcs :: AstContext -> AstContextSet -> AstContextSet
-- unsetAcs :: (Ord a) => a -> ACS' a -> ACS' a
-- unsetAcs ctxt (ACS a) = ACS $ Map.delete ctxt a

-- ---------------------------------------------------------------------

-- | Are any of the contexts currently active?
-- inAcs :: Set.Set AstContext -> AstContextSet -> Bool
-- inAcs :: (Ord a) => Set.Set a -> ACS' a -> Bool
-- inAcs ctxt (ACS a) = not $ Set.null $ Set.intersection ctxt (Set.fromList $ Map.keys a)

-- -- | propagate the ACS down a level, dropping all values which hit zero
-- -- pushAcs :: AstContextSet -> AstContextSet
-- pushAcs :: ACS' a -> ACS' a
-- pushAcs (ACS a) = ACS $ Map.mapMaybe f a
--   where
--     f n
--       | n <= 1    = Nothing
--       | otherwise = Just (n - 1)

-- |Sometimes we have to pass the context down unchanged. Bump each count up by
-- one so that it is unchanged after a @pushAcs@ call.
-- bumpAcs :: AstContextSet -> AstContextSet
-- bumpAcs :: ACS' a -> ACS' a
-- bumpAcs (ACS a) = ACS $ Map.mapMaybe f a
--   where
--     f n = Just (n + 1)


-- ---------------------------------------------------------------------
-- ---------------------------------------------------------------------

-- printStringAtMaybeAnn :: (Monad m, Monoid w) => KeywordId -> Maybe String -> EP w m ()
-- printStringAtMaybeAnn an mstr = printStringAtMaybeAnnThen an mstr (return ())

-- -- printStringAtMaybeAnnAll :: (Monad m, Monoid w) => KeywordId -> Maybe String -> EP w m ()
-- -- printStringAtMaybeAnnAll an mstr = go
-- --   where
-- --     go = printStringAtMaybeAnnThen an mstr go

-- printStringAtMaybeAnnThen :: (Monad m, Monoid w)
--                           => KeywordId -> Maybe String -> EP w m () -> EP w m ()
-- printStringAtMaybeAnnThen an mstr next = do
--   let str = fromMaybe (keywordToString an) mstr
--   annFinal <- getAnnFinal an
--   case (annFinal, an) of
--     -- Could be unicode syntax
--     -- TODO: This is a bit fishy, refactor
--     (Nothing, G kw') -> do
--       let kw = unicodeAnn kw'
--       let str' = fromMaybe (keywordToString (G kw)) mstr
--       res <- getAnnFinal (G kw)
--       return () `debug` ("printStringAtMaybeAnn:missed:Unicode:(an,res)" ++ show (an,res))
--       unless (null res) $ do
--         forM_
--           res
--           (\(comments, ma) -> printStringAtLsDelta comments ma str')
--         next
--     (Just (comments, ma),_) -> printStringAtLsDelta comments ma str >> next
--     (Nothing, _) -> return () `debug` ("printStringAtMaybeAnn:missed:(an)" ++ show an)
--                     -- Note: do not call next, nothing to chain
--     -- ++AZ++: Enabling the following line causes a very weird error associated with AnnPackageName. I suspect it is because it is forcing the evaluation of a non-existent an or str
--     -- `debug` ("printStringAtMaybeAnn:(an,ma,str)=" ++ show (an,ma,str))

-- ---------------------------------------------------------------------

-- |This should be the final point where things are mode concrete,
-- before output. Hence the point where comments can be inserted
printStringAtLsDelta :: (Monad m, Monoid w) => [(Comment, DeltaPos)] -> DeltaPos -> String -> EP w m ()
printStringAtLsDelta cs cl s = do
  p <- getPos
  colOffset <- getLayoutOffset
  if isGoodDeltaWithOffset cl colOffset
    then do
      mapM_ (uncurry printQueuedComment) cs
      printStringAt (undelta p cl colOffset) s
        `debug` ("printStringAtLsDelta:(pos,s):" ++ show (undelta p cl colOffset,s))
    else return () `debug` ("printStringAtLsDelta:bad delta for (mc,s):" ++ show (cl,s))

-- ---------------------------------------------------------------------

-- -- |destructive get, hence use an annotation once only
-- getAnnFinal :: (Monad m, Monoid w)
--   => KeywordId -> EP w m (Maybe ([(Comment, DeltaPos)], DeltaPos))
-- getAnnFinal kw = do
--   kd <- gets epAnnKds
--   case kd of
--     []    -> return Nothing -- Should never be triggered
--     (k:kds) -> do
--       let (res, kd') = destructiveGetFirst kw ([],k)
--       modify (\s -> s { epAnnKds = kd' : kds })
--       return res

-- -- | Get and remove the first item in the (k,v) list for which the k matches.
-- -- Return the value, together with any comments skipped over to get there.
-- destructiveGetFirst :: KeywordId
--                     -> ([(KeywordId, v)],[(KeywordId,v)])
--                     -> (Maybe ([(Comment, v)], v),[(KeywordId,v)])
-- destructiveGetFirst _key (acc,[]) = (Nothing, acc)
-- destructiveGetFirst  key (acc, (k,v):kvs )
--   | k == key = (Just (skippedComments, v), others ++ kvs)
--   | otherwise = destructiveGetFirst key (acc ++ [(k,v)], kvs)
--   where
--     (skippedComments, others) = foldr comments ([], []) acc
--     comments (AnnComment comment' , dp ) (cs, kws) = ((comment', dp) : cs, kws)
--     comments kw (cs, kws)                          = (cs, kw : kws)



isGoodDeltaWithOffset :: DeltaPos -> LayoutStartCol -> Bool
isGoodDeltaWithOffset dp colOffset = isGoodDelta (DP (undelta (0,0) dp colOffset))

printQueuedComment :: (Monad m, Monoid w) => Comment -> DeltaPos -> EP w m ()
printQueuedComment Comment{commentContents} dp = do
  p <- getPos
  colOffset <- getLayoutOffset
  let (dr,dc) = undelta (0,0) dp colOffset
  debugM $ "printQueuedComment: (p,dp,colOffset,undelta)=" ++ show (p,dp,colOffset,undelta p dp colOffset)
  -- do not lose comments against the left margin
  when (isGoodDelta (DP (dr,max 0 dc))) $
    printCommentAt (undelta p dp colOffset) commentContents


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
  local (\s -> s { epAnn = a, epContext = pushAcs (epContext s) })


-- ---------------------------------------------------------------------
--
-- Necessary as there are destructive gets of Kds across scopes
-- withKds :: (Monad m, Monoid w) => [(KeywordId, DeltaPos)] -> EP w m a -> EP w m a
-- withKds kd action = do
--   modify (\s -> s { epAnnKds = kd : epAnnKds s })
--   r <- action
--   modify (\s -> s { epAnnKds = tail (epAnnKds s) })
--   return r

------------------------------------------------------------------------

setLayout :: (Monad m, Monoid w) => EP w m () -> EP w m ()
setLayout k = do
  oldLHS <- gets epLHS
  modify (\a -> a { epMarkLayout = True } )
  let reset = modify (\a -> a { epMarkLayout = False
                              , epLHS = oldLHS } )
  k <* reset

getPos :: (Monad m, Monoid w) => EP w m Pos
getPos = gets epPos

setPos :: (Monad m, Monoid w) => Pos -> EP w m ()
setPos l = modify (\s -> s {epPos = l})

getUnallocatedComments :: (Monad m, Monoid w) => EP w m [Comment]
getUnallocatedComments = gets epComments

putUnallocatedComments :: (Monad m, Monoid w) => [Comment] -> EP w m ()
putUnallocatedComments cs = modify (\s -> s { epComments = cs } )

-- |Get the current column offset
getLayoutOffset :: (Monad m, Monoid w) => EP w m LayoutStartCol
getLayoutOffset = gets epLHS

getEofPos :: (Monad m, Monoid w) => EP w m RealSrcSpan
getEofPos = do
  as <- gets epApiAnns
  case apiAnnEofPos as of
    Nothing -> return placeholderRealSpan
    Just ss -> return ss

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
advance cl = do
  p <- getPos
  colOffset <- getLayoutOffset
  debugM $ "advance:(p,colOffset,ws)=" ++ show (p,colOffset,undelta p cl colOffset)
  printWhitespace (undelta p cl colOffset)

-- getAndRemoveAnnotation :: (Monad m, Monoid w, Data a) => GHC.Located a -> EP w m (Maybe Annotation)
-- getAndRemoveAnnotation a = gets (getAnnotationEP a . epAnns)

-- ---------------------------------------------------------------------

-- adjustDeltaForOffsetM :: DeltaPos -> EPP DeltaPos
-- adjustDeltaForOffsetM dp = do
--   colOffset <- gets epLHS
--   return (adjustDeltaForOffset colOffset dp)

adjustDeltaForOffset :: LayoutStartCol -> DeltaPos -> DeltaPos
adjustDeltaForOffset _colOffset              dp@(DP (0,_)) = dp -- same line
adjustDeltaForOffset (LayoutStartCol colOffset) (DP (l,c)) = DP (l,c - colOffset)

-- ---------------------------------------------------------------------
-- Printing functions




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

  -- Debug stuff
  -- pp <- getPos
  -- debugM $ "printString: (p,pp,str)" ++ show (p,pp,str)
  -- Debug end

  --
  if not layout && c == 0
    then lift (epWhitespacePrint str) >>= \s -> tell EPWriter { output = s}
    else lift (epTokenPrint      str) >>= \s -> tell EPWriter { output = s}


newLine :: (Monad m, Monoid w) => EP w m ()
newLine = do
    (l,_) <- getPos
    printString False "\n"
    setPos (l+1,1)

padUntil :: (Monad m, Monoid w) => Pos -> EP w m ()
padUntil (l,c) = do
    (l1,c1) <- getPos
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
printStringAt p str = printWhitespace p >> printString False str
