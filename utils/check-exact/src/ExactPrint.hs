{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module ExactPrint
  (
    ExactPrint(..)
  , exactPrint
  -- , exactPrintWithOptions
  , showGhc
  ) where

import GHC
import GHC.Data.Bag
-- import GHC.Hs.Exact
-- import GHC.Hs.Extension
-- import GHC.Parser.Lexer (AddApiAnn(..))
import GHC.Types.Basic hiding (EP)
-- import GHC.Types.Name.Reader
import GHC.Types.SrcLoc
import GHC.Utils.Outputable hiding ( (<>) )
import GHC.Types.ForeignCall

import Control.Monad.Identity
import Control.Monad.RWS
import Data.Data ( Data )
import Data.Foldable
import Data.List ( partition, intercalate, sort, sortBy )
import Data.Maybe (fromMaybe)
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
markAnnotated a = enterAnn (getApiAnnotation a) a

data Entry = Entry RealSrcSpan [RealLocated AnnotationComment]
           | NoEntryVal

instance (HasEntry (ApiAnn' ann)) =>  HasEntry (SrcSpanAnn' (ApiAnn' ann)) where
  fromAnn (SrcSpanAnn ApiAnnNotUsed ss) = Entry (realSrcSpan ss) []
  fromAnn (SrcSpanAnn ann _) = fromAnn ann

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
  addComments cs
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

addComments :: [RealLocated AnnotationComment] -> EPP ()
addComments csNew = do
  cs <- getUnallocatedComments
  -- AZ:TODO: sortedlist?
  putUnallocatedComments (sort $ (map tokComment csNew) ++ cs)

-- ---------------------------------------------------------------------

-- Temporary function to simply reproduce the "normal" pretty printer output
withPpr :: (Outputable a) => a -> Annotated ()
withPpr a = printString False (showGhc a)

-- ---------------------------------------------------------------------
-- Modeled on Outputable

-- | An AST fragment with an annotation must be able to return the
-- requirements for nesting another one, captured in an 'Entry', and
-- to be able to use the rest of the exactprint machinery to print the
-- element.  In the analogy to Outputable, 'exact' plays the role of
-- 'ppr'.
class ExactPrint a where
  getApiAnnotation :: a -> Entry
  exact :: a -> Annotated ()

-- ---------------------------------------------------------------------

-- | Bare Located elements are simply stripped off without further
-- processing.
instance (ExactPrint a) => ExactPrint (Located a) where
  getApiAnnotation (L l _) = Entry (realSrcSpan l) []
  exact (L _ a) = markAnnotated a

instance (ExactPrint a) => ExactPrint (LocatedA a) where
  getApiAnnotation = entryFromLocatedA
  exact (L la a) = do
    markAnnotated a
    markALocatedA (ann la)

-- ---------------------------------------------------------------------

-- | 'Located (HsModule GhcPs)' corresponds to 'ParsedSource'
instance ExactPrint HsModule where
  getApiAnnotation hsmod = fromAnn (hsmodAnn hsmod)

  exact hsmod@(HsModule ApiAnnNotUsed _ _ _ _ _ _) = withPpr hsmod
  exact (HsModule anns@(ApiAnn ss as cs) mmn mexports imports decls mdeprec mbDoc) = do

    case mmn of
      Nothing -> return ()
      Just (L ln mn) -> do
        markApiAnn' anns am_main AnnModule
        debugM $ "HsModule name: (ss,ln)=" ++ show (ss2pos ss,ss2pos (realSrcSpan ln))
        printStringAtSs ln (moduleNameString mn)

        -- forM_ mdeprec markLocated

        forM_ mexports markAnnotated

        markApiAnn' anns am_main AnnWhere
        -- markApiAnn (am_main anns) AnnWhere

    -- markOptional GHC.AnnOpenC -- Possible '{'
    -- markManyOptional GHC.AnnSemi -- possible leading semis
    -- setContextLevel (Set.singleton TopLevel) 2 $ markListWithLayout imports
    markListWithLayout imports

    -- setContextLevel (Set.singleton TopLevel) 2 $ markListWithLayout decls
    markListWithLayout decls

    -- markOptional GHC.AnnCloseC -- Possible '}'

    -- markEOF
    eof <- getEofPos
    printStringAtKw' eof ""


-- ---------------------------------------------------------------------
-- Start of utility functions
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

markLocatedMAA :: ApiAnn' a -> (a -> Maybe AddApiAnn) -> EPP ()
markLocatedMAA ApiAnnNotUsed  _  = return ()
markLocatedMAA (ApiAnn _ a _) f =
  case f a of
    Nothing -> return ()
    Just aa@(AddApiAnn kw _) -> mark [aa] kw

markLocatedAA :: ApiAnn' a -> (a -> AddApiAnn) -> EPP ()
markLocatedAA ApiAnnNotUsed  _  = return ()
markLocatedAA (ApiAnn _ a _) f = mark [f a] kw
  where
    AddApiAnn kw _ = f a

markLocatedAAL :: ApiAnn' a -> (a -> [AddApiAnn]) -> AnnKeywordId -> EPP ()
markLocatedAAL ApiAnnNotUsed  _ _ = return ()
markLocatedAAL (ApiAnn _ a _) f kw = go (f a)
  where
    go [] = return ()
    go (a@(AddApiAnn kw _):_) = mark [a] kw
    go (_:as) = go as

-- ---------------------------------------------------------------------

-- markLocatedMaybe :: a -> (a -> Maybe RealSrcSpan) -> AnnKeywordId -> EPP ()
-- markLocatedMaybe a

-- ---------------------------------------------------------------------

markArrow :: ApiAnn -> (HsArrow GhcPs) -> EPP ()
markArrow an mult
  = case mult of
      HsLinearArrow ->  markApiAnn an AnnLolly
      HsUnrestrictedArrow -> markApiAnn an AnnRarrow
      HsExplicitMult p -> do
        printString False "#"
        markAnnotated p
        markApiAnn an AnnRarrow

-- ---------------------------------------------------------------------

markAnnOpen :: Maybe RealSrcSpan -> SourceText -> String -> EPP ()
markAnnOpen ms NoSourceText txt   = printStringAtMkw ms txt
markAnnOpen ms (SourceText txt) _ = printStringAtMkw ms txt

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
markApiAnnAll (ApiAnn _ a _) f kw = mapM_ markKw anns
  where
    anns = filter (\(AddApiAnn ka _) -> ka == kw) (f a)

mark :: [AddApiAnn] -> AnnKeywordId -> EPP ()
mark anns kw = do
  case find (\(AddApiAnn k _) -> k == kw) anns of
    Nothing -> return ()
    Just aa -> markKw aa

markKwT :: TrailingAnn -> EPP ()
markKwT (AddSemiAnn ss)  = markKw' AnnSemi ss
markKwT (AddCommaAnn ss) = markKw' AnnComma ss
markKwT (AddVbarAnn ss)  = markKw' AnnVbar ss

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

-- printTrailingComments :: EPP ()
-- printTrailingComments = do
--   cs <- getUnallocatedComments
--   mapM_ printOneComment cs

-- ---------------------------------------------------------------------

printComments :: RealSrcSpan -> EPP ()
printComments ss = do
  cs <- commentAllocation ss
  debugM $ "printComments: (ss,comment locations): " ++ showGhc (ss,map commentIdentifier cs)
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
  getApiAnnotation _ = NoEntryVal
  exact = withPpr

-- ---------------------------------------------------------------------

-- instance ExactPrint (LocatedA WarningTxt) where
--   exact (L _ a) = withPpr a -- TODO:AZ: use annotations

-- ---------------------------------------------------------------------

-- instance ExactPrint (LIE GhcPs) where
--   exact = withPpr -- TODO:AZ use annotations

-- ---------------------------------------------------------------------

-- instance ExactPrint (LHsDecl GhcPs) where
--   exact = withPpr -- TODO:AZ use annotations

-- ---------------------------------------------------------------------

-- instance ExactPrint (LImportDecl GhcPs) where
--   getApiAnnotation (L ann _) = fromAnn ann
--   exact (L _ a) = exact a
--     -- Used to print the annotations related to being in a
--     -- list. Perhaps rely on the generic LocatedA one?

instance ExactPrint (ImportDecl GhcPs) where
  getApiAnnotation idecl = fromAnn (ideclExt idecl)
  exact x@(ImportDecl ApiAnnNotUsed _ _ _ _ _ _ _ _ _) = withPpr x
  exact (ImportDecl ann@(ApiAnn _ an _) msrc (L lm modname) mpkg _src safeflag qualFlag _impl mAs hiding) = do

    markAnnKw ann importDeclAnnImport AnnImport

    -- "{-# SOURCE" and "#-}"
    case msrc of
      SourceText _txt -> do
        debugM $ "ImportDecl sourcetext"
        let mo = fmap fst $ importDeclAnnPragma an
        let mc = fmap snd $ importDeclAnnPragma an
        markAnnOpen mo msrc "{-# SOURCE"
        printStringAtMkw mc "#-}"
      NoSourceText -> return ()
 --   when safeflag (mark GHC.AnnSafe)
    case qualFlag of
      QualifiedPre  -- 'qualified' appears in prepositive position.
        -> printStringAtMkw (importDeclAnnQualified an) "qualified"
      _ -> return ()
 --   case mpkg of
 --    Just (GHC.StringLiteral (GHC.SourceText srcPkg) _) ->
 --      markWithString GHC.AnnPackageName srcPkg
 --    _ -> return ()

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
  getApiAnnotation _ = NoEntryVal
  exact = withPpr -- TODO:AZ use annotations

-- ---------------------------------------------------------------------

-- instance (ExactPrint a) => ExactPrint (LocatedA a) where
--   exact (L _ a) = exact a -- TODO:AZ: use annotations

-- ---------------------------------------------------------------------

instance ExactPrint (HsDecl GhcPs) where
  getApiAnnotation (TyClD      _ d) = getApiAnnotation d
  getApiAnnotation (InstD      _ d) = getApiAnnotation d
  -- getApiAnnotation (DerivD     _ d) = getApiAnnotation d
  getApiAnnotation (ValD       _ d) = getApiAnnotation d
  getApiAnnotation (SigD       _ d) = getApiAnnotation d
  -- getApiAnnotation (KindSigD   _ d) = getApiAnnotation d
  -- getApiAnnotation (DefD       _ d) = getApiAnnotation d
  -- getApiAnnotation (ForD       _ d) = getApiAnnotation d
  -- getApiAnnotation (WarningD   _ d) = getApiAnnotation d
  -- getApiAnnotation (AnnD       _ d) = getApiAnnotation d
  -- getApiAnnotation (RuleD      _ d) = getApiAnnotation d
  -- getApiAnnotation (SpliceD    _ d) = getApiAnnotation d
  -- getApiAnnotation (DocD       _ d) = getApiAnnotation d
  -- getApiAnnotation (RoleAnnotD _ d) = getApiAnnotation d
  getApiAnnotation x = error $ "LHsDecl: getApiAnnotation for " ++ showAst x

  exact (TyClD _ d) = exact d
  exact (InstD _ d) = exact d
  exact (ValD  _ d) = exact d
  exact (SigD  _ d) = exact d
  exact x = error $ "LHsDecl: exact for " ++ showAst x
  -- exact d = withPpr d -- TODO:AZ use annotations

-- ---------------------------------------------------------------------

instance ExactPrint (InstDecl GhcPs) where
  getApiAnnotation (ClsInstD     _  _) = NoEntryVal
  getApiAnnotation (DataFamInstD an _) = fromAnn an
  getApiAnnotation (TyFamInstD   an _) = fromAnn an

-- instance Annotate (GHC.InstDecl GHC.GhcPs) where

--   markAST l (GHC.ClsInstD     _  cid) = markAST l  cid
--   markAST l (GHC.DataFamInstD _ dfid) = markAST l dfid
--   markAST l (GHC.TyFamInstD   _ tfid) = markAST l tfid
--   markAST _ (GHC.XInstDecl x) = error $ "got XInstDecl for:" ++ showGhc x

  exact (ClsInstD     _  cid) = markAnnotated cid
  exact (DataFamInstD an decl) = do
    exactDataFamInstDecl an TopLevel decl
  exact (TyFamInstD an eqn) = do
    exactTyFamInstDecl an TopLevel eqn
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

exactTyFamInstDecl :: ApiAnn -> TopLevelFlag -> (TyFamInstDecl GhcPs) -> EPP ()
exactTyFamInstDecl an top_lvl (TyFamInstDecl { tfid_eqn = eqn }) = do
  markApiAnn an AnnType
  markApiAnn an AnnInstance -- TODO: depend on top_lvl flag
  markAnnotated eqn

-- ---------------------------------------------------------------------

instance ExactPrint (TyFamInstEqn GhcPs) where
  getApiAnnotation = const NoEntryVal
  exact (HsIB { hsib_body = FamEqn { feqn_ext = an
                                   , feqn_tycon  = tycon
                                   , feqn_bndrs  = bndrs
                                   , feqn_pats   = pats
                                   , feqn_fixity = fixity
                                   , feqn_rhs    = rhs }}) = do
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
  mapM_ markAnnotated bndrs
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

instance ExactPrint (LHsTypeArg GhcPs) where
  getApiAnnotation = const NoEntryVal

  exact (HsValArg tm)    = markAnnotated tm
  exact (HsTypeArg ss ty) = printStringAtSs ss "@" >> markAnnotated ty
  exact x@(HsArgPar sp)   = withPpr x

-- ---------------------------------------------------------------------

instance ExactPrint [LHsTyVarBndr () GhcPs] where
  getApiAnnotation = const NoEntryVal
  exact bs = mapM_ markAnnotated bs

-- ---------------------------------------------------------------------

instance ExactPrint (ClsInstDecl GhcPs) where
  getApiAnnotation cid = fromAnn (cid_ext cid)

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
          applyListAnnotations (prepareListAnnotation ats
                             ++ prepareListAnnotationF (exactDataFamInstDecl an NotTopLevel ) adts
                             ++ prepareListAnnotationA (bagToList binds)
                             ++ prepareListAnnotation sigs
                               )
          markApiAnn an AnnCloseC -- '}'

      where
        top_matter = do
          markApiAnn an AnnInstance
          mapM_ markAnnotated mbOverlap
          markAnnotated inst_ty
          -- text "instance" <+> ppOverlapPragma mbOverlap
          --                                    <+> ppr inst_ty

-- ---------------------------------------------------------------------

instance ExactPrint (TyFamInstDecl GhcPs) where
  getApiAnnotation _ = NoEntryVal
  exact = withPpr

-- ---------------------------------------------------------------------

-- instance ExactPrint (DataFamInstDecl GhcPs) where
--   getApiAnnotation _ = NoEntryVal
--   exact = withPpr

-- ---------------------------------------------------------------------

instance ExactPrint (LHsSigType GhcPs) where
  getApiAnnotation (HsIB an _) = fromAnn an
  exact (HsIB an t) = markAnnotated t

-- ---------------------------------------------------------------------

instance ExactPrint (LocatedP OverlapMode) where
  getApiAnnotation _ = NoEntryVal
  exact = withPpr

-- ---------------------------------------------------------------------

-- instance ExactPrint (LHsBind GhcPs) where
--   getApiAnnotation = entryFromLocatedA
--   exact (L _ a) = exact a

instance ExactPrint (HsBind GhcPs) where
  getApiAnnotation FunBind{} = NoEntryVal
  getApiAnnotation PatBind{} = NoEntryVal
  getApiAnnotation VarBind{} = NoEntryVal
  getApiAnnotation AbsBinds{} = NoEntryVal
  getApiAnnotation PatSynBind{} = NoEntryVal

  exact (FunBind _ _ matches _) = do
    markAnnotated matches
  exact (PatBind an pat grhss _) = do
    markAnnotated pat
    markAnnotated grhss

  exact x = error $ "HsBind: exact for " ++ showAst x
  -- exact b = withPpr b


-- ---------------------------------------------------------------------

instance ExactPrint (LocatedL [LMatch GhcPs (LHsExpr GhcPs)]) where
  getApiAnnotation = entryFromLocatedA
  exact (L la a) = do
    markLocatedMAA (ann la) al_open
    markApiAnnAll (ann la) al_rest AnnSemi
    mapM_ markAnnotated a
    markLocatedMAA (ann la) al_close

instance ExactPrint (Match GhcPs (LHsExpr GhcPs)) where
  getApiAnnotation (Match ann _ _ _) = fromAnn ann

  exact match@(Match ApiAnnNotUsed _ _ _) = withPpr match
  exact (Match an mctxt pats grhss) = do
  -- Based on Expr.pprMatch

    debugM $ "exact Match entered"

    -- herald
    case mctxt of
      FunRhs fun fixity _strictness -> do
        debugM $ "exact Match FunRhs:" ++ showGhc fun
        -- exact fun
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

    -- -- case grhs of
    -- --   (GHC.L _ (GHC.GRHS _ [] _):_) -> when (isFunBind mctxt) $ markApiAnn an AnnEqual -- empty guards
    -- --   _ -> return ()
    -- -- case mctxt of
    -- --   LambdaExpr -> markApiAnn anns AnnRarrow -- For HsLam
    -- --   _ -> return ()

    -- mapM_ markAnnotated grhs

    -- markAnnotated lb
    -- -- case lb of
    -- --   GHC.EmptyLocalBinds{} -> return ()
    -- --   _ -> do
    -- --     -- mark GHC.AnnWhere
    -- --     -- markOptional GHC.AnnOpenC -- '{'
    -- --     -- markInside GHC.AnnSemi
    -- --     -- markLocalBindsWithLayout lb
    -- --     -- markOptional GHC.AnnCloseC -- '}'
    -- --     return ()

-- ---------------------------------------------------------------------

instance ExactPrint (GRHSs GhcPs (LHsExpr GhcPs)) where
  getApiAnnotation = const NoEntryVal

  exact (GRHSs an grhss binds) = do
    markLocatedAA an id
    mapM_ markAnnotated grhss
    markAnnotated binds

-- ---------------------------------------------------------------------

instance ExactPrint (LHsLocalBinds GhcPs) where
  -- If the binds are empty, they may have a null location
  getApiAnnotation = entryFromLocatedA

  exact (L (SrcSpanAnn ann _) a) = do
    debugM $ "exact:LHsLocalBinds:" ++ showGhc a
    markLocatedAAL ann al_rest AnnWhere
    markLocatedMAA ann al_open
    markAnnotated a
    markLocatedMAA ann al_close

instance ExactPrint (HsLocalBinds GhcPs) where
  getApiAnnotation (HsValBinds an _) = fromAnn an
  getApiAnnotation (HsIPBinds{}) = NoEntryVal
  getApiAnnotation (EmptyLocalBinds{}) = NoEntryVal

  -- exact (HsValBinds _ bs) = markAnnotated bs
  exact (HsValBinds _ bs) = markAnnotated bs
    -- applyListAnnotations
    --    (prepareListAnnotation (bagToList binds)
    --  ++ prepareListAnnotation sigs
    --    )

    -- withPpr bs
  exact (HsIPBinds _ bs) = withPpr bs
  exact (EmptyLocalBinds _) = return ()

instance ExactPrint (HsValBindsLR GhcPs GhcPs) where
  getApiAnnotation _ = NoEntryVal

  exact (ValBinds sortkey binds sigs) = do
    -- printString False "ValBinds"
    applyListAnnotations
       (prepareListAnnotationA (bagToList binds)
     ++ prepareListAnnotation sigs
       )
-- ---------------------------------------------------------------------
-- Managing lists which have been separated, e.g. Sigs and Binds


-- AZ:TODO: generalise this, and the next one
prepareListAnnotationFamilyD :: [LFamilyDecl GhcPs] -> [(SrcSpan,EPP ())]
prepareListAnnotationFamilyD ls
  = map (\b -> (getLocA b,exactFamilyDecl NotTopLevel (unLoc b))) ls

prepareListAnnotationF :: (a -> EPP ()) -> [Located a] -> [(SrcSpan,EPP ())]
prepareListAnnotationF f ls
  = map (\b -> (getLoc b, f (unLoc b))) ls

prepareListAnnotation :: ExactPrint (Located a)
  => [Located a] -> [(SrcSpan,EPP ())]
prepareListAnnotation ls = map (\b -> (getLoc b,markAnnotated b)) ls

prepareListAnnotationA :: ExactPrint (LocatedAn an a)
  => [LocatedAn an a] -> [(SrcSpan,EPP ())]
prepareListAnnotationA ls = map (\b -> (getLocA b,markAnnotated b)) ls


applyListAnnotations :: [(SrcSpan, EPP ())] -> EPP ()
applyListAnnotations ls = withSortKey ls

withSortKey :: [(SrcSpan, EPP ())] -> EPP ()
withSortKey xs = do
  Ann{annSortKey} <- asks epAnn
  let ordered = case annSortKey of
                  Nothing   -> sortBy orderByFst xs
                  Just keys -> orderByKey xs keys
                                -- `debug` ("withSortKey:" ++
                                --          showGhc (map fst (sortBy (comparing (flip elemIndex keys . fst)) xs),
                                --                  map fst xs,
                                --                  keys)
                                --          )
  mapM_ snd ordered

orderByFst (a,_) (b,_) = compare a b

-- ---------------------------------------------------------------------

instance ExactPrint (Sig GhcPs) where
  getApiAnnotation (TypeSig a _ _)  = fromAnn a
  getApiAnnotation (PatSynSig a _ _) = fromAnn a
  getApiAnnotation (ClassOpSig a _ _ _) = fromAnn a
  getApiAnnotation (IdSig {}) = NoEntryVal
  getApiAnnotation (FixSig a _) = fromAnn a
  getApiAnnotation (InlineSig a _ _) = fromAnn a
  getApiAnnotation (SpecSig a _ _ _) = fromAnn a
  getApiAnnotation (SpecInstSig a _ _) = fromAnn a
  getApiAnnotation (MinimalSig a _ _) = fromAnn a
  getApiAnnotation (SCCFunSig a _ _ _) = fromAnn a
  getApiAnnotation (CompleteMatchSig a _ _ _) = fromAnn a

-- instance Annotate (Sig GhcPs) where

--   markAST _ (TypeSig _ lns st)  = do
--     setContext (Set.singleton PrefixOp) $ markListNoPrecedingSpace True lns
--     mark AnnDcolon
--     markLHsSigWcType st
--     markTrailingSemi
--     tellContext (Set.singleton FollowingLine)

--   markAST _ (PatSynSig _ lns (HsIB _ typ)) = do
--     mark AnnPattern
--     setContext (Set.singleton PrefixOp) $ markListIntercalate lns
--     mark AnnDcolon
--     markLocated typ
--     markTrailingSemi

  exact (ClassOpSig an is_deflt vars ty)
    | is_deflt  = markApiAnn an AnnDefault >> exactVarSig an vars ty
    | otherwise = exactVarSig an vars ty

--   markAST _ (ClassOpSig _ isDefault ns (HsIB _ typ)) = do
--     when isDefault $ mark AnnDefault
--     setContext (Set.singleton PrefixOp) $ markListIntercalate ns
--     mark AnnDcolon
--     markLocated typ
--     markTrailingSemi

--   markAST _ (IdSig {}) =
--     traceM "warning: Introduced after renaming"

--   markAST _ (FixSig _ (FixitySig _ lns (Fixity src v fdir))) = do
--     let fixstr = case fdir of
--          InfixL -> "infixl"
--          InfixR -> "infixr"
--          InfixN -> "infix"
--     markWithString AnnInfix fixstr
--     markSourceText src (show v)
--     setContext (Set.singleton InfixOp) $ markListIntercalate lns
--     markTrailingSemi

--   markAST l (InlineSig _ ln inl) = do
--     markAnnOpen (inl_src inl) "{-# INLINE"
--     markActivation l (inl_act inl)
--     setContext (Set.singleton PrefixOp) $ markLocated ln
--     markWithString AnnClose "#-}" -- '#-}'
--     markTrailingSemi

--   markAST l (SpecSig _ ln typs inl) = do
--     markAnnOpen (inl_src inl) "{-# SPECIALISE" -- Note: may be {-# SPECIALISE_INLINE
--     markActivation l (inl_act inl)
--     markLocated ln
--     mark AnnDcolon -- '::'
--     markListIntercalateWithFunLevel markLHsSigType 2 typs
--     markWithString AnnClose "#-}" -- '#-}'
--     markTrailingSemi


--   markAST _ (SpecInstSig _ src typ) = do
--     markAnnOpen src "{-# SPECIALISE"
--     mark AnnInstance
--     markLHsSigType typ
--     markWithString AnnClose "#-}" -- '#-}'
--     markTrailingSemi


--   markAST _ (MinimalSig _ src formula) = do
--     markAnnOpen src "{-# MINIMAL"
--     markLocated formula
--     markWithString AnnClose "#-}"
--     markTrailingSemi

--   markAST _ (SCCFunSig _ src ln ml) = do
--     markAnnOpen src "{-# SCC"
--     markLocated ln
--     markMaybe ml
--     markWithString AnnClose "#-}"
--     markTrailingSemi

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

exactVarSig :: ApiAnn -> [LocatedN RdrName] -> LHsSigType GhcPs -> EPP ()
exactVarSig an vars ty = do
  mapM_ markAnnotated vars
  markApiAnn an AnnDcolon
  markAnnotated ty

-- ---------------------------------------------------------------------

instance ExactPrint (GRHS GhcPs (LHsExpr GhcPs)) where
  getApiAnnotation (GRHS ann _ _) = fromAnn ann

  exact (GRHS anns guards expr) = do
    markLocatedAA anns ga_sep
    -- markAnnKwM anns ga_vbar AnnVbar
    markAnnotated expr

-- ---------------------------------------------------------------------

instance ExactPrint (LocatedA (StmtLR GhcPs GhcPs (LHsExpr GhcPs))) where
  getApiAnnotation = const NoEntryVal
  exact = withPpr -- AZ TODO

-- ---------------------------------------------------------------------

-- instance ExactPrint (LHsExpr GhcPs) where
--   getApiAnnotation = entryFromLocatedA
--   exact (L _ a) = do
--     debugM $ "exact:LHsExpr:" ++ showGhc a
--     markAnnotated a

instance ExactPrint (HsExpr GhcPs) where
  getApiAnnotation (HsVar{})                    = NoEntryVal
  getApiAnnotation (HsUnboundVar ann _)         = fromAnn ann
  getApiAnnotation (HsConLikeOut{})             = NoEntryVal
  getApiAnnotation (HsRecFld{})                 = NoEntryVal
  getApiAnnotation (HsOverLabel ann _ _)        = fromAnn ann
  getApiAnnotation (HsIPVar ann _)              = fromAnn ann
  getApiAnnotation (HsOverLit ann _)            = fromAnn ann
  getApiAnnotation (HsLit ann _)                = fromAnn ann
  getApiAnnotation (HsLam _ _)                  = NoEntryVal
  getApiAnnotation (HsLamCase ann _)            = fromAnn ann
  getApiAnnotation (HsApp ann _ _)              = fromAnn ann
  getApiAnnotation (HsAppType ann _ _)          = fromAnn ann
  getApiAnnotation (OpApp ann _ _ _)            = fromAnn ann
  getApiAnnotation (NegApp ann _ _)             = fromAnn ann
  getApiAnnotation (HsPar ann _)                = fromAnn ann
  getApiAnnotation (SectionL ann _ _)           = fromAnn ann
  getApiAnnotation (SectionR ann _ _)           = fromAnn ann
  getApiAnnotation (ExplicitTuple ann _ _)      = fromAnn ann
  getApiAnnotation (ExplicitSum ann _ _ _)      = fromAnn ann
  getApiAnnotation (HsCase (ApiAnn a _ cs) _ _) = Entry a cs
  getApiAnnotation (HsCase ApiAnnNotUsed   _ _) = NoEntryVal
  getApiAnnotation (HsIf (ann,_) _ _ _ _)       = fromAnn ann
  getApiAnnotation (HsMultiIf ann _)            = fromAnn ann
  getApiAnnotation (HsLet ann _ _)              = fromAnn ann
  getApiAnnotation (HsDo ann _ _)               = fromAnn ann
  getApiAnnotation (ExplicitList ann _ _)       = fromAnn ann
  getApiAnnotation (RecordCon ann _ _)          = fromAnn ann
  getApiAnnotation (RecordUpd ann _ _)          = fromAnn ann
  getApiAnnotation (ExprWithTySig ann _ _)      = fromAnn ann
  getApiAnnotation (ArithSeq ann _ _)           = fromAnn ann
  getApiAnnotation (HsBracket ann _)            = fromAnn ann
  getApiAnnotation (HsRnBracketOut{})           = NoEntryVal
  getApiAnnotation (HsTcBracketOut{})           = NoEntryVal
  getApiAnnotation (HsSpliceE ann _)            = fromAnn ann
  getApiAnnotation (HsProc ann _ _)             = fromAnn ann
  getApiAnnotation (HsStatic{})                 = NoEntryVal
  getApiAnnotation (HsTick {})                  = NoEntryVal
  getApiAnnotation (HsBinTick {})               = NoEntryVal
  getApiAnnotation (HsPragE{})                  = NoEntryVal


  exact (HsVar _ n) = markAnnotated n
  -- exact x@(HsUnboundVar ann _)         = withPpr x
  -- exact x@(HsConLikeOut{})             = withPpr x
  -- exact x@(HsRecFld{})                 = withPpr x
  -- exact x@(HsOverLabel ann _ _)        = withPpr x
  -- exact x@(HsIPVar ann _)              = withPpr x
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




  -- exact x@(HsLamCase ann _)            = withPpr x
  exact (HsApp ann e1 e2) = do
    p <- getPos
    debugM $ "HsApp entered. p=" ++ show p
    markAnnotated e1
    markAnnotated e2
  -- exact x@(HsAppType ann _ _)          = withPpr x
  exact x@(OpApp ann e1 e2 e3) = do
  -- markExpr _ (GHC.OpApp _ e1 e2 e3) = do
    let
      isInfix = case e2 of
        -- TODO: generalise this. Is it a fixity thing?
        GHC.L _ (GHC.HsVar{}) -> True
        _                     -> False

      -- normal =
      --   -- When it is the leftmost item in a GRHS, e1 needs to have PrefixOp context
      --   ifInContext (Set.singleton LeftMost)
      --     (setContextLevel (Set.fromList [LeftMost,PrefixOp]) 2 $ markLocated e1)
      --     (markLocated e1)

    -- if isInfix
    --     then setContextLevel (Set.singleton PrefixOp) 2 $ markLocated e1
    --     else normal
    exact e1

    -- unsetContext PrefixOp $ setContext (Set.singleton InfixOp) $ markLocated e2
    exact e2

    -- if isInfix
    --   then setContextLevel (Set.singleton PrefixOp) 2 $ markLocated e3
    --   else markLocated e3
    exact e3

  -- exact x@(NegApp ann _ _)             = withPpr x
  exact x@(HsPar an e) = do
    markOpeningParen an
    markAnnotated e
    debugM $ "HsPar closing paren"
    markClosingParen an
    debugM $ "HsPar done"
      -- markExpr _ (GHC.HsPar _ e) = do
      --   mark GHC.AnnOpenP -- '('
      --   markLocated e
      --   mark GHC.AnnCloseP -- ')'

  -- exact x@(SectionL ann _ _)           = withPpr x
  -- exact x@(SectionR ann _ _)           = withPpr x
  exact (ExplicitTuple an args b) = do
    if b == Boxed then markApiAnn an AnnOpenP
                  else markApiAnn an AnnOpenPH

    mapM_ markAnnotated args

    if b == Boxed then markApiAnn an AnnCloseP
                  else markApiAnn an AnnClosePH
    debugM $ "ExplicitTuple done"


  -- exact x@(ExplicitSum ann _ _ _)      = withPpr x
  exact x@(HsCase an e alts) = do
    markAnnKw an hsCaseAnnCase AnnCase
    markAnnotated e
    markAnnKw an hsCaseAnnOf AnnOf
    markApiAnn' an hsCaseAnnsRest AnnOpenC
    markApiAnnAll an hsCaseAnnsRest AnnSemi
    markAnnotated alts
    markApiAnn' an hsCaseAnnsRest AnnCloseC

  -- exact x@(HsCase ApiAnnNotUsed   _ _) = withPpr x
  -- exact x@(HsIf (ann,_) _ _ _ _)       = withPpr x
  -- exact x@(HsMultiIf ann _)            = withPpr x
  exact (HsLet an binds e) = do
    markApiAnn an AnnLet
    markApiAnn an AnnOpenC -- '{'
    markAnnotated binds
    markApiAnn an AnnCloseC -- '}'
    markApiAnn an AnnIn
    markAnnotated e
      -- markExpr _ (GHC.HsLet _ (GHC.L _ binds) e) = do
      --   setLayoutFlag (do -- Make sure the 'in' gets indented too
      --     mark GHC.AnnLet
      --     markOptional GHC.AnnOpenC
      --     markInside GHC.AnnSemi
      --     markLocalBindsWithLayout binds
      --     markOptional GHC.AnnCloseC
      --     mark GHC.AnnIn
      --     markLocated e)

  -- exact x@(HsDo ann _ _)               = withPpr x
  -- exact x@(ExplicitList ann _ _)       = withPpr x
  -- exact x@(RecordCon ann _ _)          = withPpr x
  -- exact x@(RecordUpd ann _ _)          = withPpr x
  -- exact x@(ExprWithTySig ann _ _)      = withPpr x
  -- exact x@(ArithSeq ann _ _)           = withPpr x
  -- exact x@(HsBracket ann _)            = withPpr x
  -- exact x@(HsRnBracketOut{})           = withPpr x
  -- exact x@(HsTcBracketOut{})           = withPpr x
  -- exact x@(HsSpliceE ann _)            = withPpr x
  exact x@(HsProc ann p c) = do
    markApiAnn ann AnnProc
    markAnnotated p
    markApiAnn ann AnnRarrow
    markAnnotated c
      -- markExpr _ (GHC.HsProc _ p c) = do
      --   mark GHC.AnnProc
      --   markLocated p
      --   mark GHC.AnnRarrow
      --   markLocated c

  -- exact x@(HsStatic{})                 = withPpr x
  -- exact x@(HsTick {})                  = withPpr x
  -- exact x@(HsBinTick {})               = withPpr x
  -- exact x@(HsPragE{})                  = withPpr x
  exact x = error $ "exact HsExpr for:" ++ showAst x

-- ---------------------------------------------------------------------

instance ExactPrint (MatchGroup GhcPs (LHsExpr GhcPs)) where
  getApiAnnotation = const NoEntryVal
  exact (MG _ matches _) = do
    -- TODO:AZ use SortKey, in MG ann.
    markAnnotated matches

-- ---------------------------------------------------------------------

-- instance ExactPrint (LHsTupArg GhcPs) where
--   getApiAnnotation = entryFromLocatedA
--   exact (L an a) = do
--     exact a
--     markALocatedA (ann an)

-- ---------------------------------------------------------------------

instance ExactPrint (HsTupArg GhcPs) where
  getApiAnnotation = const NoEntryVal

  exact (Present _ e) = markAnnotated e
  exact (Missing _) = return ()

-- ---------------------------------------------------------------------

instance ExactPrint (HsCmdTop GhcPs) where
  getApiAnnotation = const NoEntryVal
  exact (HsCmdTop _ cmd) = markAnnotated cmd

-- ---------------------------------------------------------------------

-- instance ExactPrint (LHsCmd GhcPs) where
--   getApiAnnotation (L a _) = fromAnn a
--   exact (L an a) = do
--     exact a
--     markALocatedA (ann an)

-- ---------------------------------------------------------------------

instance ExactPrint (HsCmd GhcPs) where
  getApiAnnotation (HsCmdArrApp {}) = NoEntryVal
  getApiAnnotation (HsCmdArrForm{}) = NoEntryVal
  getApiAnnotation (HsCmdApp    {}) = NoEntryVal
  getApiAnnotation (HsCmdLam    {}) = NoEntryVal
  getApiAnnotation (HsCmdPar    {}) = NoEntryVal
  getApiAnnotation (HsCmdCase   {}) = NoEntryVal
  getApiAnnotation (HsCmdLamCase{}) = NoEntryVal
  getApiAnnotation (HsCmdIf     {}) = NoEntryVal
  getApiAnnotation (HsCmdLet    {}) = NoEntryVal
  getApiAnnotation (HsCmdDo     {}) = NoEntryVal


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

--   markAST l (GHC.HsCmdLam _ match) = do
--     setContext (Set.singleton LambdaExpr) $ do markMatchGroup l match

  exact (HsCmdPar an e) = do
    markOpeningParen an
    markAnnotated e
    markClosingParen an

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

--   markAST _ (GHC.XCmd x) = error $ "got XCmd for:" ++ showGhc x

  exact x = error $ "exact HsCmd for:" ++ showAst x

-- ---------------------------------------------------------------------

-- instance ExactPrint (CmdLStmt GhcPs) where
--   getApiAnnotation = const NoEntryVal
--   exact (L _ a) = markAnnotated a

-- ---------------------------------------------------------------------

instance ExactPrint (StmtLR GhcPs GhcPs (LHsCmd GhcPs)) where
  getApiAnnotation = const NoEntryVal


  -- markAST _ (GHC.LastStmt _ body _ _)
  --   = setContextLevel (Set.fromList [LeftMost,PrefixOp]) 2 $ markLocated body

  exact (BindStmt an pat body) = do
    markAnnotated pat
    markApiAnn an AnnLarrow
    markAnnotated body

  -- markAST _ (GHC.BindStmt _ pat body _ _) = do
  --   unsetContext Intercalate $ setContext (Set.singleton PrefixOp) $ markLocated pat
  --   mark GHC.AnnLarrow
  --   unsetContext Intercalate $ setContextLevel (Set.fromList [LeftMost,PrefixOp]) 2 $ markLocated body

  --   ifInContext (Set.singleton Intercalate)
  --     (mark GHC.AnnComma)
  --     (inContext (Set.singleton AddVbar) $ mark GHC.AnnVbar)
  --   markTrailingSemi

  -- markAST _ GHC.ApplicativeStmt{}
  --   = error "ApplicativeStmt should not appear in ParsedSource"

  exact (BodyStmt _ body _ _) = do
    markAnnotated body
  -- markAST _ (GHC.BodyStmt _ body _ _) = do
  --   unsetContext Intercalate $ markLocated body
  --   inContext (Set.singleton AddVbar)     $ mark GHC.AnnVbar
  --   inContext (Set.singleton Intercalate) $ mark GHC.AnnComma
  --   markTrailingSemi

  -- markAST _ (GHC.LetStmt _ (GHC.L _ lb)) = do
  --   mark GHC.AnnLet
  --   markOptional GHC.AnnOpenC -- '{'
  --   markInside GHC.AnnSemi
  --   markLocalBindsWithLayout lb
  --   markOptional GHC.AnnCloseC -- '}'
  --   ifInContext (Set.singleton Intercalate)
  --     (mark GHC.AnnComma)
  --     (inContext (Set.singleton AddVbar)     $ mark GHC.AnnVbar)
  --   markTrailingSemi

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

  -- markAST _ (GHC.RecStmt _ stmts _ _ _ _ _) = do
  --   mark GHC.AnnRec
  --   markOptional GHC.AnnOpenC
  --   markInside GHC.AnnSemi
  --   markListWithLayout stmts
  --   markOptional GHC.AnnCloseC
  --   inContext (Set.singleton AddVbar)     $ mark GHC.AnnVbar
  --   inContext (Set.singleton Intercalate) $ mark GHC.AnnComma
  --   markTrailingSemi

  exact x = error $ "exact CmdLStmt for:" ++ showAst x


-- ---------------------------------------------------------------------

instance ExactPrint (TyClDecl GhcPs) where
  getApiAnnotation = const NoEntryVal

-- instance Annotate (GHC.TyClDecl GHC.GhcPs) where

  exact (FamDecl an decl) = do
    exactFamilyDecl TopLevel decl
--   markAST l (GHC.FamDecl _ famdecl) = markAST l famdecl >> markTrailingSemi

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

  exact (ClassDecl {tcdCExt = an,
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
          markApiAnn an AnnWhere
          markApiAnn an AnnOpenC
          applyListAnnotations
                               (prepareListAnnotation sigs
                             ++ prepareListAnnotationA (bagToList methods)
                             ++ prepareListAnnotationFamilyD ats
                             ++ prepareListAnnotation at_defs
                             -- ++ prepareListAnnotation docs
                               )
          markApiAnn an AnnCloseC
      where
        top_matter = do
          markApiAnn an AnnClass
          exactVanillaDeclHead lclas tyvars fixity context
          -- markAnnotated fundeps
          return ()

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

exactFamilyDecl :: TopLevelFlag -> FamilyDecl GhcPs -> EPP ()
exactFamilyDecl top_level (FamilyDecl { fdExt = an
                                      , fdInfo = info, fdLName = ltycon
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
  exactVanillaDeclHead ltycon tyvars fixity Nothing
  exact_kind
  mapM_ markAnnotated mb_inj
  return ()
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
  exact_condecls an2 condecls
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
          printString False "leg a"
          return ()
      | fixity == Infix = do
         -- = hsep [ppr (unLoc varl), pprInfixOcc (unLoc thing)
         -- , hsep (map (ppr.unLoc) varsr)]
          printString False "leg b"
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

-- instance ExactPrint (FamilyDecl GhcPs) where
--   getApiAnnotation = const NoEntryVal
--   exact = withPpr

-- ---------------------------------------------------------------------

instance ExactPrint (InjectivityAnn GhcPs) where
  getApiAnnotation (InjectivityAnn an _ _) = fromAnn an
  exact (InjectivityAnn an lhs rhs) = do
    markApiAnn an AnnVbar
    markAnnotated lhs
    markApiAnn an AnnRarrow
    mapM_ markAnnotated rhs
    --               Just (L _ (InjectivityAnn _ lhs rhs)) ->
    --                 hsep [ vbar, ppr lhs, text "->", hsep (map ppr rhs) ]
    --               Nothing -> empty

-- ---------------------------------------------------------------------

instance ExactPrint (HsTyVarBndr () GhcPs) where
  getApiAnnotation (UserTyVar an _ _)     = fromAnn an
  getApiAnnotation (KindedTyVar an _ _ _) = fromAnn an
  exact = withPpr

-- ---------------------------------------------------------------------

-- NOTE: this is also an alias for LHsKind
-- instance ExactPrint (LHsType GhcPs) where
--   getApiAnnotation = entryFromLocatedA
--   exact (L _ a) = markAnnotated a

instance ExactPrint (HsType GhcPs) where
  getApiAnnotation (HsForAllTy an _ _)       = fromAnn an
  getApiAnnotation (HsQualTy an _ _)         = fromAnn an
  getApiAnnotation (HsTyVar an _ _)          = fromAnn an
  getApiAnnotation (HsAppTy an _ _)          = NoEntryVal
  getApiAnnotation (HsAppKindTy an _ _)      = NoEntryVal
  getApiAnnotation (HsFunTy an _ _ _)        = fromAnn an
  getApiAnnotation (HsListTy an _)           = fromAnn an
  getApiAnnotation (HsTupleTy an _ _)        = fromAnn an
  getApiAnnotation (HsSumTy an _)            = fromAnn an
  getApiAnnotation (HsOpTy an _ _ _)         = fromAnn an
  getApiAnnotation (HsParTy an _)            = fromAnn an
  getApiAnnotation (HsIParamTy an _ _)       = fromAnn an
  getApiAnnotation (HsStarTy an _)           = NoEntryVal
  getApiAnnotation (HsKindSig an _ _)        = fromAnn an
  getApiAnnotation (HsSpliceTy an _)         = NoEntryVal
  getApiAnnotation (HsDocTy an _ _)          = fromAnn an
  getApiAnnotation (HsBangTy an _ _)         = fromAnn an
  getApiAnnotation (HsRecTy an _)            = fromAnn an
  getApiAnnotation (HsExplicitListTy an _ _) = fromAnn an
  getApiAnnotation (HsExplicitTupleTy an _)  = fromAnn an
  getApiAnnotation (HsTyLit an _)            = NoEntryVal
  getApiAnnotation (HsWildCardTy _)          = NoEntryVal

  exact x@(HsForAllTy an _ _)       = withPpr x
  exact x@(HsQualTy an _ _)         = withPpr x
  exact x@(HsTyVar an _ _)          = withPpr x
  exact x@(HsAppTy _ t1 t2)         = markAnnotated t1 >> markAnnotated t2
  exact x@(HsAppKindTy an _ _)      = withPpr x
  exact x@(HsFunTy an mult ty1 ty2) = do
    markAnnotated ty1
    markArrow an mult
    markAnnotated ty2
  exact x@(HsListTy an _)           = withPpr x
  exact x@(HsTupleTy an _ _)        = withPpr x
  exact x@(HsSumTy an _)            = withPpr x
  exact x@(HsOpTy an _ _ _)         = withPpr x
  exact x@(HsParTy an _)            = withPpr x
  exact x@(HsIParamTy an _ _)       = withPpr x
  exact x@(HsStarTy an _)           = withPpr x
  exact x@(HsKindSig an _ _)        = withPpr x
  exact x@(HsSpliceTy an _)         = withPpr x
  exact x@(HsDocTy an _ _)          = withPpr x
  exact x@(HsBangTy an _ _)         = withPpr x
  exact x@(HsRecTy an _)            = withPpr x
  exact x@(HsExplicitListTy an _ _) = withPpr x
  exact x@(HsExplicitTupleTy an _)  = withPpr x
  exact x@(HsTyLit an _)            = withPpr x
  exact x@(HsWildCardTy _)          = withPpr x

-- ---------------------------------------------------------------------

instance ExactPrint (HsDerivingClause GhcPs) where
  getApiAnnotation d@(HsDerivingClause{}) = fromAnn (deriv_clause_ext d)
  exact = withPpr

-- ---------------------------------------------------------------------

instance ExactPrint (LocatedN RdrName) where
  getApiAnnotation (L sann _) = fromAnn sann

  exact (L (SrcSpanAnn ApiAnnNotUsed _) n) = do
    printString False (showGhc n)
  exact (L (SrcSpanAnn (ApiAnn _anchor ann _cs) _) n) = do
    case ann of
      NameAnn a o l c t -> do
        markName a o (Just (l,n)) c
        markTrailing t
      NameAnnCommas a o cs c t -> do
        markName a o Nothing c
        markTrailing t
      NameAnnOnly a o c t -> do
        markName a o Nothing c
        markTrailing t
      NameAnnRArrow nl t -> do
        markKw (AddApiAnn AnnRarrow nl)
        markTrailing t
      NameAnnTrailing t -> do
        markTrailing t

markName :: NameAdornment
         -> RealSrcSpan -> Maybe (RealSrcSpan,RdrName) -> RealSrcSpan -> EPP ()
markName adorn open mname close = do
  let (kwo,kwc) = adornments adorn
  markKw (AddApiAnn kwo open)
  case mname of
    Nothing -> return ()
    Just (name, a) -> printStringAtKw' name (showGhc a)
  markKw (AddApiAnn kwc close)
  where
    adornments :: NameAdornment -> (AnnKeywordId, AnnKeywordId)
    adornments NameParens     = (AnnOpenP, AnnCloseP)
    adornments NameParensHash = (AnnOpenPH, AnnClosePH)
    adornments NameBackquotes = (AnnBackquote, AnnBackquote)
    adornments NameSquare     = (AnnOpenS, AnnCloseS)

markTrailing :: [TrailingAnn] -> EPP ()
markTrailing ts = do
  mapM_ markKwT (sort ts)

-- ---------------------------------------------------------------------

-- TODO:AZ generalise LocatedL
instance ExactPrint (LocatedL [CmdLStmt GhcPs]) where
  getApiAnnotation (L (SrcSpanAnn ann _) _) = fromAnn ann
  exact (L (SrcSpanAnn ann _) es) = do

    -- markLocatedAAL ann al_rest AnnHiding
    markLocatedMAA ann al_open
    mapM_ markAnnotated es
    markLocatedMAA ann al_close

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
      (L _ (XConDecl x)  : _) -> True

-- ---------------------------------------------------------------------

-- -- TODO: generalise LocatedA
-- instance ExactPrint (LConDecl GhcPs) where
--   getApiAnnotation (L (SrcSpanAnn ann _) _) = fromAnn ann
--   exact (L _ a) = markAnnotated a

instance ExactPrint (ConDecl GhcPs) where
  getApiAnnotation x@(ConDeclGADT{}) = fromAnn (con_g_ext x)
  getApiAnnotation x@(ConDeclH98{})  = fromAnn (con_ext x)
  getApiAnnotation x@(XConDecl{})    = NoEntryVal

-- based on pprConDecl
  exact (ConDeclH98 { con_ext = an
                    , con_name = con
                    , con_ex_tvs = ex_tvs
                    , con_mb_cxt = mcxt
                    , con_args = args
                    , con_doc = doc }) = do
    -- = sep [ ppr_mbDoc doc
    --       , pprHsForAll (mkHsForAllInvisTele ex_tvs) mcxt
    --       , ppr_details args ]
    -- where
    --   -- In ppr_details: let's not print the multiplicities (they are always 1, by
    --   -- definition) as they do not appear in an actual declaration.
    --   ppr_details (InfixCon t1 t2) = hsep [ppr (hsScaledThing t1),
    --                                        pprInfixOcc con,
    --                                        ppr (hsScaledThing t2)]
    --   ppr_details (PrefixCon tys)  = hsep (pprPrefixOcc con
    --                                  : map (pprHsType . unLoc . hsScaledThing) tys)
    --   ppr_details (RecCon fields)  = pprPrefixOcc con
    --                                <+> pprConDeclFields (unLoc fields)
    mapM_ markAnnotated doc
    -- exactHsForall (mkHsForAllInvisTele ex_tvs) mcxt
    mapM_ markAnnotated mcxt
    case args of
      InfixCon _ _ -> return ()
      _ -> markAnnotated con

    exactHsConDeclDetails an False False [con] args

  -- -----------------------------------

  exact x = withPpr x

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

exactHsConDeclDetails :: ApiAnn
  -> Bool -> Bool -> [LocatedN RdrName] -> HsConDeclDetails GhcPs -> EPP ()
exactHsConDeclDetails an isDeprecated inGadt lns dets = do
  case dets of
    InfixCon a1 a2 -> do
      markAnnotated a1
      mapM_ markAnnotated lns
      markAnnotated a2
    PrefixCon args ->
      mapM_ markAnnotated args
    RecCon fs -> do
      markApiAnn an AnnOpenC
      markAnnotated fs
      markApiAnn an AnnCloseC

-- ---------------------------------------------------------------------

instance ExactPrint (LocatedL [LConDeclField GhcPs]) where
  getApiAnnotation (L (SrcSpanAnn ann _) _) = fromAnn ann
  exact (L (SrcSpanAnn an _) fs) = do
    markLocatedMAA an al_open
    mapM_ markAnnotated fs
    markLocatedMAA an al_close

-- ---------------------------------------------------------------------

-- instance ExactPrint [LConDeclField GhcPs] where
--   getApiAnnotation = const NoEntryVal
--   exact fs = mapM_ markAnnotated fs

-- ---------------------------------------------------------------------

-- instance ExactPrint (LConDeclField GhcPs) where
--   getApiAnnotation (L (SrcSpanAnn ann _) _) = fromAnn ann
--   exact (L _ f) = markAnnotated f

-- ---------------------------------------------------------------------

instance ExactPrint (ConDeclField GhcPs) where
  getApiAnnotation f@(ConDeclField{}) = fromAnn (cd_fld_ext f)
  exact (ConDeclField an names ftype mdoc) = do
    markAnnotated names
    markApiAnn an AnnDcolon
    markAnnotated ftype

-- ---------------------------------------------------------------------

instance ExactPrint [LFieldOcc GhcPs] where
  getApiAnnotation = const NoEntryVal
  exact fs = mapM_ markAnnotated fs

-- ---------------------------------------------------------------------

instance ExactPrint (FieldOcc GhcPs) where
  getApiAnnotation = const NoEntryVal
  exact (FieldOcc _ n) = markAnnotated n

-- ---------------------------------------------------------------------

instance (ExactPrint a) => ExactPrint (HsScaled GhcPs a) where
  getApiAnnotation = const NoEntryVal
  exact (HsScaled arr t) = markAnnotated t

-- ---------------------------------------------------------------------

instance ExactPrint (LHsContext GhcPs) where
  getApiAnnotation (L (SrcSpanAnn ann _) _) = fromAnn ann
  exact = withPpr

-- ---------------------------------------------------------------------

instance ExactPrint (LocatedP CType) where
  getApiAnnotation (L (SrcSpanAnn ann _) _) = fromAnn ann
  exact = withPpr

-- ---------------------------------------------------------------------

instance ExactPrint (LocatedL [LIE GhcPs]) where
  getApiAnnotation (L (SrcSpanAnn ann _) _) = fromAnn ann
  exact (L (SrcSpanAnn ann _) ies) = do

    markLocatedAAL ann al_rest AnnHiding
    markLocatedMAA ann al_open
    mapM_ markAnnotated ies
    markLocatedMAA ann al_close

-- ---------------------------------------------------------------------

-- instance ExactPrint (LIE GhcPs) where
--   getApiAnnotation _ = NoEntryVal
--   exact (L (SrcSpanAnn ann _) a) = do
--     markAnnotated a
--     markALocatedA ann

instance ExactPrint (IE GhcPs) where
  getApiAnnotation (IEVar anns _)             = fromAnn anns
  getApiAnnotation (IEThingAbs anns _)        = fromAnn anns
  getApiAnnotation (IEThingAll anns _)        = fromAnn anns
  getApiAnnotation (IEThingWith anns _ _ _ _) = fromAnn anns
  getApiAnnotation (IEModuleContents anns _)  = fromAnn anns
  getApiAnnotation (IEGroup _ _ _)            = NoEntryVal
  getApiAnnotation (IEDoc _ _)                = NoEntryVal
  getApiAnnotation (IEDocNamed _ _)           = NoEntryVal

  exact = withPpr

-- ---------------------------------------------------------------------

-- instance ExactPrint (LocatedA (Pat GhcPs)) where
--   -- getApiAnnotation (L (SrcSpanAnn ann _) _) = fromAnn ann
--   getApiAnnotation = entryFromLocatedA
--   exact (L _ a) = do
--     debugM $ "exact:LPat:" ++ showGhc a
--     markAnnotated a

instance ExactPrint (Pat GhcPs) where
  getApiAnnotation (WildPat _)                    = NoEntryVal
  getApiAnnotation (VarPat _ ln)                  = NoEntryVal
  getApiAnnotation (LazyPat an pat)               = fromAnn an
  getApiAnnotation (AsPat an n pat)               = fromAnn an
  getApiAnnotation (ParPat _ pat)                = NoEntryVal
  getApiAnnotation (BangPat an pat)               = fromAnn an
  getApiAnnotation (ListPat an pats)              = fromAnn an
  getApiAnnotation (TuplePat an pats boxity)      = fromAnn an
  getApiAnnotation (SumPat an pat contag arity)   = fromAnn an
  getApiAnnotation (ConPat an con args)           = fromAnn an
  getApiAnnotation (ViewPat an expr pat)          = fromAnn an
  getApiAnnotation (SplicePat an splice)          = NoEntryVal
  getApiAnnotation (LitPat an lit)                = NoEntryVal
  getApiAnnotation (NPat x lit _ _)               = NoEntryVal
  getApiAnnotation (NPlusKPat an n lit1 lit2 _ _) = fromAnn an
  getApiAnnotation (SigPat an pat sig)            = fromAnn an


  exact (VarPat _ n) = markAnnotated n
  -- | WildPat _)
  -- | VarPat an ln)
  -- | LazyPat an pat)
  -- | AsPat an n pat)
  exact (ParPat an pat) = do
    markAnnKw an ap_open AnnOpenP
    markAnnotated pat
    markAnnKw an ap_close AnnCloseP

  -- | BangPat an pat)
  -- | ListPat an pats
  -- | TuplePat an pats boxity)
  -- | SumPat an pat contag arity)
  -- | ConPat an con args)
  -- | ViewPat an expr pat)
  -- | SplicePat an splice)
  -- | LitPat an lit)
  -- | NPat x lit _ _)
  -- | NPlusKPat an n lit1 lit2 _ _)
  -- | SigPat an pat sig)
  exact x = withPpr x

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
--         when (showGhc n /= pun_RDR) $
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
--       -- markPat _ (GHC.XPat x) = error $ "got XPat for:" ++ showGhc x


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
  pp <- getPos
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
