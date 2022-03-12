{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils
  -- (
  --  -- * Manipulating Positons
  --   ss2pos
  -- , ss2posEnd
  -- , undelta
  -- , isPointSrcSpan
  -- , pos2delta
  -- , ss2delta
  -- , addDP
  -- , spanLength
  -- , isGoodDelta
  -- ) where
  where
import Control.Monad.State
import Data.Function
import Data.Ord (comparing)

import GHC.Hs.Dump
import Lookup

import GHC hiding (EpaComment)
import qualified GHC
import GHC.Types.Name
import GHC.Types.Name.Reader
import GHC.Types.SrcLoc
import GHC.Driver.Ppr
import GHC.Data.FastString

import qualified GHC.Types.Name.Occurrence as OccName (OccName(..),pprNameSpaceBrief)

import Control.Arrow

import qualified Data.Map as Map
import Data.Data hiding ( Fixity )
import Data.List (sortBy, elemIndex)

import Debug.Trace
import Types

-- ---------------------------------------------------------------------

-- |Global switch to enable debug tracing in ghc-exactprint Delta / Print
debugEnabledFlag :: Bool
-- debugEnabledFlag = True
debugEnabledFlag = False

-- |Global switch to enable debug tracing in ghc-exactprint Pretty
debugPEnabledFlag :: Bool
-- debugPEnabledFlag = True
debugPEnabledFlag = False

-- |Provide a version of trace that comes at the end of the line, so it can
-- easily be commented out when debugging different things.
debug :: c -> String -> c
debug c s = if debugEnabledFlag
              then trace s c
              else c

-- |Provide a version of trace for the Pretty module, which can be enabled
-- separately from 'debug' and 'debugM'
debugP :: String -> c -> c
debugP s c = if debugPEnabledFlag
               then trace s c
               else c

debugM :: Monad m => String -> m ()
debugM s = when debugEnabledFlag $ traceM s


-- ---------------------------------------------------------------------

warn :: c -> String -> c
-- warn = flip trace
warn c _ = c

-- | A good delta has no negative values.
isGoodDelta :: DeltaPos -> Bool
isGoodDelta (SameLine co) = co >= 0
isGoodDelta (DifferentLine ro co) = ro > 0 && co >= 0
  -- Note: DifferentLine invariant is ro is nonzero and positive


-- | Create a delta from the current position to the start of the given
-- @SrcSpan@.
ss2delta :: Pos -> RealSrcSpan -> DeltaPos
ss2delta ref ss = pos2delta ref (ss2pos ss)

-- | create a delta from the end of a current span.  The +1 is because
-- the stored position ends up one past the span, this is prior to
-- that adjustment
ss2deltaEnd :: RealSrcSpan -> RealSrcSpan -> DeltaPos
ss2deltaEnd rrs ss = ss2delta ref ss
  where
    (r,c) = ss2posEnd rrs
    ref = if r == 0
             then (r,c+1)
             else (r,c)

-- | create a delta from the start of a current span.  The +1 is
-- because the stored position ends up one past the span, this is
-- prior to that adjustment
ss2deltaStart :: RealSrcSpan -> RealSrcSpan -> DeltaPos
ss2deltaStart rrs ss = ss2delta ref ss
  where
    (r,c) = ss2pos rrs
    ref = if r == 0
             then (r,c)
             else (r,c)

-- | Convert the start of the second @Pos@ to be an offset from the
-- first. The assumption is the reference starts before the second @Pos@
pos2delta :: Pos -> Pos -> DeltaPos
pos2delta (refl,refc) (l,c) = deltaPos lo co
  where
    lo = l - refl
    co = if lo == 0 then c - refc
                    else c

-- | Apply the delta to the current position, taking into account the
-- current column offset if advancing to a new line
undelta :: Pos -> DeltaPos -> LayoutStartCol -> Pos
undelta (l,c) (SameLine dc)         (LayoutStartCol _co) = (l, c + dc)
undelta (l,_) (DifferentLine dl dc) (LayoutStartCol co) = (fl,fc)
  where
    -- Note: invariant: dl > 0
    fl = l + dl
    fc = co + dc

undeltaSpan :: RealSrcSpan -> AnnKeywordId -> DeltaPos -> AddEpAnn
undeltaSpan anchor kw dp = AddEpAnn kw (EpaSpan sp)
  where
    (l,c) = undelta (ss2pos anchor) dp (LayoutStartCol 0)
    len = length (keywordToString (G kw))
    sp = range2rs ((l,c),(l,c+len))

-- | Add together two @DeltaPos@ taking into account newlines
--
-- > DP (0, 1) `addDP` DP (0, 2) == DP (0, 3)
-- > DP (0, 9) `addDP` DP (1, 5) == DP (1, 5)
-- > DP (1, 4) `addDP` DP (1, 3) == DP (2, 3)
addDP :: DeltaPos -> DeltaPos -> DeltaPos
addDP dp (DifferentLine c d) = DifferentLine (getDeltaLine dp+c) d
addDP (DifferentLine a b) (SameLine  d) = DifferentLine a (b+d)
addDP (SameLine b)        (SameLine  d) = SameLine (b+d)

-- ---------------------------------------------------------------------

adjustDeltaForOffset :: Int -> LayoutStartCol -> DeltaPos -> DeltaPos
adjustDeltaForOffset _ _colOffset                      dp@(SameLine _) = dp
adjustDeltaForOffset d (LayoutStartCol colOffset) (DifferentLine l c)
  = DifferentLine l (c - colOffset - d)

-- ---------------------------------------------------------------------

ss2pos :: RealSrcSpan -> Pos
ss2pos ss = (srcSpanStartLine ss,srcSpanStartCol ss)

ss2posEnd :: RealSrcSpan -> Pos
ss2posEnd ss = (srcSpanEndLine ss,srcSpanEndCol ss)

ss2range :: SrcSpan -> (Pos,Pos)
ss2range ss = (ss2pos $ rs ss, ss2posEnd $ rs ss)

rs2range :: RealSrcSpan -> (Pos,Pos)
rs2range ss = (ss2pos ss, ss2posEnd ss)

rs :: SrcSpan -> RealSrcSpan
rs (RealSrcSpan s _) = s
rs _ = badRealSrcSpan

range2rs :: (Pos,Pos) -> RealSrcSpan
range2rs (s,e) = mkRealSrcSpan (mkLoc s) (mkLoc e)
  where
    mkLoc (l,c) = mkRealSrcLoc (fsLit "ghc-exactprint") l c

badRealSrcSpan :: RealSrcSpan
badRealSrcSpan = mkRealSrcSpan bad bad
  where
    bad = mkRealSrcLoc (fsLit "ghc-exactprint-nospan") 0 0

spanLength :: RealSrcSpan -> Int
spanLength = (-) <$> srcSpanEndCol <*> srcSpanStartCol

-- ---------------------------------------------------------------------
-- | Checks whether a SrcSpan has zero length.
isPointSrcSpan :: RealSrcSpan -> Bool
isPointSrcSpan ss = spanLength ss == 0
                  && srcSpanStartLine ss == srcSpanEndLine ss

-- ---------------------------------------------------------------------

-- |Given a list of items and a list of keys, returns a list of items
-- ordered by their position in the list of keys.
orderByKey :: [(RealSrcSpan,a)] -> [RealSrcSpan] -> [(RealSrcSpan,a)]
orderByKey keys order
    -- AZ:TODO: if performance becomes a problem, consider a Map of the order
    -- SrcSpan to an index, and do a lookup instead of elemIndex.

    -- Items not in the ordering are placed to the start
 = sortBy (comparing (flip elemIndex order . fst)) keys

-- ---------------------------------------------------------------------

isListComp :: HsDoFlavour -> Bool
isListComp = isDoComprehensionContext

-- ---------------------------------------------------------------------

isGadt :: [LConDecl (GhcPass p)] -> Bool
isGadt [] = False
isGadt ((L _ (ConDeclGADT{})):_) = True
isGadt _ = False

-- ---------------------------------------------------------------------

-- Is a RdrName of type Exact? SYB query, so can be extended to other types too
isExactName :: (Data name) => name -> Bool
isExactName = False `mkQ` isExact

-- ---------------------------------------------------------------------

insertCppComments ::  ParsedSource -> [LEpaComment] -> ParsedSource
insertCppComments (L l p) cs = L l p'
  where
    ncs = EpaComments cs
    an' = case GHC.hsmodAnn p of
      (EpAnn a an ocs) -> EpAnn a an (ocs <> ncs)
      unused -> unused
    p' = p { GHC.hsmodAnn = an' }

-- ---------------------------------------------------------------------

ghcCommentText :: LEpaComment -> String
ghcCommentText (L _ (GHC.EpaComment (EpaDocComment s) _))      = exactPrintHsDocString s
ghcCommentText (L _ (GHC.EpaComment (EpaDocOptions s) _))      = s
ghcCommentText (L _ (GHC.EpaComment (EpaLineComment s) _))     = s
ghcCommentText (L _ (GHC.EpaComment (EpaBlockComment s) _))    = s
ghcCommentText (L _ (GHC.EpaComment (EpaEofComment) _))        = ""

tokComment :: LEpaComment -> Comment
tokComment t@(L lt _) = mkComment (normaliseCommentText $ ghcCommentText t) lt

mkLEpaComment :: String -> Anchor -> LEpaComment
-- Note: fudging the ac_prior_tok value, hope it does not cause a problem
mkLEpaComment s anc = (L anc (GHC.EpaComment (EpaLineComment s) (anchor anc)))

mkComment :: String -> Anchor -> Comment
mkComment c anc = Comment c anc Nothing

-- Windows comments include \r in them from the lexer.
normaliseCommentText :: String -> String
normaliseCommentText [] = []
normaliseCommentText ('\r':xs) = normaliseCommentText xs
normaliseCommentText (x:xs) = x:normaliseCommentText xs

-- | Makes a comment which originates from a specific keyword.
mkKWComment :: AnnKeywordId -> EpaLocation -> [Comment]
mkKWComment kw (EpaSpan ss)
  = [Comment (keywordToString $ G kw) (Anchor ss UnchangedAnchor) (Just kw)]
mkKWComment kw (EpaDelta dp cs)
  = (map tokComment cs) ++ [Comment (keywordToString $ G kw) (Anchor placeholderRealSpan (MovedAnchor dp)) (Just kw)]

comment2dp :: (Comment,  DeltaPos) -> (KeywordId, DeltaPos)
comment2dp = first AnnComment

sortAnchorLocated :: [GenLocated Anchor a] -> [GenLocated Anchor a]
sortAnchorLocated = sortBy (compare `on` (anchor . getLoc))

getAnnotationEP :: (Data a) =>  Located a  -> Anns -> Maybe Annotation
getAnnotationEP  la as =
  Map.lookup (mkAnnKey la) as

-- | The "true entry" is the distance from the last concrete element to the
-- start of the current element.
annTrueEntryDelta :: Annotation -> DeltaPos
annTrueEntryDelta Ann{annEntryDelta, annPriorComments} =
  foldr addDP (SameLine 0) (map (\(a, b) -> addDP b (dpFromString $ commentContents a)) annPriorComments )
    `addDP` annEntryDelta

-- | Return the DP of the first item that generates output, either a comment or the entry DP
annLeadingCommentEntryDelta :: Annotation -> DeltaPos
annLeadingCommentEntryDelta Ann{annPriorComments,annEntryDelta} = dp
  where
    dp = case annPriorComments of
      [] -> annEntryDelta
      ((_,ed):_) -> ed

-- | Calculates the distance from the start of a string to the end of
-- a string.
dpFromString ::  String -> DeltaPos
dpFromString xs = dpFromString' xs 0 0
  where
    dpFromString' "" line col =
      if line == 0
        then SameLine col
        else DifferentLine line col
    dpFromString' ('\n': cs) line _   = dpFromString' cs (line + 1) 0
    dpFromString' (_:cs)     line col = dpFromString' cs line       (col + 1)

-- ---------------------------------------------------------------------

isSymbolRdrName :: RdrName -> Bool
isSymbolRdrName n = isSymOcc $ rdrNameOcc n

rdrName2String :: RdrName -> String
rdrName2String r =
  case isExact_maybe r of
    Just n  -> name2String n
    Nothing ->
      case r of
        Unqual occ       -> occNameString occ
        Qual modname occ -> moduleNameString modname ++ "."
                                ++ occNameString occ
        Orig _ occ       -> occNameString occ
        Exact n          -> getOccString n

name2String :: Name -> String
name2String = showPprUnsafe

-- ---------------------------------------------------------------------

occAttributes :: OccName.OccName -> String
occAttributes o = "(" ++ ns ++ vo ++ tv ++ tc ++ d ++ ds ++ s ++ v ++ ")"
  where
    -- ns = (showSDocUnsafe $ OccName.pprNameSpaceBrief $ occNameSpace o) ++ ", "
    ns = (showSDocUnsafe $ OccName.pprNameSpaceBrief $ occNameSpace o) ++ ", "
    vo = if isVarOcc     o then "Var "     else ""
    tv = if isTvOcc      o then "Tv "      else ""
    tc = if isTcOcc      o then "Tc "      else ""
    d  = if isDataOcc    o then "Data "    else ""
    ds = if isDataSymOcc o then "DataSym " else ""
    s  = if isSymOcc     o then "Sym "     else ""
    v  = if isValOcc     o then "Val "     else ""

 -- ---------------------------------------------------------------------

locatedAnAnchor :: LocatedAn a t -> RealSrcSpan
locatedAnAnchor (L (SrcSpanAnn EpAnnNotUsed l) _) = realSrcSpan l
locatedAnAnchor (L (SrcSpanAnn (EpAnn a _ _) _) _) = anchor a

 -- ---------------------------------------------------------------------

showAst :: (Data a) => a -> String
showAst ast
  = showSDocUnsafe
    $ showAstData NoBlankSrcSpan NoBlankEpAnnotations ast

-- ---------------------------------------------------------------------
-- Putting these here for the time being, to avoid import loops

ghead :: String -> [a] -> a
ghead  info []    = error $ "ghead "++info++" []"
ghead _info (h:_) = h

glast :: String -> [a] -> a
glast  info []    = error $ "glast " ++ info ++ " []"
glast _info h     = last h

gtail :: String -> [a] -> [a]
gtail  info []   = error $ "gtail " ++ info ++ " []"
gtail _info h    = tail h

gfromJust :: String -> Maybe a -> a
gfromJust _info (Just h) = h
gfromJust  info Nothing = error $ "gfromJust " ++ info ++ " Nothing"

-- ---------------------------------------------------------------------

-- Copied from syb for the test


-- | Generic queries of type \"r\",
--   i.e., take any \"a\" and return an \"r\"
--
type GenericQ r = forall a. Data a => a -> r


-- | Make a generic query;
--   start from a type-specific case;
--   return a constant otherwise
--
mkQ :: ( Typeable a
       , Typeable b
       )
    => r
    -> (b -> r)
    -> a
    -> r
(r `mkQ` br) a = case cast a of
                        Just b  -> br b
                        Nothing -> r

-- | Make a generic monadic transformation;
--   start from a type-specific case;
--   resort to return otherwise
--
mkM :: ( Monad m
       , Typeable a
       , Typeable b
       )
    => (b -> m b)
    -> a
    -> m a
mkM = extM return

-- | Flexible type extension
ext0 :: (Typeable a, Typeable b) => c a -> c b -> c a
ext0 def ext = maybe def id (gcast ext)


-- | Extend a generic query by a type-specific case
extQ :: ( Typeable a
        , Typeable b
        )
     => (a -> q)
     -> (b -> q)
     -> a
     -> q
extQ f g a = maybe (f a) g (cast a)

-- | Flexible type extension
ext2 :: (Data a, Typeable t)
     => c a
     -> (forall d1 d2. (Data d1, Data d2) => c (t d1 d2))
     -> c a
ext2 def ext = maybe def id (dataCast2 ext)


-- | Extend a generic monadic transformation by a type-specific case
extM :: ( Monad m
        , Typeable a
        , Typeable b
        )
     => (a -> m a) -> (b -> m b) -> a -> m a
extM def ext = unM ((M def) `ext0` (M ext))

-- | Type extension of monadic transformations for type constructors
ext2M :: (Monad m, Data d, Typeable t)
      => (forall e. Data e => e -> m e)
      -> (forall d1 d2. (Data d1, Data d2) => t d1 d2 -> m (t d1 d2))
      -> d -> m d
ext2M def ext = unM ((M def) `ext2` (M ext))

-- | The type constructor for transformations
newtype M m x = M { unM :: x -> m x }

-- | Generic monadic transformations,
--   i.e., take an \"a\" and compute an \"a\"
--
type GenericM m = forall a. Data a => a -> m a

-- | Monadic variation on everywhere
everywhereM :: forall m. Monad m => GenericM m -> GenericM m

-- Bottom-up order is also reflected in order of do-actions
everywhereM f = go
  where
    go :: GenericM m
    go x = do
      x' <- gmapM go x
      f x'
