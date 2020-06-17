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
import qualified Data.ByteString as B
import GHC.Generics hiding (Fixity)
import Data.Ord (comparing)

import GHC.Hs.Dump
-- import Language.Haskell.GHC.ExactPrint.Types
import Lookup

import GHC.Data.Bag
import GHC.Driver.Session
import GHC.Data.FastString
import GHC
-- import qualified Name           as GHC
-- import qualified NameSet        as GHC
import GHC.Utils.Outputable
import GHC.Types.Name
import GHC.Types.Name.Reader
import GHC.Types.SrcLoc
import GHC.Types.Var
import GHC.Types.Name.Occurrence

-- import qualified OccName(OccName(..),occNameString,pprNameSpaceBrief)
import qualified GHC.Types.Name.Occurrence as OccName (OccName(..),occNameString,pprNameSpaceBrief)

import Control.Arrow

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Data hiding ( Fixity )
import Data.List

import Debug.Trace
import Types

-- ---------------------------------------------------------------------
-- ---------------------------------------------------------------------
-- ---------------------------------------------------------------------

-- |Global switch to enable debug tracing in ghc-exactprint Delta / Print
debugEnabledFlag :: Bool
debugEnabledFlag = True
-- debugEnabledFlag = False

-- |Global switch to enable debug tracing in ghc-exactprint Pretty
debugPEnabledFlag :: Bool
debugPEnabledFlag = True
-- debugPEnabledFlag = False

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
isGoodDelta (DP (ro,co)) = ro >= 0 && co >= 0


-- | Create a delta from the current position to the start of the given
-- @SrcSpan@.
ss2delta :: Pos -> RealSrcSpan -> DeltaPos
ss2delta ref ss = pos2delta ref (ss2pos ss)

-- | Convert the start of the second @Pos@ to be an offset from the
-- first. The assumption is the reference starts before the second @Pos@
pos2delta :: Pos -> Pos -> DeltaPos
pos2delta (refl,refc) (l,c) = DP (lo,co)
  where
    lo = l - refl
    co = if lo == 0 then c - refc
                    else c

-- | Apply the delta to the current position, taking into account the
-- current column offset if advancing to a new line
undelta :: Pos -> DeltaPos -> LayoutStartCol -> Pos
undelta (l,c) (DP (dl,dc)) (LayoutStartCol co) = (fl,fc)
  where
    fl = l + dl
    fc = if dl == 0 then c  + dc
                    else co + dc

-- | Add together two @DeltaPos@ taking into account newlines
--
-- > DP (0, 1) `addDP` DP (0, 2) == DP (0, 3)
-- > DP (0, 9) `addDP` DP (1, 5) == DP (1, 5)
-- > DP (1, 4) `addDP` DP (1, 3) == DP (2, 3)
addDP :: DeltaPos -> DeltaPos -> DeltaPos
addDP (DP (a, b)) (DP (c, d)) =
  if c >= 1 then DP (a+c, d)
            else DP (a,   b+d)

-- | "Subtract" two @DeltaPos@ from each other, in the sense of calculating the
-- remaining delta for the second after the first has been applied.
-- invariant : if c = a `addDP` b
--             then a `stepDP` c == b
--
-- Cases where first DP is <= than second
-- > DP (0, 1) `addDP` DP (0, 2) == DP (0, 1)
-- > DP (1, 1) `addDP` DP (2, 0) == DP (1, 0)
-- > DP (1, 3) `addDP` DP (1, 4) == DP (0, 1)
-- > DP (1, 4) `addDP` DP (1, 4) == DP (1, 4)
--
-- Cases where first DP is > than second
-- > DP (0,  3) `addDP` DP (0, 2) == DP (0,1)  -- advance one at least
-- > DP (3,  3) `addDP` DP (2, 4) == DP (1, 4) -- go one line forward and to expected col
-- > DP (3,  3) `addDP` DP (0, 4) == DP (0, 1) -- maintain col delta at least
-- > DP (1, 21) `addDP` DP (1, 4) == DP (1, 4) -- go one line forward and to expected col
stepDP :: DeltaPos -> DeltaPos -> DeltaPos
stepDP (DP (a,b)) (DP (c,d))
  | (a,b) == (c,d) = DP (a,b)
  | a == c = if b < d then DP (0,d - b)
                      else if d == 0
                             then DP (1,0)
                             -- else DP (0,1)
                             else DP (c,d)
  | a < c = DP (c - a,d)
  | otherwise = DP (1,d)

-- ---------------------------------------------------------------------

ss2pos :: RealSrcSpan -> Pos
ss2pos ss = (srcSpanStartLine ss,srcSpanStartCol ss)

ss2posEnd :: RealSrcSpan -> Pos
ss2posEnd ss = (srcSpanEndLine ss,srcSpanEndCol ss)

-- srcSpanEndColumn :: SrcSpan -> Int
-- srcSpanEndColumn (RealSrcSpan s) = srcSpanEndCol s
-- srcSpanEndColumn _ = 0

-- srcSpanStartColumn :: SrcSpan -> Int
-- srcSpanStartColumn (RealSrcSpan s) = srcSpanStartCol s
-- srcSpanStartColumn _ = 0

-- srcSpanEndLine :: SrcSpan -> Int
-- srcSpanEndLine (RealSrcSpan s) = srcSpanEndLine s
-- srcSpanEndLine _ = 0

-- srcSpanStartLine :: SrcSpan -> Int
-- srcSpanStartLine (RealSrcSpan s) = srcSpanStartLine s
-- srcSpanStartLine _ = 0

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
orderByKey :: [(SrcSpan,a)] -> [SrcSpan] -> [(SrcSpan,a)]
orderByKey keys order
    -- AZ:TODO: if performance becomes a problem, consider a Map of the order
    -- SrcSpan to an index, and do a lookup instead of elemIndex.

    -- Items not in the ordering are placed to the start
 = sortBy (comparing (flip elemIndex order . fst)) keys

-- ---------------------------------------------------------------------

isListComp :: HsStmtContext name -> Bool
isListComp cts = case cts of
          ListComp  -> True
          MonadComp -> True

          DoExpr       -> False
          MDoExpr      -> False
          ArrowExpr    -> False
          GhciStmtCtxt -> False

          PatGuard {}      -> False
          ParStmtCtxt {}   -> False
          TransStmtCtxt {} -> False

-- ---------------------------------------------------------------------

isGadt :: [LConDecl name] -> Bool
isGadt [] = False
isGadt ((L _ (ConDeclGADT{})):_) = True
isGadt _ = False

-- ---------------------------------------------------------------------

-- Is a RdrName of type Exact? SYB query, so can be extended to other types too
isExactName :: (Data name) => name -> Bool
isExactName = False `mkQ` isExact

-- ---------------------------------------------------------------------

ghcCommentText :: RealLocated AnnotationComment -> String
ghcCommentText (L _ (AnnDocCommentNext s))  = s
ghcCommentText (L _ (AnnDocCommentPrev s))  = s
ghcCommentText (L _ (AnnDocCommentNamed s)) = s
ghcCommentText (L _ (AnnDocSection _ s))    = s
ghcCommentText (L _ (AnnDocOptions s))      = s
ghcCommentText (L _ (AnnLineComment s))     = s
ghcCommentText (L _ (AnnBlockComment s))    = s

tokComment :: RealLocated AnnotationComment -> Comment
tokComment t@(L lt _) = mkComment (ghcCommentText t) lt

mkComment :: String -> RealSrcSpan -> Comment
mkComment c ss = Comment c ss Nothing

-- | Makes a comment which originates from a specific keyword.
mkKWComment :: AnnKeywordId -> RealSrcSpan -> Comment
mkKWComment kw ss = Comment (keywordToString $ G kw) ss (Just kw)

comment2dp :: (Comment,  DeltaPos) -> (KeywordId, DeltaPos)
comment2dp = first AnnComment


rogueComments :: ApiAnns -> [Comment]
rogueComments as = extractRogueComments as
  -- where
  --   go :: Comment -> (Comment, DeltaPos)
  --   go c@(Comment _str loc _mo) = (c, ss2delta (1,1) loc)

extractComments :: ApiAnns -> [Comment]
extractComments anns
  -- cm has type :: Map RealSrcSpan [RealLocated AnnotationComment]
  -- = map tokComment . sortRealLocated . concat $ Map.elems (apiAnnComments anns)
  = []

extractRogueComments :: ApiAnns -> [Comment]
extractRogueComments anns
  -- cm has type :: Map RealSrcSpan [RealLocated AnnotationComment]
  = map tokComment $ sortRealLocated  (apiAnnRogueComments anns)


getAnnotationEP :: (Data a) =>  Located a  -> Anns -> Maybe Annotation
getAnnotationEP  la as =
  Map.lookup (mkAnnKey la) as

-- | The "true entry" is the distance from the last concrete element to the
-- start of the current element.
annTrueEntryDelta :: Annotation -> DeltaPos
annTrueEntryDelta Ann{annEntryDelta, annPriorComments} =
  foldr addDP (DP (0,0)) (map (\(a, b) -> addDP b (dpFromString $ commentContents a)) annPriorComments )
    `addDP` annEntryDelta

-- | Take an annotation and a required "true entry" and calculate an equivalent
-- one relative to the last comment in the annPriorComments.
annCommentEntryDelta :: Annotation -> DeltaPos -> DeltaPos
annCommentEntryDelta Ann{annPriorComments} trueDP = dp
  where
    commentDP =
      foldr addDP (DP (0,0)) (map (\(a, b) -> addDP b (dpFromString $ commentContents a)) annPriorComments )
    dp = stepDP commentDP trueDP

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
    dpFromString' "" line col = DP (line, col)
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
name2String = showGhc

-- ---------------------------------------------------------------------

-- | Put the provided context elements into the existing set with fresh level
-- counts
setAcs :: Set.Set AstContext -> AstContextSet -> AstContextSet
setAcs ctxt acs = setAcsWithLevel ctxt 3 acs

-- | Put the provided context elements into the existing set with given level
-- counts
-- setAcsWithLevel :: Set.Set AstContext -> Int -> AstContextSet -> AstContextSet
-- setAcsWithLevel ctxt level (ACS a) = ACS a'
--   where
--     upd s (k,v) = Map.insert k v s
--     a' = foldl' upd a $ zip (Set.toList ctxt) (repeat level)
setAcsWithLevel :: (Ord a) => Set.Set a -> Int -> ACS' a -> ACS' a
setAcsWithLevel ctxt level (ACS a) = ACS a'
  where
    upd s (k,v) = Map.insert k v s
    a' = foldl' upd a $ zip (Set.toList ctxt) (repeat level)

-- ---------------------------------------------------------------------
-- | Remove the provided context element from the existing set
-- unsetAcs :: AstContext -> AstContextSet -> AstContextSet
unsetAcs :: (Ord a) => a -> ACS' a -> ACS' a
unsetAcs ctxt (ACS a) = ACS $ Map.delete ctxt a

-- ---------------------------------------------------------------------

-- | Are any of the contexts currently active?
-- inAcs :: Set.Set AstContext -> AstContextSet -> Bool
inAcs :: (Ord a) => Set.Set a -> ACS' a -> Bool
inAcs ctxt (ACS a) = not $ Set.null $ Set.intersection ctxt (Set.fromList $ Map.keys a)

-- | propagate the ACS down a level, dropping all values which hit zero
-- pushAcs :: AstContextSet -> AstContextSet
pushAcs :: ACS' a -> ACS' a
pushAcs (ACS a) = ACS $ Map.mapMaybe f a
  where
    f n
      | n <= 1    = Nothing
      | otherwise = Just (n - 1)

-- |Sometimes we have to pass the context down unchanged. Bump each count up by
-- one so that it is unchanged after a @pushAcs@ call.
-- bumpAcs :: AstContextSet -> AstContextSet
bumpAcs :: ACS' a -> ACS' a
bumpAcs (ACS a) = ACS $ Map.mapMaybe f a
  where
    f n = Just (n + 1)

-- ---------------------------------------------------------------------
{-
-- | Show a GHC AST with interleaved Annotation information.
showAnnData :: Data a => Anns -> Int -> a -> String
showAnnData anns n =
  generic -- `ext1Q` located
          `ext1Q` list
          `extQ` string `extQ` fastString `extQ` srcSpan
          `extQ` bytestring
          `extQ` name `extQ` occName `extQ` moduleName `extQ` var `extQ` dataCon
          -- `extQ` overLit
          `extQ` bagName `extQ` bagRdrName `extQ` bagVar `extQ` nameSet
          `extQ` fixity
          `ext2Q` located
  where generic :: Data a => a -> String
        generic t = indent n ++ "(" ++ showConstr (toConstr t)
                 ++ space (unwords (gmapQ (showAnnData anns (n+1)) t)) ++ ")"
        space "" = ""
        space s  = ' ':s
        indent i = "\n" ++ replicate i ' '
        string     = show :: String -> String
        fastString = ("{FastString: "++) . (++"}") . show :: FastString -> String
        bytestring = show :: B.ByteString -> String
        list l     = indent n ++ "["
                              ++ intercalate "," (map (showAnnData anns (n+1)) l) ++ "]"

        name       = ("{Name: "++) . (++"}") . showSDocDebug_ . ppr :: Name -> String
        -- occName    = ("{OccName: "++) . (++"}") .  OccName.occNameString
        occName o   = "{OccName: "++ OccName.occNameString o ++ " " ++ occAttributes o ++ "}"
        moduleName = ("{ModuleName: "++) . (++"}") . showSDoc_ . ppr :: ModuleName -> String

        -- srcSpan    = ("{"++) . (++"}") . showSDoc_ . ppr :: SrcSpan -> String
        srcSpan :: SrcSpan -> String
        srcSpan ss = "{ "++ showSDoc_ (hang (ppr ss) (n+2)
                                                 -- (ppr (Map.lookup ss anns)
                                                 (text "")
                                                 )
                      ++"}"

        var        = ("{Var: "++) . (++"}") . showSDocDebug_ . ppr :: Var -> String
        dataCon    = ("{DataCon: "++) . (++"}") . showSDoc_ . ppr :: DataCon -> String

        -- overLit :: HsOverLit RdrName -> String
        -- overLit    = ("{HsOverLit:"++) . (++"}") . showSDoc_ . ppr

        bagRdrName:: Bag (Located (HsBind GhcPs)) -> String
        bagRdrName = ("{Bag(Located (HsBind RdrName)): "++) . (++"}") . list . bagToList
        bagName   :: Bag (Located (HsBind GhcRn)) -> String
        bagName    = ("{Bag(Located (HsBind Name)): "++) . (++"}") . list . bagToList
        bagVar    :: Bag (Located (HsBind GhcTc)) -> String
        bagVar     = ("{Bag(Located (HsBind Var)): "++) . (++"}") . list . bagToList

        nameSet = ("{NameSet: "++) . (++"}") . list . nameSetElemsStable

        fixity = ("{Fixity: "++) . (++"}") . showSDoc_ . ppr :: Fixity -> String

        located :: (Data b,Data loc) => GenLocated loc b -> String
        -- located la = show (getAnnotationEP la anns)
        located (L ss a) =
          indent n ++ "("
            ++ case cast ss of
                    Just (s :: SrcSpan) ->
                      srcSpan s
                      ++ indent (n + 1) ++
                      show (getAnnotationEP (L s a) anns)
                      -- ++ case showWrappedDeclAnns (L s a) of
                      --   Nothing -> ""
                      --   Just annStr  -> indent (n + 1) ++ annStr
                    Nothing -> "nnnnnnnn"
                  ++ showAnnData anns (n+1) a
                  ++ ")"
-}

occAttributes :: OccName.OccName -> String
occAttributes o = "(" ++ ns ++ vo ++ tv ++ tc ++ d ++ ds ++ s ++ v ++ ")"
  where
    ns = (showSDocUnsafe $ OccName.pprNameSpaceBrief $ occNameSpace o) ++ ", "
    vo = if isVarOcc     o then "Var "     else ""
    tv = if isTvOcc      o then "Tv "      else ""
    tc = if isTcOcc      o then "Tc "      else ""
    d  = if isDataOcc    o then "Data "    else ""
    ds = if isDataSymOcc o then "DataSym " else ""
    s  = if isSymOcc     o then "Sym "     else ""
    v  = if isValOcc     o then "Val "     else ""

{-
data NameSpace = VarName        -- Variables, including "real" data constructors
               | DataName       -- "Source" data constructors
               | TvName         -- Type variables
               | TcClsName      -- Type constructors and classes; Haskell has them
                                -- in the same name space for now.
-}

 -- ---------------------------------------------------------------------

showSDoc_ :: SDoc -> String
showSDoc_ = showSDoc unsafeGlobalDynFlags

showSDocDebug_ :: SDoc -> String
showSDocDebug_ = showSDocDebug unsafeGlobalDynFlags


 -- ---------------------------------------------------------------------

showAst :: (Data a) => a -> String
showAst ast
  = showSDoc unsafeGlobalDynFlags
    $ showAstData NoBlankSrcSpan NoBlankApiAnnotations ast

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
