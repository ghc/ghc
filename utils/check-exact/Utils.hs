{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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

import Control.Monad (when)
import GHC.Utils.Monad.State.Strict
import Data.Function

import GHC.Hs.Dump

import GHC hiding (EpaComment)
import qualified GHC
import GHC.Types.Name
import GHC.Types.Name.Reader
import GHC.Types.SrcLoc
import GHC.Driver.Ppr
import GHC.Data.FastString
import GHC.Base (NonEmpty(..))
import GHC.Parser.Lexer (allocateComments)

import Data.Data hiding ( Fixity )
import Data.List (sortBy, partition)
import qualified Data.Map.Strict as Map

import Debug.Trace
import Types

-- ---------------------------------------------------------------------

-- |Global switch to enable debug tracing in ghc-exactprint Delta / Print
debugEnabledFlag :: Bool
-- debugEnabledFlag = True
debugEnabledFlag = False

-- |Provide a version of trace that comes at the end of the line, so it can
-- easily be commented out when debugging different things.
debug :: c -> String -> c
debug c s = if debugEnabledFlag
              then trace s c
              else c
debugM :: Monad m => String -> m ()
debugM s = when debugEnabledFlag $ traceM s

warn :: c -> String -> c
-- warn = flip trace
warn c _ = c

-- ---------------------------------------------------------------------

captureOrderBinds :: [LHsDecl GhcPs] -> AnnSortKey BindTag
captureOrderBinds ls = AnnSortKey $ map go ls
  where
    go (L _ (ValD _ _))       = BindTag
    go (L _ (SigD _ _))       = SigDTag
    go d      = error $ "captureOrderBinds:" ++ showGhc d

-- ---------------------------------------------------------------------

notDocDecl :: LHsDecl GhcPs -> Bool
notDocDecl (L _ DocD{}) = False
notDocDecl _ = True

notIEDoc :: LIE GhcPs -> Bool
notIEDoc (L _ IEGroup {})    = False
notIEDoc (L _ IEDoc {})      = False
notIEDoc (L _ IEDocNamed {}) = False
notIEDoc _ = True

-- ---------------------------------------------------------------------
-- | A good delta has no negative values.
isGoodDelta :: DeltaPos -> Bool
isGoodDelta (SameLine co) = co >= 0
isGoodDelta (DifferentLine ro _co) = ro > 0
  -- Note: DifferentLine invariant is ro is nonzero and positive


-- | Create a delta from the current position to the start of the given
-- @RealSrcSpan@.
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

-- ---------------------------------------------------------------------

adjustDeltaForOffset :: LayoutStartCol -> DeltaPos -> DeltaPos
adjustDeltaForOffset _colOffset                      dp@(SameLine _) = dp
adjustDeltaForOffset (LayoutStartCol colOffset) (DifferentLine l c)
  = DifferentLine l (c - colOffset)

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


-- | Useful for debug dumps
eloc2str :: EpaLocation -> String
eloc2str (EpaSpan r) = "EpaSpan " ++ show (ss2range r)
eloc2str epaLoc = show epaLoc

-- ---------------------------------------------------------------------
-- | Checks whether a SrcSpan has zero length.
isPointSrcSpan :: RealSrcSpan -> Bool
isPointSrcSpan ss = spanLength ss == 0
                  && srcSpanStartLine ss == srcSpanEndLine ss

-- ---------------------------------------------------------------------

-- | A GHC comment includes the span of the preceding token.  Take an
-- original comment, and convert the 'Anchor to have a have a
-- `MovedAnchor` operation based on the original location, only if it
-- does not already have one.
commentOrigDelta :: LEpaComment -> LEpaComment
commentOrigDelta (L (EpaSpan ss@(RealSrcSpan la _)) (GHC.EpaComment t pp))
  = (L (EpaDelta ss dp NoComments) (GHC.EpaComment t pp))
                  `debug` ("commentOrigDelta: (la, pp, r,c, dp)=" ++ showAst (la, pp, r,c, dp))
  where
        (r,c) = ss2posEnd pp

        dp = if r == 0
               then (ss2delta (r,c+1) la)
               else (ss2delta (r,c)   la)
commentOrigDelta c = c

origDelta :: RealSrcSpan -> RealSrcSpan -> DeltaPos
origDelta pos pp = ss2delta (ss2posEnd pp) pos

-- ---------------------------------------------------------------------

needsWhere :: DataDefnCons (LConDecl (GhcPass p)) -> Bool
needsWhere (NewTypeCon _) = True
needsWhere (DataTypeCons _ []) = True
needsWhere (DataTypeCons _ ((L _ (ConDeclGADT{})):_)) = True
needsWhere _ = False

-- ---------------------------------------------------------------------

-- | Insert the comments at the appropriate places in the AST
insertCppComments ::  ParsedSource -> [LEpaComment] -> ParsedSource
-- insertCppComments p [] = p
insertCppComments (L l p) cs0 = insertRemainingCppComments (L l p2) remaining
  where
    (EpAnn anct ant cst) = hsmodAnn $ hsmodExt p
    cs = sortEpaComments $ priorComments cst ++ getFollowingComments cst ++ cs0
    p0 = p { hsmodExt = (hsmodExt p) { hsmodAnn = EpAnn anct ant emptyComments }}
    -- Comments embedded within spans
    -- everywhereM is a bottom-up traversal
    (p1, toplevel) = runState (everywhereM (mkM   addCommentsListItem
                                           `extM` addCommentsGrhs
                                           `extM` addCommentsList) p0) cs
    (p2, remaining) = insertTopLevelCppComments p1 toplevel

    addCommentsListItem :: EpAnn AnnListItem -> State [LEpaComment] (EpAnn AnnListItem)
    addCommentsListItem = addComments

    addCommentsList :: EpAnn (AnnList ()) -> State [LEpaComment] (EpAnn (AnnList ()))
    addCommentsList = addComments

    addCommentsGrhs :: EpAnn GrhsAnn -> State [LEpaComment] (EpAnn GrhsAnn)
    addCommentsGrhs = addComments

    addComments :: forall ann. EpAnn ann -> State [LEpaComment] (EpAnn ann)
    addComments (EpAnn anc an ocs) = do
      case anc of
        EpaSpan (RealSrcSpan s _) -> do
          unAllocated <- get
          let
            (rest, these) = GHC.Parser.Lexer.allocateComments s unAllocated
            cs' = workInComments ocs these
          put rest
          return $ EpAnn anc an cs'

        _ -> return $ EpAnn anc an ocs

workInComments :: EpAnnComments -> [LEpaComment] -> EpAnnComments
workInComments ocs [] = ocs
workInComments ocs new = cs'
  where
    pc = priorComments ocs
    fc = getFollowingComments ocs
    cs' = case fc of
      [] -> EpaComments $ sortEpaComments $ pc ++ fc ++ new
      (L ac _:_) -> epaCommentsBalanced (sortEpaComments $ pc ++ cs_before)
                                        (sortEpaComments $ fc ++ cs_after)
             where
               (cs_before,cs_after)
                   = break (\(L ll _) -> (ss2pos $ epaLocationRealSrcSpan ll) > (ss2pos $ epaLocationRealSrcSpan ac) )
                           new

insertTopLevelCppComments ::  HsModule GhcPs -> [LEpaComment] -> (HsModule GhcPs, [LEpaComment])
insertTopLevelCppComments (HsModule (XModulePs an lo mdeprec mbDoc) mmn mexports imports decls) cs
  = (HsModule (XModulePs an4 lo mdeprec mbDoc) mmn mexports' imports' decls', cs3)
    -- `debug` ("insertTopLevelCppComments: (cs2,cs3,hc0,hc1,hc_cs)" ++ showAst (cs2,cs3,hc0,hc1,hc_cs))
    -- `debug` ("insertTopLevelCppComments: (cs2,cs3,hc0i,hc0,hc1,hc_cs)" ++ showAst (cs2,cs3,hc0i,hc0,hc1,hc_cs))
  where
    -- Comments at the top level.
    (an0, cs0) =
      case mmn of
        Nothing -> (an, cs)
        Just _ ->
            -- We have a module name. Capture all comments up to the `where`
            let
              (these, remaining) = splitOnWhere Before (am_where $ anns an) cs
              (EpAnn a anno ocs) = an :: EpAnn AnnsModule
              anm = EpAnn a anno (workInComments ocs these)
            in
              (anm, remaining)
    (an1,cs0a) = case lo of
        EpExplicitBraces (EpTok (EpaSpan (RealSrcSpan s _))) _close ->
            let
                (stay,cs0a') = break (\(L ll _) -> (ss2pos $ epaLocationRealSrcSpan ll) > (ss2pos $ s)) cs0
                cs' = workInComments (comments an0) stay
            in (an0 { comments = cs' }, cs0a')
        _ -> (an0,cs0)
    -- Deal with possible leading semis
    (an2, cs0b) = case am_decls $ anns an1 of
        (AddSemiAnn (EpTok (EpaSpan (RealSrcSpan s _))):_) -> (an1 {comments = cs'}, cs0b')
          where
            (stay,cs0b') = break (\(L ll _) -> (ss2pos $ epaLocationRealSrcSpan ll) > (ss2pos $ s)) cs0a
            cs' = workInComments (comments an1) stay
        _ -> (an1,cs0a)

    (mexports', an3, cs1) =
      case mexports of
        Nothing -> (Nothing, an2, cs0b)
        Just (L l exports) -> (Just (L l exports'), an3', cse)
                         where
                           hc1' = workInComments (comments an2) csh'
                           an3' = an2 { comments = hc1' }
                           (csh', cs0b') = case annListBracketsLocs $ al_brackets $ anns l of
                               (EpaSpan (RealSrcSpan s _),_) ->(h, n)
                                 where
                                   (h,n) = break (\(L ll _) -> (ss2pos $ epaLocationRealSrcSpan ll) > (ss2pos s) )
                                       cs0b

                               _ -> ([], cs0b)
                           (exports', cse) = allocPreceding exports cs0b'
    (imports0, cs2) = allocPreceding imports cs1
    (imports', hc0i) = balanceFirstLocatedAComments imports0

    (decls0, cs3) = allocPreceding decls cs2
    (decls', hc0d) = balanceFirstLocatedAComments decls0

    -- Either hc0i or hc0d should have comments. Combine them
    hc0 = hc0i ++ hc0d

    (hc1,hc_cs) = if NoEpTok == (am_where $ anns an3)
        then (hc0,[])
        else splitOnWhere After (am_where $ anns an3)  hc0
    hc2 = workInComments (comments an3) hc1
    an4 = an3 { anns = (anns an3) {am_cs = hc_cs}, comments = hc2 }

    allocPreceding :: [LocatedA a] -> [LEpaComment] -> ([LocatedA a], [LEpaComment])
    allocPreceding [] cs' = ([], cs')
    allocPreceding (L (EpAnn anc4 an5 cs4) a:xs) cs' = ((L (EpAnn anc4 an5 cs4') a:xs'), rest')
      where
        (rest, these) =
          case anc4 of
            EpaSpan (RealSrcSpan s _) ->
                allocatePriorComments (ss2pos s) cs'
            _ -> (cs', [])
        cs4' = workInComments cs4 these
        (xs',rest') = allocPreceding xs rest

annListBracketsLocs :: AnnListBrackets -> (EpaLocation,EpaLocation)
annListBracketsLocs (ListParens o c) = (getEpTokenLoc o,    getEpTokenLoc c)
annListBracketsLocs (ListBraces o c) = (getEpTokenLoc o,    getEpTokenLoc c)
annListBracketsLocs (ListSquare o c) = (getEpTokenLoc o,    getEpTokenLoc c)
annListBracketsLocs (ListBanana o c) = (getEpUniTokenLoc o, getEpUniTokenLoc c)
annListBracketsLocs ListNone         = (noAnn,              noAnn)


data SplitWhere = Before | After

splitOnWhere :: SplitWhere -> EpToken "where" -> [LEpaComment] -> ([LEpaComment], [LEpaComment])
splitOnWhere w (EpTok (EpaSpan (RealSrcSpan s _))) csIn = (hc, fc)
  where
    splitFunc Before anc_pos c_pos = c_pos < anc_pos
    splitFunc After  anc_pos c_pos = anc_pos < c_pos
    (hc,fc) = break (\(L ll _) ->  splitFunc w (ss2pos $ epaLocationRealSrcSpan ll) (ss2pos s)) csIn
splitOnWhere _ _ csIn = (csIn,[])

balanceFirstLocatedAComments :: [LocatedA a] -> ([LocatedA a], [LEpaComment])
balanceFirstLocatedAComments [] = ([],[])
balanceFirstLocatedAComments ((L (EpAnn anc an csd) a):ds) = (L (EpAnn anc an csd0) a:ds, hc')
  where
    (csd0, hc') = case anc of
        EpaSpan (RealSrcSpan s _) -> (csd', hc)
               `debug` ("balanceFirstLocatedAComments: (csd,csd',attached,header)=" ++ showAst (csd,csd',attached,header))
          where
            (priors, inners) =  break (\(L ll _) -> (ss2pos $ epaLocationRealSrcSpan ll) > (ss2pos s) )
                                       (priorComments csd)
            pcds = priorCommentsDeltas' s priors
            (attached, header) = break (\(d,_c) -> d /= 1) pcds
            csd' = setPriorComments csd (reverse (map snd attached) ++ inners)
            hc = reverse (map snd header)
        _ -> (csd, [])



priorCommentsDeltas' :: RealSrcSpan -> [LEpaComment]
                    -> [(Int, LEpaComment)]
priorCommentsDeltas' r cs = go r (reverse cs)
  where
    go :: RealSrcSpan -> [LEpaComment] -> [(Int, LEpaComment)]
    go _   [] = []
    go _   (la@(L l@(EpaDelta _ dp _) _):las) = (getDeltaLine dp, la) : go (epaLocationRealSrcSpan l) las
    go rs' (la@(L l _):las) = deltaComment rs' la : go (epaLocationRealSrcSpan l) las

    deltaComment :: RealSrcSpan -> LEpaComment -> (Int, LEpaComment)
    deltaComment rs' (L loc c) = (abs(ll - al), L loc c)
      where
        (al,_) = ss2pos rs'
        (ll,_) = ss2pos (epaLocationRealSrcSpan loc)

allocatePriorComments
  :: Pos
  -> [LEpaComment]
  -> ([LEpaComment], [LEpaComment])
allocatePriorComments ss_loc comment_q =
  let
    cmp (L l _) = ss2pos (epaLocationRealSrcSpan l) <= ss_loc
    (newAnns,after) = partition cmp comment_q
  in
    (after, newAnns)

insertRemainingCppComments ::  ParsedSource -> [LEpaComment] -> ParsedSource
insertRemainingCppComments (L l p) cs = L l p'
    -- `debug` ("insertRemainingCppComments: (cs,an')=" ++ showAst (cs,an'))
  where
    (EpAnn a an ocs) = GHC.hsmodAnn $ GHC.hsmodExt p
    an' = EpAnn a an (addTrailingComments end_loc ocs cs)
    p' = p { GHC.hsmodExt = (GHC.hsmodExt p) { GHC.hsmodAnn = an' } }
    end_loc = case GHC.hsmodLayout $ GHC.hsmodExt p of
        EpExplicitBraces _open close -> case close of
            EpTok (EpaSpan (RealSrcSpan s _)) -> ss2pos s
            _ -> (1,1)
        _ -> (1,1)
    (new_before, new_after) = break (\(L ll _) -> (ss2pos $ epaLocationRealSrcSpan ll) > end_loc ) cs

    addTrailingComments end_loc' cur new = epaCommentsBalanced pc' fc'
      where
        pc = priorComments cur
        fc = getFollowingComments cur
        (pc', fc') = case reverse pc of
            [] -> (sortEpaComments $ pc ++ new_before, sortEpaComments $ fc ++ new_after)
            (L ac _:_) -> (sortEpaComments $ pc ++ cs_before, sortEpaComments $ fc ++ cs_after)
              where
               (cs_before,cs_after)
                   = if (ss2pos $ epaLocationRealSrcSpan ac) > end_loc'
                       then break (\(L ll _) -> (ss2pos $ epaLocationRealSrcSpan ll) > (ss2pos $ epaLocationRealSrcSpan ac) ) new
                       else (new_before, new_after)

-- ---------------------------------------------------------------------

ghcCommentText :: LEpaComment -> String
ghcCommentText (L _ (GHC.EpaComment (EpaDocComment s) _))      = exactPrintHsDocString s
ghcCommentText (L _ (GHC.EpaComment (EpaDocOptions s) _))      = s
ghcCommentText (L _ (GHC.EpaComment (EpaLineComment s) _))     = s
ghcCommentText (L _ (GHC.EpaComment (EpaBlockComment s) _))    = s

tokComment :: LEpaComment -> [Comment]
tokComment t@(L lt c) =
  case c of
    (GHC.EpaComment (EpaDocComment dc) pt) -> hsDocStringComments (noCommentsToEpaLocation lt) pt dc
    _ -> [mkComment (normaliseCommentText (ghcCommentText t)) lt (ac_prior_tok c)]

hsDocStringComments :: EpaLocation -> RealSrcSpan -> GHC.HsDocString -> [Comment]
hsDocStringComments _ pt (MultiLineDocString dec (x :| xs)) =
  let
    decStr = printDecorator dec
    L lx x' = dedentDocChunkBy (3 + length decStr) x
    str = "-- " ++ decStr ++ unpackHDSC x'
    docChunk _ [] = []
    docChunk pt' (L l chunk:cs)
      = Comment ("--" ++ unpackHDSC chunk) (spanAsAnchor l) pt' Nothing : docChunk (rs l) cs
  in
    (Comment str (spanAsAnchor lx) pt Nothing : docChunk (rs lx) (map dedentDocChunk xs))
hsDocStringComments anc pt (NestedDocString dec@(HsDocStringNamed _) (L _ chunk))
  = [Comment ("{- " ++ printDecorator dec ++ unpackHDSC chunk ++ "-}") (epaToNoCommentsLocation anc) pt Nothing ]
hsDocStringComments anc pt (NestedDocString dec (L _ chunk))
  = [Comment ("{-" ++ printDecorator dec ++ unpackHDSC chunk ++ "-}") (epaToNoCommentsLocation anc) pt Nothing ]

hsDocStringComments _ _ (GeneratedDocString _) = [] -- Should not appear in user-written code

-- At the moment the locations of the 'HsDocStringChunk's are from the start of
-- the string part, leaving aside the "--". So we need to subtract 2 columns from it
dedentDocChunk :: LHsDocStringChunk -> LHsDocStringChunk
dedentDocChunk chunk = dedentDocChunkBy 2 chunk

dedentDocChunkBy :: Int -> LHsDocStringChunk -> LHsDocStringChunk
dedentDocChunkBy  dedent (L (RealSrcSpan l mb) c) = L (RealSrcSpan l' mb) c
  where
    f = srcSpanFile l
    sl = srcSpanStartLine l
    sc = srcSpanStartCol l
    el = srcSpanEndLine l
    ec = srcSpanEndCol l
    l' = mkRealSrcSpan (mkRealSrcLoc f sl (sc - dedent))
                       (mkRealSrcLoc f el (ec - dedent))

dedentDocChunkBy _ x = x


epaCommentsBalanced :: [LEpaComment] -> [LEpaComment] -> EpAnnComments
epaCommentsBalanced priorCs     [] = EpaComments priorCs
epaCommentsBalanced priorCs postCs = EpaCommentsBalanced priorCs postCs

mkEpaComments :: [Comment] -> [Comment] -> EpAnnComments
mkEpaComments priorCs []
  = EpaComments (map comment2LEpaComment priorCs)
mkEpaComments priorCs postCs
  = epaCommentsBalanced (map comment2LEpaComment priorCs) (map comment2LEpaComment postCs)

comment2LEpaComment :: Comment -> LEpaComment
comment2LEpaComment (Comment s anc r _mk) = mkLEpaComment s anc r

mkLEpaComment :: String -> NoCommentsLocation -> RealSrcSpan -> LEpaComment
mkLEpaComment s loc r = (L loc (GHC.EpaComment (EpaLineComment s) r))

mkComment :: String -> NoCommentsLocation -> RealSrcSpan -> Comment
mkComment c loc r = Comment c loc r Nothing

-- Windows comments include \r in them from the lexer.
normaliseCommentText :: String -> String
normaliseCommentText [] = []
normaliseCommentText ('\r':xs) = normaliseCommentText xs
normaliseCommentText (x:xs) = x:normaliseCommentText xs

-- |Must compare without span filenames, for CPP injected comments with fake filename
cmpComments :: Comment -> Comment -> Ordering
cmpComments (Comment _ l1 _ _) (Comment _ l2 _ _) = compare (ss2pos $ epaLocationRealSrcSpan l1) (ss2pos $ epaLocationRealSrcSpan l2)

-- |Sort, comparing without span filenames, for CPP injected comments with fake filename
sortComments :: [Comment] -> [Comment]
sortComments cs = sortBy cmpComments cs

-- |Sort, comparing without span filenames, for CPP injected comments with fake filename
sortEpaComments :: [LEpaComment] -> [LEpaComment]
sortEpaComments cs = sortBy cmp cs
  where
    cmp (L l1 _) (L l2 _) = compare (ss2pos $ epaLocationRealSrcSpan l1) (ss2pos $ epaLocationRealSrcSpan l2)

-- | Makes a comment which originates from a specific keyword.
mkKWComment :: String -> NoCommentsLocation -> Comment
mkKWComment kw (EpaSpan (RealSrcSpan ss mb)) = Comment kw (EpaSpan (RealSrcSpan ss mb)) ss (Just kw)
mkKWComment kw (EpaSpan (UnhelpfulSpan _))   = Comment kw (EpaDelta noSrcSpan (SameLine 0) NoComments) placeholderRealSpan (Just kw)
mkKWComment kw (EpaDelta ss dp cs)           = Comment kw (EpaDelta ss dp cs) placeholderRealSpan (Just kw)

sortAnchorLocated :: [GenLocated EpaLocation a] -> [GenLocated EpaLocation a]
sortAnchorLocated = sortBy (compare `on` (epaLocationRealSrcSpan . getLoc))

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

type DeclsByTag a = Map.Map DeclTag [(RealSrcSpan, a)]

orderedDecls
  :: AnnSortKey DeclTag
  -> DeclsByTag a
  -> [(RealSrcSpan, a)]
orderedDecls sortKey declGroup  =
  case sortKey of
    NoAnnSortKey ->
      sortBy (\a b -> compare (fst a) (fst b)) (concat $ Map.elems declGroup)
    AnnSortKey keys ->
      let
        go :: [DeclTag] -> DeclsByTag a -> [(RealSrcSpan, a)]
        go [] _                      = []
        go (tag:ks) dbt = d : go ks dbt'
          where
            dbt' = Map.adjust (\ds -> drop 1 ds) tag dbt
            d = case Map.lookup tag dbt of
              Just (d':_) -> d'
              _           -> error $ "orderedDecls: could not look up "
                                       ++ show tag ++ " in " ++ show (Map.keys dbt)
      in
        go keys declGroup

hsDeclsClassDecl :: TyClDecl GhcPs -> [LHsDecl GhcPs]
hsDeclsClassDecl dec = case dec of
  ClassDecl { tcdCExt = (_an2, _layout, sortKey),
              tcdSigs = sigs,tcdMeths = methods,
              tcdATs = ats, tcdATDefs = at_defs
            } -> map snd decls
    where
      srs :: EpAnn a -> RealSrcSpan
      srs a = realSrcSpan $ locA a
      decls
          = orderedDecls sortKey $ Map.fromList
              [(ClsSigTag,    map (\(L l s) -> (srs l, L l (SigD noExtField s))) sigs),
               (ClsMethodTag, map (\(L l s) -> (srs l, L l (ValD noExtField s))) methods),
               (ClsAtTag,     map (\(L l s) -> (srs l, L l (TyClD noExtField $ FamDecl noExtField s))) ats),
               (ClsAtdTag,    map (\(L l s) -> (srs l, L l (InstD noExtField $ TyFamInstD noExtField s))) at_defs)
              ]
  _ -> error $ "hsDeclsClassDecl:dec=" ++ showAst dec

replaceDeclsClassDecl :: TyClDecl GhcPs -> [LHsDecl GhcPs] -> TyClDecl GhcPs
replaceDeclsClassDecl decl decls = case decl of
  ClassDecl { tcdCExt = (an2, layout, _) } -> decl'
    where
      (tags, methods', sigs', ats', at_defs', _, _docs) = partitionWithSortKey decls
      decl' = decl { tcdCExt = (an2, layout, AnnSortKey tags),
                     tcdSigs = sigs',tcdMeths = methods',
                     tcdATs = ats', tcdATDefs = at_defs'
                   }

  _ -> error $ "replaceDeclsClassDecl:decl=" ++ showAst decl

partitionWithSortKey
  :: [LHsDecl GhcPs]
  -> ([DeclTag], LHsBinds GhcPs, [LSig GhcPs], [LFamilyDecl GhcPs],
      [LTyFamInstDecl GhcPs], [LDataFamInstDecl GhcPs], [LDocDecl GhcPs])
partitionWithSortKey = go
  where
    go [] = ([], [], [], [], [], [], [])
    go ((L l decl) : ds) =
      let (tags, bs, ss, ts, tfis, dfis, docs) = go ds in
      case decl of
        ValD _ b
          -> (ClsMethodTag:tags, L l b : bs, ss, ts, tfis, dfis, docs)
        SigD _ s
          -> (ClsSigTag:tags, bs, L l s : ss, ts, tfis, dfis, docs)
        TyClD _ (FamDecl _ t)
          -> (ClsAtTag:tags, bs, ss, L l t : ts, tfis, dfis, docs)
        InstD _ (TyFamInstD { tfid_inst = tfi })
          -> (ClsAtdTag:tags, bs, ss, ts, L l tfi : tfis, dfis, docs)
        InstD _ (DataFamInstD { dfid_inst = dfi })
          -> (tags, bs, ss, ts, tfis, L l dfi : dfis, docs)
        DocD _ d
          -> (tags, bs, ss, ts, tfis, dfis, L l d : docs)
        _ -> error $ "partitionBindsAndSigs" ++ (showAst decl)


-- ---------------------------------------------------------------------

orderedDeclsBinds
  :: AnnSortKey BindTag
  -> [LHsDecl GhcPs] -> [LHsDecl GhcPs]
  -> [LHsDecl GhcPs]
orderedDeclsBinds sortKey binds sigs =
  case sortKey of
    NoAnnSortKey ->
      sortBy (\a b -> compare (realSrcSpan $ getLocA a)
                              (realSrcSpan $ getLocA b)) (binds ++ sigs)
    AnnSortKey keys ->
      let
        go [] _ _                      = []
        go (BindTag:ks) (b:bs) ss = b : go ks bs ss
        go (SigDTag:ks) bs (s:ss) = s : go ks bs ss
        go (_:ks) bs ss           =     go ks bs ss
      in
        go keys binds sigs

hsDeclsLocalBinds :: HsLocalBinds GhcPs -> [LHsDecl GhcPs]
hsDeclsLocalBinds lb = case lb of
    HsValBinds _ (ValBinds sortKey bs sigs) ->
      let
        bds = map wrapDecl bs
        sds = map wrapSig sigs
      in
        orderedDeclsBinds sortKey bds sds
    HsValBinds _ (XValBindsLR _) -> error $ "hsDecls.XValBindsLR not valid"
    HsIPBinds {}       -> []
    EmptyLocalBinds {} -> []

hsDeclsValBinds :: (HsValBindsLR GhcPs GhcPs) -> [LHsDecl GhcPs]
hsDeclsValBinds (ValBinds sortKey bs sigs) =
      let
        bds = map wrapDecl bs
        sds = map wrapSig sigs
      in
        orderedDeclsBinds sortKey bds sds
hsDeclsValBinds XValBindsLR{} = error "hsDeclsValBinds"

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

showAst :: (Data a) => a -> String
showAst ast
  = showSDocUnsafe
    $ showAstData BlankSrcSpanFile NoBlankEpAnnotations ast

-- ---------------------------------------------------------------------
-- Putting these here for the time being, to avoid import loops

ghead :: String -> [a] -> a
ghead  info []    = error $ "ghead "++info++" []"
ghead _info (h:_) = h

glast :: String -> [a] -> a
glast  info []    = error $ "glast " ++ info ++ " []"
glast _info h     = last h

gtail :: String -> [a] -> [a]
gtail  info []    = error $ "gtail " ++ info ++ " []"
gtail _info (_:t) = t

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
