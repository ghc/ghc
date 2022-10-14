{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE LambdaCase          #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.StgToJS.Linker.Compactor
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Luite Stegeman <luite.stegeman@iohk.io>
--                Sylvain Henry  <sylvain.henry@iohk.io>
--                Jeffrey Young  <jeffrey.young@iohk.io>
--                Josh Meredith  <josh.meredith@iohk.io>
-- Stability   :  experimental
--
-- The compactor does link-time optimization. It is much simpler than the
-- Optimizer, no fancy dataflow analysis here.
--
-- Optimizations:
-- - rewrite all variables starting with h$$ to shorter names, these are internal names
-- - write all function metadata compactly
--
-- Note: - This module is not yet complete (as of 23/09/2022), for the complete
-- version to adapt see GHCJS's Gen2/Compactor.hs module. For now we have only
-- the functions that constitue the API for the module so that the JS Backend
-- Linker and RTS can compile and run.
-----------------------------------------------------------------------------

module GHC.StgToJS.Linker.Compactor
  ( compact
  , packStrings
  ) where


import           GHC.Utils.Panic
import           GHC.Utils.Misc
import           GHC.Types.Unique.Map
import           GHC.Types.Unique.Set
import           GHC.Types.Unique.DSet

import           Control.Applicative
import           GHC.Utils.Monad.State.Strict
import           Data.Function

import           Data.Bifunctor (second)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Builder as BB
import qualified Data.Graph as G
import qualified Data.Map.Strict as M
import           Data.Map (Map)
import           Data.Int
import qualified Data.List as List
import           Data.Maybe
import           GHC.Data.FastString

import           GHC.JS.Syntax
import           GHC.JS.Make
import           GHC.JS.Transform
import           GHC.StgToJS.Printer             (pretty)
import           GHC.StgToJS.Types
import           GHC.StgToJS.Linker.Types
import           GHC.StgToJS.Closure
import           GHC.StgToJS.Arg

import Prelude
import GHC.Utils.Encoding


{- create a single string initializer for all StaticUnboxedString references
   in the code, and rewrite all references to point to it

   if incremental linking is used, each increment gets its own packed string
   blob. if a string value already exists in an earlier blob it is not added
   again
 -}
packStrings :: HasDebugCallStack
            => JSLinkConfig
            -> CompactorState
            -> [LinkedUnit]
            -> (CompactorState, [LinkedUnit])
packStrings _settings _cstate _code = panic "Compactor.packstrings not yet implemented!"

renameInternals :: HasDebugCallStack
                => JSLinkConfig
                -> StgToJSConfig
                -> CompactorState
                -> [FastString]
                -> [LinkedUnit]
                -> (CompactorState, [JStat], JStat)
renameInternals ln_cfg cfg cs0 rtsDeps stats0a = (cs, stats, meta)
  where
    (stbs, stats0) = (if lcDedupe ln_cfg
                      then dedupeBodies rtsDeps . dedupe rtsDeps
                      else (mempty,)) stats0a
    ((stats, meta), cs) = runState renamed cs0

    renamed :: State CompactorState ([JStat], JStat)
    renamed

      | True = do
        cs <- get
        let renamedStats = map (identsS' (lookupRenamed cs) . lu_js_code) stats0
            statics      = map (renameStaticInfo cs)  $
                               concatMap lu_statics stats0
            infos        = map (renameClosureInfo cs) $
                               concatMap lu_closures stats0
            -- render metadata as individual statements
            meta = mconcat (map staticDeclStat statics) <>
                   identsS' (lookupRenamed cs) stbs <>
                   mconcat (map (staticInitStat $ csProf cfg) statics) <>
                   mconcat (map (closureInfoStat True) infos)
        return (renamedStats, meta)

-- | initialize a global object. all global objects have to be declared (staticInfoDecl) first
--   (this is only used with -debug, normal init would go through the static data table)
staticInitStat :: Bool         -- ^ profiling enabled
               -> StaticInfo
               -> JStat
staticInitStat _prof (StaticInfo i sv cc) =
  case sv of
    StaticData con args -> appS "h$sti" ([var i, var con, jsStaticArgs args] ++ ccArg)
    StaticFun  f   args -> appS "h$sti" ([var i, var f, jsStaticArgs args] ++ ccArg)
    StaticList args mt   ->
      appS "h$stl" ([var i, jsStaticArgs args, toJExpr $ maybe null_ (toJExpr . TxtI) mt] ++ ccArg)
    StaticThunk (Just (f,args)) ->
      appS "h$stc" ([var i, var f, jsStaticArgs args] ++ ccArg)
    _                    -> mempty
  where
    ccArg = maybeToList (fmap toJExpr cc)

-- | declare and do first-pass init of a global object (create JS object for heap objects)
staticDeclStat :: StaticInfo -> JStat
staticDeclStat (StaticInfo global_name static_value _) = decl
  where
    global_ident = TxtI global_name
    decl_init v  = global_ident ||= v
    decl_no_init = appS "h$di" [toJExpr global_ident]

    decl = case static_value of
      StaticUnboxed u     -> decl_init (unboxed_expr u)
      StaticThunk Nothing -> decl_no_init -- CAF initialized in an alternative way
      _                   -> decl_init (app "h$d" [])

    unboxed_expr = \case
      StaticUnboxedBool b          -> app "h$p" [toJExpr b]
      StaticUnboxedInt i           -> app "h$p" [toJExpr i]
      StaticUnboxedDouble d        -> app "h$p" [toJExpr (unSaneDouble d)]
      StaticUnboxedString str      -> app "h$rawStringData" [ValExpr (to_byte_list str)]
      StaticUnboxedStringOffset {} -> 0

    to_byte_list = JList . map (Int . fromIntegral) . BS.unpack

lookupRenamed :: CompactorState -> Ident -> Ident
lookupRenamed cs i@(TxtI t) =
  fromMaybe i (lookupUniqMap (csNameMap cs) t)

-- | rename a compactor info entry according to the compactor state (no new renamings are added)
renameClosureInfo :: CompactorState
                  -> ClosureInfo
                  -> ClosureInfo
renameClosureInfo cs (ClosureInfo v rs n l t s)  =
  ClosureInfo (renameV v) rs n l t (f s)
    where
      renameV t = maybe t itxt (lookupUniqMap m t)
      m                   = csNameMap cs
      f (CIStaticRefs rs) = CIStaticRefs (map renameV rs)

-- | rename a static info entry according to the compactor state (no new renamings are added)
renameStaticInfo :: CompactorState
                 -> StaticInfo
                 -> StaticInfo
renameStaticInfo cs = staticIdents renameIdent
  where
    renameIdent t = maybe t itxt (lookupUniqMap (csNameMap cs) t)

staticIdents :: (FastString -> FastString)
             -> StaticInfo
             -> StaticInfo
staticIdents f (StaticInfo i v cc) = StaticInfo (f i) (staticIdentsV f v) cc

staticIdentsV ::(FastString -> FastString) -> StaticVal -> StaticVal
staticIdentsV f (StaticFun i args) = StaticFun (f i) (staticIdentsA f <$> args)
staticIdentsV f (StaticThunk (Just (i, args))) = StaticThunk . Just $
                                                 (f i, staticIdentsA f <$> args)
staticIdentsV f (StaticData con args) = StaticData (f con) (staticIdentsA f <$> args)
staticIdentsV f (StaticList xs t)              = StaticList (staticIdentsA f <$> xs) (f <$> t)
staticIdentsV _ x                              = x

staticIdentsA :: (FastString -> FastString) -> StaticArg -> StaticArg
staticIdentsA f (StaticObjArg t) = StaticObjArg $! f t
staticIdentsA _ x = x

compact :: JSLinkConfig
        -> StgToJSConfig
        -> CompactorState
        -> [FastString]
        -> [LinkedUnit]
        -> (CompactorState, [JStat], JStat)
compact ln_cfg cfg cs0 rtsDeps0 input0
  =
  let rtsDeps1 = rtsDeps0 ++
                 map (<> "_e") rtsDeps0 ++
                 map (<> "_con_e") rtsDeps0
  in  renameInternals ln_cfg cfg cs0 rtsDeps1 input0


-- hash compactification
dedupeBodies :: [FastString]
             -> [LinkedUnit]
             -> (JStat, [LinkedUnit])
dedupeBodies rtsDeps input = (renderBuildFunctions bfN bfCB, input')
  where
    (bfN, bfCB, input') = rewriteBodies globals hdefsR hdefs input
    hdefs   = M.fromListWith (\(s,ks1) (_,ks2) -> (s, ks1++ks2))
                             (map (\(k, s, bs) -> (bs, (s, [k]))) hdefs0)
    hdefsR  = listToUniqMap $ map (\(k, _, bs) -> (k, bs)) hdefs0
    hdefs0 :: [(FastString, Int, BS.ByteString)]
    hdefs0  = concatMap ((map (\(k,h) ->
                            let (s,fh, _deps) = finalizeHash' h
                            in (k, s, fh))
                        . hashDefinitions globals) . lu_js_code)
                        input
    globals = List.foldl' delOneFromUniqSet (findAllGlobals input) rtsDeps

renderBuildFunctions :: [BuildFunction] -> [BuildFunction] -> JStat
renderBuildFunctions normalBfs cycleBreakerBfs =
  cycleBr1 <> mconcat (map renderBuildFunction normalBfs) <> cycleBr2
  where
    renderCbr f = mconcat (zipWith f cycleBreakerBfs [1..])
    cbName :: Int -> FastString
    cbName = mkFastString . ("h$$$cb"++) . show
    cycleBr1 = renderCbr $ \bf n ->
      let args = map (TxtI . mkFastString . ('a':) . show) [1..bfArgs bf]
          body = ReturnStat $ ApplExpr (ValExpr (JVar (TxtI $ cbName n)))
                                       (map (ValExpr . JVar) args)
          bfn = bfName bf
      in  (TxtI bfn) ||= (ValExpr (JFunc args body))
    cycleBr2 = renderCbr $ \bf n -> renderBuildFunction (bf { bfName = cbName n })

data BuildFunction = BuildFunction
  { bfName    :: !FastString
  , bfBuilder :: !Ident
  , bfDeps    :: [FastString]
  , bfArgs    :: !Int
  } deriving (Eq, Show)

{-
  Stack frame initialization order is important when code is reused:
    all dependencies have to be ready when the closure is built.

  This function sorts the initializers and returns an additional list
    of cycle breakers, which are built in a two-step fashion
 -}
sortBuildFunctions :: [BuildFunction] -> ([BuildFunction], [BuildFunction])
sortBuildFunctions bfs = (map snd normBFs, map snd cbBFs)
  where
    (normBFs, cbBFs) = List.partition (not.fst) . concatMap fromSCC $ sccs bfs
    bfm :: UniqMap FastString BuildFunction
    bfm = listToUniqMap (map (\x -> (bfName x, x)) bfs)
    fromSCC :: G.SCC LexicalFastString -> [(Bool, BuildFunction)]
    fromSCC (G.AcyclicSCC (LexicalFastString x)) = [(False, fromJust $ lookupUniqMap bfm x)]
    fromSCC (G.CyclicSCC xs) = breakCycles $ map (\(LexicalFastString f) -> f) xs
    sccs :: [BuildFunction] -> [G.SCC LexicalFastString]
    sccs b = G.stronglyConnComp $
      map (\bf -> let n = bfName bf in (LexicalFastString n, LexicalFastString n, map LexicalFastString $ bfDeps bf)) b
    {-
       finding the maximum acyclic subgraph is the Minimum Feedback Arc Set problem,
       which is NP-complete. We use an approximation here.
     -}
    breakCycles :: [FastString] -> [(Bool, BuildFunction)]
    breakCycles nodes =
      (True, fromJust $ lookupUniqMap bfm selected)
      : concatMap fromSCC (sccs (map (fromJust . lookupUniqMap bfm) $ filter (/=selected) nodes))
      where
        outDeg, inDeg :: UniqMap FastString Int
        outDeg = listToUniqMap $ map (\n -> (n, length (bfDeps (fromJust $ lookupUniqMap bfm n)))) nodes
        inDeg  = listToUniqMap_C (+) (map (,1) . concatMap (bfDeps . (fromJust . lookupUniqMap bfm)) $ nodes)
        -- ELS heuristic (Eades et. al.)
        selected :: FastString
        selected = List.maximumBy (compare `on` (\x -> fromJust (lookupUniqMap outDeg x) - fromJust (lookupUniqMap inDeg x))) nodes

rewriteBodies :: UniqSet FastString
              -> UniqMap FastString BS.ByteString
              -> Map BS.ByteString (Int, [FastString])
              -> [LinkedUnit]
              -> ([BuildFunction], [BuildFunction], [LinkedUnit])
rewriteBodies globals idx1 idx2 input = (bfsNormal, bfsCycleBreaker, input')
  where
    (bfs1, input')               = unzip (map rewriteBlock input)
    (bfsNormal, bfsCycleBreaker) = sortBuildFunctions (concat bfs1)

    -- this index only contains the entries we actually want to dedupe
    idx2' :: Map BS.ByteString (Int, [FastString])
    idx2' = M.filter (\(s, xs) -> dedupeBody (length xs) s) idx2

    rewriteBlock :: LinkedUnit -> ([BuildFunction], LinkedUnit)
    rewriteBlock (LinkedUnit st cis sis) =
      let (bfs, st') = rewriteFunctions st
      -- remove the declarations for things that we just deduped
          st''       = removeDecls (mkUniqSet $ map bfName bfs) st'
      in  (bfs, LinkedUnit st'' cis sis)

    removeDecls :: UniqSet FastString -> JStat -> JStat
    removeDecls t (BlockStat ss) = BlockStat (map (removeDecls t) ss)
    removeDecls t (DeclStat (TxtI i) _)
      | elementOfUniqSet i t = mempty
    removeDecls _ s = s

    rewriteFunctions :: JStat -> ([BuildFunction], JStat)
    rewriteFunctions (BlockStat ss) =
      let (bfs, ss') = unzip (map rewriteFunctions ss)
      in  (concat bfs, BlockStat ss')
    rewriteFunctions (AssignStat (ValExpr (JVar (TxtI i)))
                                 (ValExpr (JFunc args st)))
      | Just h         <- lookupUniqMap idx1  i
      , Just (_s, his) <- M.lookup h idx2' =
          let (bf, st') = rewriteFunction i h his args st in ([bf], st')
    rewriteFunctions x = ([], x)

    rewriteFunction :: FastString
                    -> BS.ByteString
                    -> [FastString]
                    -> [Ident]
                    -> JStat
                    -> (BuildFunction, JStat)
    rewriteFunction i h his args body
      | i == iFirst = (bf, createFunction i idx g args body)
      | otherwise   = (bf, mempty)
       where
          bf :: BuildFunction
          bf       = BuildFunction i (buildFunId idx) g (length args)
          g :: [FastString]
          g        = findGlobals globals body
          iFirst   = head his
          Just idx = M.lookupIndex h idx2'

    createFunction :: FastString
                   -> Int
                   -> [FastString]
                   -> [Ident]
                   -> JStat
                   -> JStat
    createFunction _i idx g args body =
      bi ||= ValExpr (JFunc bargs bbody)
      where
        ng    = length g
        bi    = buildFunId idx
        bargs :: [Ident]
        bargs = map (TxtI . mkFastString . ("h$$$g"++) . show) [1..ng]
        bgm :: UniqMap FastString Ident
        bgm   = listToUniqMap (zip g bargs)
        bbody :: JStat
        bbody = ReturnStat (ValExpr $ JFunc args ibody)
        ibody :: JStat
        ibody = identsS' (\ti@(TxtI i) -> fromMaybe ti (lookupUniqMap bgm i)) body

renderBuildFunction :: BuildFunction -> JStat
renderBuildFunction (BuildFunction i bfid deps _nargs) =
  (TxtI i) ||= (ApplExpr (ValExpr (JVar bfid)) (map (ValExpr . JVar . TxtI) deps))

dedupeBody :: Int -> Int -> Bool
dedupeBody n size
  | n < 2          = False
  | size * n > 200 = True
  | n > 6          = True
  | otherwise      = False

buildFunId :: Int -> Ident
buildFunId i = TxtI (mkFastString $ "h$$$f" ++ show i)

-- result is ordered, does not contain duplicates
findGlobals :: UniqSet FastString -> JStat -> [FastString]
findGlobals globals stat = filter isGlobal . map itxt . uniqDSetToList $ identsS stat
  where
    locals     = mkUniqSet (findLocals stat)
    isGlobal i = elementOfUniqSet i globals && not (elementOfUniqSet i locals)

findLocals :: JStat -> [FastString]
findLocals (BlockStat ss)        = concatMap findLocals ss
findLocals (DeclStat (TxtI i) _) = [i]
findLocals _                     = []


data HashIdx = HashIdx (UniqMap FastString Hash) (Map Hash FastString)


dedupe :: [FastString]
       -> [LinkedUnit]
       -> [LinkedUnit]
dedupe rtsDeps input
--  | dumpHashIdx idx
  =
  map (dedupeBlock idx) input
  where
    idx    = HashIdx hashes hr
    hashes0 = buildHashes rtsDeps input
    hashes  = List.foldl' delFromUniqMap hashes0 rtsDeps
    -- Adding to a map, and selecting a deterministic element on overlapping keys
    -- using pickShortest avoids the non-determinism introduced by nonDetEltsUniqMap.
    hr     = M.fromListWith pickShortest $
             map (\(i, h) -> (h, i)) (nonDetEltsUniqMap hashes)
    pickShortest :: FastString -> FastString -> FastString
    pickShortest x y
      | x == y                                    = x
      | lengthFS x < lengthFS y                   = x
      | lengthFS x > lengthFS y                   = y
      | LexicalFastString x < LexicalFastString y = x -- these are the same length, so pick the
      | otherwise                                 = y -- lexically first one for determinism


dedupeBlock :: HashIdx
            -> LinkedUnit
            -> LinkedUnit
dedupeBlock hi (LinkedUnit st ci si) = LinkedUnit
  { lu_js_code  = dedupeStat hi st
  , lu_closures = mapMaybe (dedupeClosureInfo hi) ci
  , lu_statics  = mapMaybe (dedupeStaticInfo hi) si
  }

dedupeStat :: HashIdx -> JStat -> JStat
dedupeStat hi = go
  where
    go (BlockStat ss) = BlockStat (map go ss)
    go s@(DeclStat (TxtI i) _)
      | not (isCanon hi i) = mempty
      | otherwise          = s
    go (AssignStat v@(ValExpr (JVar (TxtI i))) e)
      | not (isCanon hi i) = mempty
      | otherwise          = AssignStat v (identsE' (toCanonI hi) e)
    -- rewrite identifiers in e
    go s = identsS' (toCanonI hi) s

dedupeClosureInfo :: HashIdx -> ClosureInfo -> Maybe ClosureInfo
dedupeClosureInfo hi (ClosureInfo i rs n l ty st)
  | isCanon hi i = Just (ClosureInfo i rs n l ty (dedupeCIStatic hi st))
dedupeClosureInfo _ _ = Nothing

dedupeStaticInfo :: HashIdx -> StaticInfo -> Maybe StaticInfo
dedupeStaticInfo hi (StaticInfo i val ccs)
  | isCanon hi i = Just (StaticInfo i (dedupeStaticVal hi val) ccs)
dedupeStaticInfo _ _ = Nothing

dedupeCIStatic :: HashIdx -> CIStatic -> CIStatic
dedupeCIStatic hi (CIStaticRefs refs) = CIStaticRefs (List.nub $ map (toCanon hi) refs)

dedupeStaticVal :: HashIdx -> StaticVal -> StaticVal
dedupeStaticVal hi (StaticFun t args) =
  StaticFun (toCanon hi t) (map (dedupeStaticArg hi) args)
dedupeStaticVal hi (StaticThunk (Just (o, args))) =
  StaticThunk (Just (toCanon hi o, map (dedupeStaticArg hi) args))
dedupeStaticVal hi (StaticData dcon args) =
  StaticData (toCanon hi dcon) (map (dedupeStaticArg hi) args)
dedupeStaticVal hi (StaticList args lt) =
  StaticList (map (dedupeStaticArg hi) args) (fmap (toCanon hi) lt)
dedupeStaticVal _ v = v -- unboxed value or thunk with alt init, no rewrite needed

dedupeStaticArg :: HashIdx -> StaticArg -> StaticArg
dedupeStaticArg hi (StaticObjArg o)
  = StaticObjArg (toCanon hi o)
dedupeStaticArg hi (StaticConArg c args)
  = StaticConArg (toCanon hi c)
                 (map (dedupeStaticArg hi) args)
dedupeStaticArg _hi a@StaticLitArg{}    = a

isCanon :: HashIdx -> FastString -> Bool
isCanon (HashIdx a b) t
  | Nothing <- la = True
  | Just h  <- la
  , Just t' <- M.lookup h b = t == t'
  | otherwise = False
  where la = lookupUniqMap a t

toCanon :: HashIdx -> FastString -> FastString
toCanon (HashIdx a b) t
  | Just h  <- lookupUniqMap a t
  , Just t' <- M.lookup h b = t'
  | otherwise = t

toCanonI :: HashIdx -> Ident -> Ident
toCanonI hi (TxtI x) = TxtI $ toCanon hi x

type Hash = (BS.ByteString, [LexicalFastString])

data HashBuilder = HashBuilder !BB.Builder ![FastString]

instance Monoid HashBuilder where
  mempty = HashBuilder mempty mempty

instance Semigroup HashBuilder where
  (<>) (HashBuilder b1 l1) (HashBuilder b2 l2) =
    HashBuilder (b1 <> b2) (l1 <> l2)

{-
dumpHashIdx :: HashIdx -> Bool
dumpHashIdx hi@(HashIdx ma mb) =
  let ks = M.keys ma
      difCanon i = let i' = toCanon hi i
                   in if i == i' then Nothing else Just i'
      writeHashIdx = do
        putStrLn "writing hash idx"
        T.writeFile "hashidx.txt"
          (T.unlines . sort $ mapMaybe (\i -> fmap ((i <> " -> ") <>) (difCanon i)) ks)
        putStrLn "writing full hash idx"
        T.writeFile "hashIdxFull.txt"
          (T.unlines . sort $ M.keys ma)
  in unsafePerformIO writeHashIdx `seq` True
-}
-- debug thing
{-
dumpHashes' :: [(JStat, [ClosureInfo], [StaticInfo])] -> Bool
dumpHashes' input =
  let hashes      = buildHashes input
      writeHashes = do
        putStrLn "writing hashes"
        BL.writeFile "hashes.json" (Aeson.encode $ dumpHashes hashes)
  in unsafePerformIO writeHashes `seq` True
-}
buildHashes :: [FastString] -> [LinkedUnit] -> UniqMap FastString Hash
buildHashes rtsDeps xss
  -- - | dumpHashes0 hashes0
  = fixHashes (mapUniqMap finalizeHash hashes0)
  where
    globals = List.foldl' delOneFromUniqSet (findAllGlobals xss) rtsDeps
    hashes0 = foldl plusUniqMap emptyUniqMap (map buildHashesBlock xss)
    buildHashesBlock (LinkedUnit st cis sis) =
      let hdefs = hashDefinitions globals st
          hcis  = map hashClosureInfo cis
          hsis  = map hashStaticInfo (filter (not . ignoreStatic) sis)
      in  listToUniqMap (combineHashes hdefs hcis ++ hsis)

findAllGlobals :: [LinkedUnit] -> UniqSet FastString
findAllGlobals xss = mkUniqSet $ concatMap f xss
  where
    f (LinkedUnit _js_code closures statics) =
      map (\(ClosureInfo i _ _ _ _ _) -> i) closures ++
      map (\(StaticInfo i _ _) -> i) statics

fixHashes :: UniqMap FastString Hash -> UniqMap FastString Hash
fixHashes hashes = fmap (second (map replaceHash)) hashes
  where
    replaceHash :: LexicalFastString -> LexicalFastString
    replaceHash h'@(LexicalFastString h) = maybe h' (LexicalFastString . mkFastString) (lookupUniqMap finalHashes h)
    hashText  bs = "h$$$" <> utf8DecodeByteString bs
    sccs :: [[FastString]]
    sccs         = map fromSCC $
                   G.stronglyConnComp (map (\(k, (_bs, deps)) -> (k, LexicalFastString k, deps)) kvs)
    kvs          = List.sortOn (LexicalFastString . fst) $ nonDetEltsUniqMap hashes -- sort lexically to avoid non-determinism
    ks           = fst $ unzip kvs
    invDeps      = listToUniqMap_C (++) (concatMap mkInvDeps kvs)
    mkInvDeps (k, (_, ds)) = map (\(LexicalFastString d) -> (d,[k])) ds
    finalHashes  = fmap hashText (fixHashesIter 500 invDeps ks ks sccs hashes mempty)

fromSCC :: G.SCC a -> [a]
fromSCC (G.AcyclicSCC x) = [x]
fromSCC (G.CyclicSCC xs) = xs

fixHashesIter :: Int
              -> UniqMap FastString [FastString]
              -> [FastString]
              -> [FastString]
              -> [[FastString]]
              -> UniqMap FastString Hash
              -> UniqMap FastString BS.ByteString
              -> UniqMap FastString BS.ByteString
fixHashesIter n invDeps allKeys checkKeys sccs hashes finalHashes
  -- - | unsafePerformIO (putStrLn ("fixHashesIter: " ++ show n)) `seq` False = undefined
  | n < 0                = finalHashes
  | not (null newHashes) = fixHashesIter (n-1) invDeps allKeys checkKeys' sccs hashes
      (addListToUniqMap finalHashes newHashes)
  -- - | unsafePerformIO (putStrLn ("fixHashesIter killing cycles:\n" ++ show rootSCCs)) `seq` False = undefined
  | not (null rootSCCs)  = fixHashesIter n {- -1 -} invDeps allKeys allKeys sccs hashes
      (addListToUniqMap finalHashes (concatMap hashRootSCC rootSCCs))
  | otherwise            = finalHashes
  where
    checkKeys' | length newHashes > sizeUniqMap hashes `div` 10 = allKeys
               | otherwise = uniqDSetToList . mkUniqDSet $ concatMap (newHashDeps) newHashes
    newHashDeps :: (FastString, BSC.ByteString) -> [FastString]
    newHashDeps (k, _) = fromMaybe [] (lookupUniqMap invDeps k)
    mkNewHash k | not $ elemUniqMap k finalHashes
                , Just (hb, htxt) <- lookupUniqMap hashes k
                , Just bs <- mapM (\(LexicalFastString ht) -> lookupUniqMap finalHashes ht) htxt =
                  Just (k, makeFinalHash hb bs)
                | otherwise = Nothing
    newHashes :: [(FastString, BS.ByteString)]
    newHashes = mapMaybe mkNewHash checkKeys
    rootSCCs :: [[FastString]]
    rootSCCs = filter isRootSCC sccs
    isRootSCC :: [FastString] -> Bool
    isRootSCC scc = not (all (`elemUniqMap` finalHashes) scc) && all check scc
      where
        check n = let Just (_bs, out) = lookupUniqMap hashes n
                  in  all checkEdge out
        checkEdge (LexicalFastString e) = e `elementOfUniqSet` s || e `elemUniqMap` finalHashes
        s = mkUniqSet scc
    hashRootSCC :: [FastString] -> [(FastString,BS.ByteString)]
    hashRootSCC scc
      | any (`elemUniqMap` finalHashes) scc = panic "Gen2.Compactor.hashRootSCC: has finalized nodes"
      | otherwise = map makeHash toHash
      where
        makeHash k = let Just (bs,deps) = lookupUniqMap hashes k
                         luds           = map lookupDep deps
                     in (k, makeFinalHash bs luds)
        lookupDep :: LexicalFastString -> BS.ByteString
        lookupDep (LexicalFastString d)
          | Just b <- lookupUniqMap finalHashes d = b
          | Just i <- lookupUniqMap toHashIdx d
              = grpHash <> (utf8EncodeByteString . show $ i)
          | otherwise
              = panic $ "Gen2.Compactor.hashRootSCC: unknown key: " ++
                              unpackFS d
        toHashIdx :: UniqMap FastString Integer
        toHashIdx = listToUniqMap $ zip toHash [1..]
        grpHash :: BS.ByteString
        grpHash = BL.toStrict
                . BB.toLazyByteString
                $ mconcat (map (mkGrpHash . fromJust . lookupUniqMap hashes) toHash)
        mkGrpHash (h, deps) =
          let deps' = mapMaybe (\(LexicalFastString d) -> lookupUniqMap finalHashes d) deps
          in  BB.byteString h <>
              BB.int64LE (fromIntegral $ length deps') <>
              mconcat (map BB.byteString deps')
        toHash :: [FastString]
        toHash = List.sortBy (compare `on` fst . (fromJust . lookupUniqMap hashes)) scc

makeFinalHash :: BS.ByteString -> [BS.ByteString] -> BS.ByteString
makeFinalHash b bs = mconcat (b:bs)

-- do not deduplicate thunks
ignoreStatic :: StaticInfo -> Bool
ignoreStatic (StaticInfo _ StaticThunk {} _) = True
ignoreStatic _                               = False

-- combine hashes from x and y, leaving only those which have an entry in both
combineHashes :: [(FastString, HashBuilder)]
              -> [(FastString, HashBuilder)]
              -> [(FastString, HashBuilder)]
combineHashes x y = map unlexical . M.toList $ M.intersectionWith (<>)
                                                  (M.fromList $ map lexical x)
                                                  (M.fromList $ map lexical y)
  where
    lexical (f, x) = (LexicalFastString f, x)
    unlexical (LexicalFastString f, x) = (f, x)

{-
dumpHashes0 :: Map ShortText HashBuilder -> Bool
dumpHashes0 hashes = unsafePerformIO writeHashes `seq` True
  where
    hashLine (n, HashBuilder bb txt) =
      n <> " ->\n    " <>
      escapeBS (BB.toLazyByteString bb) <> "\n    [" <> T.intercalate " " txt <> "]\n"
    escapeBS :: BL.ByteString -> T.Text
    escapeBS = mkFastString . concatMap escapeCH . BL.unpack
    escapeCH c | c < 32 || c > 127 = '\\' : show c
               | c == 92           = "\\\\"
               | otherwise         = [chr (fromIntegral c)]

    writeHashes = do
      putStrLn "writing hashes0"
      T.writeFile "hashes0.dump" (T.unlines $ map hashLine (M.toList hashes))

dumpHashes :: Map ShortText Hash -> Value
dumpHashes idx = toJSON iidx
   where
       iidx :: Map ShortText [(Text, [ShortText])]
       iidx = M.fromListWith (++) $
         map (\(t, (b, deps)) -> (TE.decodeUtf8 (B16.encode b), [(t,deps)])) (M.toList idx)
-}

ht :: Int8 -> HashBuilder
ht x = HashBuilder (BB.int8 x) []

hi :: Int -> HashBuilder
hi x = HashBuilder (BB.int64LE $ fromIntegral x) []

hi' :: (Show a, Integral a) => a -> HashBuilder
hi' x | x' > toInteger (maxBound :: Int64) || x' < toInteger (minBound :: Int64) =
        panic $ "Gen2.Compactor.hi': integer out of range: " ++ show x
      | otherwise = HashBuilder (BB.int64LE $ fromInteger x') []
  where
    x' = toInteger x

hd :: Double -> HashBuilder
hd d = HashBuilder (BB.doubleLE d) []

htxt :: FastString -> HashBuilder
htxt x = HashBuilder (BB.int64LE (fromIntegral $ BS.length bs) <> BB.byteString bs) []
  where
    bs = utf8EncodeByteString $ unpackFS x

hobj :: FastString -> HashBuilder
hobj x = HashBuilder (BB.int8 127) [x]

hb :: BS.ByteString -> HashBuilder
hb x = HashBuilder (BB.int64LE (fromIntegral $ BS.length x) <> BB.byteString x) []

hashDefinitions :: UniqSet FastString -> JStat -> [(FastString, HashBuilder)]
hashDefinitions globals st =
  let defs = findDefinitions st
  in  map (uncurry (hashSingleDefinition globals)) defs

findDefinitions :: JStat -> [(Ident, JExpr)]
findDefinitions (BlockStat ss)                    = concatMap findDefinitions ss
findDefinitions (AssignStat (ValExpr (JVar i)) e) = [(i,e)]
findDefinitions _                                 = []

hashSingleDefinition :: UniqSet FastString -> Ident -> JExpr -> (FastString, HashBuilder)
hashSingleDefinition globals (TxtI i) expr = (i, ht 0 <> render st <> mconcat (map hobj globalRefs))
  where
    globalRefs = filter (`elementOfUniqSet` globals) . map itxt $ uniqDSetToList (identsE expr)
    globalMap  = listToUniqMap $ zip globalRefs (map (mkFastString . ("h$$$global_"++) . show) [(1::Int)..])
    expr'      = identsE' (\i@(TxtI t) ->  maybe i TxtI (lookupUniqMap globalMap t)) expr
    st         = AssignStat (ValExpr (JVar (TxtI "dummy"))) expr'
    render     = htxt . mkFastString. show . pretty


identsE' :: (Ident -> Ident) -> JExpr -> JExpr
identsE' f (ValExpr v)         = ValExpr     $! identsV' f v
identsE' f (SelExpr e i)       = SelExpr     (identsE' f e) i -- do not rename properties
identsE' f (IdxExpr e1 e2)     = IdxExpr     (identsE' f e1) (identsE' f e2)
identsE' f (InfixExpr s e1 e2) = InfixExpr s  (identsE' f e1) (identsE' f e2)
identsE' f (UOpExpr o e)       = UOpExpr o   $! identsE' f e
identsE' f (IfExpr e1 e2 e3)   = IfExpr      (identsE' f e1) (identsE' f e2) (identsE' f e3)
identsE' f (ApplExpr e es)     = ApplExpr    (identsE' f e)  (identsE' f <$> es)
identsE' _ UnsatExpr{}         = error "identsE': UnsatExpr"

identsV' :: (Ident -> Ident) -> JVal -> JVal
identsV' f (JVar i)       = JVar  $! f i
identsV' f (JList xs)     = JList $! (fmap . identsE') f xs
identsV' _ d@JDouble{}    = d
identsV' _ i@JInt{}       = i
identsV' _ s@JStr{}       = s
identsV' _ r@JRegEx{}     = r
identsV' f (JHash m)      = JHash $! (fmap . identsE') f m
identsV' f (JFunc args s) = JFunc (fmap f args) (identsS' f s)
identsV' _ UnsatVal{}     = error "identsV': UnsatVal"

identsS' :: (Ident -> Ident) -> JStat -> JStat
identsS' f (DeclStat i e)       = DeclStat       (f i) e
identsS' f (ReturnStat e)       = ReturnStat     $! identsE' f e
identsS' f (IfStat e s1 s2)     = IfStat         (identsE' f e) (identsS' f s1) (identsS' f s2)
identsS' f (WhileStat b e s)    = WhileStat b    (identsE' f e) (identsS' f s)
identsS' f (ForInStat b i e s)  = ForInStat b    (f i) (identsE' f e) (identsS' f s)
identsS' f (SwitchStat e xs s)  = SwitchStat     (identsE' f e) (fmap (traverseCase f) xs) (identsS' f s)
  where traverseCase g (e,s) = (identsE' g e, identsS' g s)
identsS' f (TryStat s1 i s2 s3) = TryStat     (identsS' f s1) (f i) (identsS' f s2) (identsS' f s3)
identsS' f (BlockStat xs)       = BlockStat   $! identsS' f <$> xs
identsS' f (ApplStat e es)      = ApplStat    (identsE' f e) (identsE' f <$> es)
identsS' f (UOpStat op e)       = UOpStat op  $! identsE' f e
identsS' f (AssignStat e1 e2)   = AssignStat  (identsE' f e1) (identsE' f e2)
identsS' _ UnsatBlock{}         = error "identsS': UnsatBlock"
identsS' f (LabelStat l s)      = LabelStat l $! identsS' f s
identsS' _ b@BreakStat{}        = b
identsS' _ c@ContinueStat{}     = c

hashClosureInfo :: ClosureInfo -> (FastString, HashBuilder)
hashClosureInfo (ClosureInfo civ cir _cin cil cit cis) =
  (civ, ht 1 <> hashCIRegs cir <> hashCILayout cil <> hashCIType cit <> hashCIStatic cis)

hashStaticInfo :: StaticInfo -> (FastString, HashBuilder)
hashStaticInfo (StaticInfo sivr sivl _sicc) =
  (sivr, ht 2 <> hashStaticVal sivl)

hashCIType :: CIType -> HashBuilder
hashCIType (CIFun a r)  = ht 1 <> hi a <> hi r
hashCIType CIThunk      = ht 2
hashCIType (CICon c)    = ht 3 <> hi c
hashCIType CIPap        = ht 4
hashCIType CIBlackhole  = ht 5
hashCIType CIStackFrame = ht 6


hashCIRegs :: CIRegs -> HashBuilder
hashCIRegs CIRegsUnknown   = ht 1
hashCIRegs (CIRegs sk tys) = ht 2 <> hi sk <> hashList hashVT tys

hashCILayout :: CILayout -> HashBuilder
hashCILayout CILayoutVariable       = ht 1
hashCILayout (CILayoutUnknown size) = ht 2 <> hi size
hashCILayout (CILayoutFixed n l)    = ht 3 <> hi n <> hashList hashVT l

hashCIStatic :: CIStatic -> HashBuilder
hashCIStatic CIStaticRefs{} = mempty -- hashList hobj xs -- we get these from the code

hashList :: (a -> HashBuilder) -> [a] -> HashBuilder
hashList f xs = hi (length xs) <> mconcat (map f xs)

hashVT :: VarType -> HashBuilder
hashVT = hi . fromEnum

hashStaticVal :: StaticVal -> HashBuilder
hashStaticVal (StaticFun t args)       = ht 1 <> hobj t <> hashList hashStaticArg args
hashStaticVal (StaticThunk mtn)        = ht 2 <> hashMaybe htobj mtn
  where
    htobj (o, args) = hobj o <> hashList hashStaticArg args
hashStaticVal (StaticUnboxed su)      = ht 3 <> hashStaticUnboxed su
hashStaticVal (StaticData dcon args)  = ht 4 <> hobj dcon <> hashList hashStaticArg args
hashStaticVal (StaticList args lt)    = ht 5 <> hashList hashStaticArg args <> hashMaybe hobj lt

hashMaybe :: (a -> HashBuilder) -> Maybe a -> HashBuilder
hashMaybe _ Nothing  = ht 1
hashMaybe f (Just x) = ht 2 <> f x

hashStaticUnboxed :: StaticUnboxed -> HashBuilder
hashStaticUnboxed (StaticUnboxedBool b)           = ht 1 <> hi (fromEnum b)
hashStaticUnboxed (StaticUnboxedInt iv)           = ht 2 <> hi' iv
hashStaticUnboxed (StaticUnboxedDouble sd)        = ht 3 <> hashSaneDouble sd
hashStaticUnboxed (StaticUnboxedString str)       = ht 4 <> hb str
hashStaticUnboxed (StaticUnboxedStringOffset str) = ht 5 <> hb str


hashStaticArg :: StaticArg -> HashBuilder
hashStaticArg (StaticObjArg t)       = ht 1 <> hobj t
hashStaticArg (StaticLitArg sl)      = ht 2 <> hashStaticLit sl
hashStaticArg (StaticConArg cn args) = ht 3 <> hobj cn <> hashList hashStaticArg args

hashStaticLit :: StaticLit -> HashBuilder
hashStaticLit (BoolLit b)       = ht 1 <> hi (fromEnum b)
hashStaticLit (IntLit iv)       = ht 2 <> hi (fromIntegral iv)
hashStaticLit  NullLit          = ht 3
hashStaticLit (DoubleLit d)     = ht 4 <> hashSaneDouble d
hashStaticLit (StringLit tt) = ht 5 <> htxt tt
hashStaticLit (BinLit bs)       = ht 6 <> hb bs
hashStaticLit (LabelLit bb ln) = ht 7 <> hi (fromEnum bb) <> htxt ln

hashSaneDouble :: SaneDouble -> HashBuilder
hashSaneDouble (SaneDouble sd) = hd sd

finalizeHash :: HashBuilder -> Hash
finalizeHash (HashBuilder hb tt) =
  let h = (BL.toStrict $ BB.toLazyByteString hb)
  in  h `seq` (h, map LexicalFastString tt)

finalizeHash' :: HashBuilder -> (Int, BS.ByteString, [FastString])
finalizeHash' (HashBuilder hb tt) =
  let b  = BL.toStrict (BB.toLazyByteString hb)
      bl = BS.length b
      h  = b
  in  h `seq` bl `seq` (bl, h, tt)
