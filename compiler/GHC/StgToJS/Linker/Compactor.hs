{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE OverloadedStrings   #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.StgToJS.Linker
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
-- TODO: Jeff (2022,03): I've adapted this to ghcHEAD but have not actually
-- implemented the compactor. The key work function is @packString@ which
-- currently explodes if called. The todo is to fix this, and actually implement
-- the compactor once we have a linker that actually works.
-----------------------------------------------------------------------------

module GHC.StgToJS.Linker.Compactor
  ( compact
    -- FIXME (Sylvain 2022-04): remove or use these exports
  , collectGlobals
  , debugShowStat
  , packStrings
  , staticInfoArgs
  , staticValArgs
  ) where


import           GHC.Utils.Panic
import           GHC.Utils.Misc
import           GHC.Types.Unique.Map
import           GHC.Types.Unique.Set
import           GHC.Types.Unique.DSet

import           Control.Applicative
import           GHC.Utils.Monad.State.Strict
import           Data.Function

import qualified Data.Bits as Bits
import           Data.Bits (shiftL, shiftR)
import           Data.Bifunctor (second)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Builder as BB
import           Data.Char (chr)
import qualified Data.Graph as G
import qualified Data.Map.Strict as M
import           Data.Map (Map)
import           Data.Int
import qualified Data.List as List
import           Data.Maybe
import qualified Data.Set as S
import           Data.Set (Set)
import           GHC.Data.FastString

import           GHC.JS.Syntax
import           GHC.JS.Make
import           GHC.JS.Transform
import           GHC.StgToJS.Printer             (pretty)
import           GHC.StgToJS.Types
import           GHC.StgToJS.Linker.Types
import           GHC.StgToJS.CoreUtils
import           GHC.StgToJS.Closure
import           GHC.StgToJS.Arg()

import Prelude
import GHC.Utils.Encoding


-- | collect global objects (data / CAFs). rename them and add them to the table
collectGlobals :: [StaticInfo]
               -> State CompactorState ()
collectGlobals = mapM_ (\(StaticInfo i _ _) -> renameObj i)

debugShowStat :: (JStat, [ClosureInfo], [StaticInfo]) -> String
debugShowStat (_s, cis, sis) =
  "closures:\n" ++
  unlines (map show cis) ++
  "\nstatics:" ++
  unlines (map show sis) ++
  "\n\n"

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
  -- let allStatics :: [StaticInfo]
  --     allStatics = concatMap (\(_,_,x) -> x) code

  --     origStringTable :: StringTable
  --     origStringTable = cstate ^. stringTable

  --     allStrings :: Set ByteString
  --     allStrings = S.fromList $
  --                  filter (not . isExisting)
  --                         (mapMaybe (staticString . siVal) allStatics)
  --       where
  --         isExisting bs = isJust (M.lookup bs $ stOffsets origStringTable)

  --     staticString :: StaticVal -> Maybe ByteString
  --     staticString (StaticUnboxed (StaticUnboxedString bs)) = Just bs
  --     staticString (StaticUnboxed (StaticUnboxedStringOffset bs)) = Just bs
  --     staticString _ = Nothing

  --     allStringsList :: [ByteString]
  --     allStringsList = S.toList allStrings

  --     -- we may see two kinds of null characters
  --     --   - string separator, packed as \0
  --     --   - within a string, packed as \cz\0
  --     -- we transform the strings to
  --     transformPackedLiteral :: ShortText -> ShortText
  --     transformPackedLiteral = mconcat. fmap f
  --       where
  --         f :: Char -> ShortText
  --         f '\0'  = "\^Z\0"
  --         f '\^Z' = "\^Z\^Z"
  --         f x     = x

  --     allStringsPacked :: ShortText
  --     allStringsPacked = T.intercalate "\0" $
  --       map (\str -> maybe (packBase64 str)
  --                    transformPackedLiteral
  --                    (U.decodeModifiedUTF8 str))
  --           allStringsList

  --     packBase64 :: ByteString -> ShortText
  --     packBase64 bs
  --       | BS.null bs = mempty
  --       | otherwise =
  --         let (h,t) = BS.splitAt 128 bs
  --             esc   = T.singleton '\^Z' <>
  --                     T.singleton (chr . fromIntegral $ BS.length h + 0x1f)
  --             b64   = esc <> fromJust (U.decodeModifiedUTF8 (B64.encode h))
  --         in  maybe b64 transformPackedLiteral (U.decodeModifiedUTF8 h) <>
  --             packBase64 t

  --     allStringsWithOffset :: [(ByteString, Int)]
  --     allStringsWithOffset = snd $
  --       mapAccumL (\o b -> let o' = o + fromIntegral (BS.length b) + 1
  --                          in  o' `seq` (o', (b, o)))
  --                 0
  --                 allStringsList

  --     -- the offset of each of the strings in the big blob
  --     offsetIndex :: HashMap ByteString Int
  --     offsetIndex = M.fromList allStringsWithOffset

  --     stringSymbol :: Ident
  --     stringSymbol = head $ cstate ^. identSupply

  --     stringSymbolT :: ShortText
  --     stringSymbolT = let (TxtI t) = stringSymbol in t

  --     stringSymbolIdx :: Int
  --     stringSymbolIdx = snd (bounds $ stTableIdents origStringTable) + 1

  --     -- append the new string symbol
  --     newTableIdents :: Array Int ShortText
  --     newTableIdents =
  --       listArray (0, stringSymbolIdx)
  --                 (elems (stTableIdents origStringTable) ++ [stringSymbolT])

  --     newOffsetsMap :: Map ByteString (Int, Int)
  --     newOffsetsMap = M.union (stOffsets origStringTable)
  --                              (fmap (stringSymbolIdx,) offsetIndex)

  --     newIdentsMap :: HashMap ShortText (Either Int Int)
  --     newIdentsMap =
  --       let f (StaticInfo s (StaticUnboxed (StaticUnboxedString bs)) _)
  --             = Just (s, Left . fst $ newOffsetsMap M.! bs)
  --           f (StaticInfo s (StaticUnboxed (StaticUnboxedStringOffset bs)) _)
  --             = Just (s, Right . snd $ newOffsetsMap M.! bs)
  --           f _ = Nothing
  --       in M.union (stIdents origStringTable)
  --                   (M.fromList $ mapMaybe f allStatics)

  --     newStringTable :: StringTable
  --     newStringTable = StringTable newTableIdents newOffsetsMap newIdentsMap

  --     newOffsetsInverted :: HashMap (Int, Int) ByteString
  --     newOffsetsInverted = M.fromList .
  --                          map (\(x,y) -> (y,x)) .
  --                          M.toList $
  --                          newOffsetsMap

  --     replaceSymbol :: ShortText -> Maybe JVal
  --     replaceSymbol t =
  --       let f (Left i)  = JVar (TxtI $ newTableIdents ! i)
  --           f (Right o) = JInt (fromIntegral o)
  --       in  fmap f (M.lookup t newIdentsMap)

  --     cstate0 :: CompactorState
  --     cstate0 = cstate & identSupply %~ tail
  --                      & stringTable .~ newStringTable

  --     initStr :: JStat
  --     initStr =
  --       DeclStat stringSymbol <>
  --       AssignStat (ValExpr $ JVar stringSymbol)
  --         (ApplExpr (ApplExpr (ValExpr $ JVar (TxtI "h$pstr"))
  --                             [ValExpr (JStr allStringsPacked)])
  --                   [])

  --     rewriteValsE :: JExpr -> JExpr
  --     rewriteValsE (ApplExpr e xs)
  --       | Just t <- appMatchStringLit e xs = ValExpr (JStr t)
  --     rewriteValsE (ValExpr v) = ValExpr (rewriteVals v)
  --     rewriteValsE e = e & exprsE %~ rewriteValsE

  --     rewriteVals :: JVal -> JVal
  --     rewriteVals (JVar (TxtI t))
  --       | Just v <- replaceSymbol t = v
  --     rewriteVals (JList es) = JList (map rewriteValsE es)
  --     rewriteVals (JHash m) = JHash (fmap rewriteValsE m)
  --     rewriteVals (JFunc args body) = JFunc args (body & exprsS %~ rewriteValsE)
  --     rewriteVals v = v

  --     rewriteStat :: JStat -> JStat
  --     rewriteStat st = st & exprsS %~ rewriteValsE

  --     appMatchStringLit :: JExpr -> [JExpr] -> Maybe ShortText
  --     appMatchStringLit (ValExpr (JVar (TxtI "h$decodeUtf8z")))
  --                       [ValExpr (JVar (TxtI x)), ValExpr (JVar (TxtI y))]
  --      | Just (Left i)  <- M.lookup x newIdentsMap
  --      , Just (Right j) <- M.lookup y newIdentsMap
  --      , Just bs        <- M.lookup (i,j) newOffsetsInverted =
  --        U.decodeModifiedUTF8 bs
  --     appMatchStringLit _ _ = Nothing

  --     rewriteStatic :: StaticInfo -> Maybe StaticInfo
  --     rewriteStatic (StaticInfo _i
  --                               (StaticUnboxed StaticUnboxedString{})
  --                               _cc) =
  --       Nothing
  --     rewriteStatic (StaticInfo _i
  --                               (StaticUnboxed StaticUnboxedStringOffset {})
  --                               _cc) =
  --       Nothing
  --     rewriteStatic si = Just (si & staticInfoArgs %~ rewriteStaticArg)

  --     rewriteStaticArg :: StaticArg -> StaticArg
  --     rewriteStaticArg a@(StaticObjArg t) =
  --       case M.lookup t newIdentsMap of
  --         Just (Right v)       -> StaticLitArg (IntLit $ fromIntegral v)
  --         Just (Left idx)      -> StaticObjArg (newTableIdents ! idx)
  --         _                    -> a
  --     rewriteStaticArg (StaticConArg v es)
  --       = StaticConArg v (map rewriteStaticArg es)
  --     rewriteStaticArg x = x

  --     initStatic :: LinkedUnit
  --     initStatic =
  --       let (TxtI ss) = stringSymbol
  --       in  (initStr, [], [StaticInfo ss (StaticThunk Nothing) Nothing])

  --     rewriteBlock :: LinkedUnit -> LinkedUnit
  --     rewriteBlock (stat, ci, si)
  --       = (rewriteStat stat, ci, mapMaybe rewriteStatic si)

  --   in (cstate0, initStatic : map rewriteBlock code)

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
      -- \| csDebugAlloc cfg || csProf cfg = do -- FIXME: Jeff (2022,03): Move these Way flags into JSLinkConfig

      -- FIXME (Sylvain, 2022-05): forced for now until packStrings creates a
      -- proper string table.
      -- NOTE (Jeff, 2022-06): I've commented out the block of code for the
      -- otherwise case in the below guard. This is to silence warnings about
      -- the redundant pattern match. Once packStrings works make sure to
      -- re-enable and remove this comment and previous fixme
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
       {-
      | otherwise = do
        -- collect all global objects and entries, add them to the renaming table
        mapM_ (\(_, cis, sis) -> do
               mapM_ (renameEntry . TxtI . ciVar) cis
               mapM_ (renameObj . siVar) sis
               mapM_ collectLabels sis) stats0

        -- sort our entries, store the results
        -- propagate all renamings throughtout the code
        cs <- get
            -- FIXME: Jeff (2022,03): Is this workaround still needed?
            -- Safari on iOS 10 (64 bit only?) crashes on very long arrays
            -- safariCrashWorkaround :: [Ident] -> JExpr
            -- safariCrashWorkaround xs =
            --   case chunksOf 10000 xs of
            --     (y:ys) | not (null ys)
            --       -> ApplExpr (SelExpr (toJExpr y) (TxtI "concat"))
            --                   (map toJExpr ys)
            --     _ -> toJExpr xs
        let renamedStats = map (\(s,_,_) -> identsS' (lookupRenamed cs) s)
                               stats0
            sortedInfo   = concatMap (\(_,xs,_) -> map (renameClosureInfo cs)
                                                       xs)
                                     stats0
            -- entryArr     = safariCrashWorkaround $
            entryArr     = toJExpr
                           . map (TxtI . fst)
                           . List.sortBy (compare `on` snd)
                           . M.toList
                           $ csEntries cs
            lblArr       = map (TxtI . fst)
                           . List.sortBy (compare `on` snd)
                           . M.toList
                           $ csLabels cs
            ss           = concatMap (\(_,_,xs) -> map (renameStaticInfo cs) xs)
                                     stats0
            infoBlock    = encodeStr (concatMap (encodeInfo cs) sortedInfo)
            staticBlock  = encodeStr (concatMap (encodeStatic cs) ss)
            stbs'        = identsS' (lookupRenamed cs) stbs
            staticDecls  = mconcat (map staticDeclStat ss) <> stbs'
            meta = staticDecls `mappend`
                   appS "h$scheduleInit" [ entryArr
                                         , var "h$staticDelayed"
                                         , toJExpr lblArr
                                         , toJExpr infoBlock
                                         , toJExpr staticBlock
                                         ]
                   -- error "scheduleInit"
                   {-
                   [j| h$scheduleInit( `entryArr`
                                     , h$staticDelayed
                                     , `lblArr`
                                     , `infoBlock`
                                     , `staticBlock`);
                       h$staticDelayed = [];
                     |] -}

        return (renamedStats, meta)
        -}

-- | initialize a global object. all global objects have to be declared (staticInfoDecl) first
--   (this is only used with -debug, normal init would go through the static data table)
staticInitStat :: Bool         -- ^ profiling enabled
               -> StaticInfo
               -> JStat
staticInitStat _prof (StaticInfo i sv cc) =
  case sv of
    StaticData con args -> appS "h$sti" ([var i, var con, toJExpr args] ++ ccArg)
    StaticFun  f   args -> appS "h$sti" ([var i, var f, toJExpr args] ++ ccArg)
    StaticList args mt   ->
      appS "h$stl" ([var i, toJExpr args, toJExpr $ maybe null_ (toJExpr . TxtI) mt] ++ ccArg)
    StaticThunk (Just (f,args)) ->
      appS "h$stc" ([var i, var f, toJExpr args] ++ ccArg)
    _                    -> mempty
  where
    ccArg = maybeToList (fmap toJExpr cc)

-- | declare and do first-pass init of a global object (create JS object for heap objects)
staticDeclStat :: StaticInfo -> JStat
staticDeclStat (StaticInfo si sv _) =
  let si' = TxtI si
      ssv (StaticUnboxed u)       = Just (ssu u)
      ssv (StaticThunk Nothing)   = Nothing
      ssv _                       = Just (app "h$d" [])
      ssu (StaticUnboxedBool b)   = app "h$p" [toJExpr b]
      ssu (StaticUnboxedInt i)    = app "h$p" [toJExpr i]
      ssu (StaticUnboxedDouble d) = app "h$p" [toJExpr (unSaneDouble d)]
      ssu (StaticUnboxedString str) = ApplExpr (initStr str) []
      ssu StaticUnboxedStringOffset {} = 0
  -- FIXME, we shouldn't do h$di, we need to record the statement to init the thunks
  in  maybe (appS "h$di" [toJExpr si']) (\v -> DeclStat si' `mappend` (toJExpr si' |= v)) (ssv sv)

initStr :: BS.ByteString -> JExpr
initStr str = app "h$str" [ValExpr (JStr . mkFastString . BSC.unpack $! str)]
      --TODO: Jeff (2022,03): This function used to call @decodeModifiedUTF8 in
      --Gen2.Utils. I've removed the call site and opted to keep the Just case.
      --We'll need to double check to see if we indeed do need to decoded the
      --UTF8 strings and implement a replace function on bytestrings once the
      --Linker is up.
      -- Nothing -> app "h$rstr" [toJExpr $ map toInteger (BS.unpack str)]
      -- error "initStr"
      -- [je| h$rstr(`map toInteger (B.unpack str)`) |]

-- | rename a heap object, which means adding it to the
--  static init table in addition to the renamer
renameObj :: FastString
          -> State CompactorState FastString
renameObj xs = do
  (TxtI xs') <- renameVar (TxtI xs)               -- added to the renamer
  modify (addStaticEntry xs') -- and now the table
  return xs'

renameEntry :: Ident
            -> State CompactorState Ident
renameEntry i = do
  i'@(TxtI i'') <- renameVar i
  modify (addEntry i'')
  return i'

collectLabels :: StaticInfo -> State CompactorState ()
collectLabels si = mapM_ go (labelsV . siVal $ si)
  where
    go :: FastString -> State CompactorState ()
    go = modify . addLabel
    labelsV (StaticData _ args) = concatMap labelsA args
    labelsV (StaticList args _) = concatMap labelsA args
    labelsV _                   = []
    labelsA (StaticLitArg l) = labelsL l
    labelsA _                = []
    labelsL (LabelLit _ lbl) = [lbl]
    labelsL _                = []

lookupRenamed :: CompactorState -> Ident -> Ident
lookupRenamed cs i@(TxtI t) =
  fromMaybe i (lookupUniqMap (csNameMap cs) t)

renameVar :: Ident                      -- ^ text identifier to rename
          -> State CompactorState Ident -- ^ the updated renamer state and the new ident
renameVar i@(TxtI t)
  | "h$$" `List.isPrefixOf` unpackFS t = do
      m <- gets csNameMap
      case lookupUniqMap m t of
        Just r  -> return r
        Nothing -> do
          y <- newIdent
          let add_var cs' = cs' {csNameMap = addToUniqMap (csNameMap cs') t y}
          modify add_var
          return y
  | otherwise = return i

newIdent :: State CompactorState Ident
newIdent = do
  yys <- gets csIdentSupply
  case yys of
    (y:ys) -> do
      modify (\cs -> cs {csIdentSupply = ys})
      return y
    _ -> error "newIdent: empty list"

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

-- staticIdentsA :: Traversal' StaticArg ShortText
staticIdentsA :: (FastString -> FastString) -> StaticArg -> StaticArg
staticIdentsA f (StaticObjArg t) = StaticObjArg $! f t
staticIdentsA _ x = x


{-
   simple encoding of naturals using only printable low char points,
   rely on gzip to compress repeating sequences,
   most significant bits first
      1 byte: ascii code 32-123  (0-89), \ and " unused
      2 byte: 124 a b            (90-8189)
      3 byte: 125 a b c          (8190-737189)
-}
encodeStr :: HasDebugCallStack => [Int] -> String
encodeStr = concatMap encodeChr
  where
    c :: HasDebugCallStack => Int -> Char
    c i | i > 90 || i < 0 = error ("encodeStr: c " ++ show i)
        | i >= 59   = chr (34+i)
        | i >= 2    = chr (33+i)
        | otherwise = chr (32+i)
    encodeChr :: HasDebugCallStack => Int -> String
    encodeChr i
      | i < 0       = panic "encodeStr: negative"
      | i <= 89     = [c i]
      | i <= 8189   = let (c1, c2)  = (i - 90) `divMod` 90 in [chr 124, c c1, c c2]
      | i <= 737189 = let (c2a, c3) = (i - 8190) `divMod` 90
                          (c1, c2)  = c2a `divMod` 90
                      in [chr 125, c c1, c c2, c c3]
      | otherwise = panic "encodeStr: overflow"

entryIdx :: HasDebugCallStack
         => String
         -> CompactorState
         -> FastString
         -> Int
entryIdx msg cs i = fromMaybe lookupParent (lookupUniqMap (csEntries cs) i')
  where
    (TxtI i')    = lookupRenamed cs (TxtI i)
    lookupParent = maybe err
                         (+ csNumEntries cs)
                         (lookupUniqMap (csParentEntries cs) i')
    err = panic (msg ++ ": invalid entry: " ++ unpackFS i')

objectIdx :: HasDebugCallStack
          => String
          -> CompactorState
          -> FastString
          -> Int
objectIdx msg cs i = fromMaybe lookupParent (lookupUniqMap (csStatics cs) i')
  where
    (TxtI i')    = lookupRenamed cs (TxtI i)
    lookupParent = maybe err
                         (+ csNumStatics cs)
                         (lookupUniqMap (csParentStatics cs) i')
    err          = panic (msg ++ ": invalid static: " ++ unpackFS i')

labelIdx :: HasDebugCallStack
         => String
         -> CompactorState
         -> FastString
         -> Int
labelIdx msg cs l = fromMaybe lookupParent (lookupUniqMap (csLabels cs) l)
  where
    lookupParent = maybe err
                         (+ csNumLabels cs)
                         (lookupUniqMap (csParentLabels cs) l)
    err          = panic (msg ++ ": invalid label: " ++ unpackFS l)

encodeInfo :: HasDebugCallStack
           => CompactorState
           -> ClosureInfo  -- ^ information to encode
           -> [Int]
encodeInfo cs (ClosureInfo _var regs name layout typ static)
  | CIThunk              <- typ = 0 : ls
  | (CIFun _arity regs0) <- typ, regs0 /= argSize regs
     = panic ("encodeInfo: inconsistent register metadata for " ++ unpackFS name)
  | (CIFun arity _regs0) <- typ = [1, arity, encodeRegs regs] ++ ls
  | (CICon tag)          <- typ = [2, tag] ++ ls
  | CIStackFrame         <- typ = [3, encodeRegs regs] ++ ls
-- (CIPap ar)         <- typ = [4, ar] ++ ls  -- these should only appear during runtime
  | otherwise                  = panic $
      "encodeInfo, unexpected closure type: " ++ show typ
  where
    ls         = encodeLayout layout ++ encodeSrt static
    encodeLayout CILayoutVariable      = [0]
    encodeLayout (CILayoutUnknown s)   = [s+1]
    encodeLayout (CILayoutFixed s _vs) = [s+1]
    encodeSrt (CIStaticRefs rs) = length rs : map (objectIdx "encodeInfo" cs) rs
    encodeRegs CIRegsUnknown = 0
    encodeRegs (CIRegs skip regTypes) = let nregs = sum (map varSize regTypes)
                                        in  encodeRegsTag skip nregs
    encodeRegsTag skip nregs
      | skip < 0 || skip > 1 = panic "encodeRegsTag: unexpected skip"
      | otherwise            = 1 + nregs `shiftL` 1 + skip
    argSize (CIRegs skip regTypes) = sum (map varSize regTypes) - 1 + skip
    argSize _ = 0

encodeStatic :: HasDebugCallStack
             => CompactorState
             -> StaticInfo
             -> [Int]
encodeStatic cs si =
  -- U.trace' ("encodeStatic: " ++ show si)
           encodeStatic0 cs si

encodeStatic0 :: HasDebugCallStack
             => CompactorState
             -> StaticInfo
             -> [Int]
encodeStatic0 cs (StaticInfo _to sv _)
    | StaticFun f args <- sv =
      [1, entry f, length args] ++ concatMap encodeArg args
    | StaticThunk (Just (t, args)) <- sv =
      [2, entry t, length args] ++ concatMap encodeArg args
    | StaticThunk Nothing <- sv =
      [0]
    | StaticUnboxed (StaticUnboxedBool b) <- sv =
      [3 + fromEnum b]
    | StaticUnboxed (StaticUnboxedInt _i) <- sv =
      [5] -- ++ encodeInt i
    | StaticUnboxed (StaticUnboxedDouble _d) <- sv =
      [6] -- ++ encodeDouble d
    | (StaticUnboxed _) <- sv = [] -- unboxed strings have their own table
--    | StaticString t <- sv         = [7, T.length t] ++ map encodeChar (unpackFS t)
--    | StaticBin bs <- sv           = [8, BS.length bs] ++ map fromIntegral (BS.unpack bs)
    | StaticList [] Nothing <- sv =
      [8]
    | StaticList args t <- sv =
      [9, length args] ++
      maybe [0] (\t' -> [1, obj t']) t ++
      concatMap encodeArg (reverse args)
    | StaticData con args <- sv =
      (if length args <= 6
        then [11+length args]
        else [10,length args]) ++
      [entry con] ++
      concatMap encodeArg args
  where
    obj   = objectIdx "encodeStatic" cs
    entry = entryIdx  "encodeStatic" cs
    lbl   = labelIdx  "encodeStatic" cs
    --   an argument is either a reference to a heap object or a primitive value
    encodeArg (StaticLitArg (BoolLit b)) =
      [0 + fromEnum b]
    encodeArg (StaticLitArg (IntLit 0)) =
      [2]
    encodeArg (StaticLitArg (IntLit 1)) =
      [3]
    encodeArg (StaticLitArg (IntLit i)) =
      4 : encodeInt i
    encodeArg (StaticLitArg NullLit) =
      [5]
    encodeArg (StaticLitArg (DoubleLit d)) =
      6 : encodeDouble d
    encodeArg (StaticLitArg (StringLit s)) =
      7 : encodeString s
    encodeArg (StaticLitArg (BinLit b)) =
      8 : encodeBinary b
    encodeArg (StaticLitArg (LabelLit b l)) =
      [9, fromEnum b, lbl l]
    encodeArg (StaticConArg con args) =
      [10, entry con, length args] ++ concatMap encodeArg args
    encodeArg (StaticObjArg t) =
      [11 + obj t]
    -- encodeArg x                             = panic ("encodeArg: unexpected: " ++ show x)
    -- encodeChar = ord -- fixme make characters more readable

-- FIXME: Jeff (2022,03): Use FastString or ShortByteString and remove this
-- serialization/deserialization
encodeString :: FastString -> [Int]
encodeString = encodeBinary . BSC.pack . unpackFS

-- ByteString is prefixed with length, then blocks of 4 numbers encoding 3 bytes
encodeBinary :: BS.ByteString -> [Int]
encodeBinary bs = BS.length bs : go bs
  where
    go b | BS.null b = []
         | l == 1    = let b0 = b `BS.index` 0
                       in  map fromIntegral [ b0 `shiftR` 2, (b0 Bits..&. 3) `shiftL` 4 ]
         | l == 2    = let b0 = b `BS.index` 0
                           b1 = b `BS.index` 1
                       in  map fromIntegral [ b0 `shiftR` 2
                                            , ((b0 Bits..&. 3) `shiftL` 4) Bits..|. (b1 `shiftR` 4)
                                            , (b1 Bits..&. 15) `shiftL` 2
                                            ]
         | otherwise = let b0 = b `BS.index` 0
                           b1 = b `BS.index` 1
                           b2 = b `BS.index` 2
                       in  map fromIntegral [ b0 `shiftR` 2
                                            , ((b0 Bits..&. 3)  `shiftL` 4) Bits..|. (b1 `shiftR` 4)
                                            , ((b1 Bits..&. 15) `shiftL` 2) Bits..|. (b2 `shiftR` 6)
                                            , b2 Bits..&. 63
                                            ] ++ go (BS.drop 3 b)
      where l = BS.length b

encodeInt :: Integer -> [Int]
encodeInt i
  | i >= -10 && i < encodeMax - 11 = [fromIntegral i + 12]
  | i > 2^(31::Int)-1 || i < -2^(31::Int)
    = panic "encodeInt: integer outside 32 bit range"
  | otherwise = let i' :: Int32 = fromIntegral i
                in [ 0
                   , fromIntegral ((i' `shiftR` 16) Bits..&. 0xffff)
                   , fromIntegral (i' Bits..&. 0xffff)
                   ]

-- encode a possibly 53 bit int
encodeSignificand :: Integer -> [Int]
encodeSignificand i
  | i >= -10 && i < encodeMax - 11      = [fromIntegral i + 12]
  | i > 2^(53::Int) || i < -2^(53::Int)
    = panic ("encodeInt: integer outside 53 bit range: " ++ show i)
  | otherwise = let i' = abs i
                in  (if i < 0 then 0 else 1) :
                    map (\r -> fromIntegral ((i' `shiftR` r) Bits..&. 0xffff))
                        [48,32,16,0]

encodeDouble :: SaneDouble -> [Int]
encodeDouble (SaneDouble d)
  | isNegativeZero d      = [0]
  | d == 0                = [1]
  | isInfinite d && d > 0 = [2]
  | isInfinite d          = [3]
  | isNaN d               = [4]
  | abs exponent <= 30
    = (6 + fromIntegral exponent + 30) : encodeSignificand significand
  | otherwise
    = [5] ++ encodeInt (fromIntegral exponent) ++ encodeSignificand significand
    where
      (significand, exponent) = decodeFloat d

encodeMax :: Integer
encodeMax = 737189

{- |
  The Base data structure contains the information we need
  to do incremental linking against a base bundle.

  base file format:
  GHCJSBASE
  [renamer state]
  [linkedPackages]
  [packages]
  [modules]
  [symbols]
 -}

staticInfoArgs :: Applicative f => (StaticArg -> f StaticArg) -> StaticInfo -> f StaticInfo
staticInfoArgs f (StaticInfo si sv sa) = StaticInfo si <$> staticValArgs f sv <*> pure sa

staticValArgs :: Applicative f => (StaticArg -> f StaticArg) -> StaticVal -> f StaticVal
staticValArgs f (StaticFun fn as) = StaticFun fn <$> traverse f as
staticValArgs f (StaticThunk (Just (t, as))) = StaticThunk . Just . (t,) <$> traverse f as
staticValArgs f (StaticData c as) = StaticData c <$> traverse f as
staticValArgs f (StaticList as mt) = StaticList <$> traverse f as <*> pure mt
staticValArgs _ x = pure x

compact :: JSLinkConfig
        -> StgToJSConfig
        -> CompactorState
        -> [FastString]
        -> [LinkedUnit]
        -> (CompactorState, [JStat], JStat)
compact ln_cfg cfg cs0 rtsDeps0 input0
--  | dumpHashes' input
  =
  let rtsDeps1 = rtsDeps0 ++
                 map (<> "_e") rtsDeps0 ++
                 map (<> "_con_e") rtsDeps0
      -- FIXME (Sylvain, 2022-05): disabled (again)
      -- (cs1, input1) = packStrings ln_cfg cs0 input0
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
      in  DeclStat (TxtI bfn) <>
          AssignStat (ValExpr (JVar (TxtI bfn)))
                     (ValExpr (JFunc args body))
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
    removeDecls t (DeclStat (TxtI i))
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
      DeclStat bi <>
      AssignStat (ValExpr (JVar bi))
                 (ValExpr (JFunc bargs bbody))
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
  DeclStat (TxtI i) <>
  AssignStat (ValExpr (JVar (TxtI i)))
             (ApplExpr (ValExpr (JVar bfid)) (map (ValExpr . JVar . TxtI) deps))

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
findGlobals globals stat = filter isGlobal . map itxt . S.toList $ identsS stat
  where
    locals     = mkUniqSet (findLocals stat)
    isGlobal i = elementOfUniqSet i globals && not (elementOfUniqSet i locals)

findLocals :: JStat -> [FastString]
findLocals (BlockStat ss)      = concatMap findLocals ss
findLocals (DeclStat (TxtI i)) = [i]
findLocals _                   = []

nub' :: Ord a => [a] -> [a]
nub' = go S.empty
  where
    go _ []            = []
    go s (x:xs)
      | x `S.member` s = go s xs
      | otherwise      = x : go (S.insert x s) xs

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
    go s@(DeclStat (TxtI i))
      | not (isCanon hi i) = mempty
      | otherwise                              = s
    go (AssignStat v@(ValExpr (JVar (TxtI i))) e)
      | not (isCanon hi i) = mempty
      | otherwise                              = AssignStat v (identsE' (toCanonI hi) e)
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
    -- FIXME: Can we make this more efficient by avoiding lists and staying in GHC Unique collections?
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
-- FIXME: Jeff (2022,03): I've removed the SHA256.hash function which would be
-- producing this final bytestring. Do we need it? If so how to replace it?

-- do not deduplicate thunks
ignoreStatic :: StaticInfo -> Bool
ignoreStatic (StaticInfo _ StaticThunk {} _) = True
ignoreStatic _                               = False

-- combine hashes from x and y, leaving only those which have an entry in both
-- FIXME: Make users of this function consume a UniqMap
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
    globalRefs = filter (`elementOfUniqSet` globals) . map itxt $ S.toList (identsE expr)
    globalMap  = listToUniqMap $ zip globalRefs (map (mkFastString . ("h$$$global_"++) . show) [(1::Int)..])
    expr'      = identsE' (\i@(TxtI t) ->  maybe i TxtI (lookupUniqMap globalMap t)) expr
    st         = AssignStat (ValExpr (JVar (TxtI "dummy"))) expr'
    render     = htxt . mkFastString. show . pretty


-- FIXME: Jeff (2022,03): reduce the redundancy between these idents functions
-- and the idents functions in GHC.JS.Transform These helper functions also
-- exist in non-ticked for, e.g., @identsE@ in GHC.JS.Transform. These are
-- essential Functor instances over the JS syntax tree. We rewrite them here for
-- consumers like hashSingleDefinition. Had we used the Transform version we'll
-- end up with a compiler error in @expr'@ since AssignStat takes an Expr, but
-- Transform.IdentsE returns [Ident]
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
identsS' f (DeclStat i)         = DeclStat       $! f i
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
-- FIXME: Jeff (2022,03): I've removed the SHA256.hash function which would be
-- producing h. Do we need it? If so how to replace it?
  let h = (BL.toStrict $ BB.toLazyByteString hb)
  in  h `seq` (h, map LexicalFastString tt)

finalizeHash' :: HashBuilder -> (Int, BS.ByteString, [FastString])
finalizeHash' (HashBuilder hb tt) =
  let b  = BL.toStrict (BB.toLazyByteString hb)
      bl = BS.length b
-- FIXME: Jeff (2022,03): I've removed the SHA256.hash function which would be
-- producing h. So it is purposeful that `h = b` looks unnecessary. Do we need
-- it? If so how to replace it?
      h  = b
  in  h `seq` bl `seq` (bl, h, tt)
