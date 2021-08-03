{-# LANGUAGE CPP,
             TupleSections,
             OverloadedStrings,
             LambdaCase,
             MultiWayIf,
             ViewPatterns
  #-}

{-
  Main generator module
-}

module Gen2.Generator (generate) where

import           Fingerprint
import           ForeignCall
import           CostCentre
import           FastString
import           TysWiredIn
import           BasicTypes
import           ListSetOps
import           PrelNames
import           DynFlags
import           Encoding
import           UniqSet
import           Literal
import           DataCon
import           CoreSyn
import           IdInfo
import           TcType
import           UniqFM
import           Unique
import           StgSyn
import           PrimOp
import           Module
import           VarSet
import           Panic
import           TyCon
import           Util
import           Type hiding (typeSize)
import           RepType
import           TysPrim
import           Name
import           GHC
import           Id
import           HscTypes
import           Outputable hiding ((<>))
import qualified Outputable
import           Data.Function

import Prelude

import           Control.Applicative
import           Control.DeepSeq
-- import           Control.Lens hiding ((||=), (#))
import           Compiler.JMacro.Lens
import           Control.Monad.State.Strict

import           Data.Array
-- import qualified Data.Binary as Bin
-- import qualified Data.Binary.Put as Bin
-- import qualified Data.Binary.Get as Bin
import qualified Data.Bits as Bits
  -- ((.|.), shiftL, shiftR, (.&.), testBit, xor, complement)
import           Data.ByteString (ByteString)
-- import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Char (ord, {-chr, -} isDigit)
import           Data.Either (partitionEithers)
import           Data.Function (on)
import           Data.Generics.Aliases (mkT)
import           Data.Generics.Schemes (everywhere)
-- import           Data.Int
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet        as IS
import           Data.Maybe
  (isJust, isNothing, catMaybes, fromMaybe, {-maybeToList,-} listToMaybe)
import           Data.Map (Map)
import qualified Data.Map as M
-- import           Data.Set (Set)
import qualified Data.Set as S
import           Data.List
  (partition, intercalate, {-sort, -} sortBy, foldl', scanl')
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Text (Text)
import           Data.Ord

import           Compiler.JMacro
import           Compiler.JMacro.Symbols
import           Compiler.JMacro.Combinators
import qualified Text.Parsec as P

import           Compiler.Compat
import           Compiler.Settings

import           Gen2.Base
import           Gen2.Deps
import           Gen2.Utils
import           Gen2.Prim
import           Gen2.Rts
import           Gen2.RtsTypes
import           Gen2.StgAst
import           Gen2.RtsAlloc
import           Gen2.RtsApply
import qualified Gen2.Linker    as Linker
import           Gen2.ClosureInfo
import qualified Gen2.Optimizer as O
import qualified Gen2.Object    as Object
import           Gen2.Sinker
import           Gen2.Profiling
import qualified Gen2.Compactor as Compactor

import           GHC.Float

-- debug
import           Gen2.Printer             (pretty)

import qualified Data.Text.Lazy           as TL
import           Text.PrettyPrint.Leijen.Text (displayT, renderPretty)

data DependencyDataCache = DDC
         { _ddcModule   :: !(IntMap        Object.Package)  -- ^ Unique Module -> Object.Package
         , _ddcId       :: !(IntMap        Object.Fun)      -- ^ Unique Id     -> Object.Fun (only to other modules)
         , _ddcOther    :: !(Map OtherSymb Object.Fun)
         }

ddcId :: Lens' DependencyDataCache (IntMap Object.Fun)
ddcId f ddc = fmap (\x -> ddc { _ddcId = x }) (f $ _ddcId ddc)
{-# INLINE ddcId #-}

ddcModule :: Lens' DependencyDataCache (IntMap Object.Package)
ddcModule f ddc = fmap (\x -> ddc { _ddcModule = x }) (f $ _ddcModule ddc)
{-# INLINE ddcModule #-}

ddcOther :: Lens' DependencyDataCache (Map OtherSymb Object.Fun)
ddcOther f ddc = fmap (\x -> ddc { _ddcOther = x }) (f $ _ddcOther ddc)
{-# INLINE ddcOther #-}


data ExprCtx = ExprCtx
       { _ctxTop        :: Id
       , _ctxTarget     :: [(PrimRep,[JExpr])]
       , _ctxEval       :: UniqSet Id
       , _ctxLne        :: UniqSet Id -- ^ all lne-bound things
       , _ctxLneFrameBs :: UniqFM Int -- ^ binds in current lne frame (defined at size)
       , _ctxLneFrame   :: [(Id,Int)] -- ^ contents of current lne frame
       , _ctxSrcSpan    :: Maybe RealSrcSpan
       }

ctxEval :: Lens' ExprCtx (UniqSet Id)
ctxEval f ec = fmap (\x -> ec { _ctxEval = x }) (f $ _ctxEval ec)
{-# INLINE ctxEval #-}

ctxLne :: Lens' ExprCtx (UniqSet Id)
ctxLne f ec = fmap (\x -> ec { _ctxLne = x }) (f $ _ctxLne ec)
{-# INLINE ctxLne #-}

ctxLneFrame :: Lens' ExprCtx [(Id, Int)]
ctxLneFrame f ec = fmap (\x -> ec { _ctxLneFrame = x }) (f $ _ctxLneFrame ec)
{-# INLINE ctxLneFrame #-}

ctxLneFrameBs :: Lens' ExprCtx (UniqFM Int)
ctxLneFrameBs f ec = fmap (\x -> ec { _ctxLneFrameBs = x }) (f $ _ctxLneFrameBs ec)
{-# INLINE ctxLneFrameBs #-}

ctxSrcSpan :: Lens' ExprCtx (Maybe RealSrcSpan)
ctxSrcSpan f ec = fmap (\x -> ec { _ctxSrcSpan = x }) (f $ _ctxSrcSpan ec)
{-# INLINE ctxSrcSpan #-}

ctxTarget :: Lens' ExprCtx [(PrimRep, [JExpr])]
ctxTarget f ec = fmap (\x -> ec { _ctxTarget = x }) (f $ _ctxTarget ec)
{-# INLINE ctxTarget #-}

ctxTop :: Lens' ExprCtx Id
ctxTop f ec = fmap (\x -> ec { _ctxTop = x }) (f $ _ctxTop ec)
{-# INLINE ctxTop #-}

instance Show ExprCtx where
  show (ExprCtx top tgt eval lne _lnefbs lnef _mbSpan) =
    "ExprCtx\n" ++ unlines [show top, show tgt, sus eval, sus lne, show lnef]
    where
      sus = show . nonDetEltsUniqSet

clearCtxStack :: ExprCtx -> ExprCtx
clearCtxStack ctx = ctx & ctxLneFrameBs .~ emptyUFM
                        & ctxLneFrame   .~ []

adjustCtxStack :: Int -> ExprCtx -> ExprCtx
adjustCtxStack n ctx
  | l < n     = panic $ "adjustCtxStack: let-no-escape stack too short: " ++
                        show l ++ " < " ++ show n
  | otherwise = ctx & ctxLneFrame %~ take n
  where
    l = ctx ^. ctxLneFrame . to length

addEval :: Id -> ExprCtx -> ExprCtx
addEval i = over ctxEval (flip addOneToUniqSet i)

generate :: GhcjsSettings
         -> DynFlags
         -> Module
         -> [StgTopBinding]
         -> [SptEntry]
         -> ForeignStubs
         -> CollectedCCs
         -> ByteString -- ^ binary data for the .js_o object file
generate settings df m s spt_entries foreign_stubs cccs =
  let (uf, s') = sinkPgm m s
  in trace' ("generate\n" ++ intercalate "\n\n" (map showIndent s)) $

     flip evalState (initState df m uf) $ do
        ifProfiling' $ initCostCentres cccs
        (st, lus) <- genUnits df m s' spt_entries foreign_stubs
        -- (exported symbol names, javascript statements) for each linkable unit
        p <- forM lus $ \u ->
           mapM (fmap (\(TxtI i) -> i) . jsIdI) (luIdExports u) >>=
            \ts -> return (ts ++ luOtherExports u, luStat u)
        let (st', dbg) = dumpAst st settings df s'
            mod_name   = moduleNameText m
        deps <- genDependencyData df m lus
        -- p first, so numbering of linkable units lines up
        pure . BL.toStrict $
          Object.object' mod_name st' deps (p ++ dbg)
{- |
  Generate an extra linkable unit for the object file if -debug is active.
  this unit is never actually linked, but it contains the optimized STG AST
  so it can be easily reviewed using ghcjs --print-obj to aid in solving
  code generator problems.
 -}
dumpAst :: Object.SymbolTable
        -> GhcjsSettings
        -> DynFlags
        -> [StgTopBinding]
        -> (Object.SymbolTable, [([Text], BL.ByteString)])
dumpAst st _settings _dflags s
  | True {-|| buildingDebug dflags-} = (st', [(["h$debug", "h$dumpAst"], bs)])
  | otherwise            = (st, [])
      where
        (st', bs) = Object.serializeStat st [] [] (    BlockStat [(AssignStat (ValExpr (JVar (TxtI (T.pack "h$dumpAst"))))) (toJExpr x)]) {-[j| h$dumpAst = `x` |]-} "" [] []
        x = T.intercalate "\n\n" (map (T.pack . showIndent) s)

-- | variable prefix for the nth block in module
modulePrefix :: Module -> Int -> Text
modulePrefix m n =
  let encMod = zEncodeString . moduleNameString . moduleName $ m
  in  T.pack $ "h$" ++ encMod ++ "_id_" ++ show n

-- | data used to generate one ObjUnit in our object file
data LinkableUnit = LinkableUnit
  { luStat         :: BL.ByteString -- ^ serialized JS AST
  , luIdExports    :: [Id]          -- ^ exported names from haskell identifiers
  , luOtherExports :: [Text]        -- ^ other exports
  , luIdDeps       :: [Id]          -- ^ identifiers this unit depends on
  , luPseudoIdDeps :: [Unique]      -- ^ pseudo-id identifiers this unit depends on (fixme)
  , luOtherDeps    :: [OtherSymb]   -- ^ symbols not from a haskell id that this unit depends on
  , luRequired     :: Bool          -- ^ always link this unit
  , luForeignRefs  :: [ForeignRef]
  } deriving (Eq, {- Ord, -} Show)

-- | Generate the ingredients for the linkable units for this module
genUnits :: HasDebugCallStack
         => DynFlags
         -> Module
         -> [StgTopBinding]
         -> [SptEntry]
         -> ForeignStubs
         -> G (Object.SymbolTable, [LinkableUnit]) -- ^ the final symbol table and the linkable units
genUnits dflags m ss spt_entries foreign_stubs
                                 = generateGlobalBlock =<<
                                   generateExportsBlock =<<
                                   go 2 Object.emptySymbolTable ss
    where
      go :: HasDebugCallStack
         => Int                 -- ^ the block we're generating (block 0 is the global unit for the module)
         -> Object.SymbolTable  -- ^ the shared symbol table
         -> [StgTopBinding]
         -> G (Object.SymbolTable, [LinkableUnit])
      go n st (x:xs) = do
        (st', mlu) <- generateBlock st x n
        (st'', lus)  <- go (n+1) st' xs
        return (st'', maybe lus (:lus) mlu)
      go _ st []     = return (st, [])

      -- | Generate the global unit that all other blocks in the module depend on
      --   used for cost centres and static initializers
      --   the global unit has no dependencies, exports the moduleGlobalSymbol
      generateGlobalBlock :: HasDebugCallStack
                          => (Object.SymbolTable, [LinkableUnit])
                          -> G (Object.SymbolTable, [LinkableUnit])
      generateGlobalBlock (st, lus) = do
        glbl <- use gsGlobal
        staticInit <-
          initStaticPtrs spt_entries
        (st', _, bs) <- serializeLinkableUnit m st [] [] []
                         ( O.optimize
                         . jsSaturate (Just $ modulePrefix m 1)
                         $ mconcat (reverse glbl) <> staticInit) "" [] []
        return ( st'
               , LinkableUnit bs
                              []
                              [moduleGlobalSymbol dflags m]
                              []
                              []
                              []
                              False
                              []
                 : lus
               )

      generateExportsBlock :: HasDebugCallStack
                          => (Object.SymbolTable, [LinkableUnit])
                          -> G (Object.SymbolTable, [LinkableUnit])
      generateExportsBlock (st, lus) = do
        glbl <- use gsGlobal
        let (f_hdr, f_c) = case foreign_stubs of
                                  NoStubs            -> (Outputable.empty, Outputable.empty)
                                  ForeignStubs hdr c -> (hdr, c)
            unique_deps = map mkUniqueDep (lines $ showSDoc dflags f_hdr)
            mkUniqueDep (tag:xs) = mkUnique tag (read xs)

        (st', _, bs) <- serializeLinkableUnit m
                                              st
                                              []
                                              []
                                              []
                                              mempty
                                              (T.pack $ showSDoc dflags f_c)
                                              []
                                              []
        return ( st'
               , LinkableUnit bs
                              []
                              [moduleExportsSymbol dflags m]
                              [] -- id deps
                              unique_deps -- pseudo id deps
                              []
                              True
                              []
                 : lus
               )

      -- | Generate the linkable unit for one binding or group of
      --   mutually recursive bindings
      generateBlock :: HasDebugCallStack
                    => Object.SymbolTable
                    -> StgTopBinding
                    -> Int
                    -> G (Object.SymbolTable, Maybe LinkableUnit)
      generateBlock st (StgTopStringLit bnd str) n = do
        bids <- genIdsI bnd
        case bids of
          [(TxtI b1t),(TxtI b2t)] -> do
            -- [e1,e2] <- genLit (MachStr str)
            emitStatic b1t (StaticUnboxed (StaticUnboxedString str)) Nothing
            emitStatic b2t (StaticUnboxed (StaticUnboxedStringOffset str)) Nothing
            _extraTl   <- use (gsGroup . ggsToplevelStats)
            si        <- use (gsGroup . ggsStatic)
            let stat = mempty -- mconcat (reverse extraTl) <> b1 ||= e1 <> b2 ||= e2
            (st', _ss, bs) <- serializeLinkableUnit m st [bnd] [] si
                              (jsSaturate (Just $ modulePrefix m n) stat) "" [] []
            pure (st', Just $ LinkableUnit bs [bnd] [] [] [] [] False [])
          _ -> panic "generateBlock: invalid size"
      generateBlock st (StgTopLifted decl) n =
        trace' ("generateBlock:\n" ++ showIndent decl) $
       do
        tl        <- genToplevel decl
        extraTl   <- use (gsGroup . ggsToplevelStats)
        ci        <- use (gsGroup . ggsClosureInfo)
        si        <- use (gsGroup . ggsStatic)
        unf       <- use gsUnfloated
        extraDeps <- use (gsGroup . ggsExtraDeps)
        fRefs     <- use (gsGroup . ggsForeignRefs)
        resetGroup
        let allDeps  = collectIds unf decl
            topDeps  = collectTopIds decl
            required = hasExport decl
            stat     = {-decl -} O.optimize
                     . jsSaturate (Just $ modulePrefix m n)
                     $ mconcat (reverse extraTl) <> tl
        (st', _ss, bs) <- serializeLinkableUnit m st topDeps ci si stat mempty [] fRefs
        return $! seqList topDeps `seq` seqList allDeps `seq` st' `seq`
          (st', Just $ LinkableUnit bs topDeps [] allDeps [] (S.toList extraDeps) required fRefs)

initStaticPtrs :: [SptEntry] -> C
initStaticPtrs ptrs = mconcat <$> mapM initStatic ptrs
  where
    initStatic (SptEntry sp_id (Fingerprint w1 w2)) = do
      i <- jsId sp_id
      fpa <- concat <$> mapM (genLit . mkLitWord64 . fromIntegral) [w1,w2]
      let sptInsert = app "h$hs_spt_insert" (fpa ++ [i])
      -- fixme can precedence be so that parens aren't needed?
      return $ (var "h$initStatic" .^ "push") |$ [jLam sptInsert]

hasExport :: StgBinding -> Bool
hasExport bnd =
  case bnd of
    StgNonRec b e -> isExportedBind b e
    StgRec bs     -> any (uncurry isExportedBind) bs
  where
    isExportedBind _i (StgRhsCon _cc con _) =
      getUnique con == staticPtrDataConKey
    isExportedBind _ _ = False

{- |
   serialize the payload of a linkable unit in the object file, adding
   strings to the SymbolTable where necessary
-}
serializeLinkableUnit :: HasDebugCallStack
                      => Module
                      -> Object.SymbolTable  -- symbol table to start with
                      -> [Id]                -- id's exported by unit
                      -> [ClosureInfo]
                      -> [StaticInfo]
                      -> JStat               -- generated code for the unit
                      -> Text
                      -> [Object.ExpFun]
                      -> [Object.ForeignRef]
                      -> G (Object.SymbolTable, [Text], BL.ByteString)
serializeLinkableUnit _m st i ci si stat rawStat fe fi = do
  i' <- mapM idStr i
  let (st', o) = Object.serializeStat st ci si stat rawStat fe fi
  rnf i' `seq` rnf o `seq` return (st', i', o)
    where
      idStr i = itxt <$> jsIdI i

collectTopIds :: StgBinding -> [Id]
collectTopIds (StgNonRec b _) = [b]
collectTopIds (StgRec bs) = let xs = map (zapFragileIdInfo . fst) bs
                            in  seqList xs `seq` xs

collectIds :: UniqFM StgExpr -> StgBinding -> [Id]
collectIds unfloated b =
  let xs = map zapFragileIdInfo .
           filter acceptId $ S.toList (bindingRefs unfloated b)
  in  seqList xs `seq` xs
  where
    acceptId i = all ($ i) [not . isForbidden] -- fixme test this: [isExported[isGlobalId, not.isForbidden]
    -- the GHC.Prim module has no js source file
    isForbidden i
      | Just m <- nameModule_maybe (getName i) =
                    moduleNameText m   == T.pack "GHC.Prim" &&
                    modulePackageKey m == primPackageKey
      | otherwise = False

{- |
     generate the object's dependy data, taking care that package and module names
     are only stored once
 -}
genDependencyData :: HasDebugCallStack
                  => DynFlags
                  -> Module
                  -> [LinkableUnit]
                  -> G Object.Deps
genDependencyData _dflags mod units = do
    -- [(blockindex, blockdeps, required, exported)]
    ds <- evalStateT (mapM (uncurry oneDep) blocks)
                     (DDC IM.empty IM.empty M.empty)
    return $ Object.Deps (Linker.mkPackage $
                           toInstalledUnitId (moduleUnitId mod))
                         (moduleNameText mod)
                         (IS.fromList [ n | (n, _, True, _) <- ds ])
                         (M.fromList $ (\(n,_,_,es) -> map (,n) es) =<< ds)
                         (listArray (0, length blocks-1) (ds ^.. traverse . _2))
  where
      -- Id -> Block
      unitIdExports :: UniqFM Int
      unitIdExports = listToUFM $
                      concatMap (\(u,n) -> map (,n) (luIdExports u)) blocks

      -- OtherSymb -> Block
      unitOtherExports :: Map OtherSymb Int
      unitOtherExports = M.fromList $
                         concatMap (\(u,n) -> map (,n)
                                                  (map (OtherSymb mod)
                                                       (luOtherExports u)))
                                   blocks

      blocks :: [(LinkableUnit, Int)]
      blocks = zip units [0..]

      -- generate the list of exports and set of dependencies for one unit
      oneDep :: LinkableUnit
             -> Int
             -> StateT DependencyDataCache G (Int, Object.BlockDeps, Bool, [Object.Fun])
      oneDep (LinkableUnit _ idExports otherExports idDeps pseudoIdDeps otherDeps req _frefs) n = do
        (edi, bdi) <- partitionEithers <$> mapM (lookupIdFun n) idDeps
        (edo, bdo) <- partitionEithers <$> mapM lookupOtherFun otherDeps
        (edp, bdp) <- partitionEithers <$> mapM (lookupPseudoIdFun n) pseudoIdDeps
        expi <- mapM lookupExportedId (filter isExportedId idExports)
        expo <- mapM lookupExportedOther otherExports
        -- fixme thin deps, remove all transitive dependencies!
        let bdeps = Object.BlockDeps
                      (IS.toList . IS.fromList . filter (/=n) $ bdi++bdo++bdp)
                      (S.toList . S.fromList $ edi++edo++edp)
        return (n, bdeps, req, expi++expo)

      idModule :: Id -> Maybe Module
      idModule i = nameModule_maybe (getName i) >>= \m ->
                   guard (m /= mod) >> return m

      lookupPseudoIdFun :: Int -> Unique
                        -> StateT DependencyDataCache G (Either Object.Fun Int)
      lookupPseudoIdFun n u =
        case lookupUFM_Directly unitIdExports u of
          Just k -> return (Right k)
          _      -> panic "lookupPseudoIdFun"

      -- get the function for an Id from the cache, add it if necessary
      -- result: Left Object.Fun   if function refers to another module
      --         Right blockNumber if function refers to current module
      --
      --         assumes function is internal to the current block if it's
      --         from teh current module and not in the unitIdExports map.
      lookupIdFun :: Int -> Id
                  -> StateT DependencyDataCache G (Either Object.Fun Int)
      lookupIdFun n i = case lookupUFM unitIdExports i of
        Just k  -> return (Right k)
        Nothing -> case idModule i of
          Nothing -> return (Right n)
          Just m ->
            let k = getKey . getUnique $ i
                addEntry :: StateT DependencyDataCache G Object.Fun
                addEntry = do
                  (TxtI idTxt) <- lift (jsIdI i)
                  lookupExternalFun (Just k) (OtherSymb m idTxt)
            in  if m == mod
                   then panic ("local id not found: " ++ show m)
                    else Left <$> (maybe addEntry return =<<
                                   use (ddcId . to (IM.lookup k)))

      -- get the function for an OtherSymb from the cache, add it if necessary
      lookupOtherFun :: OtherSymb
                     -> StateT DependencyDataCache G (Either Object.Fun Int)
      lookupOtherFun od@(OtherSymb m idTxt) =
        case M.lookup od unitOtherExports of
          Just n  -> return (Right n)
          Nothing | m == mod -> panic ("genDependencyData.lookupOtherFun: unknown local other id: " ++ T.unpack idTxt)
          Nothing ->  Left <$> (maybe (lookupExternalFun Nothing od) return =<<
                        use (ddcOther . to (M.lookup od)))

      lookupExportedId :: Id -> StateT DependencyDataCache G Object.Fun
      lookupExportedId i = do
        (TxtI idTxt) <- lift (jsIdI i)
        lookupExternalFun (Just . getKey . getUnique $ i) (OtherSymb mod idTxt)

      lookupExportedOther :: Text -> StateT DependencyDataCache G Object.Fun
      lookupExportedOther = lookupExternalFun Nothing . OtherSymb mod

      -- lookup a dependency to another module, add to the id cache if there's
      -- an id key, otherwise add to other cache
      lookupExternalFun :: Maybe Int
                        -> OtherSymb -> StateT DependencyDataCache G Object.Fun
      lookupExternalFun mbIdKey od@(OtherSymb m idTxt) = do
        let mk        = getKey . getUnique $ m
            mpk       = Linker.mkPackage (toInstalledUnitId (moduleUnitId m))
            inCache p = Object.Fun p (moduleNameText m) idTxt
            addCache  = do
              let cache' = IM.insert mk mpk
              ddcModule %= cache'
              cache' `seq` return (Object.Fun mpk (moduleNameText m) idTxt)
        f <- maybe addCache (return . inCache) =<<
                   use (ddcModule . to (IM.lookup mk))
        maybe (ddcOther %= M.insert od f) (\k -> ddcId %= IM.insert k f) mbIdKey
        return f

moduleNameText :: Module -> Text
moduleNameText m
  | xs == ":Main" = T.pack "Main"
  | otherwise     = T.pack xs
    where xs      = moduleNameString . moduleName $ m

genToplevel :: StgBinding -> C
genToplevel (StgNonRec bndr rhs) = genToplevelDecl bndr rhs
genToplevel (StgRec bs)          =
  mconcat $ map (\(bndr, rhs) -> genToplevelDecl bndr rhs) bs

-- entry function of the worker
enterDataCon :: DataCon -> G JExpr
enterDataCon d = jsDcEntryId (dataConWorkId d)

enterDataConI :: DataCon -> G Ident
enterDataConI d = jsDcEntryIdI (dataConWorkId d)

genToplevelDecl :: Id -> StgRhs -> C
genToplevelDecl i rhs = do
  s1 <- resetSlots (genToplevelConEntry i rhs)
  s2 <- resetSlots (genToplevelRhs i rhs)
  return (s1 <> s2)

genToplevelConEntry :: Id -> StgRhs -> C
genToplevelConEntry i rhs@(StgRhsCon _cc con _args)
    | i `elem` [ i' | AnId i' <- dataConImplicitTyThings con ]
    = genSetConInfo i con (stgRhsLive rhs) -- NoSRT
genToplevelConEntry i rhs@(StgRhsClosure _ _cc _upd_flag
                    _args (removeTick -> StgConApp dc _cargs _))
    | i `elem` [ i' | AnId i' <- dataConImplicitTyThings dc ]
    = genSetConInfo i dc (stgRhsLive rhs) -- srt
genToplevelConEntry _ _ = mempty

removeTick :: StgExpr -> StgExpr
removeTick (StgTick _ e) = e
removeTick e             = e

genStaticRefsRhs :: StgRhs -> G CIStatic
genStaticRefsRhs lv = genStaticRefs (stgRhsLive lv)

-- fixme, update to new way to compute static refs dynamically
genStaticRefs :: LiveVars -> G CIStatic
genStaticRefs lv
  | isEmptyDVarSet sv = return noStatic
  | otherwise         = do
      unfloated <- use gsUnfloated
      let xs = filter (\x -> not (elemUFM x unfloated ||
                                  isLiftedType_maybe (idType x) == Just False))
                      (dVarSetElems sv)
      CIStaticRefs . catMaybes <$> mapM getStaticRef xs
  where
    sv = liveStatic lv

getStaticRef :: Id -> G (Maybe Text)
getStaticRef = fmap (fmap itxt . listToMaybe) . genIdsI

genToplevelRhs :: Id
               -> StgRhs
               -> C
-- general cases:
genToplevelRhs i (StgRhsCon cc con args) = do
  ii <- jsIdI i
  allocConStatic ii cc con args
  return mempty
genToplevelRhs i rhs@(StgRhsClosure _ext cc _upd_flag {- srt -} args body) = do
  eid@(TxtI eidt) <- jsEnIdI i
  (TxtI idt)   <- jsIdI i
  body <- genBody (ExprCtx i [] emptyUniqSet emptyUniqSet emptyUFM [] Nothing) i R2 args body
  (lidents, lids) <- unzip <$> liftToGlobal (jsSaturate (Just . T.pack $ "ghcjs_tmp_sat_") body)
  let lidents' = map (\(TxtI t) -> t) lidents
  CIStaticRefs sr0 <- genStaticRefsRhs rhs
  let sri = filter (`notElem` lidents') sr0
      sr   = CIStaticRefs sri
  et <- genEntryType args
  ll <- loadLiveFun lids
  (static, regs, upd) <-
        if et == CIThunk
          then (StaticThunk (Just (eidt, map StaticObjArg lidents')), CIRegs 0 [PtrV],) <$> updateThunk
          else return (StaticFun eidt (map StaticObjArg lidents'),
                      (if null lidents then CIRegs 1 (concatMap idVt args)
                                       else CIRegs 0 (PtrV : concatMap idVt args))
                        , mempty)
  setcc <- ifProfiling $
             if et == CIThunk
               then enterCostCentreThunk
               else enterCostCentreFun cc
  emitClosureInfo (ClosureInfo eidt
                               regs
                               idt
                               (fixedLayout $ map (uTypeVt . idType) lids)
                               et
                               sr)
  ccId <- costCentreStackLbl cc
  emitStatic idt static ccId
  return $ (eid ||= e (JFunc [] (ll # upd # setcc # body)))

liftToGlobal :: JStat -> G [(Ident, Id)]
liftToGlobal jst = do
  GlobalIdCache gidc <- use globalIdCache
  let sids  = filter (`M.member` gidc) (jst ^.. Compactor.identsS)
      cnt   = M.fromListWith (+) (map (,(1::Integer)) sids)
      sids' = sortBy (compare `on` (cnt M.!)) (nub' sids)
  pure $ map (\s -> (s, snd $ gidc M.! s)) sids'

nub' :: (Ord a, Eq a) => [a] -> [a]
nub' xs = go S.empty xs
  where
    go _ []     = []
    go s (x:xs) | S.member x s = go s xs
                | otherwise    = x : go (S.insert x s) xs
--       ids  = filter M.member gidc
{-
  algorithm:
   - collect all Id refs that are in the cache, count usage
   - order by increasing use
   - prepend loading lives var to body: body can stay the same
-}

{-
   todo for stack frames:
    - change calling convention?
    - return stack[sp] -> return stack[sp].f ?
       -> no we miss the continuation object then
       -> set h$rS
       -> return h$rs(); instead
 -}

loadLiveFun :: [Id] -> C
loadLiveFun l = do
   l' <- concat <$> mapM genIdsI l
   case l' of
     []  -> return mempty
     [v] -> return (v ||= r1 .^ "d1")
     [v1,v2] -> return (v1 ||= r1 .^ "d1"  #
                        v2 ||= r1 .^ "d2")
     (v:vs)  -> do
       d <- makeIdent
       let l'' = mconcat . zipWith (loadLiveVar $ toJExpr d) [(1::Int)..] $ vs
       return (v ||= r1 .^ "d1" #
               d ||= r1 .^ "d2" #
               l'')
  where
        loadLiveVar d n v = let ident = dataFields ! n
                            in  decl v # e v |= SelExpr d ident

dataFields :: Array Int Ident
dataFields = listArray (1,16384) (map (TxtI . T.pack . ('d':) . show) [(1::Int)..16384])

genBody :: HasDebugCallStack => ExprCtx -> Id -> StgReg -> [Id] -> StgExpr -> C
genBody ctx i startReg args e =
  trace' ("genBody: " ++ show args)
  (genBody0 ctx i startReg args e)

genBody0 :: HasDebugCallStack
         => ExprCtx
         -> Id
         -> StgReg
         -> [Id]
         -> StgExpr
         -> C
genBody0 ctx i startReg args e = do
  la <- loadArgs startReg args
  lav <- verifyRuntimeReps args
  let ids :: [(PrimRep, [JExpr])]
      ids = -- take (resultSize args $ idType i) (map toJExpr $ enumFrom R1)
            reverse . fst $
            foldl' (\(rs, vs) (rep, size) ->
                       let (vs0, vs1) = splitAt size vs
                       in  ((rep, vs0):rs,vs1))
                   ([], map toJExpr $ enumFrom R1)
                   (resultSize args $ idType i)
  (e, _r) <-  trace' ("genBody0 ids:\n" ++ show ids) (genExpr (ctx & ctxTarget .~ ids) e)
  return $ la <> lav <> e <> returnStack -- [j| return `Stack`[`Sp`]; |]

-- find the result type after applying the function to the arguments
resultSize :: HasDebugCallStack => [Id] -> Type -> [(PrimRep, Int)]
resultSize xs t = trace' ("resultSize\n" ++ show xs ++ "\n" ++ show t)
                    (let r = resultSize0 xs t
                     in trace' ("resultSize -> " ++ show r) r
                    )

resultSize0 :: HasDebugCallStack
            => [Id]
            -> Type
            -> [(PrimRep, Int)] -- Int
resultSize0 xxs@(_:xs) t
  -- - | isUnboxedTupleType
  -- - | t' <- piResultTys t (map idType xxs) = resultSize0 [] t'
  -- - | MultiRep _ <- {- trace' "resultSize0 ubx" -} (repType (idType x)) = panic "genBody: unboxed tuple argument"
  -- - | otherwise = {- trace' "resultSize0 not" $ -}
  | t' <- unwrapType t
  , Just (fa, fr) <- splitFunTy_maybe t' -- isFunTy t' =
  , Just (tc, ys) <- splitTyConApp_maybe fa
  , isUnboxedTupleTyCon tc =
      resultSize0 xxs (mkVisFunTys (dropRuntimeRepArgs ys) fr)
  | t' <- unwrapType t
  , Just (_fa, fr) <- splitFunTy_maybe t' = -- isFunTy t' =
      resultSize0 xs fr
        -- let (fa, fr) = splitFunTy t'
        -- let    t''     = mkFunTys (map primRepToType . typePrimRep $ unwrapType fa) fr
        -- in  resultSize0 xs (maybe fr snd . splitFunTy_maybe $ t'')
  | otherwise = [(LiftedRep, 1)] -- possibly newtype family, must be boxed
--      case typePrimRep (unwrapType t) of -- repType t of
       -- (UnaryRep t' | isFunTy t' ->
      --                   let (fa,fr) = splitFunTy t'
        --                     t''     = mkFunTys (map slotTyToType . repTypeSlots $ repType fa) fr
--                         in  {- trace' ("resultSize0 fun: " ++ show (fa, fr)) $ -}
  --                           resultSize0 xs (snd . splitFunTy $ t'')
--       _                          -> 1 -- possibly newtype family, must be boxed
resultSize0 [] t
  | isRuntimeRepKindedTy t' = []
  | isRuntimeRepTy t' = []
  | Nothing <- isLiftedType_maybe t' = [(LiftedRep, 1)]
  | otherwise = typeTarget t
  where
    t' = unwrapType t
       -- map (\t -> (t, varSize (primRepVt t))) $ typePrimRep (unwrapType t)
    {- trace' "resultSize0 eol" $ -}
  -- case repType t of
    -- UnaryRep t'     -> {- trace' ("resultSize0 eol2: " ++ show t') $ -} typeSize t'
    -- MultiRep tys -> {- trace' ("resultSize0 eol3: " ++ show tys) $ -} sum (map (typeSize . slotTyToType) tys)

loadArgs :: HasDebugCallStack => StgReg -> [Id] -> C
loadArgs start args = do
  args' <- concatMapM genIdArgI args
  return (mconcat $ zipWith (\a r -> a ||= e r) args' [start..])

data ExprResult = ExprCont
                | ExprInline (Maybe [JExpr])
  deriving (Eq, Ord, Show)

data ExprValData = ExprValData [JExpr]
  deriving (Eq, Ord, Show)

-- not a Monoid
branchResult :: HasDebugCallStack => [ExprResult] -> ExprResult
branchResult []           = panic "branchResult: empty list"
branchResult [e]          = e
branchResult (ExprCont:_) = ExprCont
branchResult (_:es)
  | elem ExprCont es      = ExprCont
  | otherwise             = ExprInline Nothing

genExpr :: HasDebugCallStack => ExprCtx -> StgExpr -> G (JStat, ExprResult)
genExpr top e = trace' ("genExpr\n" ++ showIndent e)
                       (genExpr0 top e)

genExpr0 :: HasDebugCallStack
         => ExprCtx
         -> StgExpr
         -> G (JStat, ExprResult)
genExpr0 top (StgApp f args) = genApp top f args
genExpr0 top (StgLit l) =
  -- fixme check primRep here?
  (,ExprInline Nothing) .
  assignAllCh ("genExpr StgLit " ++ show (top ^. ctxTarget))
                                         (concatMap snd $ top ^. ctxTarget)
                         <$> genLit l
genExpr0 top (StgConApp con args _) = do
  as <- concatMapM genArg args
  c <- genCon top con as
  return (c, ExprInline (Just as))
genExpr0 top (StgOpApp (StgFCallOp f _) args t) =
   genForeignCall top f t (concatMap snd $ top ^. ctxTarget) args
genExpr0 top (StgOpApp (StgPrimOp op) args t)    = genPrimOp top op args t
genExpr0 top (StgOpApp (StgPrimCallOp c) args t) = genPrimCall top c args t
genExpr0 _   StgLam{} = panic "genExpr: StgLam"
genExpr0 top stg@(StgCase e b at alts) =
  genCase top b e at alts (liveVars $ stgExprLive False stg)
genExpr0 top (StgLet _ b e) = do
  (b',top') <- genBind top b
  (s,r)     <- genExpr top' e
  return (b' <> s, r)
genExpr0 top (StgLetNoEscape _ b e) = do
  (b', top') <- genBindLne top b
  (s, r)     <- genExpr top' e
  return (b' <> s, r)
genExpr0 top (StgTick (ProfNote cc count scope) e) = do
  setSCCstats <- ifProfilingM $ setCC cc count scope
  (stats, result) <- genExpr top e
  return (setSCCstats <> stats, result)
genExpr0 top (StgTick (SourceNote span _sname) e) =
  genExpr (top & ctxSrcSpan ?~ span) e
genExpr0 top (StgTick _m e) = genExpr top e

might_be_a_function :: HasDebugCallStack => Type -> Bool
-- Return False only if we are *sure* it's a data type
-- Look through newtypes etc as much as poss
might_be_a_function ty
  | [LiftedRep] <- typePrimRep ty
  , Just tc <- tyConAppTyCon_maybe (unwrapType ty)
  , isDataTyCon tc
  = False
  | otherwise
  = True

matchVarName :: String -> FastString -> FastString -> Id -> Bool
matchVarName pkg modu occ (idName -> n)
  | Just m <- nameModule_maybe n =
    occ  == occNameFS (nameOccName n) &&
    modu == moduleNameFS (moduleName m) &&
    pkg `L.isPrefixOf` (unitIdString (moduleUnitId m))
  | otherwise = False

genApp :: HasDebugCallStack
       => ExprCtx
       -> Id
       -> [StgArg]
       -> G (JStat, ExprResult)
-- special cases for JSString literals
 -- we could handle unpackNBytes# here, but that's probably not common
 -- enough to warrant a special case
genApp ctx i [StgVarArg v]
   | [top] <- concatMap snd (ctx ^. ctxTarget)
   -- , Just (Lit (MachStr bs)) <- expandUnfolding_maybe (idUnfolding v)
   -- , Just t <- decodeModifiedUTF8 bs -- unpackFS fs -- Just t <- decodeModifiedUTF8 bs
   , matchVarName "ghcjs-prim" "GHCJS.Prim" "unsafeUnpackJSStringUtf8##" i =
      (,ExprInline Nothing) . (|=) top . app "h$decodeUtf8z" <$> genIds v
genApp ctx i [StgLitArg (LitString bs), x]
    | [top] <- concatMap snd (ctx ^. ctxTarget), getUnique i == unpackCStringAppendIdKey, Just d <- decodeModifiedUTF8 bs = do
        -- fixme breaks assumption in codegen if bs doesn't decode
        prof <- csProf <$> use gsSettings
        let profArg = if prof then [jCafCCS] else []
        a <- genArg x
        return (top |= app "h$appendToHsStringA" ([toJExpr d, toJExpr a] ++ profArg)
               ,ExprInline Nothing)
genApp top i a
    | Just n <- top ^. ctxLneFrameBs . to (flip lookupUFM i) = do -- let-no-escape
        as'      <- concatMapM genArg a
        ei       <- jsEntryId i
        let ra = mconcat . reverse $
                   zipWith (\r a -> e r |= a{-[j| `r` = `a`; |]-}) [R1 ..] as'
        p <- pushLneFrame n top
        a <- adjSp 1 -- for the header (which will only be written when the thread is suspended)
        return (ra <> p <> a <> returnS ei, ExprCont)
    | n == 0 && (isUnboxedTupleType (idType i) || isStrictType (idType i)) = do
                a <- assignAllCh1 "genApp" (top ^. ctxTarget) .
                                          (alignTarget (idTarget i)) <$> genIds i
                return (a, ExprInline Nothing)
    | [vt] <- idVt i, isUnboxable vt && n == 0 && i `elementOfUniqSet` (top ^. ctxEval) = do
                let [c] =  concatMap snd $ top ^. ctxTarget
                is <- genIds i
                case is of
                  [i'] ->
                    return ({-[j| `c` = (typeof `i'` === 'object') ? `i'`.d1 : `i'`; |]-}
                            c |= if_ (typeof i' .===. "object") (i' .^ "d1") i'
                           ,ExprInline Nothing)
                  _ -> panic "genApp: invalid size"
    | n == 0 && (i `elementOfUniqSet` (top ^. ctxEval) || isStrictId i) = do
                a <- assignAllCh1 ("genApp:" ++ show i ++ " " ++ show (idFunRepArity i, idVt i))
                                 (top ^. ctxTarget) .
                                 (alignTarget (idTarget i))
                                 <$> genIds i
                settings <- use gsSettings
                let ww = case concatMap snd (top ^. ctxTarget) of
                           [t] | csAssertRts settings ->
                                   ifS' (typeof t .===. "object" .&&. isThunk t)
                                        (appS "throw" ["unexpected thunk"]) -- yuck
                           _   -> mempty
                return (a # ww, ExprInline Nothing)
    | DataConWrapId dc <- idDetails i, isNewTyCon (dataConTyCon dc) = do
                as <- concatMapM genArg a
                case as of
                  [ai] -> do
                    let [t] = concatMap snd (top ^. ctxTarget)
                        [StgVarArg a'] = a
                    if isStrictId a' || a' `elementOfUniqSet` (top ^. ctxEval)
                      then return (t |= ai, ExprInline Nothing)
                      else return (returnS (app "h$e" [ai]), ExprCont)
                  _ -> panic "genApp: invalid size"
    | idFunRepArity i == 0 && n == 0 && not (might_be_a_function (idType i)) = do
             ii <- enterId
             return (returnS (app "h$e" [ii]), ExprCont)
    | idFunRepArity i == n && not (isLocalId i) && isStrictId i && n /= 0 = do
        as' <- concatMapM genArg a
        jmp <- jumpToII i as' =<< r1
        return (jmp, ExprCont)
    | idFunRepArity i < n && isStrictId i && idFunRepArity i > 0 =
         let (reg,over) = splitAt (idFunRepArity i) a
         in  do
           reg' <- concatMapM genArg reg
           pc   <- pushCont over
           jmp  <- jumpToII i reg' =<< r1
           return (pc <> jmp, ExprCont)
    | otherwise = do
           jmp <- jumpToFast a =<< r1
           return (jmp, ExprCont)
  where
    enterId :: G JExpr
    enterId = genArg (StgVarArg i) >>=
                \case
                   [x] -> return x
                   xs  -> panic $ "genApp: unexpected multi-var argument (" ++ show (length xs) ++ ")\n" ++ showIndent i

    r1 :: C
    r1 = do
      ids <- genIds i
      return $ mconcat $ zipWith (\r u -> e r |= e u) (enumFrom R1) ids
    n = length a

pushCont :: HasDebugCallStack
         => [StgArg]
         -> C
pushCont as = do
  as' <- concatMapM genArg as
  (app, spec) <- selectApply False (as,as')
  if spec
    then push $ reverse $ app : as'
    else push $ reverse $ app : mkTag as' as : as'
  where
    mkTag rs ns = toJExpr ((length rs `Bits.shiftL` 8) Bits..|. length ns)

-- regular let binding: allocate heap object
genBind :: HasDebugCallStack
        => ExprCtx
        -> StgBinding
        -> G (JStat, ExprCtx)
genBind ctx bndr =
  case bndr of
    StgNonRec b r -> do
       j <- assign b r >>= \case
         Just ja -> return ja
         Nothing -> allocCls Nothing [(b,r)]
       return (j, addEvalRhs ctx [(b,r)])
    StgRec bs     -> do
       jas <- mapM (uncurry assign) bs -- fixme these might depend on parts initialized by allocCls
       let m = if null jas then Nothing else Just (mconcat $ catMaybes jas)
       j <- allocCls m . map snd . filter (isNothing . fst) $ zip jas bs
       return (j, addEvalRhs ctx bs)
   where
     ctx' = clearCtxStack ctx

     assign :: Id -> StgRhs -> G (Maybe JStat)
     assign b (StgRhsClosure _ _ccs {-[the_fv]-} _upd [] expr)
       | let strip = snd . stripStgTicksTop (not . tickishIsCode)
       , StgCase (StgApp scrutinee []) _ (AlgAlt _) [(DataAlt _, params, sel_expr)] <- strip expr
       , StgApp selectee [] <- strip sel_expr
       , let params_w_offsets = zip params (scanl' (+) 1 $ map (typeSize . idType) params)
       , let total_size = sum (map (typeSize . idType) params)
       -- , the_fv == scrutinee -- fixme check
       , Just the_offset <- assocMaybe params_w_offsets selectee
       , the_offset <= 16 -- fixme make this some configurable constant
       = do
           let the_fv = scrutinee -- error "the_fv" -- fixme
           let sel_tag | the_offset == 2 = if total_size == 2 then "2a"
                                                              else "2b"
                       | otherwise       = show the_offset
           tgts <- genIdsI b
           the_fvjs <- genIds the_fv
           case (tgts, the_fvjs) of
             ([tgt], [the_fvj]) -> return $ Just
               (tgt ||= app ("h$c_sel_" <> T.pack sel_tag) [the_fvj])
             _ -> panic "genBind.assign: invalid size"
     assign b (StgRhsClosure _ext _ccs _upd [] expr)
       | snd (isInlineExpr (ctx ^. ctxEval) expr) = do
           d   <- declIds b
           tgt <- genIds b
           (j, _) <- genExpr (ctx & ctxTarget .~ alignTarget (idTarget b) tgt) expr
           return (Just (d <> j))
     assign _b StgRhsCon{} = return Nothing
     assign  b r           = genEntry ctx' b r >> return Nothing

     addEvalRhs c [] = c
     addEvalRhs c ((b,r):xs)
       | StgRhsCon{} <- r                       = addEvalRhs (addEval b c) xs
       | (StgRhsClosure _ _ ReEntrant _ _) <- r = addEvalRhs (addEval b c) xs
       | otherwise                              = addEvalRhs c xs

genBindLne :: HasDebugCallStack
           => ExprCtx
           -> StgBinding
           -> G (JStat, ExprCtx)
genBindLne ctx bndr =
  trace' ("genBindLne\n" ++ showIndent bndr)
         (genBindLne0 ctx bndr)

genBindLne0 :: HasDebugCallStack
            => ExprCtx
            -> StgBinding
            -> G (JStat, ExprCtx)
genBindLne0 ctx bndr = do
  vis  <- map (\(x,y,_) -> (x,y)) <$>
            optimizeFree oldFrameSize (newLvs++map fst updBinds)
  declUpds <- mconcat <$> mapM (fmap (\x -> x ||= null_) . jsIdI . fst) updBinds
  let newFrameSize = oldFrameSize + length vis
      ctx' = ctx & ctxLne        %~ flip addListToUniqSet bound
                 & ctxLneFrameBs %~ flip addListToUFM (map (,newFrameSize) bound)
                 & ctxLneFrame   %~ (++vis)
  mapM_ (uncurry $ genEntryLne ctx') binds
  return (declUpds, ctx')
  where
    oldFrame     = ctx ^. ctxLneFrame
    oldFrameSize = length oldFrame
    isOldLv i    = i `elementOfUniqSet` (ctx ^. ctxLne) ||
                   i `elem` (map fst oldFrame)
    live         = liveVars $ mkDVarSet $ stgLneLive' bndr
    newLvs       = filter (not . isOldLv) (dVarSetElems live)
    binds = case bndr of
              StgNonRec b e -> [(b,e)]
              StgRec    bs  -> bs
    bound = map fst binds
    (updBinds, _nonUpdBinds) = partition (isUpdatableRhs . snd) binds

stgLneLive' :: StgBinding -> [Id]
stgLneLive' b = filter (`notElem` bindees b) (stgLneLive b)

stgLneLive :: StgBinding -> [Id]
stgLneLive (StgNonRec _b e) = stgLneLiveExpr e
stgLneLive (StgRec bs)      = L.nub $ concatMap (stgLneLiveExpr . snd) bs

stgLneLiveExpr :: StgRhs -> [Id]
stgLneLiveExpr rhs = dVarSetElems (liveVars $ stgRhsLive rhs)
-- stgLneLiveExpr (StgRhsClosure _ _ _ _ e) = dVarSetElems (liveVars (stgExprLive e))
-- stgLneLiveExpr StgRhsCon {}              = []

isUpdatableRhs :: StgRhs -> Bool
isUpdatableRhs (StgRhsClosure _ _ u _ _) = isUpdatable u
isUpdatableRhs _                         = False

{-
  Let-no-escape entries live on the stack. There is no heap object associated with them.

  A let-no-escape entry is called like a normal stack frame, although as an optimization,
  `Stack`[`Sp`] is not set when making the call. This done later if the thread needs to
  be suspended.

  Updatable let-no-escape binders have one 'private' slot in the stack frame. This slot
  is initially set to null, changed to h$blackhole when the thunk is being evaluated.
 -}

genEntryLne :: HasDebugCallStack => ExprCtx -> Id -> StgRhs -> G ()
genEntryLne ctx i rhs@(StgRhsClosure _ext _cc update args body) =
  resetSlots $ do
  let payloadSize = length frame
      frame       = ctx ^. ctxLneFrame
      myOffset    =
        maybe (panic "genEntryLne: updatable binder not found in let-no-escape frame")
              ((payloadSize-) . fst)
              (listToMaybe $ filter ((==i).fst.snd) (zip [0..] frame))
      bh | isUpdatable update =
             jVar (\x -> x |= app "h$bh_lne" [sp - e myOffset, e (payloadSize+1)] #
                         ifS x (returnS x) mempty)
         | otherwise = mempty
  lvs  <- popLneFrame True payloadSize ctx
  body <- genBody ctx i R1 args body
  ei@(TxtI eii)   <- jsEntryIdI i
  sr   <- genStaticRefsRhs rhs
  let f = JFunc [] (bh <> lvs <> body)
  emitClosureInfo $
    ClosureInfo eii
                (CIRegs 0 $ concatMap idVt args)
                (eii <> ", " <> T.pack (show i))
                (fixedLayout . reverse $
                    map (stackSlotType . fst) (ctx ^. ctxLneFrame))
                CIStackFrame
                sr
  emitToplevel (ei ||= e f)
genEntryLne ctx i (StgRhsCon cc con args) = resetSlots $ do
  let payloadSize = length (ctx ^. ctxLneFrame)
  ei@(TxtI _eii) <- jsEntryIdI i
  -- di <- enterDataCon con
  ii <- makeIdent
  p  <- popLneFrame True payloadSize ctx
  args' <- concatMapM genArg args
  ac    <- allocCon ii con cc args'
  emitToplevel (ei ||= e (JFunc []
    (decl ii <> p <> ac <> (r1 |= e ii) <> returnStack)))

-- generate the entry function for a local closure
genEntry :: HasDebugCallStack => ExprCtx -> Id -> StgRhs -> G ()
genEntry _ _i (StgRhsCon _cc _con _args) = return () -- mempty -- error "local data entry"

genEntry ctx i rhs@(StgRhsClosure _ext cc {-_bi live-} upd_flag args body) = resetSlots $ do
  let live = stgLneLiveExpr rhs -- error "fixme" -- probably find live vars in body
  ll    <- loadLiveFun live
  llv   <- verifyRuntimeReps live
  upd   <- genUpdFrame upd_flag i
  body  <- genBody entryCtx i R2 args body
  ei@(TxtI eii) <- jsEntryIdI i
  et    <- genEntryType args
  setcc <- ifProfiling $
             if et == CIThunk
               then enterCostCentreThunk
               else enterCostCentreFun cc
  sr <- genStaticRefsRhs rhs
  emitClosureInfo $ ClosureInfo eii
                                (CIRegs 0 $ PtrV : concatMap idVt args)
                                (eii <> ", " <> T.pack (show i))
                                (fixedLayout $ map (uTypeVt . idType) live)
                                et
                                sr
  emitToplevel (ei ||= e (JFunc [] (ll # llv # upd # setcc # body)))
  where
    entryCtx = ExprCtx i [] (ctx ^. ctxEval) (ctx ^. ctxLne) emptyUFM [] (ctx ^. ctxSrcSpan)

genEntryType :: HasDebugCallStack => [Id] -> G CIType
genEntryType []   = return CIThunk
genEntryType args0 = {- trace' "genEntryType" $ -} do
  args' <- mapM genIdArg args
  return $ CIFun (length args) (length $ concat args')
  where
    args = filter (not . isRuntimeRepKindedTy . idType) args0

genSetConInfo :: HasDebugCallStack => Id -> DataCon -> LiveVars -> C
genSetConInfo i d l {- srt -} = do
  ei@(TxtI eii) <- jsDcEntryIdI i
  sr <- genStaticRefs l
  emitClosureInfo $ ClosureInfo eii
                                (CIRegs 0 [PtrV])
                                (T.pack $ show d)
                                (fixedLayout $ map uTypeVt fields)
                                (CICon $ dataConTag d)
                                sr
  return (ei ||= mkDataEntry)
    where
      -- dataConRepArgTys sometimes returns unboxed tuples. is that a bug?
      fields = concatMap (map primRepToType . typePrimRep . unwrapType)
                         (dataConRepArgTys d)
        -- concatMap (map slotTyToType . repTypeSlots . repType) (dataConRepArgTys d)

mkDataEntry :: JExpr
mkDataEntry = ValExpr $ JFunc [] returnStack

genUpdFrame :: UpdateFlag -> Id -> C
genUpdFrame u i
  | isReEntrant u   = mempty
  | isOneShotBndr i = maybeBh
  | isUpdatable u   = updateThunk
  | otherwise       = maybeBh
  where
    isReEntrant ReEntrant = True
    isReEntrant _         = False
    maybeBh = do
      settings <- use gsSettings
      assertRtsStat (return $ bhSingleEntry settings)

-- allocate local closures
allocCls :: Maybe JStat -> [(Id, StgRhs)] -> C
allocCls dynMiddle xs = do
   (stat, dyn) <- partitionEithers <$> mapM toCl xs
   cs <- use gsSettings
   return (mconcat stat) <> allocDynAll cs True dynMiddle dyn
  where
    -- left = static, right = dynamic
    toCl :: (Id, StgRhs)
         -> G (Either JStat (Ident,JExpr,[JExpr],CostCentreStack))
    -- statics
    {- making zero-arg constructors static is problematic, see #646
       proper candidates for this optimization should have been floated
       already
      toCl (i, StgRhsCon cc con []) = do
      ii <- jsIdI i
      Left <$> (return (decl ii) <> allocCon ii con cc []) -}
    toCl (i, StgRhsCon cc con [a]) | isUnboxableCon con = do
      ii <- jsIdI i
      Left <$> (return (decl ii) <> (allocCon ii con cc =<< genArg a))

    -- dynamics
    toCl (i, StgRhsCon cc con ar) =
      -- fixme do we need to handle unboxed?
      Right <$> ((,,,) <$> jsIdI i
                       <*> enterDataCon con
                       <*> concatMapM genArg ar
                       <*> pure cc)
    toCl (i, cl@(StgRhsClosure _ext cc _upd_flag _args _body)) =
      let live = stgLneLiveExpr cl
      in  Right <$> ((,,,) <$> jsIdI i
                       <*> jsEntryId i
                       <*> concatMapM genIds live
                       <*> pure cc)


genCase :: HasDebugCallStack
        => ExprCtx
        -> Id
        -> StgExpr
        -> AltType
        -> [StgAlt]
        -> LiveVars
        -> G (JStat, ExprResult)
genCase top bnd e at alts l =
  trace' ("genCase\n" ++ showIndent e ++ "\n" ++ unlines (map showIndent alts))
 (genCase0 top bnd e at alts l)
-- fixme CgCase has a reps_compatible check here
genCase0 :: HasDebugCallStack
        => ExprCtx
        -> Id
        -> StgExpr
        -> AltType
        -> [StgAlt]
        -> LiveVars
        -> G (JStat, ExprResult)
genCase0 top bnd e at alts l
  | snd (isInlineExpr (top ^. ctxEval) e) = withNewIdent $ \ccsVar -> do
      bndi <- genIdsI bnd
      (ej, r) <- genExpr (top & ctxTop .~ bnd
                              & ctxTarget .~ alignTarget (idTarget bnd)
                                                         (map toJExpr bndi)) e
      -- ExprCtx bnd (map toJExpr bndi) (top ^. ctxEval) (top ^. ctxLneV) (top ^. ctxLneB) (top ^. ctxLne)) e
      let d = case r of
                ExprInline d0 -> d0
                ExprCont -> panic $ "genCase: expression was not inline:\n" ++
                                    showIndent e ++ "\n" ++
                                    (TL.unpack . (<>"\n") . displayT . renderPretty 0.8 150 . pretty . jsSaturate (Just "debug") $ ej)

          ww = mempty -- if snd (isInlineExpr emptyUniqSet e) then mempty else [j| h$log('danger will robinson'); |]
      (aj, ar) <- genAlts (addEval bnd top) bnd at d alts
      saveCCS <- ifProfiling (toJExpr ccsVar |= toJExpr jCurrentCCS)
      restoreCCS <- ifProfiling (toJExpr jCurrentCCS |= toJExpr ccsVar)
      return ( decl ccsVar <>
               mconcat (map decl bndi) <>
               saveCCS <>
               ww <>
               ej <>
               restoreCCS <>
               aj
             , ar
             )
  | otherwise = do
      rj       <- genRet (addEval bnd top) bnd at alts l
      (ej, _r) <- genExpr (top & ctxTop .~ bnd
                               & ctxTarget .~ alignTarget (idTarget bnd)
                                                          (map toJExpr [R1 ..])) e
      return (rj <> ej, ExprCont)

alignTarget :: [(PrimRep, Int)] -> [a] -> [(PrimRep, [a])]
alignTarget []     _  = []
alignTarget ((rep, size):xs) vs
  | length vs0 == size = (rep, vs0) : alignTarget xs vs1
  | otherwise          = panic "alignTarget: target size insufficient"
  where (vs0, vs1) = splitAt size vs

idTarget :: Id -> [(PrimRep, Int)]
idTarget = typeTarget . idType

typeTarget :: Type -> [(PrimRep, Int)]
typeTarget = map (\t -> (t, varSize (primRepVt t))) . typePrimRep . unwrapType

assignAll :: [JExpr] -> [JExpr] -> JStat
assignAll xs ys = mconcat (zipWith (|=) xs ys)

-- assign ys to xs, checking if the lengths are compatible
assignAllCh :: String -> [JExpr] -> [JExpr] -> JStat
assignAllCh msg xs ys
  | length xs == length ys = mconcat (zipWith (|=) xs ys)
  | otherwise =
     panic $ "assignAllCh: lengths do not match: " ++
             show (length xs, length ys) ++
             "\n    " ++
             msg

-- fixme what does this do?
assignAllCh1 :: String
             -> [(PrimRep, [JExpr])]
             -> [(PrimRep, [JExpr])]
             -> JStat
assignAllCh1 _msg ((rx,ex):_xs) ((ry,ey):_ys) =
  assignPrimReps rx ry ex ey
assignAllCh1 _   [] [] = mempty
assignAllCh1 _   _  _  =
  panic $ "assignAllCh1: lengths do not match"

-- assign p2 to p1
assignPrimReps :: PrimRep -> PrimRep -> [JExpr] -> [JExpr] -> JStat
assignPrimReps _p1 _p2 e1 e2
-- Allow same size assignment, even if rep is not the same
--  | p1 /= p2 && Debug.Trace.trace ("implicit conversion: " ++ show p2 ++ " -> " ++ show p1) False = undefined
  | length e1 == length e2 = mconcat (zipWith (|=) e1 e2)
-- Coercion between StablePtr# and Addr#
assignPrimReps AddrRep UnliftedRep [a_val, a_off] [sptr] =
  a_val |= var "h$stablePtrBuf" # a_off |= sptr
assignPrimReps UnliftedRep AddrRep [sptr] [_a_val, a_off] =
  sptr |= a_off
assignPrimReps p1 p2 e1 e2 =
  let sr r s = show r ++ " (size " ++ show (length s) ++ ")"
  in  panic $ "cannot assign " ++ sr p2 e2 ++ " to " ++ sr p1 e1

genRet :: HasDebugCallStack
       => ExprCtx
       -> Id
       -> AltType
       -> [StgAlt]
       -> LiveVars
       -> C
genRet ctx e at as l = -- withNewIdent f
  trace' ("genRet" ++ unlines (map showIndent as))
         (genRet0 ctx e at as l)

genRet0 :: HasDebugCallStack
       => ExprCtx
       -> Id
       -> AltType
       -> [StgAlt]
       -> LiveVars
       -> C
genRet0 ctx e at as l = withNewIdent f
  where
    allRefs :: [Id]
    allRefs =  S.toList . S.unions $ as ^.. traverse . _3 . to (exprRefs emptyUFM)
    lneLive :: Int
    lneLive    = maximum $ 0 : map (fromMaybe 0 . lookupUFM (ctx ^. ctxLneFrameBs)) allRefs
    ctx'       = adjustCtxStack lneLive ctx
    lneVars    = map fst $ take lneLive (ctx ^. ctxLneFrame)
    isLne i    = i `elem` lneVars || i `elementOfUniqSet` (ctx ^. ctxLne)
    nonLne     = filter (not . isLne) (dVarSetElems l)

    f :: Ident -> C
    f r@(TxtI ri)    =  do
      pushLne  <- pushLneFrame lneLive ctx
      saveCCS  <- ifProfilingM $ push [jCurrentCCS]
      free     <- trace' ("nonLne: " ++ show nonLne) (optimizeFree 0 nonLne)
      pushRet  <- pushRetArgs free (toJExpr r)
      fun'     <- fun free
      sr       <- genStaticRefs l -- srt
      prof     <- profiling
      emitClosureInfo $
        ClosureInfo ri
                    (CIRegs 0 altRegs)
                    ri
                    (fixedLayout . reverse $
                       map (stackSlotType . fst3) free
                       ++ if prof then [ObjV] else []
                       ++ map stackSlotType lneVars)
                    CIStackFrame
                    sr
      emitToplevel $ r ||= toJExpr (JFunc [] fun')
      return (pushLne <> saveCCS <> pushRet)
    fst3 ~(x,_,_)  = x

    altRegs :: HasDebugCallStack => [VarType]
    altRegs = case at of
      PrimAlt ptc    -> [primRepVt ptc]
      MultiValAlt _n -> idVt e
      _              -> [PtrV]

    fun free = resetSlots $ do
      decs          <- declIds e
      load          <- flip assignAll (map toJExpr [R1 ..]) . map toJExpr <$> genIdsI e
      loadv         <- verifyRuntimeReps [e]
      ras           <- loadRetArgs free
      rasv          <- verifyRuntimeReps (map (\(x,_,_)->x) free)
      restoreCCS    <- ifProfilingM $ popUnknown [jCurrentCCS]
      rlne          <- popLneFrame False lneLive ctx'
      rlnev         <- verifyRuntimeReps (map fst $ take lneLive (ctx' ^. ctxLneFrame))
      (alts, _altr) <- genAlts ctx' e at Nothing as
      return $ decs <> load <> loadv <> ras <> rasv <> restoreCCS <> rlne <> rlnev <> alts <>
               returnStack


-- 2-var values might have been moved around separately, use DoubleV as substitute
-- ObjV is 1 var, so this is no problem for implicit metadata
stackSlotType :: Id -> VarType
stackSlotType i
  | varSize otype == 1 = otype
  | otherwise          = DoubleV
  where otype = uTypeVt (idType i)

popLneFrame :: Bool -> Int -> ExprCtx -> C
popLneFrame inEntry size ctx
  | l < size  = panic $ "popLneFrame: let-no-escape frame too short: " ++
                        show l ++ " < " ++ show size
  | otherwise = popSkipI skip
                  =<< mapM (\(i,n) -> (,SlotId i n) <$> genIdsIN i n)
                           (take size $ ctx ^. ctxLneFrame)
  where
    skip = if inEntry then 1 else 0 -- pop the frame header
    l    = ctx ^. ctxLneFrame . to length

pushLneFrame :: HasDebugCallStack => Int -> ExprCtx -> C
pushLneFrame size ctx
  | l < size  = panic $ "pushLneFrame: let-no-escape frame too short " ++
                        show l ++ " < " ++ show size
  | otherwise = pushOptimized' (take size $ ctx ^. ctxLneFrame)
  where
    l = ctx ^. ctxLneFrame . to length

-- reorder the things we need to push to reuse existing stack values as much as possible
-- True if already on the stack at that location
optimizeFree :: HasDebugCallStack => Int -> [Id] -> G [(Id,Int,Bool)]
optimizeFree offset ids = do
  -- this line goes wrong                               vvvvvvv
  let -- ids' = concat $ map (\i -> map (i,) [1..varSize . uTypeVt . idType $ i]) ids
      idSize :: Id -> Int
      idSize i = let s = idSize0 i in trace' ("idSize: " ++ show i ++ " -> " ++ show s) s
      idSize0 :: Id -> Int
      idSize0 i = sum $ map varSize (typeVt . idType $ i)
      ids' = concatMap (\i -> map (i,) [1..idSize i]) ids
      -- 1..varSize] . uTypeVt . idType $ i]) (typeVt ids)
      l    = length ids'
  slots <- drop offset . take l . (++repeat SlotUnknown) <$> getSlots
  let slm                = M.fromList (zip slots [0..])
      (remaining, fixed) = partitionEithers $
         map (\inp@(i,n) -> maybe (Left inp) (\j -> Right (i,n,j,True))
            (M.lookup (SlotId i n) slm)) ids'
      takenSlots         = S.fromList (fixed ^.. traverse . _3)
      freeSlots          = filter (`S.notMember` takenSlots) [0..l-1]
      remaining'         = zipWith (\(i,n) j -> (i,n,j,False)) remaining freeSlots
      allSlots           = sortBy (compare `on` \(_,_,x,_) -> x) (fixed ++ remaining')
  return $ map (\(i,n,_,b) -> (i,n,b)) allSlots

(!!!) :: HasDebugCallStack => [a] -> Int -> a
xs !!! n = case (drop n xs) of
            x:_ -> x
            _   -> error "list too short"

pushRetArgs :: HasDebugCallStack => [(Id,Int,Bool)] -> JExpr -> C
pushRetArgs free fun =
  pushOptimized . (++[(fun,False)]) =<< mapM (\(i,n,b) -> (\es->(es!!(n-1),b)) <$> genIdArg i) free

loadRetArgs :: HasDebugCallStack => [(Id,Int,Bool)] -> C
loadRetArgs free = popSkipI 1 =<< ids
    where
       ids = mapM (\(i,n,_b) -> (!!!(n-1)) <$> genIdStackArgI i) free

genAlts :: HasDebugCallStack
        => ExprCtx        -- ^ lhs to assign expression result to
        -> Id             -- ^ id being matched
        -> AltType        -- ^ type
        -> Maybe [JExpr]  -- ^ if known, fields in datacon from earlier expression
        -> [StgAlt]       -- ^ the alternatives
        -> G (JStat, ExprResult)
genAlts top e at me as =
  trace''
  ("genAlts0\n" ++ unlines ([{- show top, -} show e, show at] ++ map show as)) $ do
    ver <- verifyMatchRep e at
    (st, er) <- genAlts0 top e at me as
    pure (ver <> st, er)
  --(\(_,s,r) -> (s,r)) <$> mkAlgBranch top e alt


genAlts0 :: HasDebugCallStack
         => ExprCtx        -- ^ lhs to assign expression result to
         -> Id             -- ^ id being matched
         -> AltType        -- ^ type
         -> Maybe [JExpr]  -- ^ if known, fields in datacon from earlier expression
         -> [StgAlt]       -- ^ the alternatives
         -> G (JStat, ExprResult)
genAlts0 top e PolyAlt _ [alt] = (\(_,s,r) -> (s,r)) <$> mkAlgBranch top e alt
genAlts0 _   _ PolyAlt _ _ = panic "genAlts: multiple polyalt"
genAlts0 top e (PrimAlt _tc) _ [(_, bs, expr)] = do
  ie       <- genIds e
  dids     <- mconcat (map declIds bs)
  bss      <- concatMapM genIds bs
  (ej, er) <- genExpr top expr
  return (dids <> assignAll bss ie <> ej, er)
genAlts0 top e (PrimAlt tc) _ alts = do
  ie <- genIds e
  (r, bss) <- normalizeBranches top <$>
     mapM (isolateSlots . mkPrimIfBranch top [primRepVt tc]) alts
  setSlots []
  return (mkSw ie bss, r)
genAlts0 top e (MultiValAlt n) _ [(_, bs, expr)] = do
  eids     <- genIds e
  l        <- loadUbxTup eids bs n
  (ej, er) <- genExpr top expr
  return (l <> ej, er)
genAlts0 _   _ (AlgAlt tc) _ [_alt] | isUnboxedTupleTyCon tc =
  panic "genAlts: unexpected unboxed tuple"
genAlts0 top _ (AlgAlt _tc) (Just es) [(DataAlt dc, bs, expr)]
  | not (isUnboxableCon dc) = do
      bsi <- mapM genIdsI bs
      let args = zipWith (||=) (concat bsi) es
      (ej, er) <- genExpr top expr
      return (mconcat args <> ej, er)
genAlts0 top e (AlgAlt _tc) _ [alt] = do
  (_,s,r) <- mkAlgBranch top e alt
  return (s, r)
genAlts0 top e (AlgAlt _tc) _ alts@[(DataAlt dc,_,_),_]
  | isBoolTy (dataConType dc) = do
      i <- jsId e
      nbs <- normalizeBranches top <$>
          mapM (isolateSlots . mkAlgBranch top e) alts
      case nbs of
        (r, [(_,s1,_), (_,s2,_)]) -> do
          let s = if   dataConTag dc == 2
                  then ifS i s1 s2
                  else ifS i s2 s1
          setSlots []
          return (s, r)
        _ -> error "genAlts: invalid branches for Bool"
-- fixme, add all alts
genAlts0 top e (AlgAlt _tc) _ alts = do
      ei <- jsId e
      (r, brs) <- normalizeBranches top <$>
          mapM (isolateSlots . mkAlgBranch top e) alts
      setSlots []
      return (mkSwitch (ei .^ "f" .^ "a") brs, r)
genAlts0 _  _ a _ l = do
  ap <- showPpr' a
  panic $ "genAlts: unhandled case variant: " ++
          ap ++
          " (" ++
          show (length l) ++
          ")"

-- if one branch ends in a continuation but another is inline,
-- we need to adjust the inline branch to use the continuation convention
normalizeBranches :: ExprCtx
                  -> [(a, JStat, ExprResult)]
                  -> (ExprResult, [(a, JStat, ExprResult)])
normalizeBranches e brs
    | all (==ExprCont) (brs ^.. traverse . _3) =
        (ExprCont, brs)
    | branchResult (brs ^.. traverse  ._3) == ExprCont =
        (ExprCont, map mkCont brs)
    | otherwise =
        (ExprInline Nothing, brs)
  where
    mkCont (me, s, ExprInline{}) = ( me
                                   , s <> assignAll (map toJExpr $ enumFrom R1)
                                                    (concatMap snd $ e ^. ctxTarget)
                                   , ExprCont)
    mkCont x                     = x

loadUbxTup :: [JExpr] -> [Id] -> Int -> C
loadUbxTup es bs _n = do
  bs' <- concatMapM genIdsI bs
  return $ mconcat $ zipWith (||=) bs' es

mkSw :: [JExpr] -> [(Maybe [JExpr], JStat, ExprResult)] -> JStat
mkSw [e] cases = mkSwitch e (over (mapped._1.mapped) head cases)
mkSw es cases  = mkIfElse es cases

-- switch for pattern matching on constructors or prims
mkSwitch :: JExpr -> [(Maybe JExpr, JStat, ExprResult)] -> JStat
mkSwitch e cases
    | [(Just c1,s1,_)] <- n, [(_,s2,_)] <- d =
        ifS (e .===. c1) s1 s2
    | [(Just c1,s1,_),(_,s2,_)] <- n, null d =
        ifS (e .===. c1) s1 s2
    | null d =
        SwitchStat e (map addBreak (init n)) (last n ^. _2)
    | [(_,d0,_)] <- d =
        SwitchStat e (map addBreak n) d0
    | otherwise = panic "mkSwitch: multiple default cases"
    where
      addBreak (Just c, s, _) = (c, s # BreakStat Nothing)
      addBreak _              = panic "mkSwitch: addBreak"
      (n,d) = partition (isJust . (^. _1)) cases

-- if/else for pattern matching on things that js cannot switch on
mkIfElse :: [JExpr] -> [(Maybe [JExpr], JStat, ExprResult)] -> JStat
mkIfElse e s = go (L.sortOn Down s)
    where
      go [] = panic "mkIfElse: empty expression list"
      go [(_, s, _)] = s -- only one 'nothing' allowed
      go ((Just e0, s, _):xs) =
          ifS (mkEq e e0) s (go xs)
      go _ = panic "mkIfElse: multiple DEFAULT cases"

mkEq :: [JExpr] -> [JExpr] -> JExpr
mkEq es1 es2
  | length es1 == length es2 = foldl1 (.&&.) (zipWith (.===.) es1 es2)
  | otherwise                = panic "mkEq: incompatible expressions"

mkAlgBranch :: ExprCtx  -- ^ toplevel id for the result
            -> Id      -- ^ datacon to match
            -> StgAlt  -- ^ match alternative with binders
            -> G (Maybe JExpr, JStat, ExprResult)
mkAlgBranch top d (DataAlt dc,[b],expr)
  | isUnboxableCon dc = do
      idd      <- jsId d
      fldx <- genIdsI b
      case fldx of
        [fld] -> do
          (ej, er) <- genExpr top expr
          return (Nothing, fld ||= idd # ej, er)
        _ -> panic "mkAlgBranch: invalid size"
mkAlgBranch top d (a, bs, expr) = do
  cc       <- caseCond a
  idd      <- jsId d
  b        <- loadParams idd bs
  (ej, er) <- genExpr top expr
  return (cc, b <> ej, er)

mkPrimIfBranch :: ExprCtx
               -> [VarType]
               -> StgAlt
               -> G (Maybe [JExpr], JStat, ExprResult)
mkPrimIfBranch top _vt (cond, _, e) =
  (\ic (ej,er) -> (ic,ej,er)) <$> ifCond cond <*> genExpr top e

-- fixme are bool things always checked correctly here?
ifCond :: AltCon -> G (Maybe [JExpr])
ifCond (DataAlt da) = return $ Just [e (dataConTag da)]
ifCond (LitAlt l)   = Just <$> genLit l
ifCond DEFAULT      = return Nothing

caseCond :: AltCon -> G (Maybe JExpr)
caseCond (DataAlt da) = return $ Just (e $ dataConTag da)
caseCond (LitAlt l)   = Just <$> genSingleLit l
caseCond DEFAULT      = return Nothing

-- load parameters from constructor
-- fixme use single tmp var for all branches
loadParams :: JExpr -> [Id] -> C
loadParams from args = do
  as <- concat <$> zipWithM (\a u -> map (,u) <$> genIdsI a) args use
  return $ case as of
    []                 -> mempty
    [(x,u)]            -> loadIfUsed (from .^ "d1") x  u
    [(x1,u1),(x2,u2)]  -> loadIfUsed (from .^ "d1") x1 u1 #
                          loadIfUsed (from .^ "d2") x2 u2
    ((x,u):xs)         -> loadIfUsed (from .^ "d1") x  u  #
                          jVar (\d -> d |= from .^ "d2" # loadConVarsIfUsed d xs)
  where
    use = repeat True -- fixme clean up
    loadIfUsed fr tgt True = -- decl' tgt fr
      decl tgt # e tgt |= fr
    loadIfUsed  _ _ _  = mempty

    loadConVarsIfUsed fr cs = mconcat $ zipWith f cs [(1::Int)..]
      where f (x,u) n = loadIfUsed (SelExpr fr (dataFields ! n)) x u


genPrimOp :: ExprCtx -> PrimOp -> [StgArg] -> Type -> G (JStat, ExprResult)
genPrimOp top op args t = do

  as <- concatMapM genArg args
  df <- use gsDynFlags
  -- fixme: should we preserve/check the primreps?
  return $ case genPrim df t op (map toJExpr . concatMap snd $ top ^. ctxTarget) as of
             PrimInline s -> (s, ExprInline Nothing)
             PRPrimCall s -> (s, ExprCont)

genArg :: HasDebugCallStack => StgArg -> G [JExpr]
genArg (StgLitArg l) = genLit l
genArg a@(StgVarArg i) = do
  unFloat <- use gsUnfloated
  case lookupUFM unFloat i of
    Nothing -> reg
    Just expr -> unfloated expr
   where
     -- if our argument is a joinid, it can be an unboxed tuple
     r :: HasDebugCallStack => VarType
     r = trace' ("r: " ++ showIndent a) r0
     r0 :: HasDebugCallStack => VarType
     r0 = uTypeVt . stgArgType $ a
     reg
       | isVoid r     = return []
       | i == trueDataConId  = return [true_]
       | i == falseDataConId = return [false_]
       | isMultiVar r = mapM (jsIdN i) [1..varSize r]
       | otherwise    = (:[]) <$> jsId i

     unfloated :: HasDebugCallStack => StgExpr -> G [JExpr]
     unfloated (StgLit l) = genLit l
     unfloated (StgConApp dc args _)
       | isBoolTy (dataConType dc) || isUnboxableCon dc =
           (:[]) . allocUnboxedCon dc . concat <$> mapM genArg args
       | null args = (:[]) <$> jsId (dataConWorkId dc)
       | otherwise = do
           as <- concat <$> mapM genArg args
           e  <- enterDataCon dc
           cs <- use gsSettings
           return [allocDynamicE cs e as Nothing] -- FIXME: ccs
     unfloated x = panic $ "genArg: unexpected unfloated expression: " ++
                           show x

genStaticArg :: HasDebugCallStack => StgArg -> G [StaticArg]
genStaticArg (StgLitArg l) = map StaticLitArg <$> genStaticLit l
genStaticArg a@(StgVarArg i) = do
  unFloat <- use gsUnfloated
  case lookupUFM unFloat i of
    Nothing -> reg
    Just expr -> unfloated expr
   where
     r = uTypeVt . stgArgType $ a
     reg
       | isVoid r            =
           return []
       | i == trueDataConId  =
           return [StaticLitArg (BoolLit True)]
       | i == falseDataConId =
           return [StaticLitArg (BoolLit False)]
       | isMultiVar r        =
           map (\(TxtI t) -> StaticObjArg t) <$> mapM (jsIdIN i) [1..varSize r] -- this seems wrong, not an obj?
       | otherwise           = (\(TxtI it) -> [StaticObjArg it]) <$> jsIdI i

     unfloated :: StgExpr -> G [StaticArg]
     unfloated (StgLit l) = map StaticLitArg <$> genStaticLit l
     unfloated (StgConApp dc args _)
       | isBoolTy (dataConType dc) || isUnboxableCon dc =
           (:[]) . allocUnboxedConStatic dc . concat <$> mapM genStaticArg args -- fixme what is allocunboxedcon?
       | null args = (\(TxtI t) -> [StaticObjArg t]) <$> jsIdI (dataConWorkId dc)
       | otherwise = do
           as       <- concat <$> mapM genStaticArg args
           (TxtI e) <- enterDataConI dc
           return [StaticConArg e as]
     unfloated x = panic ("genArg: unexpected unfloated expression: " ++ show x)

allocateStaticList :: [StgArg] -> StgArg -> G StaticVal
allocateStaticList xs a@(StgVarArg i)
  | isDataConId_maybe i == Just nilDataCon = listAlloc xs Nothing
  | otherwise = do
      unFloat <- use gsUnfloated
      case lookupUFM unFloat i of
        Just (StgConApp dc [h,t] _)
          | dc == consDataCon -> allocateStaticList (h:xs) t
        _ -> listAlloc xs (Just a)
  where
    listAlloc :: [StgArg] -> Maybe StgArg -> G StaticVal
    listAlloc xs Nothing  = do
      as <- concat . reverse <$> mapM genStaticArg xs
      return (StaticList as Nothing)
    listAlloc xs (Just r) = do
      as <- concat . reverse <$> mapM genStaticArg xs
      r' <- genStaticArg r
      case r' of
        [StaticObjArg ri] -> return (StaticList as (Just ri))
        _                 ->
          panic $ "allocateStaticList: invalid argument (tail): " ++
                  show xs ++
                  " " ++
                  show r
allocateStaticList _ _ = panic "allocateStaticList: unexpected literal in list"

-- generate arg to be passed to FFI call, with marshalling JStat to be run
-- before the call
genFFIArg :: Bool -> StgArg -> G (JStat, [JExpr])
genFFIArg _isJavaScriptCc (StgLitArg l) = (mempty,) <$> genLit l
genFFIArg isJavaScriptCc a@(StgVarArg i)
    | not isJavaScriptCc &&
      (tycon == byteArrayPrimTyCon || tycon == mutableByteArrayPrimTyCon) =
        (\x -> (mempty,[x, 0])) <$> jsId i
    | isVoid r                  = return (mempty, [])
--    | Just x <- marshalFFIArg a = x
    | isMultiVar r              = (mempty,) <$> mapM (jsIdN i) [1..varSize r]
    | otherwise                 = (\x -> (mempty,[x])) <$> jsId i
   where
     tycon  = tyConAppTyCon (unwrapType arg_ty)
     arg_ty = stgArgType a
     r      = uTypeVt arg_ty

genIdArg :: HasDebugCallStack => Id -> G [JExpr]
genIdArg i = genArg (StgVarArg i)

genIdArgI :: HasDebugCallStack => Id -> G [Ident]
genIdArgI i = trace' ("genIdArgI: " ++ show i) (genIdArgI0 i)

genIdArgI0 :: HasDebugCallStack => Id -> G [Ident]
genIdArgI0 i
    | isVoid r     = return []
    | isMultiVar r = mapM (jsIdIN i) [1..varSize r]
    | otherwise    = (:[]) <$> jsIdI i
    where
      r = uTypeVt . idType $ i


genIdStackArgI :: HasDebugCallStack => Id -> G [(Ident,StackSlot)]
genIdStackArgI i = zipWith f [1..] <$> genIdArgI i
  where
    f :: Int -> Ident -> (Ident,StackSlot)
    f n ident = (ident, SlotId i n)

r2d :: Rational -> Double
r2d = realToFrac

r2f :: Rational -> Double
r2f = float2Double . realToFrac

{-
genStrThunk :: HasDebugCallStack
            => Id
            -> Bool
            -> B.ByteString
            -> CostCentreStack
            -> C
genStrThunk i nonAscii str cc = do
  ii@(TxtI iit) <- jsIdI i
  let d = decl ii
      iie = e ii
  ccs <- costCentreStackLbl cc
  let ccsArg = map toJExpr $ maybeToList ccs
  emitStatic iit (StaticThunk Nothing) Nothing
  return $ case decodeModifiedUTF8 str of
    Just t -> d <>
      if nonAscii
      then iie |= app "h$strt" ([e (T.unpack t)]++ccsArg)
      else iie |= app "h$strta" ([e (T.unpack t)]++ccsArg)
    Nothing -> d <>
      if nonAscii
      then iie |= app "h$strtb" ([e (map toInteger $ B.unpack str)]++ccsArg)
      else iie |= app "h$strta" ([e $ map (chr . fromIntegral) (B.unpack str)] ++ ccsArg)
-}

genLit :: HasDebugCallStack => Literal -> G [JExpr]
genLit (LitChar c)      = return [ e (ord c) ]
genLit (LitString str)    =
  withNewIdent $ \strLit@(TxtI strLitT) ->
    withNewIdent $ \strOff@(TxtI strOffT) -> do
      emitStatic strLitT (StaticUnboxed (StaticUnboxedString str)) Nothing
      emitStatic strOffT (StaticUnboxed (StaticUnboxedStringOffset str)) Nothing
      return [ ValExpr (JVar strLit), ValExpr (JVar strOff) ]
genLit LitNullAddr      = return [ null_, 0 ]
genLit (LitNumber LitNumInt i _)       = return [ e i ]
genLit (LitNumber LitNumInt64 i _)     = return [ e (Bits.shiftR i 32)
                                                , e (toSigned i)
                                                ]
genLit (LitNumber LitNumWord w _)      = return [ e (toSigned w) ]
genLit (LitNumber LitNumWord64 w _)    = return [ e (toSigned (Bits.shiftR w 32))
                                                , e (toSigned w)
                                                ]
genLit (LitFloat r)     = return [ e (r2f r) ]
genLit (LitDouble r)    = return [ e (r2d r) ]
genLit (LitLabel name _size fod)
  | fod == IsFunction = return [ app "h$mkFunctionPtr" [var (T.pack $ "h$" ++ unpackFS name)], 0 ]
  | otherwise         = return [ e (TxtI . T.pack $ "h$" ++ unpackFS name), 0 ]
genLit l = panic $ "genLit: " ++ show l -- unhandled numeric literal" -- removed by CorePrep

-- | generate a literal for the static init tables
genStaticLit :: Literal -> G [StaticLit]
genStaticLit (LitChar c)         = return [ IntLit (fromIntegral $ ord c) ]
genStaticLit (LitString str)       =
  case T.decodeUtf8' str of
                         Right t -> return [ StringLit t, IntLit 0 ]
                         Left _  -> return [ BinLit str, IntLit 0]
genStaticLit LitNullAddr         = return [ NullLit, IntLit 0 ]
genStaticLit (LitNumber LitNumInt i _)          = return [ IntLit (fromIntegral i) ]
genStaticLit (LitNumber LitNumInt64 i _)        = return [ IntLit (i `Bits.shiftR` 32)
                                           , IntLit (toSigned i)
                                           ]
genStaticLit (LitNumber LitNumWord w _)         = return [ IntLit (toSigned w) ]
genStaticLit (LitNumber LitNumWord64 w _)       = return [ IntLit (toSigned (w `Bits.shiftR` 32))
                                           , IntLit (toSigned w)
                                           ]
genStaticLit (LitFloat r)        = return [ DoubleLit . SaneDouble . r2f $ r ]
genStaticLit (LitDouble r)       = return [ DoubleLit . SaneDouble . r2d $ r ]
genStaticLit (LitLabel name _size fod) =
  return [ LabelLit (fod == IsFunction) (T.pack $ "h$" ++ unpackFS name)
         , IntLit 0
         ]
genStaticLit l = panic $ "genStaticLit: " ++
                         show l

-- make a signed 32 bit int from this unsigned one, lower 32 bits
toSigned :: Integer -> Integer
toSigned i | Bits.testBit i 31 = Bits.complement (0x7FFFFFFF `Bits.xor` (i Bits..&. 0x7FFFFFFF))
           | otherwise         = i Bits..&. 0xFFFFFFFF

-- truncate literal to fit in 32 bit int
{-
intLit :: Integer -> Integer
intLit i = fromIntegral (fromIntegral i :: Int32)
-}

genSingleLit :: Literal -> G JExpr
genSingleLit l = do
  es <- genLit l
  case es of
    [e] -> return e
    _   -> panic "genSingleLit: expected single-variable literal"

genCon :: ExprCtx -> DataCon -> [JExpr] -> C
genCon tgt con args
  -- fixme should we check the primreps here?
  | isUnboxedTupleCon con && length (concatMap snd $ tgt^.ctxTarget) == length args =
      return $ assignAll (concatMap snd $ tgt ^. ctxTarget) args
genCon tgt con args | isUnboxedTupleCon con =
  panic ("genCon: unhandled DataCon:\n" ++
         show con ++ "\n" ++
         show (tgt ^. ctxTop) ++ "\n" ++
         show (tgt ^. ctxTarget) ++ "\n" ++
         show args)
genCon tgt con args | [ValExpr (JVar tgti)] <- concatMap snd (tgt ^. ctxTarget) =
  allocCon tgti con currentCCS args
genCon _tgt _con _args =
  return mempty -- fixme, do we get missing VecRep things because of this?
  -- panic ("genCon: unhandled DataCon: " ++ show con ++ " " ++ show (tgt ^. ctxTop, length args))

allocCon :: Ident -> DataCon -> CostCentreStack -> [JExpr] -> C
allocCon to con cc xs
  | isBoolTy (dataConType con) || isUnboxableCon con =
      return (e to |= allocUnboxedCon con xs)
{-  | null xs = do
      i <- jsId (dataConWorkId con)
      return (assignj to i) -}
  | otherwise = do
      e <- enterDataCon con
      cs <- use gsSettings
      prof <- profiling
      ccsJ <- if prof then ccsVarJ cc else return Nothing
      return $ allocDynamic cs False to e xs ccsJ

allocUnboxedCon :: DataCon -> [JExpr] -> JExpr
allocUnboxedCon con []
  | isBoolTy (dataConType con) && dataConTag con == 1 = false_
  | isBoolTy (dataConType con) && dataConTag con == 2 = true_
allocUnboxedCon con [x]
  | isUnboxableCon con = x
allocUnboxedCon con xs = panic ("allocUnboxedCon: not an unboxed constructor: " ++ show con ++ " " ++ show xs)

allocUnboxedConStatic :: DataCon -> [StaticArg] -> StaticArg
allocUnboxedConStatic con []
  | isBoolTy (dataConType con) && dataConTag con == 1 =
      StaticLitArg (BoolLit False)
  | isBoolTy (dataConType con) && dataConTag con == 2 =
      StaticLitArg (BoolLit True)
allocUnboxedConStatic _   [a@(StaticLitArg (IntLit _i))]    = a
allocUnboxedConStatic _   [a@(StaticLitArg (DoubleLit _d))] = a
allocUnboxedConStatic con _                                =
  panic ("allocUnboxedConStatic: not an unboxed constructor: " ++ show con)

allocConStatic :: HasDebugCallStack => Ident -> CostCentreStack -> DataCon -> [StgArg] {- -> Bool -} -> G ()
allocConStatic (TxtI to) cc con args -- isRecursive
{-  | trace' ("allocConStatic: " ++ show to ++ " " ++ show con ++ " " ++ show args) True -} = do
  as <- mapM genStaticArg args
  cc' <- costCentreStackLbl cc
  allocConStatic' cc' (concat as)
  where
    allocConStatic' :: HasDebugCallStack => Maybe Ident -> [StaticArg] -> G ()
    allocConStatic' cc' []
      | isBoolTy (dataConType con) && dataConTag con == 1 =
           emitStatic to (StaticUnboxed $ StaticUnboxedBool False) cc'
      | isBoolTy (dataConType con) && dataConTag con == 2 =
           emitStatic to (StaticUnboxed $ StaticUnboxedBool True) cc'
      | otherwise = do
           (TxtI e) <- enterDataConI con
           emitStatic to (StaticData e []) cc'
    allocConStatic' cc' [x]
      | isUnboxableCon con =
        case x of
          StaticLitArg (IntLit i)    ->
            emitStatic to (StaticUnboxed $ StaticUnboxedInt i) cc'
          StaticLitArg (BoolLit b)   ->
            emitStatic to (StaticUnboxed $ StaticUnboxedBool b) cc'
          StaticLitArg (DoubleLit d) ->
            emitStatic to (StaticUnboxed $ StaticUnboxedDouble d) cc'
          _                          ->
            panic $ "allocConStatic: invalid unboxed literal: " ++ show x
    allocConStatic' cc' xs =
           if con == consDataCon
              then flip (emitStatic to) cc' =<< allocateStaticList [args !!! 0] (args !!! 1)
              else do
                (TxtI e) <- enterDataConI con
                emitStatic to (StaticData e xs) cc'

-- avoid one indirection for global ids
-- fixme in many cases we can also jump directly to the entry for local?
jumpToII :: Id -> [JExpr] -> JStat -> C
jumpToII i args afterLoad
  | isLocalId i = do
     ii <- jsId i
     return (ra # afterLoad # returnS (ii .^ "f"))
  | otherwise   = do
     ei <- jsEntryId i
     return (ra # afterLoad # returnS ei)
  where
    ra = mconcat . reverse $ zipWith (\r a -> e r |= a) (enumFrom R2) args

jumpToFast :: HasDebugCallStack => [StgArg] -> JStat -> C
jumpToFast as afterLoad = do
  regs <- concatMapM genArg as
  (fun, spec) <- selectApply True (as,regs)
  if spec
    then return (mconcat (ra regs) # afterLoad # returnS (ApplExpr fun []))
    else return (mconcat (ra regs) # afterLoad # returnS (ApplExpr fun [e (mkTag regs as)]))
    where
      ra regs   = reverse $ zipWith (\r ex -> e r |= ex) (enumFrom R2) regs
      mkTag rs as = (length rs `Bits.shiftL` 8) Bits..|. length as


-- find a specialized application path if there is one
selectApply :: Bool           -- ^ true for fast apply, false for stack apply
            -> ([StgArg], [JExpr])       -- ^ arguments
            -> G (JExpr, Bool) -- ^ the function to call, true if specialized path
selectApply fast (args, as) =
  case specApply fast (length args) (length as) of
    Just e  -> return (e, True)
    Nothing -> return (jsv $ "h$ap_gen" <> fastSuff, False)
  where
    fastSuff | fast      = "_fast"
             | otherwise = ""

-- fixme: what if the call returns a thunk?
genPrimCall :: ExprCtx -> PrimCall -> [StgArg] -> Type -> G (JStat, ExprResult)
genPrimCall top (PrimCall lbl _) args t = do
  j <- parseFFIPattern False False False ("h$" ++ unpackFS lbl) t (map toJExpr . concatMap snd $ top ^. ctxTarget) args
  return (j, ExprInline Nothing)

getObjectKeyValuePairs :: [StgArg] -> Maybe [(Text, StgArg)]
getObjectKeyValuePairs [] = Just []
getObjectKeyValuePairs (k:v:xs)
  | Just t <- argJSStringLitUnfolding k =
      fmap ((t,v):) (getObjectKeyValuePairs xs)
getObjectKeyValuePairs _ = Nothing

argJSStringLitUnfolding :: StgArg -> Maybe Text
argJSStringLitUnfolding (StgVarArg _v)
  | False = Just "abc" -- fixme
argJSStringLitUnfolding _ = Nothing

genForeignCall :: HasDebugCallStack
               => ExprCtx
               -> ForeignCall
               -> Type
               -> [JExpr]
               -> [StgArg]
               -> G (JStat, ExprResult)
genForeignCall _top
               (CCall (CCallSpec (StaticTarget _ tgt Nothing True)
                                   JavaScriptCallConv
                                   PlayRisky _ _))
               _t
               [obj]
               args
  | tgt == fsLit "h$buildObject"
  , Just pairs <- getObjectKeyValuePairs args = do
      pairs' <- mapM (\(k,v) -> genArg v >>= \[v'] -> return (k,v')) pairs
      return ( (|=) obj (ValExpr (JHash $ M.fromList pairs'))
             , ExprInline Nothing
             )
genForeignCall top (CCall (CCallSpec ccTarget cconv safety _ _)) t tgt args = do
  emitForeign (top ^. ctxSrcSpan) (T.pack lbl) safety cconv (map showArgType args) (showType t)
  (,exprResult) <$> parseFFIPattern catchExcep async isJsCc lbl t tgt' args
  where
    isJsCc = cconv == JavaScriptCallConv

    lbl | (StaticTarget _ clbl _mpkg _isFunPtr) <- ccTarget
            = let clbl' = unpackFS clbl
              in  if | isJsCc -> clbl'
                     | wrapperPrefix `L.isPrefixOf` clbl' ->
                         ("h$" ++ (drop 2 $ dropWhile isDigit $ drop (length wrapperPrefix) clbl'))
                     | otherwise -> "h$" ++ clbl'
        | otherwise = "h$callDynamic"

    exprResult | async     = ExprCont
               | otherwise = ExprInline Nothing

    catchExcep = (cconv == JavaScriptCallConv) &&
                 playSafe safety || playInterruptible safety

    async | isJsCc    = playInterruptible safety
          | otherwise = playInterruptible safety || playSafe safety

    tgt'  | async     = take (length tgt) (map toJExpr $ enumFrom R1)
          | otherwise = tgt

    wrapperPrefix = "ghczuwrapperZC"

-- | generate the actual call
{-
  parse FFI patterns:
   "&value         -> value
  1. "function"      -> ret = function(...)
  2. "$r = $1.f($2)  -> r1 = a1.f(a2)

  arguments, $1, $2, $3 unary arguments
     $1_1, $1_2, for a binary argument

  return type examples
  1. $r                      unary return
  2. $r1, $r2                binary return
  3. $r1, $r2, $r3_1, $r3_2  unboxed tuple return
 -}
parseFFIPattern :: Bool  -- ^ catch exception and convert them to haskell exceptions
                -> Bool  -- ^ async (only valid with javascript calling conv)
                -> Bool  -- ^ using javascript calling convention
                -> String
                -> Type
                -> [JExpr]
                -> [StgArg]
                -> C
parseFFIPattern catchExcep async jscc pat t es as
  | catchExcep = do
      c <- parseFFIPatternA async jscc pat t es as
      let ex = TxtI "except"
      return (TryStat c ex (returnS (app "h$throwJSException" [e ex])) mempty)
       {-[j| try {
                   `c`;
                 } catch(e) {
                   return h$throwJSException(e);
                 }
               |]-}
  | otherwise  = parseFFIPatternA async jscc pat t es as

parseFFIPatternA :: Bool  -- ^ async
                 -> Bool  -- ^ using JavaScript calling conv
                 -> String
                 -> Type
                 -> [JExpr]
                 -> [StgArg]
                 -> C
-- async calls get an extra callback argument
-- call it with the result
parseFFIPatternA True True pat t es as  = do
  cb <- makeIdent
  x  <- makeIdent
  d  <- makeIdent
  stat <- parseFFIPattern' (Just (toJExpr cb)) True pat t es as
  return
     (x ||= (e (JHash (M.fromList [("mv", null_)]))) #
      cb ||= app "h$mkForeignCallback" [e x] #
      stat #
      ifS (e x .^ "mv" .===. null_)
          (e x .^ "mv" |= UOpExpr NewOp (app "h$MVar" []) #
           sp |= sp + 1 #
           stack .! sp |= var "h$unboxFFIResult" #
           returnS (app "h$takeMVar" [e x .^ "mv"]))
          (decl d #
           e d |= e x .^ "mv" #
           copyResult (e d))
     )
     where nrst = typeSize t
           copyResult d = assignAll es (map (\i -> d .! e i) [0..nrst-1])
parseFFIPatternA _async javascriptCc pat t es as =
  parseFFIPattern' Nothing javascriptCc pat t es as

-- parseFFIPatternA _ _ _ _ _ _ = error "parseFFIPattern: non-JavaScript pattern must be synchronous"

parseFFIPattern' :: Maybe JExpr -- ^ Nothing for sync, Just callback for async
                 -> Bool        -- ^ javascript calling convention used
                 -> String      -- ^ pattern called
                 -> Type        -- ^ return type
                 -> [JExpr]     -- ^ expressions to return in (may be more than necessary)
                 -> [StgArg]    -- ^ arguments
                 -> C
parseFFIPattern' callback javascriptCc pat t ret args
  | not javascriptCc = mkApply pat
  | otherwise = do
      u <- freshUnique
      case parseFfiJME pat u of
        Right (ValExpr (JVar (TxtI _ident))) -> mkApply pat
        Right expr | not async && length tgt < 2 -> do
          (statPre, ap) <- argPlaceholders javascriptCc args
          let rp  = resultPlaceholders async t ret
              env = M.fromList (rp ++ ap)
          if length tgt == 1
            then return $ statPre <> (everywhere (mkT $ replaceIdent env) (var "$r" |= expr))
            else return $ statPre <> (everywhere (mkT $ replaceIdent env) (toStat expr))
        Right _ -> p $ "invalid expression FFI pattern. Expression FFI patterns can only be used for synchronous FFI " ++
                       " imports with result size 0 or 1.\n" ++ pat
        Left _ -> case parseFfiJM pat u of
          Left err -> p (show err)
          Right stat -> do
            let rp = resultPlaceholders async t ret
            let cp = callbackPlaceholders callback
            (statPre, ap) <- argPlaceholders javascriptCc args
            let env = M.fromList (rp ++ ap ++ cp)
            return $ statPre <> (everywhere (mkT $ replaceIdent env) stat) -- fixme trace?
  where
    async = isJust callback
    tgt = take (typeSize t) ret
    -- automatic apply, build call and result copy
    mkApply f
      | Just cb <- callback = do
         (stats, as) <- unzip <$> mapM (genFFIArg javascriptCc) args
         cs <- use gsSettings
         return $ traceCall cs as <> mconcat stats <> ApplStat f' (concat as++[cb])
      | {-ts@-}
        (t:ts') <- tgt = do
         (stats, as) <- unzip <$> mapM (genFFIArg javascriptCc) args
         cs <- use gsSettings
         return $ traceCall cs as
                <> mconcat stats
                <> (t |= ApplExpr f' (concat as) )
                <> copyResult ts'
           -- _ -> error "mkApply: empty list"
      | otherwise = do
         (stats, as) <- unzip <$> mapM (genFFIArg javascriptCc) args
         cs <- use gsSettings
         return $ traceCall cs as <> mconcat stats <> ApplStat f' (concat as)
        where f' = toJExpr (TxtI $ T.pack f)
    copyResult rs = mconcat $ zipWith (\t r -> e r |= e t) (enumFrom Ret1) rs
    p e = error ("Parse error in FFI pattern: " ++ pat ++ "\n" ++ e)
    replaceIdent :: Map Ident JExpr -> JExpr -> JExpr
    replaceIdent env e@(ValExpr (JVar i))
      | isFFIPlaceholder i = fromMaybe err (M.lookup i env)
      | otherwise = e
        where
          (TxtI i') = i
          err = error (pat ++ ": invalid placeholder, check function type: " ++ show (i', args, t))
    replaceIdent _ e = e
    traceCall cs as
        | csTraceForeign cs = appS "h$traceForeign" [e pat, e as]
        | otherwise         = mempty

showArgType :: StgArg -> Text
showArgType a = showType (stgArgType a)

showType :: Type -> Text
showType t
  | Just tc <- tyConAppTyCon_maybe (unwrapType t) =
      T.pack (show tc)
  | otherwise = "<unknown>"

-- parse and saturate ffi splice
parseFfiJME :: String -> Int -> Either P.ParseError JExpr
parseFfiJME xs u = fmap (saturateFFI u) . parseJME $ xs

-- parse and saturate ffi splice, check for unhygienic declarations
parseFfiJM :: String -> Int -> Either P.ParseError JStat
parseFfiJM xs u = fmap (makeHygienic . saturateFFI u) . parseJM $ xs
  where
    makeHygienic :: JStat -> JStat
    makeHygienic s = snd $ O.renameLocalsFun (map addFFIToken newLocals) ([], s)

--    addFFIToken (StrI xs) = TxtI (T.pack $ "ghcjs_ffi_" ++ show u ++ "_" ++ xs)
    addFFIToken (TxtI xs) = TxtI (T.pack ("ghcjs_ffi_" ++ show u ++ "_") <> xs)

saturateFFI :: JMacro a => Int -> a -> a
saturateFFI u = jsSaturate (Just . T.pack $ "ghcjs_ffi_sat_" ++ show u)

-- $r for single, $r1,$r2 for dual
-- $r1, $r2, etc for ubx tup, void args not counted
resultPlaceholders :: Bool -> Type -> [JExpr] -> [(Ident,JExpr)] -- ident, replacement
resultPlaceholders True _ _ = [] -- async has no direct resuls, use callback
resultPlaceholders False t rs =
  case typeVt (unwrapType t) of
    [t'] -> mkUnary (varSize t')
    uts ->
      let sizes = filter (>0) (map varSize uts)
          f _ 0 = []
          f n 1 = [["$r" ++ show n]]
          f n k = ["$r" ++ sn, "$r" ++ sn ++ "_1"] : map (\x -> ["$r" ++ sn ++ "_" ++ show x]) [2..k]
            where sn = show n
          phs   = zipWith (\size n -> f n size) sizes [(1::Int)..]
      in case sizes of
           [n] -> mkUnary n
           _   -> concat $ zipWith (\phs' r -> map (\i -> (TxtI (T.pack i), r)) phs') (concat phs) rs
  where
    mkUnary 0 = []
    mkUnary 1 = [(TxtI "$r",head rs)] -- single
    mkUnary n = [(TxtI "$r",head rs),(TxtI "$r1", head rs)] ++
       zipWith (\n r -> (TxtI . T.pack $ "$r" ++ show n, toJExpr r)) [2..n] (tail rs)

-- $1, $2, $3 for single, $1_1, $1_2 etc for dual
-- void args not counted
argPlaceholders :: Bool -> [StgArg] -> G (JStat, [(Ident,JExpr)])
argPlaceholders isJavaScriptCc args = do
  (stats, idents0) <- unzip <$> mapM (genFFIArg isJavaScriptCc) args
  let idents = filter (not . null) idents0
  return $ (mconcat stats, concat
    (zipWith (\is n -> mkPlaceholder True ("$"++show n) is) idents [(1::Int)..]))

callbackPlaceholders :: Maybe JExpr -> [(Ident,JExpr)]
callbackPlaceholders Nothing  = []
callbackPlaceholders (Just e) = [((TxtI "$c"), e)]

mkPlaceholder :: Bool -> String -> [JExpr] -> [(Ident, JExpr)]
mkPlaceholder undersc prefix aids =
      case aids of
             []       -> []
             [x]      -> [(TxtI . T.pack $ prefix, x)]
             xs@(x:_) -> (TxtI . T.pack $ prefix, x) :
                zipWith (\x m -> (TxtI . T.pack $ prefix ++ u ++ show m,x)) xs [(1::Int)..]
   where u = if undersc then "_" else ""

-- ident is $N, $N_R, $rN, $rN_R or $r or $c
isFFIPlaceholder :: Ident -> Bool
isFFIPlaceholder (TxtI x) =
  either (const False) (const True) (P.parse parser "" x)
    where
      parser = void (P.try $ P.string "$r") <|>
               void (P.try $ P.string "$c") <|> do
        _ <- P.char '$'
        P.optional (P.char 'r')
        _ <- P.many1 P.digit
        P.optional (P.char '_' >> P.many1 P.digit)

withNewIdent :: (Ident -> G a) -> G a
withNewIdent m = makeIdent >>= m

makeIdent :: G Ident
makeIdent = do
  gsId += 1
  i <- use gsId
  mod <- use gsModule
  return (TxtI . T.pack $ "h$$" ++
                          zEncodeString (show mod) ++
                          "_" ++
                          encodeUnique i
         )

freshUnique :: G Int
freshUnique = gsId += 1 >> use gsId

-- returns True if the expression is definitely inline
isInlineExpr :: UniqSet Id -> StgExpr -> (UniqSet Id, Bool)
isInlineExpr v (StgApp i args) =
  (emptyUniqSet, isInlineApp v i args)
isInlineExpr _ StgLit{} =
  (emptyUniqSet, True)
isInlineExpr _ StgConApp{} =
  (emptyUniqSet, True)
isInlineExpr _ (StgOpApp (StgFCallOp f _) _ _) =
  (emptyUniqSet, isInlineForeignCall f)
isInlineExpr v (StgOpApp (StgPrimOp SeqOp) [StgVarArg e] t) =
  (emptyUniqSet, e `elementOfUniqSet` v || isStrictType t)
isInlineExpr _ (StgOpApp (StgPrimOp op) _ _) =
  (emptyUniqSet, isInlinePrimOp op)
isInlineExpr _ (StgOpApp (StgPrimCallOp _c) _ _) =
  (emptyUniqSet, True)
isInlineExpr _ StgLam{} =
  (emptyUniqSet, True)
isInlineExpr v (StgCase e b _ alts) =
  let (_ve, ie)   = isInlineExpr v e
      v'          = addOneToUniqSet v b
      (vas, ias)  = unzip $ map (isInlineExpr v') (alts ^.. traverse . _3)
      vr          = foldl1' intersectUniqSets vas
  in (vr, (ie || b `elementOfUniqSet` v) && and ias)
isInlineExpr v (StgLet _ b e) =
  isInlineExpr (inspectInlineBinding v b) e
isInlineExpr v (StgLetNoEscape _ _b e) =
  isInlineExpr v e
isInlineExpr v (StgTick  _ e) =
  isInlineExpr v e

inspectInlineBinding :: UniqSet Id -> StgBinding -> UniqSet Id
inspectInlineBinding v (StgNonRec i r) = inspectInlineRhs v i r
inspectInlineBinding v (StgRec bs)       =
  foldl' (\v' (i,r) -> inspectInlineRhs v' i r) v bs

inspectInlineRhs :: UniqSet Id -> Id -> StgRhs -> UniqSet Id
inspectInlineRhs v i StgRhsCon{}                       = addOneToUniqSet v i
inspectInlineRhs v i (StgRhsClosure _ _ ReEntrant _ _) = addOneToUniqSet v i
inspectInlineRhs v _ _                                 = v

isInlineForeignCall :: ForeignCall -> Bool
isInlineForeignCall (CCall (CCallSpec _ cconv safety _ _)) =
  not (playInterruptible safety) &&
  not (cconv /= JavaScriptCallConv && playSafe safety)

isInlineApp :: UniqSet Id -> Id -> [StgArg] -> Bool
isInlineApp _ i _
  | isJoinId i = False
isInlineApp v i [] = isUnboxedTupleType (idType i) ||
                     isStrictType (idType i) ||
                     i `elementOfUniqSet` v ||
                     isStrictId i
isInlineApp v i [StgVarArg a]
  | DataConWrapId dc <- idDetails i
  , isNewTyCon (dataConTyCon dc)
  , isStrictType (idType a) || a `elementOfUniqSet` v || isStrictId a = True
isInlineApp _ _ _ = False

verifyMatchRep :: HasDebugCallStack => Id -> AltType -> C
#ifndef RUNTIME_ASSERTIONS
verifyMatchRep _ _ = pure mempty
#else
verifyMatchRep x (AlgAlt tc) = do
  ix <- genIds x
  pure $ appS "h$verify_match_alg" (ValExpr(JStr(T.pack (show tc))):ix)
verifyMatchRep _ _ = pure mempty
#endif

verifyRuntimeReps :: HasDebugCallStack => [Id] -> C
#ifndef RUNTIME_ASSERTIONS
verifyRuntimeReps _  = pure mempty
#else
verifyRuntimeReps xs = mconcat <$> mapM verifyRuntimeRep xs
  where
    verifyRuntimeRep i = do
      i' <- genIds i
      pure $ go i' (idVt i)
    go js         (VoidV:vs) = go js vs
    go (j1:j2:js) (LongV:vs) = v "h$verify_rep_long" [j1,j2] <> go js vs
    go (j1:j2:js) (AddrV:vs) = v "h$verify_rep_addr" [j1,j2] <> go js vs
    go (j:js)     (v:vs)     = ver j v                       <> go js vs
    go []         []         = mempty
    go _          _          = panic
      ("verifyRuntimeReps: inconsistent sizes: " ++ show xs)
    ver j PtrV    = v "h$verify_rep_heapobj" [j]
    ver j IntV    = v "h$verify_rep_int"     [j]
    ver j RtsObjV = v "h$verify_rep_rtsobj"  [j]
    ver j DoubleV = v "h$verify_rep_double"  [j]
    ver j ArrV    = v "h$verify_rep_arr"     [j]
    ver _ _       = mempty
    v f as = appS f as
#endif
