{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module GHC.StgToJS.Monad
  ( runG
  , emitGlobal
  , addDependency
  , emitToplevel
  , emitStatic
  , emitClosureInfo
  , emitForeign
  , assertRtsStat
  , getSettings
  , updateThunk
  , updateThunk'
  , liftToGlobal
  , bhStats
  -- * IDs
  , withNewIdent
  , makeIdent
  , freshUnique
  , jsIdIdent
  , jsId
  , jsIdN
  , jsIdI
  , jsIdIN
  , jsIdIdent'
  , jsIdV
  , jsEnId
  , jsEnIdI
  , jsEntryId
  , jsEntryIdI
  , jsDcEntryId
  , jsDcEntryIdI
  , genIds
  , genIdsN
  , genIdsI
  , genIdsIN
  , getStaticRef
  , declIds
  -- * Datacon
  , enterDataCon
  , enterDataConI
  -- * Group
  , modifyGroup
  , resetGroup
  -- * Stack
  , resetSlots
  , isolateSlots
  , setSlots
  , getSlots
  , addSlots
  , dropSlots
  , addUnknownSlots
  , adjPushStack
  , push
  , push'
  , adjSpN
  , adjSpN'
  , adjSp'
  , adjSp
  , pushNN
  , pushNN'
  , pushN'
  , pushN
  , pushOptimized'
  , pushOptimized
  , pushLneFrame
  , pop
  , popn
  , popUnknown
  , popSkipUnknown
  , popSkip
  , popSkip'
  , popSkipI
  , loadSkip
  )
where

import GHC.Prelude

import GHC.JS.Syntax
import GHC.JS.Make
import GHC.JS.Transform

import GHC.StgToJS.ExprCtx
import GHC.StgToJS.Heap
import GHC.StgToJS.Types
import GHC.StgToJS.Regs
import GHC.StgToJS.CoreUtils
import GHC.StgToJS.UnitUtils

import GHC.Data.ShortText as ST
import GHC.Unit.Module
import GHC.Core.DataCon
import GHC.Stg.Syntax

import GHC.Types.SrcLoc
import GHC.Types.Id
import GHC.Types.Name
import GHC.Types.Unique
import GHC.Types.Unique.FM
import GHC.Types.ForeignCall

import GHC.Utils.Encoding (zEncodeString)
import GHC.Utils.Outputable hiding ((<>))
import GHC.Utils.Misc
import qualified GHC.Utils.Monad.State.Strict as State
import GHC.Data.FastString

import qualified Data.Map  as M
import qualified Data.Set  as S
import qualified Data.Bits as Bits
import qualified Data.List as L
import Data.Function
import Data.Maybe
import Data.Array
import Data.Monoid
import Control.Monad

runG :: StgToJSConfig -> Module -> UniqFM Id CgStgExpr -> G a -> a
runG config m unfloat action = State.evalState action (initState config m unfloat)

initState :: StgToJSConfig -> Module -> UniqFM Id CgStgExpr -> GenState
initState config m unfloat = GenState
  { gsSettings  = config
  , gsModule    = m
  , gsId        = 1
  , gsIdents    = emptyIdCache
  , gsUnfloated = unfloat
  , gsGroup     = defaultGenGroupState
  , gsGlobal    = []
  }


modifyGroup :: (GenGroupState -> GenGroupState) -> G ()
modifyGroup f = State.modify mod_state
  where
    mod_state s = s { gsGroup = f (gsGroup s) }

-- | emit a global (for the current module) toplevel statement
emitGlobal :: JStat -> G ()
emitGlobal stat = State.modify (\s -> s { gsGlobal = stat : gsGlobal s })

-- | add a dependency on a particular symbol to the current group
addDependency :: OtherSymb -> G ()
addDependency symbol = modifyGroup mod_group
  where
    mod_group g = g { ggsExtraDeps = S.insert symbol (ggsExtraDeps g) }

-- | emit a top-level statement for the current binding group
emitToplevel :: JStat -> G ()
emitToplevel s = modifyGroup mod_group
  where
    mod_group g = g { ggsToplevelStats = s : ggsToplevelStats g}

-- | emit static data for the binding group
emitStatic :: ShortText -> StaticVal -> Maybe Ident -> G ()
emitStatic ident val cc = modifyGroup mod_group
  where
    mod_group  g = g { ggsStatic = mod_static (ggsStatic g) }
    mod_static s = StaticInfo ident val cc : s

-- | add closure info in our binding group. all heap objects must have closure info
emitClosureInfo :: ClosureInfo -> G ()
emitClosureInfo ci = modifyGroup mod_group
  where
    mod_group g = g { ggsClosureInfo = ci : ggsClosureInfo g}

emitForeign :: Maybe RealSrcSpan
            -> ShortText
            -> Safety
            -> CCallConv
            -> [ShortText]
            -> ShortText
            -> G ()
emitForeign mbSpan pat safety cconv arg_tys res_ty = modifyGroup mod_group
  where
    mod_group g = g { ggsForeignRefs = new_ref : ggsForeignRefs g }
    new_ref = ForeignJSRef spanTxt pat safety cconv arg_tys res_ty
    spanTxt = case mbSpan of
                Just sp -> ST.pack $
                  unpackFS (srcSpanFile sp) ++
                  " " ++
                  show (srcSpanStartLine sp, srcSpanStartCol sp) ++
                  "-" ++
                  show (srcSpanEndLine sp, srcSpanEndCol sp)
                Nothing -> "<unknown>"


withNewIdent :: (Ident -> G a) -> G a
withNewIdent m = makeIdent >>= m

makeIdent :: G Ident
makeIdent = do
  i <- freshUnique
  mod <- State.gets gsModule
  let !name = ST.pack $ mconcat
                [ "h$$"
                , zEncodeString (unitModuleString mod)
                , "_"
                , encodeUnique i
                ]
  return (TxtI name)

encodeUnique :: Int -> String
encodeUnique = reverse . iToBase62  -- reversed is more compressible

jsId :: Id -> G JExpr
jsId i
--  | i == trueDataConId  = return $ toJExpr True
--  | i == falseDataConId = return $ toJExpr False
  | otherwise = ValExpr . JVar <$> jsIdIdent i Nothing IdPlain

jsIdI :: Id -> G Ident
jsIdI i = jsIdIdent i Nothing IdPlain

-- some types, Word64, Addr#, unboxed tuple have more than one javascript var
jsIdIN :: Id -> Int -> G Ident
jsIdIN i n = jsIdIdent i (Just n) IdPlain

jsIdN :: Id -> Int -> G JExpr
jsIdN i n = ValExpr . JVar <$> jsIdIdent i (Just n) IdPlain

-- uncached
jsIdIdent' :: Id -> Maybe Int -> IdType -> G Ident
jsIdIdent' i mn suffix0 = do
  (prefix, u) <- mkPrefixU
  let i' = (\x -> ST.pack $ "h$"++prefix++x++mns++suffix++u) . zEncodeString $ name
  i' `seq` return (TxtI i')
    where
      suffix = idTypeSuffix suffix0
      mns = maybe "" (('_':).show) mn
      name = ('.':) . nameStableString . localiseName . getName $ i

      mkPrefixU :: G (String, String)
      mkPrefixU
        | isExportedId i, Just x <- (nameModule_maybe . getName) i = do
           let xstr = unitModuleString x
           return (zEncodeString xstr, "")
        | otherwise = (,('_':) . encodeUnique . getKey . getUnique $ i) . ('$':)
                    . zEncodeString . unitModuleString <$> State.gets gsModule

-- entry id
jsEnId :: Id -> G JExpr
jsEnId i = ValExpr . JVar <$> jsEnIdI i

jsEnIdI :: Id -> G Ident
jsEnIdI i = jsIdIdent i Nothing IdEntry

jsEntryId :: Id -> G JExpr
jsEntryId i = ValExpr . JVar <$> jsEntryIdI i

jsEntryIdI :: Id -> G Ident
jsEntryIdI i = jsIdIdent i Nothing IdEntry

-- datacon entry, different name than the wrapper
jsDcEntryId :: Id -> G JExpr
jsDcEntryId i = ValExpr . JVar <$> jsDcEntryIdI i

jsDcEntryIdI :: Id -> G Ident
jsDcEntryIdI i = jsIdIdent i Nothing IdConEntry

-- entry function of the worker
enterDataCon :: DataCon -> G JExpr
enterDataCon d = jsDcEntryId (dataConWorkId d)

enterDataConI :: DataCon -> G Ident
enterDataConI d = jsDcEntryIdI (dataConWorkId d)


jsIdV :: Id -> G JVal
jsIdV i = JVar <$> jsIdIdent i Nothing IdPlain


-- | generate all js vars for the ids (can be multiple per var)
genIds :: Id -> G [JExpr]
genIds i
  | s == 0    = return mempty
  | s == 1    = (:[]) <$> jsId i
  | otherwise = mapM (jsIdN i) [1..s]
  where
    s  = typeSize (idType i)

genIdsN :: Id -> Int -> G JExpr
genIdsN i n = do
  xs <- genIds i
  return $ xs !! (n-1)

-- | get all idents for an id
genIdsI :: Id -> G [Ident]
genIdsI i
  | s == 1    = (:[]) <$> jsIdI i
  | otherwise = mapM (jsIdIN i) [1..s]
        where
          s = typeSize (idType i)

genIdsIN :: Id -> Int -> G Ident
genIdsIN i n = do
  xs <- genIdsI i
  return $ xs !! (n-1)

jsIdIdent :: Id -> Maybe Int -> IdType -> G Ident
jsIdIdent i mi suffix = do
  IdCache cache <- State.gets gsIdents
  ident <- case M.lookup key cache of
    Just ident -> pure ident
    Nothing -> do
      mod <- State.gets gsModule
      let !ident  = makeIdIdent i mi suffix mod
      let !cache' = IdCache (M.insert key ident cache)
      State.modify (\s -> s { gsIdents = cache' })
      pure ident
  updateGlobalIdCache ident
  where
    !key = IdKey (getKey . getUnique $ i) (fromMaybe 0 mi) suffix
    updateGlobalIdCache :: Ident -> G Ident
    updateGlobalIdCache ji
      -- fixme also allow caching entries for lifting?
      | not (isGlobalId i) || isJust mi || suffix /= IdPlain = pure ji
      | otherwise = do
          GlobalIdCache gidc <- getGlobalIdCache
          case M.lookup ji gidc of
            Nothing -> do
              let mod_group g = g { ggsGlobalIdCache = GlobalIdCache (M.insert ji (key, i) gidc) }
              State.modify (\s -> s { gsGroup = mod_group (gsGroup s) })
            Just _  -> pure ()
          pure ji

getStaticRef :: Id -> G (Maybe ShortText)
getStaticRef = fmap (fmap itxt . listToMaybe) . genIdsI

-- uncached
makeIdIdent :: Id -> Maybe Int -> IdType -> Module -> Ident
makeIdIdent i mn suffix0 mod = TxtI txt
  where
    !txt = ST.pack full_name

    full_name = mconcat
      ["h$"
      , prefix
      , zEncodeString ('.':name)
      , mns
      , suffix
      , u
      ]

    -- prefix and suffix (unique)
    (prefix,u)
      | isExportedId i
      , Just x <- (nameModule_maybe . getName) i
      = ( zEncodeString (unitModuleString x)
        , ""
        )
      | otherwise
      = ( '$':zEncodeString (unitModuleString mod)
        , '_': encodeUnique (getKey (getUnique i))
        )

    suffix = idTypeSuffix suffix0
    mns = maybe "" (('_':).show) mn
    name = renderWithContext defaultSDocContext . pprNameUnqualified . getName $ i



idTypeSuffix :: IdType -> String
idTypeSuffix IdPlain = ""
idTypeSuffix IdEntry = "_e"
idTypeSuffix IdConEntry = "_con_e"

-- | start with a new binding group
resetGroup :: G ()
resetGroup = State.modify (\s -> s { gsGroup = defaultGenGroupState })

defaultGenGroupState :: GenGroupState
defaultGenGroupState = GenGroupState [] [] [] [] 0 S.empty emptyGlobalIdCache []

emptyGlobalIdCache :: GlobalIdCache
emptyGlobalIdCache = GlobalIdCache M.empty

emptyIdCache :: IdCache
emptyIdCache = IdCache M.empty

-- | run the action with no stack info
resetSlots :: G a -> G a
resetSlots m = do
  s <- getSlots
  d <- getStackDepth
  setSlots []
  a <- m
  setSlots s
  setStackDepth d
  return a

-- | run the action with current stack info, but don't let modifications propagate
isolateSlots :: G a -> G a
isolateSlots m = do
  s <- getSlots
  d <- getStackDepth
  a <- m
  setSlots s
  setStackDepth d
  pure a

-- | Set stack depth
setStackDepth :: Int -> G ()
setStackDepth d = modifyGroup (\s -> s { ggsStackDepth = d})

-- | Get stack depth
getStackDepth :: G Int
getStackDepth = State.gets (ggsStackDepth . gsGroup)

-- | Modify stack depth
modifyStackDepth :: (Int -> Int) -> G ()
modifyStackDepth f = modifyGroup (\s -> s { ggsStackDepth = f (ggsStackDepth s) })

-- | overwrite our stack knowledge
setSlots :: [StackSlot] -> G ()
setSlots xs = modifyGroup (\g -> g { ggsStack = xs})

-- | retrieve our current stack knowledge
getSlots :: G [StackSlot]
getSlots = State.gets (ggsStack . gsGroup)

-- | Modify stack slots
modifySlots :: ([StackSlot] -> [StackSlot]) -> G ()
modifySlots f = modifyGroup (\g -> g { ggsStack = f (ggsStack g)})

-- | add `n` unknown slots to our stack knowledge
addUnknownSlots :: Int -> G ()
addUnknownSlots n = addSlots (replicate n SlotUnknown)

-- | add knowledge about the stack slots
addSlots :: [StackSlot] -> G ()
addSlots xs = do
  s <- getSlots
  setSlots (xs ++ s)

dropSlots :: Int -> G ()
dropSlots n = modifySlots (drop n)

adjPushStack :: Int -> G ()
adjPushStack n = do
  modifyStackDepth (+n)
  dropSlots n

push :: [JExpr] -> G JStat
push xs = do
  dropSlots (length xs)
  modifyStackDepth (+ (length xs))
  flip push' xs <$> getSettings

push' :: StgToJSConfig -> [JExpr] -> JStat
push' _ [] = mempty
push' cs xs
   | csInlinePush cs || l > 32 || l < 2 = adjSp' l <> mconcat items
   | otherwise                          = ApplStat (toJExpr $ pushN ! l) xs
  where
    items = zipWith (\i e -> AssignStat ((IdxExpr stack) (toJExpr (offset i))) (toJExpr e))
                    [(1::Int)..] xs
    offset i | i == l    = sp
             | otherwise = InfixExpr SubOp sp (toJExpr (l - i))
    l = length xs


adjSp' :: Int -> JStat
adjSp' 0 = mempty
adjSp' n = sp |= InfixExpr AddOp sp (toJExpr n)

adjSpN' :: Int -> JStat
adjSpN' 0 = mempty
adjSpN' n = sp |= InfixExpr SubOp sp (toJExpr n)

adjSp :: Int -> G JStat
adjSp 0 = return mempty
adjSp n = do
  modifyStackDepth (+n)
  return (adjSp' n)

adjSpN :: Int -> G JStat
adjSpN 0 = return mempty
adjSpN n = do
  modifyStackDepth (\x -> x - n)
  return (adjSpN' n)

pushN :: Array Int Ident
pushN = listArray (1,32) $ map (TxtI . ST.pack . ("h$p"++) . show) [(1::Int)..32]

pushN' :: Array Int JExpr
pushN' = fmap (ValExpr . JVar) pushN

pushNN :: Array Integer Ident
pushNN = listArray (1,255) $ map (TxtI . ST.pack . ("h$pp"++) . show) [(1::Int)..255]

pushNN' :: Array Integer JExpr
pushNN' = fmap (ValExpr . JVar) pushNN

pushOptimized' :: [(Id,Int)] -> G JStat
pushOptimized' xs = do
  slots  <- getSlots
  pushOptimized =<< (zipWithM f xs (slots++repeat SlotUnknown))
  where
    f (i1,n1) (SlotId i2 n2) = (,i1==i2&&n1==n2) <$> genIdsN i1 n1
    f (i1,n1) _              = (,False)          <$> genIdsN i1 n1

-- | optimized push that reuses existing values on stack automatically chooses
-- an optimized partial push (h$ppN) function when possible.
pushOptimized :: [(JExpr,Bool)] -- ^ contents of the slots, True if same value is already there
              -> G JStat
pushOptimized [] = return mempty
pushOptimized xs = do
  dropSlots l
  modifyStackDepth (+ length xs)
  go .  csInlinePush <$> getSettings
  where
    go True = inlinePush
    go _
     | all snd xs                  = adjSp' l
     | all (not.snd) xs && l <= 32 =
        ApplStat (pushN' ! l) (map fst xs)
     | l <= 8 && not (snd $ last xs) =
        ApplStat (pushNN' ! sig) [ e | (e,False) <- xs ]
     | otherwise = inlinePush
    l   = length xs
    sig :: Integer
    sig = L.foldl1' (Bits..|.) $ zipWith (\(_e,b) i -> if not b then Bits.bit i else 0) xs [0..]
    inlinePush = adjSp' l <> mconcat (zipWith pushSlot [1..] xs)
    pushSlot i (ex, False) = IdxExpr stack (offset i) |= ex
    pushSlot _ _           = mempty
    offset i | i == l    = sp
             | otherwise = InfixExpr SubOp sp (toJExpr (l - i))

pushLneFrame :: HasDebugCallStack => Int -> ExprCtx -> G JStat
pushLneFrame size ctx =
  let ctx' = ctxLneShrinkStack ctx size
  in pushOptimized' (ctxLneFrameVars ctx')

popUnknown :: [JExpr] -> G JStat
popUnknown xs = popSkipUnknown 0 xs

popSkipUnknown :: Int -> [JExpr] -> G JStat
popSkipUnknown n xs = popSkip n (map (,SlotUnknown) xs)

pop :: [(JExpr,StackSlot)] -> G JStat
pop = popSkip 0

-- | pop the expressions, but ignore the top n elements of the stack
popSkip :: Int -> [(JExpr,StackSlot)] -> G JStat
popSkip 0 [] = pure mempty
popSkip n [] = addUnknownSlots n >> adjSpN n
popSkip n xs = do
  addUnknownSlots n
  addSlots (map snd xs)
  a <- adjSpN (length xs + n)
  return (loadSkip n (map fst xs) <> a)

-- | pop things, don't upstate stack knowledge
popSkip' :: Int     -- ^ number of slots to skip
         -> [JExpr] -- ^ assign stack slot values to these
         -> JStat
popSkip' 0 []  = mempty
popSkip' n []  = adjSpN' n
popSkip' n tgt = loadSkip n tgt <> adjSpN' (length tgt + n)

-- | like popSkip, but without modifying the stack pointer
loadSkip :: Int -> [JExpr] -> JStat
loadSkip = loadSkipFrom sp

loadSkipFrom :: JExpr -> Int -> [JExpr] -> JStat
loadSkipFrom fr n xs = mconcat items
    where
      items = reverse $ zipWith (\i ex -> ex |= IdxExpr stack (toJExpr (offset (i+n))))
                                [(0::Int)..]
                                (reverse xs)
      offset 0 = toJExpr fr
      offset n = InfixExpr SubOp (toJExpr fr) (toJExpr n)


-- declare and pop
popSkipI :: Int -> [(Ident,StackSlot)] -> G JStat
popSkipI 0 [] = pure mempty
popSkipI n [] = adjSpN n
popSkipI n xs = do
  addUnknownSlots n
  addSlots (map snd xs)
  a <- adjSpN (length xs + n)
  return (loadSkipI n (map fst xs) <> a)

-- like popSkip, but without modifying sp
loadSkipI :: Int -> [Ident] -> JStat
loadSkipI = loadSkipIFrom sp

loadSkipIFrom :: JExpr -> Int -> [Ident] -> JStat
loadSkipIFrom fr n xs = mconcat items
    where
      items = reverse $ zipWith f [(0::Int)..] (reverse xs)
      offset 0 = fr
      offset n = InfixExpr SubOp fr (toJExpr n)
      f i ex = ex ||= IdxExpr stack (toJExpr (offset (i+n)))

popn :: Int -> G JStat
popn n = addUnknownSlots n >> adjSpN n


assertRtsStat :: G JStat -> G JStat
assertRtsStat stat = do
  s <- State.gets gsSettings
  if csAssertRts s then stat else pure mempty

getSettings :: G StgToJSConfig
getSettings = State.gets gsSettings

getGlobalIdCache :: G GlobalIdCache
getGlobalIdCache = State.gets (ggsGlobalIdCache . gsGroup)

updateThunk' :: StgToJSConfig -> JStat
updateThunk' settings =
  if csInlineBlackhole settings
    then bhStats settings True
    else ApplStat (var "h$bh") []

-- | Generate statemeents to update the current node with a blackhole
bhStats :: StgToJSConfig -> Bool -> JStat
bhStats s pushUpd = mconcat
  [ if pushUpd then push' s [r1, var "h$upd_frame"] else mempty
  , toJExpr R1 .^ closureEntry_  |= var "h$blackhole"
  , toJExpr R1 .^ closureField1_ |= var "h$currentThread"
  , toJExpr R1 .^ closureField2_ |= null_ -- will be filled with waiters array
  ]

updateThunk :: G JStat
updateThunk = do
  settings <- getSettings
  adjPushStack 2 -- update frame size
  return $ (updateThunk' settings)

-- | declare all js vars for the id
declIds :: Id -> G JStat
declIds  i
  | s == 0    = return mempty
  | s == 1    = DeclStat <$> jsIdI i
  | otherwise = mconcat <$> mapM (\n -> DeclStat <$> jsIdIN i n) [1..s]
  where
    s  = typeSize (idType i)

freshUnique :: G Int
freshUnique = do
  State.modify (\s -> s { gsId = gsId s + 1})
  State.gets gsId

liftToGlobal :: JStat -> G [(Ident, Id)]
liftToGlobal jst = do
  GlobalIdCache gidc <- getGlobalIdCache
  let sids  = filter (`M.member` gidc) (identsS jst)
      cnt   = M.fromListWith (+) (map (,(1::Integer)) sids)
      sids' = L.sortBy (compare `on` (cnt M.!)) (nub' sids)
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
