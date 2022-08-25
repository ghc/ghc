{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Stack manipulation
module GHC.StgToJS.Stack
  ( resetSlots
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
  -- * Thunk update
  , updateThunk
  , updateThunk'
  , bhStats
  )
where

import GHC.Prelude

import GHC.JS.Syntax
import GHC.JS.Make

import GHC.StgToJS.Types
import GHC.StgToJS.Monad
import GHC.StgToJS.Ids
import GHC.StgToJS.ExprCtx
import GHC.StgToJS.Heap
import GHC.StgToJS.Regs

import GHC.Types.Id
import GHC.Utils.Misc
import GHC.Data.FastString

import qualified Data.Bits as Bits
import qualified Data.List as L
import qualified Control.Monad.Trans.State.Strict as State
import Data.Array
import Data.Monoid
import Control.Monad

-- | Run the action with no stack info
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
pushN = listArray (1,32) $ map (TxtI . mkFastString . ("h$p"++) . show) [(1::Int)..32]

pushN' :: Array Int JExpr
pushN' = fmap (ValExpr . JVar) pushN

pushNN :: Array Integer Ident
pushNN = listArray (1,255) $ map (TxtI . mkFastString . ("h$pp"++) . show) [(1::Int)..255]

pushNN' :: Array Integer JExpr
pushNN' = fmap (ValExpr . JVar) pushNN

pushOptimized' :: [(Id,Int)] -> G JStat
pushOptimized' xs = do
  slots  <- getSlots
  pushOptimized =<< (zipWithM f xs (slots++repeat SlotUnknown))
  where
    f (i1,n1) xs2 = do
      xs <- varsForId i1
      let !id_n1 = xs !! (n1-1)

      case xs2 of
        SlotId i2 n2 -> pure (id_n1,i1==i2&&n1==n2)
        _            -> pure (id_n1,False)

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

updateThunk' :: StgToJSConfig -> JStat
updateThunk' settings =
  if csInlineBlackhole settings
    then bhStats settings True
    else ApplStat (var "h$bh") []

-- | Generate statements to update the current node with a blackhole
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
