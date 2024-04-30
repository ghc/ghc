{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.JS.Stack
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Jeffrey Young  <jeffrey.young@iohk.io>
--                Luite Stegeman <luite.stegeman@iohk.io>
--                Sylvain Henry  <sylvain.henry@iohk.io>
--                Josh Meredith  <josh.meredith@iohk.io>
-- Stability   :  experimental
--
-- Utilities and wrappers for Stack manipulation in JS Land.
--
-- In general, functions suffixed with a tick do the actual work, functions
-- suffixed with an "I" are identical to the non-I versions but work on 'Ident's
--
-- The stack in JS land is held in the special JS array 'h$stack' and the stack
-- pointer is held in 'h$sp'. The top of the stack thus exists at
-- 'h$stack[h$sp]'. h$stack[h$sp + i] where i > 0, moves deeper into the stack
-- into older entries, whereas h$stack[h$sp - i] moves towards the top of the
-- stack.
--
-- The stack layout algorithm is slightly peculiar. It makes an effort to
-- remember recently popped things so that if these values need to be pushed
-- then they can be quickly. The implementation for this is storing these values
-- above the stack pointer, and the pushing will skip slots that we know we will
-- use and fill in slots marked as unknown. Thus, you may find that our push and
-- pop functions do some non-traditional stack manipulation such as adding slots
-- in pop or removing slots in push.
-----------------------------------------------------------------------------

module GHC.StgToJS.Stack
  ( resetSlots
  , isolateSlots
  , setSlots
  , getSlots
  , addSlots
  , dropSlots
  , addUnknownSlots
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
  , popN
  , popSkip
  , popSkipI
  , loadSkip
  -- * Thunk update
  , updateThunk
  , updateThunk'
  , bhStats
  )
where

import GHC.Prelude

import GHC.JS.JStg.Syntax
import GHC.JS.Make
import GHC.JS.Ident

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

-- | Run the action, 'm', with no stack info
resetSlots :: G a -> G a
resetSlots m = do
  s <- getSlots
  d <- getStackDepth
  setSlots []
  a <- m
  setSlots s
  setStackDepth d
  return a

-- | run the action, 'm', with current stack info, but don't let modifications
-- propagate
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

-- | drop 'n' slots from our stack knowledge
dropSlots :: Int -> G ()
dropSlots n = modifySlots (drop n)

push :: [JStgExpr] -> G JStgStat
push xs = do
  dropSlots (length xs)
  modifyStackDepth (+ (length xs))
  flip push' xs <$> getSettings

push' :: StgToJSConfig -> [JStgExpr] -> JStgStat
push' _ [] = mempty
push' cs xs
   | csInlinePush cs || l > 32 || l < 2 = adjSp' l <> mconcat items
   | otherwise                          = ApplStat (toJExpr $ pushN ! l) xs
  where
    items = zipWith f [(1::Int)..] xs
    offset i | i == l    = sp
             | otherwise = InfixExpr SubOp sp (toJExpr (l - i))
    l = length xs
    f i e = AssignStat ((IdxExpr stack) (toJExpr (offset i))) AssignOp (toJExpr e)


-- | Grow the stack pointer by 'n' without modifying the stack depth. The stack
-- is just a JS array so we add to grow (instead of the traditional subtract)
adjSp' :: Int -> JStgStat
adjSp' 0 = mempty
adjSp' n = sp |= InfixExpr AddOp sp (toJExpr n)

-- | Shrink the stack pointer by 'n'. The stack grows downward so substract
adjSpN' :: Int -> JStgStat
adjSpN' 0 = mempty
adjSpN' n = sp |= InfixExpr SubOp sp (toJExpr n)

-- | Wrapper which adjusts the stack pointer /and/ modifies the stack depth
-- tracked in 'G'. See also 'adjSp'' which actually does the stack pointer
-- manipulation.
adjSp :: Int -> G JStgStat
adjSp 0 = return mempty
adjSp n = do
  -- grow depth by n
  modifyStackDepth (+n)
  return (adjSp' n)

-- | Shrink the stack and stack pointer. NB: This function is unsafe when the
-- input 'n', is negative. This function wraps around 'adjSpN' which actually
-- performs the work.
adjSpN :: Int -> G JStgStat
adjSpN 0 = return mempty
adjSpN n = do
  modifyStackDepth (\x -> x - n)
  return (adjSpN' n)

-- | A constant array that holds global function symbols which do N pushes onto
-- the stack. For example:
-- @
-- function h$p1(x1) {
--   ++h$sp;
--   h$stack[(h$sp - 0)] = x1;
-- };
-- function h$p2(x1, x2) {
--   h$sp += 2;
--   h$stack[(h$sp - 1)] = x1;
--   h$stack[(h$sp - 0)] = x2;
-- };
-- @
--
-- and so on up to 32.
pushN :: Array Int Ident
pushN = listArray (1,32) $ map (global . mkFastString . ("h$p"++) . show) [(1::Int)..32]

-- | Convert all function symbols in 'pushN' to global top-level functions. This
-- is a hack which converts the function symbols to variables. This hack is
-- caught in 'GHC.StgToJS.Printer.prettyBlock'' to turn these into global
-- functions.
pushN' :: Array Int JStgExpr
pushN' = fmap (ValExpr . JVar) pushN

-- | Partial Push functions. Like 'pushN' except these push functions skip
-- slots. For example,
-- @
-- function h$pp33(x1, x2) {
--   h$sp += 6;
--   h$stack[(h$sp - 5)] = x1;
--   h$stack[(h$sp - 0)] = x2;
-- };
-- @
--
-- The 33rd entry skips slots 1-4 to bind the top of the stack and the 6th
-- slot. See 'pushOptimized' and 'pushOptimized'' for use cases.
pushNN :: Array Integer Ident
pushNN = listArray (1,255) $ map (global . mkFastString . ("h$pp"++) . show) [(1::Int)..255]

-- | Like 'pushN'' but for the partial push functions
pushNN' :: Array Integer JStgExpr
pushNN' = fmap (ValExpr . JVar) pushNN

pushOptimized' :: [(Id,Int)] -> G JStgStat
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
pushOptimized :: [(JStgExpr,Bool)] -- ^ contents of the slots, True if same value is already there
              -> G JStgStat
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

-- | push a let-no-escape frame onto the stack
pushLneFrame :: HasDebugCallStack => Int -> ExprCtx -> G JStgStat
pushLneFrame size ctx =
  let ctx' = ctxLneShrinkStack ctx size
  in pushOptimized' (ctxLneFrameVars ctx')

-- | Pop things, don't update the stack knowledge in 'G'
popSkip :: Int      -- ^ number of slots to skip
         -> [JStgExpr] -- ^ assign stack slot values to these
         -> JStgStat
popSkip 0 []  = mempty
popSkip n []  = adjSpN' n
popSkip n tgt = loadSkip n tgt <> adjSpN' (length tgt + n)

-- | Load 'length (xs :: [JStgExpr])' things from the stack at offset 'n :: Int'.
-- This function does no stack pointer manipulation, it merely indexes into the
-- stack and loads payloads into 'xs'.
loadSkip :: Int -> [JStgExpr] -> JStgStat
loadSkip = loadSkipFrom sp
  where
    loadSkipFrom :: JStgExpr -> Int -> [JStgExpr] -> JStgStat
    loadSkipFrom fr n xs = mconcat items
      where
        items = reverse $ zipWith f [(0::Int)..] (reverse xs)
        -- helper to generate sp - n offset to index with
        offset 0 = fr
        offset n = InfixExpr SubOp fr  (toJExpr n)
        -- helper to load stack .! i into ex, e.g., ex = stack[i]
        f i ex   = ex |= IdxExpr stack (toJExpr (offset (i+n)))


-- | Pop but preserve the first N slots
popSkipI :: Int -> [(Ident,StackSlot)] -> G JStgStat
popSkipI 0 [] = pure mempty
popSkipI n [] = popN n
popSkipI n xs = do
  -- add N unknown slots
  addUnknownSlots n
  -- now add the slots from xs, after this line the stack should look like
  -- [xs] ++ [Unknown...] ++ old_stack
  addSlots (map snd xs)
  -- move stack pointer into the stack by (length xs + n), basically resetting
  -- the stack pointer
  a <- adjSpN (length xs + n)
  -- now load skipping first N slots
  return (loadSkipI n (map fst xs) <> a)

-- | Just like 'loadSkip' but operate on 'Ident's rather than 'JStgExpr'
loadSkipI :: Int -> [Ident] -> JStgStat
loadSkipI = loadSkipIFrom sp
  where loadSkipIFrom :: JStgExpr -> Int -> [Ident] -> JStgStat
        loadSkipIFrom fr n xs = mconcat items
          where
            items = reverse $ zipWith f [(0::Int)..] (reverse xs)
            offset 0 = fr
            offset n = InfixExpr SubOp fr (toJExpr n)
            f i ex   = ex ||= IdxExpr stack (toJExpr (offset (i+n)))

-- | Blindly pop N slots
popN :: Int -> G JStgStat
popN n = addUnknownSlots n >> adjSpN n

-- | Generate statements to update the current node with a blackhole
bhStats :: StgToJSConfig -> Bool -> JStgStat
bhStats s pushUpd = mconcat
  [ if pushUpd then push' s [r1, var "h$upd_frame"] else mempty
  , toJExpr R1 .^ closureInfo_   |= var "h$blackhole"
  , toJExpr R1 .^ closureField1_ |= var "h$currentThread"
  , toJExpr R1 .^ closureField2_ |= null_ -- will be filled with waiters array
  ]

-- | Wrapper around 'updateThunk'', performs the stack manipulation before
-- updating the Thunk.
updateThunk :: G JStgStat
updateThunk = do
  settings <- getSettings
  -- update frame size
  let adjPushStack :: Int -> G ()
      adjPushStack n = do modifyStackDepth (+n)
                          dropSlots n
  adjPushStack 2
  return $ (updateThunk' settings)

-- | Update a thunk by checking 'StgToJSConfig'. If the config inlines black
-- holes then update inline, else make an explicit call to the black hole
-- handler.
updateThunk' :: StgToJSConfig -> JStgStat
updateThunk' settings =
  if csInlineBlackhole settings
    then bhStats settings True
    else ApplStat (var "h$bh") []
