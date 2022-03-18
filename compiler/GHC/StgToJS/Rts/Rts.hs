{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -O0 #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.StgToJS.Rts.Rts
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Jeffrey Young  <jeffrey.young@iohk.io>
--                Luite Stegeman <luite.stegeman@iohk.io>
--                Sylvain Henry  <sylvain.henry@iohk.io>
-- Stability   :  experimental
--
-- Top level driver of the JavaScript Backend RTS. This file is an
-- implementation of the JS RTS for the JS backend written as an EDSL in
-- Haskell. It assumes the existence of pre-generated JS functions, included as
-- js-sources...
--
-- FIXME: Jeff (2022,03): Finish module description. Specifically:
-- 1. Since this is the top level module for the RTS, what is the architecture
-- of the RTS? How does it all hold together? Describe the memory layout, any
-- other tricks the RTS plays, and relevant sibling modules
-- Sylvain (2022/03): memory layout is described in GHC.StgToJS (WIP)
--
-----------------------------------------------------------------------------

module GHC.StgToJS.Rts.Rts where

import GHC.Prelude

import GHC.JS.Syntax
import GHC.JS.Make
import GHC.JS.Transform

import GHC.StgToJS.Apply
import GHC.StgToJS.Closure
import GHC.StgToJS.Heap
import GHC.StgToJS.Monad
import GHC.StgToJS.Profiling
import GHC.StgToJS.Regs
import GHC.StgToJS.Types

import qualified GHC.Data.ShortText as T

import Data.Array
import Data.Monoid
import Data.Char (toLower, toUpper)
import qualified Data.Bits          as Bits
import qualified Data.Map           as M


-----------------------------------------------------------------------------
--
-- Pre-generated RTS for the JS backend
--
-- TODO: Jeff (2022,03):

-- 1. There are numerous string literals sprinkled throughout the RTS, these
-- should be moved and isolated into a single module and then used throughout
-- the RTS and StgToJS Pipeline
--
-- 2. The RTS makes a lot of use of the Monoid instances on lists since the
-- Haskell portion is essentially building a JavaScript AST for the JS Rts and
-- then pretty printing it so it can be used by the js backend. However, all
-- this merging on lists is going to be extremely inefficient. (++) is O(n^2)
-- and furthermore we have nested list structures. This implies a better data
-- structure with an emphasis on fast merging is likely to reduce compile times
-- for this RTS.
--
-- 3. Similar to (2), most of the RTS is a function foo :: <something> with
-- definition foo [...<something>...] = mconcat ...<the body is JS land>... This
-- is fine, however it implies a monadic design for this EDSL might lead to more
-- readable code. Or in other words, `mconcat` and friends are just boiler
-- plate, and what we really have is a monadic EDSL where the monad is a kind of
-- Writer monad. Which we have essentially recreated here since bind in the
-- Writer monad is mapConcat and you'll notice that most of the functions in the
-- RTS do exactly that, i.e., apply a function which generates a list, then
-- concats. So we are dealing with a Writer monad but aren't using Haskell's
-- language facilities to be explicit about it. Hence all the boilerplate. Side
-- note, we might also consider two alternative approaches if we go with a
-- monadic design:
-- -- a. Continuation passing style so that intermediate lists fuse
-- -- b. A writer monad with a difference list, this would essentially be a
-- -- zipper but whether it is worth it or not depend on how often children need
-- -- to access their siblings, if they do that a lot then we'll have huge
-- -- speedups, if not then we likely won't gain anything

-----------------------------------------------------------------------------

garbageCollector :: JStat
garbageCollector =
  mconcat [ TxtI "h$resetRegisters"  ||= jLam (mconcat $ map resetRegister [minBound..maxBound])
          , TxtI "h$resetResultVars" ||= jLam (mconcat $ map resetResultVar [minBound..maxBound])
          ]


resetRegister :: StgReg -> JStat
resetRegister r = toJExpr r |= null_

resetResultVar :: StgRet -> JStat
resetResultVar r = toJExpr r |= null_

{-
  use h$c1, h$c2, h$c3, ... h$c24 instead of making objects manually so layouts
  and fields can be changed more easily
 -}
closureConstructors :: StgToJSConfig -> JStat
closureConstructors s = BlockStat
  [ declClsConstr "h$c" ["f"] $ Closure
      { clEntry  = var "f"
      , clField1 = null_
      , clField2 = null_
      , clMeta   = 0
      , clCC     = ccVal
      }
    -- FIXME: same as h$c, maybe remove one of them?
  , declClsConstr "h$c0" ["f"] $ Closure
      { clEntry  = var "f"
      , clField1 = null_
      , clField2 = null_
      , clMeta   = 0
      , clCC     = ccVal
      }
  , declClsConstr "h$c1" ["f", "x1"] $ Closure
      { clEntry  = var "f"
      , clField1 = var "x1"
      , clField2 = null_
      , clMeta   = 0
      , clCC     = ccVal
      }
  , declClsConstr "h$c2" ["f", "x1", "x2"] $ Closure
      { clEntry  = var "f"
      , clField1 = var "x1"
      , clField2 = var "v2"
      , clMeta   = 0
      , clCC     = ccVal
      }
  , mconcat (map mkClosureCon [3..24])
  , mconcat (map mkDataFill [1..24])
  ]
  where
    prof = csProf s
    (ccArg,ccVal)
      -- the cc argument happens to be named just like the cc field...
      | prof      = ([TxtI closureCC_], Just (var closureCC_))
      | otherwise = ([], Nothing)
    addCCArg as = map TxtI as ++ ccArg
    addCCArg' as = as ++ ccArg

    declClsConstr i as cl = TxtI i ||= ValExpr (JFunc (addCCArg as)
      ( jVar $ \x ->
          [ checkC
          , x |= newClosure cl
          , notifyAlloc x
          , traceAlloc x
          , returnS x
          ]
         ))

    traceAlloc x | csTraceRts s = appS "h$traceAlloc" [x]
                 | otherwise    = mempty

    notifyAlloc x | csDebugAlloc s = appS "h$debugAlloc_notifyAlloc" [x]
                  | otherwise      = mempty

    -- only JSVal can typically contain undefined or null
    -- although it's possible (and legal) to make other Haskell types
    -- to contain JS refs directly
    -- this can cause false positives here
    checkC :: JStat
    checkC | csAssertRts s =
      jVar $ \msg ->
      jwhenS (var "arguments" .! 0 .!==. jString "h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e")
                                  (loop 1 (.<. var "arguments" .^ "length")
                                          (\i ->
                                             mconcat [msg |= jString "warning: undefined or null in argument: "
                                                       + i
                                                       + jString " allocating closure: " + (var "arguments" .! 0 .^ "n")
                                                     , appS "h$log" [msg]
                                                     , jwhenS (var "console" .&&. (var "console" .^ "trace")) ((var "console" .^ "trace") `ApplStat` [msg])
                                                     , postIncrS i
                                                     ])

                                  )
           | otherwise = mempty

    -- h$d is never used for JSVal (since it's only for constructors with
    -- at least three fields, so we always warn here
    checkD | csAssertRts s =
                     loop 0 (.<. var "arguments" .^ "length")
                     (\i -> jwhenS ((var "arguments" .! i .===. null_)
                                    .||. (var "arguments" .! i .===. undefined_))
                            (jVar $ \msg ->
                                mconcat [ msg |= jString "warning: undefined or null in argument: " + i + jString " allocating fields"
                                        , jwhenS (var "console" .&&. (var "console" .^ "trace"))
                                                ((var "console" .^ "trace") `ApplStat` [msg])
                                        ]))

           | otherwise = mempty

    mkClosureCon :: Int -> JStat
    mkClosureCon n = funName ||= toJExpr fun
      where
        funName = TxtI $ T.pack ("h$c" ++ show n) -- FIXME (Sylvain 2022-03): cache this
        -- args are: f x1 x2 .. xn [cc]
        args   = TxtI "f" : addCCArg' (map (TxtI . T.pack . ('x':) . show) [(1::Int)..n])
        fun    = JFunc args funBod
        -- x1 goes into closureField1. All the other args are bundled into an
        -- object in closureField2: { d1 = x2, d2 = x3, ... }
        --
        -- FIXME (Sylvain 2022-03): share code and comment with mkDataFill
        extra_args = ValExpr . JHash . M.fromList $ zip
                   -- FIXME (Sylvain 2002-03): use dataFieldCache and another
                   -- cache for "xN" names
                   (map (T.pack . ('d':) . show) [(1::Int)..])
                   (map (toJExpr . TxtI . T.pack . ('x':) . show) [2..n])

        funBod = jVar $ \x ->
            [ checkC
            , x |= newClosure Closure
               { clEntry  = var "f"
               , clField1 = var "x1"
               , clField2 = extra_args
               , clMeta   = 0
               , clCC     = ccVal
               }
            , notifyAlloc x
            , traceAlloc x
            , returnS x
            ]

    mkDataFill :: Int -> JStat
    mkDataFill n = funName ||= toJExpr fun
      where
        -- FIXME (Sylvain 2002-03): use dataFieldCache and dataCache
        funName    = TxtI $ T.pack ("h$d" ++ show n)
        ds         = map (T.pack . ('d':) . show) [(1::Int)..n]
        extra_args = ValExpr . JHash . M.fromList . zip ds $ map (toJExpr . TxtI) ds
        fun        = JFunc (map TxtI ds) (checkD <> returnS extra_args)

stackManip :: JStat
stackManip = mconcat (map mkPush [1..32]) <>
             mconcat (map mkPpush [1..255])
  where
    mkPush :: Int -> JStat
    mkPush n = let funName = TxtI $ T.pack ("h$p" ++ show n)
                   as      = map (TxtI . T.pack . ('x':) . show) [1..n]
                   fun     = JFunc as ((sp |= sp + toJExpr n)
                                       <> mconcat (zipWith (\i a -> stack .! (sp - toJExpr (n-i)) |= toJExpr a)
                                                   [1..] as))
               in funName ||= toJExpr fun

    -- | partial pushes, based on bitmap, increases Sp by highest bit
    mkPpush :: Integer -> JStat
    mkPpush sig | sig Bits..&. (sig+1) == 0 = mempty -- already handled by h$p
    mkPpush sig = let funName = TxtI $ T.pack ("h$pp" ++ show sig)
                      bits    = bitsIdx sig
                      n       = length bits
                      h       = last bits
                      args    = map (TxtI . T.pack . ('x':) . show) [1..n]
                      fun     = JFunc args $
                        mconcat [ sp |= sp + toJExpr (h+1)
                                , mconcat (zipWith (\b a -> stack .! (sp - toJExpr (h-b)) |= toJExpr a) bits args)
                                ]
                   in funName ||= toJExpr fun

bitsIdx :: Integer -> [Int]
bitsIdx n | n < 0 = error "bitsIdx: negative"
          | otherwise = go n 0
  where
    go 0 _ = []
    go m b | Bits.testBit m b = b : go (Bits.clearBit m b) (b+1)
           | otherwise   = go (Bits.clearBit m b) (b+1)

bhLneStats :: StgToJSConfig -> JExpr -> JExpr -> JStat
bhLneStats _s p frameSize =
   jVar $ \v ->
            mconcat [ v |= stack .! p
                    , ifS v
                      ((sp |= sp - frameSize)
                       <> ifS (v .===. var "h$blackhole")
                                (returnS $ app "h$throw" [var "h$baseZCControlziExceptionziBasezinonTermination", false_])
                                (mconcat [r1 |= v
                                         , sp |= sp - frameSize
                                         , returnStack
                                         ]))
                      ((stack .! p |= var "h$blackhole") <> returnS null_)
                    ]


-- FIXME move somewhere else
declRegs :: JStat
-- FIXME prevent holes
declRegs =
  mconcat [ TxtI "h$regs" ||= toJExpr (JList [])
          , mconcat (map declReg (enumFromTo R1 R32))
          , regGettersSetters
          , loadRegs
          ]
    where
      declReg r = (decl . TxtI . T.pack . ("h$"++) . map toLower . show) r
                  <> BlockStat [AssignStat (toJExpr r) (ValExpr (JInt 0))] -- [j| `r` = 0; |]

regGettersSetters :: JStat
regGettersSetters =
  mconcat [ TxtI "h$getReg" ||= jLam (\n   -> SwitchStat n getRegCases mempty)
          , TxtI "h$setReg" ||= jLam (\n v -> SwitchStat n (setRegCases v) mempty)
          ]
  where
    getRegCases =
      map (\r -> (toJExpr (jsRegToInt r) , returnS (toJExpr r))) (enumFrom R1)
    setRegCases v =
      map (\r -> (toJExpr (jsRegToInt r), (toJExpr r |= toJExpr v) <> returnS undefined_)) (enumFrom R1)

loadRegs :: JStat
loadRegs = mconcat $ map mkLoad [1..32]
  where
    mkLoad :: Int -> JStat
    mkLoad n = let args   = map (TxtI . T.pack . ("x"++) . show) [1..n]
                   assign = zipWith (\a r -> toJExpr r |= toJExpr a)
                                -- FIXME: Jeff (2022,03) the use of reverse,
                                -- take, and enumFrom here heavily implies
                                -- Data.Sequence would be a better data
                                -- structure to hold the regs. Or perhaps we
                                -- steal the indices from the registers array?
                                -- Either way we can avoid allocating this
                                -- intermediate `enumFrom R1` list
                              args (reverse $ take n (enumFrom R1))
                   fname  = TxtI $ T.pack ("h$l" ++ show n)
                   fun    = JFunc args (mconcat assign)
               in fname ||= toJExpr fun

-- assign registers R1 ... Rn
-- assigns Rn first
assignRegs :: StgToJSConfig -> [JExpr] -> JStat
assignRegs _ [] = mempty
assignRegs s xs
  | l <= 32 && not (csInlineLoadRegs s)
      = ApplStat (ValExpr (JVar $ assignRegs'!l)) (reverse xs)
  | otherwise = mconcat . reverse $
      zipWith (\r ex -> toJExpr r |= ex) (take l $ enumFrom R1) xs
  where
    l = length xs

assignRegs' :: Array Int Ident
assignRegs' = listArray (1,32) (map (TxtI . T.pack . ("h$l"++) . show) [(1::Int)..32])

declRets :: JStat
declRets = mconcat $ map (decl . TxtI . T.pack . ("h$"++) . map toLower . show) (enumFrom Ret1)

trace :: ToJExpr a => a -> JStat
trace ex = appS "h$log" [toJExpr ex]

closureTypes :: JStat
closureTypes = mconcat (map mkClosureType (enumFromTo minBound maxBound)) <> closureTypeName
  where
    mkClosureType :: ClosureType -> JStat
    mkClosureType c = let s = TxtI . T.pack $ "h$" ++ map toUpper (show c) ++ "_CLOSURE"
                      in  s ||= toJExpr c
    closureTypeName :: JStat
    closureTypeName =
      TxtI "h$closureTypeName" ||= jLam (\c ->
                                           mconcat (map (ifCT c) [minBound..maxBound])
                                          <> returnS (jString "InvalidClosureType"))

    ifCT :: JExpr -> ClosureType -> JStat
    ifCT arg ct = jwhenS (arg .===. toJExpr ct) (returnS (toJExpr (show ct)))

rtsDecls :: JStat
rtsDecls = jsSaturate (Just "h$RTSD") $
  mconcat [ TxtI "h$currentThread"   ||= null_                   -- thread state object for current thread
          , TxtI "h$stack"           ||= null_                   -- stack for the current thread
          , TxtI "h$sp"              ||= 0                       -- stack pointer for the current thread
          , TxtI "h$initStatic"      ||= toJExpr (JList [])      -- we need delayed initialization for static objects, push functions here to be initialized just before haskell runs
          , TxtI "h$staticThunks"    ||= toJExpr (jhFromList []) --  funcName -> heapidx map for srefs
          , TxtI "h$staticThunksArr" ||= toJExpr (JList [])      -- indices of updatable thunks in static heap
          -- stg registers
          , declRegs
          , declRets]

rts :: StgToJSConfig -> JStat
rts = jsSaturate (Just "h$RTS") . rts'

rts' :: StgToJSConfig -> JStat
rts' s =
  mconcat [ closureConstructors s
          , garbageCollector
          , stackManip
          -- settings (FIXME should be const)
          , TxtI "h$rts_traceForeign" ||= toJExpr (csTraceForeign s)
          , TxtI "h$rts_profiling"    ||= toJExpr (csProf s)
          -- closure types (FIXME should be const)
          , TxtI "h$ct_fun"        ||= toJExpr Fun
          , TxtI "h$ct_con"        ||= toJExpr Con
          , TxtI "h$ct_thunk"      ||= toJExpr Thunk
          , TxtI "h$ct_pap"        ||= toJExpr Pap
          , TxtI "h$ct_blackhole"  ||= toJExpr Blackhole
          , TxtI "h$ct_stackframe" ||= toJExpr StackFrame
          -- var / closure field types (FIXME should be const)
          , TxtI "h$vt_ptr"    ||= toJExpr PtrV
          , TxtI "h$vt_void"   ||= toJExpr VoidV
          , TxtI "h$vt_double" ||= toJExpr IntV
          , TxtI "h$vt_long"   ||= toJExpr LongV
          , TxtI "h$vt_addr"   ||= toJExpr AddrV
          , TxtI "h$vt_rtsobj" ||= toJExpr RtsObjV
          , TxtI "h$vt_obj"    ||= toJExpr ObjV
          , TxtI "h$vt_arr"    ||= toJExpr ArrV
          , TxtI "h$bh"        ||= jLam (bhStats s True)
          , TxtI "h$bh_lne"    ||= jLam (\x frameSize -> bhLneStats s x frameSize)
          , closure (ClosureInfo "h$blackhole" (CIRegs 0 []) "blackhole" (CILayoutUnknown 2) CIBlackhole mempty)
               (appS "throw" [jString "oops: entered black hole"])
          , closure (ClosureInfo "h$blackholeTrap" (CIRegs 0 []) "blackhole" (CILayoutUnknown 2) CIThunk mempty)
               (appS "throw" [jString "oops: entered multiple times"])
          , closure (ClosureInfo "h$done" (CIRegs 0 [PtrV]) "done" (CILayoutUnknown 0) CIStackFrame mempty)
               (appS "h$finishThread" [var "h$currentThread"] <> returnS (var "h$reschedule"))
          , closure (ClosureInfo "h$doneMain_e" (CIRegs 0 [PtrV]) "doneMain" (CILayoutUnknown 0) CIStackFrame mempty)
               (returnS (var "h$doneMain"))
          , conClosure "h$false_e" "GHC.Types.False" (CILayoutFixed 0 []) 1
          , conClosure "h$true_e"  "GHC.Types.True"  (CILayoutFixed 0 []) 2
          , conClosure "h$integerzmwiredzminZCGHCziIntegerziTypeziSzh_con_e" "GHC.Integer.Type.S#" (CILayoutFixed 1 [IntV]) 1
          , conClosure "h$integerzmwiredzminZCGHCziIntegerziTypeziJpzh_con_e" "GHC.Integer.Type.Jp#" (CILayoutFixed 1 [ObjV]) 2
          , conClosure "h$integerzmwiredzminZCGHCziIntegerziTypeziJnzh_con_e" "GHC.Integer.Type.Jn#" (CILayoutFixed 1 [ObjV]) 3
          -- generic data constructor with 1 non-heapobj field
          , conClosure "h$data1_e" "data1" (CILayoutFixed 1 [ObjV]) 1
          -- generic data constructor with 2 non-heapobj fields
          , conClosure "h$data2_e" "data2" (CILayoutFixed 2 [ObjV,ObjV]) 1
          , closure (ClosureInfo "h$noop_e" (CIRegs 1 [PtrV]) "no-op IO ()" (CILayoutFixed 0 []) (CIFun 1 0) mempty)
               (returnS (stack .! sp))
            <> (TxtI "h$noop" ||= ApplExpr (var "h$c0") (var "h$noop_e" : [jSystemCCS | csProf s]))
          , closure (ClosureInfo "h$catch_e" (CIRegs 0 [PtrV]) "exception handler" (CILayoutFixed 2 [PtrV,IntV]) CIStackFrame mempty)
               (adjSpN' 3 <> returnS (stack .! sp))
          , closure (ClosureInfo "h$dataToTag_e" (CIRegs 0 [PtrV]) "data to tag" (CILayoutFixed 0 []) CIStackFrame mempty)
                $ mconcat [ r1 |= if_ (r1 .===. true_) 1 (if_ (typeof r1 .===. jTyObject) (r1 .^ "f" .^ "a" - 1) 0)
                          , adjSpN' 1
                          , returnS (stack .! sp)
                          ]
          -- function application to one argument
          , closure (ClosureInfo "h$ap1_e" (CIRegs 0 [PtrV]) "apply1" (CILayoutFixed 2 [PtrV, PtrV]) CIThunk mempty)
               (jVar $ \d1 d2 ->
                   mconcat [ d1 |= closureField1 r1
                           , d2 |= closureField2 r1
                           , appS "h$bh" []
                           , profStat s enterCostCentreThunk
                           , r1 |= d1
                           , r2 |= d2
                           , returnS (app "h$ap_1_1_fast" [])
                           ])
          -- function application to two arguments
          , closure (ClosureInfo "h$ap2_e" (CIRegs 0 [PtrV]) "apply2" (CILayoutFixed 3 [PtrV, PtrV, PtrV]) CIThunk mempty)
               (jVar $ \d1 d2 d3 ->
                   mconcat [ d1 |= closureField1 r1
                           , d2 |= closureField2 r1 .^ "d1" -- FIXME (Sylvain 2022-03): extra args are named like closureFieldN... not so good! Find something else
                           , d3 |= closureField2 r1 .^ "d2"
                           , appS "h$bh" []
                           , profStat s enterCostCentreThunk
                           , r1 |= d1
                           , r2 |= d2
                           , r3 |= d3
                           , returnS (app "h$ap_2_2_fast" [])
                           ])
          -- function application to three arguments
          , closure (ClosureInfo "h$ap3_e" (CIRegs 0 [PtrV]) "apply3" (CILayoutFixed 4 [PtrV, PtrV, PtrV, PtrV]) CIThunk mempty)
               (jVar $ \d1 d2 d3 d4 ->
                   mconcat [ d1 |= closureField1 r1
                           , d2 |= closureField2 r1 .^ "d1"
                           , d3 |= closureField2 r1 .^ "d2"
                           , d4 |= closureField2 r1 .^ "d3"
                           , appS "h$bh" []
                           , r1 |= d1
                           , r2 |= d2
                           , r3 |= d3
                           , r4 |= d4
                           , returnS (app "h$ap_3_3_fast" [])
                           ])
          -- select first field
          , closure (ClosureInfo "h$select1_e" (CIRegs 0 [PtrV]) "select1" (CILayoutFixed 1 [PtrV]) CIThunk mempty)
               (jVar $ \t ->
                   mconcat [ t |= closureField1 r1
                           , adjSp' 3
                           , stack .! (sp - 2) |= r1
                           , stack .! (sp - 1) |= var "h$upd_frame"
                           , stack .! sp |= var "h$select1_ret"
                           , closureEntry  r1 |= var "h$blackhole"
                           , closureField1 r1 |= var "h$currentThread"
                           , closureField2 r1 |= null_
                           , r1 |= t
                           , returnS (app "h$ap_0_0_fast" [])
                           ])
          , closure (ClosureInfo "h$select1_ret" (CIRegs 0 [PtrV]) "select1ret" (CILayoutFixed 0 []) CIStackFrame mempty)
               ((r1 |= closureField1 r1)
                <> adjSpN' 1
                <> returnS (app "h$ap_0_0_fast" [])
               )
          -- select second field of a two-field constructor
          , closure (ClosureInfo "h$select2_e" (CIRegs 0 [PtrV]) "select2" (CILayoutFixed 1 [PtrV]) CIThunk mempty)
               (jVar $ \t ->
                   mconcat [t |= closureField1 r1
                           , adjSp' 3
                           , stack .! (sp - 2) |= r1
                           , stack .! (sp - 1) |= var "h$upd_frame"
                           , stack .! sp |= var "h$select2_ret"
                           , closureEntry  r1 |= var "h$blackhole"
                           , closureField1 r1 |= var "h$currentThread"
                           , closureField2 r1 |= null_
                           , r1 |= t
                           , returnS (app "h$ap_0_0_fast" [])
                           ]
                  )
          , closure (ClosureInfo "h$select2_ret" (CIRegs 0 [PtrV]) "select2ret" (CILayoutFixed 0 []) CIStackFrame mempty)
                        $ mconcat [ r1 |= closureField2 r1
                                  , adjSpN' 1
                                  , returnS (app "h$ap_0_0_fast" [])
                                  ]
          -- a thunk that just raises a synchronous exception
          , closure (ClosureInfo "h$raise_e" (CIRegs 0 [PtrV]) "h$raise_e" (CILayoutFixed 0 []) CIThunk mempty)
               (returnS (app "h$throw" [closureField1 r1, false_]))
          , closure (ClosureInfo "h$raiseAsync_e" (CIRegs 0 [PtrV]) "h$raiseAsync_e" (CILayoutFixed 0 []) CIThunk mempty)
               (returnS  (app "h$throw" [closureField1 r1, true_]))
          , closure (ClosureInfo "h$raiseAsync_frame" (CIRegs 0 []) "h$raiseAsync_frame" (CILayoutFixed 1 []) CIStackFrame mempty)
               (jVar $ \ex ->
                   mconcat [ ex |= stack .! (sp - 1)
                           , adjSpN' 2
                           , returnS (app "h$throw" [ex, true_])
                           ])
          {- reduce result if it's a thunk, follow if it's an ind
             add this to the stack if you want the outermost result
             to always be reduced to whnf, and not an ind
          -}
          , closure (ClosureInfo "h$reduce" (CIRegs 0 [PtrV]) "h$reduce" (CILayoutFixed 0 []) CIStackFrame mempty)
               (ifS (isThunk r1)
                    (returnS (r1 .^ "f"))
                    (adjSpN' 1 <> returnS (stack .! sp))
               )
          , rtsApply s
          , closureTypes
          , closure (ClosureInfo "h$runio_e" (CIRegs 0 [PtrV]) "runio" (CILayoutFixed 1 [PtrV]) CIThunk mempty)
                        $ mconcat [ r1 |= closureField1 r1
                                  , stack .! PreInc sp |= var "h$ap_1_0"
                                  , returnS (var "h$ap_1_0")
                                  ]
          , closure (ClosureInfo "h$flushStdout_e" (CIRegs 0 []) "flushStdout" (CILayoutFixed 0 []) CIThunk mempty)
                        $ mconcat [ r1 |= var "h$baseZCGHCziIOziHandlezihFlush"
                                  , r2 |= var "h$baseZCGHCziIOziHandleziFDzistdout"
                                  , returnS (app "h$ap_1_1_fast" [])
                                  ]
          , TxtI "h$flushStdout" ||= app "h$static_thunk" [var "h$flushStdout_e"]
          -- the scheduler pushes this frame when suspending a thread that
          -- has not called h$reschedule explicitly
          , closure (ClosureInfo "h$restoreThread" (CIRegs 0 []) "restoreThread" CILayoutVariable CIStackFrame mempty)
                (jVar $ \f frameSize nregs ->
                    mconcat [f |= stack .! (sp - 2)
                            , frameSize |= stack .! (sp - 1)
                            , nregs |= frameSize - 3
                            , loop 1 (.<=. nregs)
                                     (\i -> appS "h$setReg" [i, stack .! (sp - 2 - i)] <> postIncrS i)
                            , sp |= sp - frameSize
                            , returnS f
                            ])
          -- return a closure in the stack frame to the next thing on the stack
          , closure (ClosureInfo "h$return" (CIRegs 0 []) "return" (CILayoutFixed 1 [PtrV]) CIStackFrame mempty)
                ((r1 |= stack .! (sp - 1))
                 <> adjSpN' 2
                 <> returnS (stack .! sp))
          --  return a function in the stack frame for the next call
          , closure (ClosureInfo "h$returnf" (CIRegs 0 [PtrV]) "returnf" (CILayoutFixed 1 [ObjV]) CIStackFrame mempty)
                (jVar $ \r ->
                    mconcat [ r |= stack .! (sp - 1)
                            , adjSpN' 2
                            , returnS r
                            ])
          -- return this function when the scheduler needs to come into action
          -- (yield, delay etc), returning thread needs to push all relevant
          -- registers to stack frame, thread will be resumed by calling the stack top
          , closure (ClosureInfo "h$reschedule" (CIRegs 0 []) "reschedule" (CILayoutFixed 0 []) CIThunk mempty)
                (returnS $ var "h$reschedule")
          -- debug thing, insert on stack to dump current result, should be boxed
          , closure (ClosureInfo "h$dumpRes" (CIRegs 0 [PtrV]) "dumpRes" (CILayoutFixed 1 [ObjV]) CIThunk mempty)
                (jVar $ \re ->
                    mconcat [ appS "h$log" [jString "h$dumpRes result: " + stack .! (sp-1)]
                            , appS "h$log" [r1]
                            , appS "h$log" [app "h$collectProps" [r1]]
                            , jwhenS ((r1 .^ "f") .&&. (r1 .^ "f" .^ "n"))
                                        (appS "h$log" [jString "name: " + r1 .^ "f" .^ "n"])
                            , jwhenS (ApplExpr (r1 .^ "hasOwnProperty") [jString closureField1_])
                                        (appS "h$log" [jString "d1: " + closureField1 r1])
                            , jwhenS (ApplExpr (r1 .^ "hasOwnProperty") [jString closureField2_])
                                        (appS "h$log" [jString "d2: " + closureField2 r1])
                            , jwhenS (r1 .^ "f") $ mconcat
                                [ re |= New (app "RegExp" [jString "([^\\n]+)\\n(.|\\n)*"])
                                , appS "h$log" [jString "function"
                                                + ApplExpr (ApplExpr ((jString "" + r1 .^ "f") .^ "substring") [0, 50] .^ "replace") [r1, jString "$1"]]
                                ]
                            , adjSpN' 2
                            , r1 |= null_
                            , returnS (stack .! sp)
                            ])
          , closure (ClosureInfo "h$resume_e" (CIRegs 0 [PtrV]) "resume" (CILayoutFixed 0 []) CIThunk mempty)
                  (jVar $ \ss ->
                      mconcat [ss |= closureField1 r1
                              , updateThunk' s
                              , loop 0 (.<. ss .^ "length") (\i -> (stack .! (sp+1+i) |= ss .! i)
                                                                   <> postIncrS i)
                              , sp |= sp + ss .^ "length"
                              , r1 |= null_
                              , returnS (stack .! sp)
                              ])
          , closure (ClosureInfo "h$unmaskFrame" (CIRegs 0 [PtrV]) "unmask" (CILayoutFixed 0 []) CIStackFrame mempty)
               ((var "h$currentThread" .^ "mask" |= 0)
                <> adjSpN' 1
                -- back to scheduler to give us async exception if pending
                <> ifS (var "h$currentThread" .^ "excep" .^ "length" .>. 0)
                    (push' s [r1, var "h$return"] <> returnS (var "h$reschedule"))
                    (returnS (stack .! sp)))
          , closure (ClosureInfo "h$maskFrame" (CIRegs 0 [PtrV]) "mask" (CILayoutFixed 0 []) CIStackFrame mempty)
                ((var "h$currentThread" .^ "mask" |= 2)
                 <> adjSpN' 1
                 <> returnS (stack .! sp))
          , closure (ClosureInfo "h$maskUnintFrame" (CIRegs 0 [PtrV]) "maskUnint" (CILayoutFixed 0 []) CIStackFrame mempty)
                ((var "h$currentThread" .^ "mask" |= 1)
                 <> adjSpN' 1
                 <> returnS (stack .! sp))
          , closure (ClosureInfo "h$unboxFFIResult" (CIRegs 0 [PtrV]) "unboxFFI" (CILayoutFixed 0 []) CIStackFrame mempty)
               (jVar $ \d ->
                   mconcat [d |= closureField1 r1
                           , loop 0 (.<. d .^ "length") (\i -> appS "h$setReg" [i + 1, d .! i] <> postIncrS i)
                           , adjSpN' 1
                           , returnS (stack .! sp)
                           ])
          , closure (ClosureInfo "h$unbox_e" (CIRegs 0 [PtrV]) "unboxed value" (CILayoutFixed 1 [DoubleV]) CIThunk mempty)
               ((r1 |= closureField1 r1) <> returnS (stack .! sp))
          , closure (ClosureInfo "h$retryInterrupted" (CIRegs 0 [ObjV]) "retry interrupted operation" (CILayoutFixed 1 [ObjV]) CIStackFrame mempty)
               (jVar $ \a ->
                   mconcat [ a |= stack .! (sp - 1)
                           , adjSpN' 2
                           , returnS (ApplExpr (a .! 0 .^ "apply") [var "this", ApplExpr (a .^ "slice") [1]])
                           ])
          , closure (ClosureInfo "h$atomically_e" (CIRegs 0 [PtrV]) "atomic operation" (CILayoutFixed 1 [PtrV]) CIStackFrame mempty)
               (ifS (app "h$stmValidateTransaction" [])
                    (appS "h$stmCommitTransaction" []
                     <> adjSpN' 2
                     <> returnS (stack .! sp))
                    (push' s [var "h$checkInvariants_e"]
                     <> returnS (app "h$stmStartTransaction" [stack .! (sp - 2)])))
          , closure (ClosureInfo "h$checkInvariants_e" (CIRegs 0 [PtrV]) "check transaction invariants" (CILayoutFixed 0 []) CIStackFrame mempty)
               (adjSpN' 1
                <> returnS (app "h$stmCheckInvariants" []))
          , closure (ClosureInfo "h$stmCheckInvariantStart_e" (CIRegs 0 []) "start checking invariant" (CILayoutFixed 2 [ObjV, RtsObjV]) CIStackFrame mempty)
                        (jVar $ \t inv m t1 ->
                            mconcat [ t   |= stack .! (sp - 2)
                                    , inv |= stack .! (sp - 1)
                                    , m   |= var "h$currentThread" .^ "mask"
                                    , adjSpN' 3
                                    , t1  |= UOpExpr NewOp (app "h$Transaction" [inv .^ "action", t])
                                    , t1 .^ "checkRead" |= UOpExpr NewOp (app "h$Set" [])
                                    , var "h$currentTread" .^ "transaction" |= t1
                                    , push' s [t1, m, var "h$stmInvariantViolatedHandler", var "h$catchStm_e"]
                                    , r1  |= inv .^ "action"
                                    , returnS (app "h$ap_1_0_fast" [])
                                    ])
          , closure (ClosureInfo "h$stmCheckInvariantResult_e" (CIRegs 0 [PtrV]) "finish checking invariant" (CILayoutFixed 1 [ObjV]) CIStackFrame mempty)
                        (jVar $ \inv ->
                            mconcat [ inv |= stack .! (sp -1)
                                    , adjSpN' 2
                                    , appS "h$stmUpdateInvariantDependencies" [inv]
                                    , appS "h$stmAbortTransaction" []
                                    , returnS (stack .! sp)
                                    ])

          -- update invariant TVar dependencies and rethrow exception
          -- handler must be pushed above h$stmCheckInvariantResult_e frame
          , closure (ClosureInfo "h$stmInvariantViolatedHandler_e" (CIRegs 0 [PtrV]) "finish checking invariant" (CILayoutFixed 0 []) (CIFun 2 1) mempty)
                        (jVar $ \inv ->
                            mconcat [ jwhenS (stack .! sp .===. var "h$stmCheckInvariantResult_e")
                                                (appS "throw" [jString "h$stmInvariantViolatedHandler_e: unexpected value on stack"])
                                    , inv |= stack .! (sp - 2)
                                    , adjSpN' 2
                                    , appS "h$stmUpdateInvariantDependencies" []
                                    , appS "h$stmAbortTransaction" []
                                    , returnS (app "h$throw" [r2, false_])
                                    ])
          , TxtI "h$stmInvariantViolatedHandler" ||= app "h$c" (var "h$stmInvariantViolatedHandler_e" : [jSystemCCS | csProf s])
          , closure (ClosureInfo "h$stmCatchRetry_e" (CIRegs 0 [PtrV]) "catch retry" (CILayoutFixed 1 [PtrV]) CIStackFrame mempty)
                        (adjSpN' 2
                         <> appS "h$stmCommitTransaction" []
                         <> returnS (stack .! sp))
          , closure (ClosureInfo "h$catchStm_e" (CIRegs 0 [PtrV]) "STM catch" (CILayoutFixed 3 [ObjV,PtrV,ObjV]) CIStackFrame mempty)
                       (adjSpN' 4 <> returnS (stack .! sp))
          , closure (ClosureInfo "h$stmResumeRetry_e" (CIRegs 0 [PtrV]) "resume retry" (CILayoutFixed 0 []) CIStackFrame mempty)
                        (jVar $ \blocked ->
                            mconcat [ jwhenS (stack .! (sp - 2) .!==. var "h$atomically_e")
                                                 (appS "throw" [jString "h$stmResumeRetry_e: unexpected value on stack"])
                                    , blocked |= stack .! (sp - 1)
                                    , adjSpN' 2
                                    , push' s [var "h$checkInvariants_e"]
                                    , appS "h$stmRemoveBlockedThread" [blocked, var "h$currentThread"]
                                    , returnS (app "h$stmStartTransaction" [stack .! (sp - 2)])
                                    ])
          , closure (ClosureInfo "h$lazy_e" (CIRegs 0 [PtrV]) "generic lazy value" (CILayoutFixed 0 []) CIThunk mempty)
                        (jVar $ \x ->
                            mconcat [x |= ApplExpr (closureField1 r1) []
                                    , appS "h$bh" []
                                    , profStat s enterCostCentreThunk
                                    , r1 |= x
                                    , returnS (stack .! sp)
                                    ])
          -- Top-level statements to generate only in profiling mode
          , profStat s (closure (ClosureInfo "h$setCcs_e" (CIRegs 0 [PtrV]) "set cost centre stack" (CILayoutFixed 1 [ObjV]) CIStackFrame mempty)
                        (appS "h$restoreCCS" [ stack .! (sp - 1)]
                         <> adjSpN' 2
                         <> returnS (stack .! sp)))
          ]
