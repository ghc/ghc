{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.JS.Rts.Apply
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Jeffrey Young  <jeffrey.young@iohk.io>
--                Luite Stegeman <luite.stegeman@iohk.io>
--                Sylvain Henry  <sylvain.henry@iohk.io>
-- Stability   :  experimental
--
--  Generate various apply functions for the RTS, for speeding up
--  function application in the most common cases. The code is generated
--  because it contains lots of repeating patterns, and to make it more
--  flexible when changing the RTS (for example how arguments are passed)
--
--  The code in here can be a bit hard to read due to all the generated
--  low-level access things. Reading rts.js for a compiled program can be
--  easier (the file is always the same unless you change low-level RTS
--  options)
--
--  FIXME: add selector thunks and let the gc follow them
-----------------------------------------------------------------------------

module GHC.JS.Rts.Apply where

import GHC.JS.Syntax
import GHC.JS.Make
import GHC.JS.Rts.Types

import GHC.StgToJS.DataCon
import GHC.StgToJS.Heap
import GHC.StgToJS.Monad
import GHC.StgToJS.Profiling
import GHC.StgToJS.Regs
import GHC.StgToJS.Types

import GHC.Types.CostCentre
import GHC.Data.ShortText

import qualified Data.Bits as Bits
import Data.Semigroup ((<>))
import GHC.Prelude hiding ((.|.))

rtsApply :: StgToJSConfig -> JStat
rtsApply cfg = mconcat $
  map (uncurry (stackApply cfg)) applySpec
  ++ map (uncurry (fastApply cfg)) applySpec
  ++ map (pap cfg) specPap
  ++ [ mkApplyArr
     , genericStackApply cfg
     , genericFastApply  cfg
     , zeroApply cfg
     , updates   cfg
     , papGen    cfg
     , moveRegs2
     , selectors cfg
     ]

-- specialized apply for these
-- make sure that once you are in spec, you stay there
applySpec :: [(Int,Int)] -- regs,arity
applySpec = [ (regs,arity)  | arity <- [1..4], regs <- [max 0 (arity-1)..(arity*2)]]

specApply :: Bool -> Int -> Int -> Maybe JExpr
specApply fast n r
  | (r,n) == (0,0)         = Just (toJExpr . TxtI . pack $ "h$ap_0_0" ++ fastSuff)
  | (r,n) == (0,1)         = Just (toJExpr . TxtI . pack $ "h$ap_1_0" ++ fastSuff)
  | (r,n) `elem` applySpec =
        Just (toJExpr . TxtI . pack $ "h$ap_" ++ show n ++ "_" ++ show r ++ fastSuff)
  | otherwise = Nothing
   where
      fastSuff | fast      = "_fast"
               | otherwise = ""
{-
  Build arrays to quickly lookup apply functions, getting the fast variant when possible
   - h$apply[r << 8 | n] = function application for r regs, n args
   - h$paps[r]           = partial application for r registers (number of args is in the object)
 -}
  -- FIXME (Jeff, 2022/03): Perf: This code would benefit a great deal by using
  -- a datastucture that supports fast merging.
mkApplyArr :: JStat
mkApplyArr = mconcat
  [ TxtI "h$apply" ||= toJExpr (JList mempty)
  , TxtI "h$paps"  ||= toJExpr (JList mempty)
  , (var "h$initStatic" .^ "push") `ApplStat`
    [ValExpr (JFunc []
              (jVar (\i -> mconcat
                    [ i |= 0
                    ,  WhileStat False (i .<. 65536)
                      ((var "h$apply" .! i |= var "h$ap_gen")
                       <> UOpStat PreIncOp i)
                    , i |= 0
                    , WhileStat False (i .<. 128)
                      ((var "h$paps" .! i |= var "h$pap_gen")
                       <> UOpStat PreIncOp i)
                    , var "h$apply" .! 0 |= var "h$ap_0_0"
                    , mconcat (map assignSpec applySpec)
                    , mconcat (map assignPap specPap)
                    ])))
    ]]
  where
    assignSpec :: (Int, Int) -> JStat
    assignSpec (r,n) =
      var "h$apply" .! toJExpr (Bits.shiftL r 8 Bits..|. n) |=
           toJExpr (TxtI . pack $ "h$ap_" ++ show n ++ "_" ++ show r)

    assignPap :: Int -> JStat
    assignPap p = var "h$paps" .! toJExpr p |=
                      toJExpr (TxtI . pack $ "h$pap_" ++ show p)

-- generic stack apply that can do everything, but less efficiently
-- on stack: tag: (regs << 8 | arity)
-- fixme: set closure info of stack frame
genericStackApply :: StgToJSConfig -> JStat
genericStackApply s =
   closure (ClosureInfo "h$ap_gen" (CIRegs 0 [PtrV]) "h$ap_gen" CILayoutVariable CIStackFrame mempty)
           (jVar $ \cf ->
               mconcat [ traceRts s (jString "h$ap_gen")
                       , cf |= r1 .^ "f"
                       , SwitchStat (cf .^ "t")
                         [ (toJExpr Thunk, profStat s pushRestoreCCS <> returnS cf)
                         , (toJExpr Fun,   funCase cf (funArity' cf))
                         , (toJExpr Pap,   funCase cf (papArity r1))
                         , (toJExpr Blackhole, push' s [r1, var "h$return"]
                             <> returnS (app "h$blockOnBlackhole" [r1]))
                         ] (appS "throw" [jString "h$ap_gen: unexpected closure type " + (cf .^ "t")])
                       ]
            )
  where
    funCase c arity =
      jVar $ \myArity ar myAr myRegs regs newTag newAp p dat ->
      mconcat [ myArity |= stack .! (sp - 1)
              , ar |= mask8 arity
              , myAr |= mask8 myArity
              , myRegs |= myArity .>>. 8
              , traceRts s (jString "h$ap_gen: args: " + myAr
                            + jString " regs: " + myRegs)
              , ifS (myAr .===. ar)
                -- then
                (traceRts s (jString "h$ap_gen: exact")
                  <> loop 0 (.<. myRegs)
                                (\i -> appS "h$setReg" [i+2, stack .! (sp-2-i)]
                                  <> postIncrS i)
                  <> (sp |= sp - myRegs - 2)
                  <> returnS c)
                -- else
                (ifS (myAr .>. ar)
                        --then
                        (mconcat [ regs |= arity .>>. 8
                                 , traceRts s (jString "h$ap_gen: oversat: arity: " + ar
                                               + jString " regs: " + regs)
                                 , loop 0 (.<. regs)
                                   (\i -> traceRts s (jString "h$ap_gen: loading register: " + i)
                                     <> appS "h$setReg" [i+2, stack .! (sp-2-i)]
                                     <> postIncrS i)
                                 , newTag |= ((myRegs-regs).<<.8).|.myAr - ar
                                 , newAp |= var "h$apply" .! newTag
                                 , traceRts s (jString "h$ap_gen: next: " + (newAp .^ "n"))
                                 , ifS (newAp .===. var "h$ap_gen")
                                        ((sp |= sp - regs) <> (stack .! (sp - 1) |= newTag))
                                        (sp |= sp - regs - 1)
                                 , stack .! sp |= newAp
                                 , profStat s pushRestoreCCS
                                 , returnS c
                                 ])
                        -- else
                        (traceRts s (jString "h$ap_gen: undersat")
                         <> mconcat [ p   |= var "h$paps" .! myRegs
                                    , dat |= toJExpr [r1, ((arity .>>. 8)-myRegs)*256+ar-myAr]
                                    ,  loop 0 (.<. myRegs)
                                       (\i -> (dat .^ "push") `ApplStat` [stack .! (sp - i - 2)]
                                              <> postIncrS i)
                                    , sp  |= sp - myRegs - 2
                                    , r1  |= initClosure s p dat jCurrentCCS
                                    , returnStack
                                    ]))
              ]

{-
  generic fast apply: can handle anything (slowly)
  signature tag in argument
-}
genericFastApply :: StgToJSConfig -> JStat
genericFastApply s =
   TxtI "h$ap_gen_fast" ||= jLam (\tag -> jVar (\c ->
      mconcat [traceRts s (jString "h$ap_gen_fast: " + tag)
              , c |= r1 .^ "f"
              , SwitchStat (c .^ "t")
                [ (toJExpr Thunk, traceRts s (jString "h$ap_gen_fast: thunk")
                   <> pushStackApply c tag
                   <> returnS c)
                , (toJExpr Fun, jVar (\farity ->
                                       (farity |= funArity' c)
                                       <> traceRts s (jString "h$ap_gen_fast: fun " + farity)
                                       <> funCase c tag farity))
                , (toJExpr Pap, jVar (\parity ->
                                       (parity |= papArity r1)
                                       <> traceRts s (jString "h$ap_gen_fast: pap " + parity)
                                       <> funCase c tag parity)
                          )
                , (toJExpr Con, traceRts s (jString "h$ap_gen_fast: con")
                    <> jwhenS (tag .!=. 0)
                        (appS "throw" [jString "h$ap_gen_fast: invalid apply"])
                                <> returnS c)
                , (toJExpr Blackhole, traceRts s (jString "h$ap_gen_fast: blackhole")
                    <> pushStackApply c tag
                    <> push' s [r1, var "h$return"]
                    <> returnS (app "h$blockOnBlackhole" [r1]))
                ] $ appS "throw" [jString "h$ap_gen_fast: unexpected closure type: " + c .^ "t"]
              ]))

  where
     -- thunk: push everything to stack frame, enter thunk first
    pushStackApply :: JExpr -> JExpr -> JStat
    pushStackApply _c tag =
      jVar $ \ap ->
      mconcat [ pushAllRegs tag
              , ap |= var "h$apply" .! tag
              , ifS (ap .===. var "h$ap_gen")
                      ((sp |= sp + 2) <> (stack .! (sp-1) |= tag))
                      (sp |= sp + 1)
              , stack .! sp |= ap
              , profStat s pushRestoreCCS
              ]

    funCase :: JExpr -> JExpr -> JExpr -> JStat
    funCase c tag arity =
      jVar $ \ar myAr myRegs regsStart newTag newAp dat p ->
      mconcat [ ar     |= mask8 arity
              , myAr   |= mask8 tag
              , myRegs |= tag .>>. 8
              , traceRts s (jString "h$ap_gen_fast: args: " + myAr
                            + jString " regs: "             + myRegs)
              , ifS (myAr .===. ar)
              -- call the function directly
                (traceRts s (jString "h$ap_gen_fast: exact") <> returnS c)
                (ifS (myAr .>. ar)
                -- push stack frame with remaining args, then call fun
                 (mconcat [ traceRts s (jString "h$ap_gen_fast: oversat " + sp)
                          , regsStart |= (arity .>>. 8) + 1
                          , sp |= sp + myRegs - regsStart + 1
                          , traceRts s (jString "h$ap_gen_fast: oversat " + sp)
                          , pushArgs regsStart myRegs
                          , newTag |= ((myRegs-( arity.>>.8)).<<.8).|.myAr-ar
                          , newAp |= var "h$apply" .! newTag
                          , ifS (newAp .===. var "h$ap_gen")
                                ((sp |= sp + 2) <> (stack .! (sp - 1) |= newTag))
                                (sp |= sp + 1)
                          , stack .! sp |= newAp
                          , profStat s pushRestoreCCS
                          , returnS c
                          ])
                -- else
                  (traceRts s (jString "h$ap_gen_fast: undersat: " + myRegs + jString " " + tag)
                   <> jwhenS (tag .!=. 0)
                        (mconcat [p |= var "h$paps" .! myRegs
                                 , dat |= toJExpr [r1, ((arity .>>. 8)-myRegs)*256+ar-myAr]
                                 , loop 0 (.<. myRegs)
                                   (\i -> (dat .^ "push")
                                     `ApplStat` [app "h$getReg" [i+2]] <> postIncrS i)
                                 , r1 |= initClosure s p dat jCurrentCCS
                                 ])
                   <> returnStack))
              ]


    pushAllRegs :: JExpr -> JStat
    pushAllRegs tag =
      jVar $ \regs ->
      mconcat [regs |= tag .>>. 8
              , sp |= sp + regs
              , SwitchStat regs (map pushReg [65,64..2]) mempty
              ]
      where
        pushReg :: Int -> (JExpr, JStat)
        pushReg r = (toJExpr (r-1),  stack .! (sp - toJExpr (r - 2)) |= toJExpr (intToJSReg r))

    pushArgs :: JExpr -> JExpr -> JStat
    pushArgs start end =
      loop end (.>=.start) (\i -> traceRts s (jString "pushing register: " + i)
                             <> (stack .! (sp + start - i) |= app "h$getReg" [i+1])
                             <> postDecrS i
                           )

stackApply :: StgToJSConfig
           -> Int         -- ^ number of registers in stack frame
           -> Int         -- ^ number of arguments
           -> JStat
stackApply s r n =
  closure (ClosureInfo funcName (CIRegs 0 [PtrV]) funcName layout CIStackFrame mempty)
          body
  where
    layout    = CILayoutUnknown r

    funcName = pack ("h$ap_" ++ show n ++ "_" ++ show r)

    body = jVar $
        \c ->
          mconcat [ c |= r1 .^ "f"
                  , traceRts s (toJExpr funcName
                                + jString " "
                                + (c .^ "n")
                                + jString " sp: " + sp
                                + jString " a: "  + (c .^ "a"))
                  , SwitchStat (c .^ "t")
                    [ (toJExpr Thunk, traceRts s (toJExpr $ funcName <> ": thunk") <> profStat s pushRestoreCCS <> returnS c)
                    , (toJExpr Fun, traceRts s (toJExpr $ funcName <> ": fun") <> funCase c)
                    , (toJExpr Pap, traceRts s (toJExpr $ funcName <> ": pap") <> papCase c)
                    , (toJExpr Blackhole, push' s [r1, var "h$return"] <> returnS (app "h$blockOnBlackhole" [r1]))
                    ] (appS "throw" [toJExpr ("panic: " <> funcName <> ", unexpected closure type: ") + (c .^ "t")])
                  ]

    funExact c = popSkip' 1 (reverse $ take r (map toJExpr $ enumFrom R2)) <> returnS c
    stackArgs = map (\x -> stack .! (sp - toJExpr x)) [1..r]

    papCase :: JExpr -> JStat
    papCase c = jVar $ \expr arity0 arity ->
      case expr of
        ValExpr (JVar pap) -> mconcat [ arity0 |= papArity r1
                                      , arity |= mask8 arity0
                                      , traceRts s (toJExpr (funcName <> ": found pap, arity: ") + arity)
                                      , ifS (toJExpr n .===. arity)
                                      --then
                                        (traceRts s (toJExpr (funcName <> ": exact")) <> funExact c)
                                      -- else
                                        (ifS (toJExpr n .>. arity)
                                          (traceRts s (toJExpr (funcName <> ": oversat")) <> oversatCase c arity0 arity)
                                          (traceRts s (toJExpr (funcName <> ": undersat"))
                                           <> mkPap s pap r1 (toJExpr n) stackArgs -- FIXME do we want double pap?
                                           <> (sp |= sp - toJExpr (r + 1))
                                           <> (r1 |= toJExpr pap)
                                           <> returnStack))
                                      ]
        _                   -> mempty -- FIXME: Jeff (2022,03), just quieting non-exhaustive
                                      -- patterns. That the code wants to do this
                                      -- means we should be encoding that funCase is
                                      -- only callable on ValExpr (JVar pap)'s in
                                      -- the type system, perhaps with a GADT or
                                      -- phantom


    funCase :: JExpr -> JStat
    funCase c = jVar $ \expr ar0 ar ->
      case expr of
        ValExpr (JVar pap) -> mconcat [ ar0 |= funArity' c
                                      , ar |= mask8 ar0
                                      , ifS (toJExpr n .===. ar)
                                        (traceRts s (toJExpr (funcName <> ": exact")) <> funExact c)
                                        (ifS (toJExpr n .>. ar)
                                         (traceRts s (toJExpr (funcName <> ": oversat"))
                                          <> oversatCase c ar0 ar)
                                         (traceRts s (toJExpr (funcName <> ": undersat"))
                                          <> mkPap s pap (toJExpr R1) (toJExpr n) stackArgs
                                          <> (sp |= sp - toJExpr (r+1))
                                          <> (r1 |= toJExpr pap)
                                          <> returnStack))
                                      ]
        _                  -> mempty  -- FIXME: Jeff (2022,03), just quieting non-exhaustive
                                      -- patterns. That the code wants to do this
                                      -- means we should be encoding that funCase is
                                      -- only callable on ValExpr (JVar pap)'s in
                                      -- the type system, perhaps with a GADT or
                                      -- phantom


    -- oversat: call the function but keep enough on the stack for the next
    oversatCase :: JExpr -- function
                -> JExpr -- the arity tag
                -> JExpr -- real arity (arity & 0xff)
                -> JStat
    oversatCase c arity arity0 =
      jVar (\rs newAp ->
              mconcat [ rs |= (arity .>>. 8)
                      , loadRegs rs
                      , sp |= sp - rs
                      , newAp |= (var "h$apply" .! (toJExpr n-arity0.|.((toJExpr r-rs).<<.8)))
                      , stack .! sp |= newAp
                      , profStat s pushRestoreCCS
                      , traceRts s (toJExpr (funcName <> ": new stack frame: ") + (newAp .^ "n"))
                      , returnS c
                      ])
      where
        loadRegs rs = SwitchStat rs switchAlts mempty
          where
            switchAlts = map (\x -> (toJExpr x, toJExpr (intToJSReg (x+1)) |= stack .! (sp - toJExpr x))) [r,r-1..1]

{-
  stg_ap_r_n_fast is entered if a function of unknown arity
  is called, n arguments are already in r registers
-}
fastApply :: StgToJSConfig -> Int -> Int -> JStat
fastApply s r n = func ||= toJExpr (JFunc myFunArgs body)
    where
      funName = pack ("h$ap_" ++ show n ++ "_" ++ show r ++ "_fast")
      func    = TxtI funName

      myFunArgs = []

      regArgs = take r (enumFrom R2)

      mkAp :: Int -> Int -> [JExpr]
      mkAp n' r' = [ var . pack $ "h$ap_" ++ show n' ++ "_" ++ show r' ]

      body =
        jVar $ \c farity arity ->
        mconcat [ c |= r1 .^ "f"
                , traceRts s (toJExpr (funName <> ": sp ") + sp)
                -- TODO: Jeff (2022,03): factor our and dry out this code
                , SwitchStat (c .^ "t")
                   [(toJExpr Fun, traceRts s (toJExpr (funName <> ": ")
                                              + clName c
                                              + jString " (arity: " + (c .^ "a") + jString ")")
                                  <> (farity |= funArity' c)
                                  <> funCase c farity)
                   ,(toJExpr Pap, traceRts s (toJExpr (funName <> ": pap")) <> (arity |= papArity r1) <> funCase c arity)
                   ,(toJExpr Thunk, traceRts s (toJExpr (funName <> ": thunk")) <> push' s (reverse (map toJExpr $ take r (enumFrom R2)) ++ mkAp n r) <> profStat s pushRestoreCCS <> returnS c)
                   ,(toJExpr Blackhole, traceRts s (toJExpr (funName <> ": blackhole")) <> push' s (reverse (map toJExpr $ take r (enumFrom R2)) ++ mkAp n r) <> push' s [r1, var "h$return"] <> returnS (app "h$blockOnBlackhole" [r1]))]
                   (appS "throw" [toJExpr (funName <> ": unexpected closure type: ") + c .^ "t"])
                ]

      funCase :: JExpr -> JExpr -> JStat
      funCase c arity = jVar $
        \arg ar -> case arg of
          ValExpr (JVar pap) -> (ar |= mask8 arity)
                                  <> ifS (toJExpr n .===. ar)
                                  -- then
                                  (traceRts s (toJExpr (funName <> ": exact")) <> returnS c)
                                  -- else
                                  (ifS (toJExpr n .>. ar)
                                    --then
                                    (traceRts s (toJExpr (funName <> ": oversat")) <> oversatCase c arity)
                                    -- else
                                    (traceRts s (toJExpr (funName <> ": undersat"))
                                     <> mkPap s pap r1 (toJExpr n) (map toJExpr regArgs)
                                     <> (r1 |= toJExpr pap)
                                     <> returnStack))
          _             -> mempty -- FIXME: Jeff (2022,03), just quieting non-exhaustive
                                  -- patterns. That the code wants to do this
                                  -- means we should be encoding that funCase is
                                  -- only callable on ValExpr (JVar pap)'s in
                                  -- the type system, perhaps with a GADT or
                                  -- phantom

      oversatCase :: JExpr -> JExpr -> JStat
      oversatCase c arity =
         jVar $ \rs rsRemain ->
                  mconcat [ rs |= arity .>>. 8
                          , rsRemain |= toJExpr r - rs
                          , traceRts s (toJExpr
                                        (funName <> " regs oversat ")
                                         + rs
                                         + jString " remain: "
                                         + rsRemain)
                          , saveRegs rs
                          , sp |= sp + rsRemain + 1
                          , stack .! sp |= var "h$apply" .! ((rsRemain.<<.8).|. toJExpr n - mask8 arity)
                          , profStat s pushRestoreCCS
                          , returnS c
                          ]
          where
            saveRegs n = SwitchStat n switchAlts mempty
              where
                switchAlts = map (\x -> (toJExpr x, stack .! (sp + toJExpr (r-x)) |= toJExpr (intToJSReg (x+2)))) [0..r-1]

zeroApply :: StgToJSConfig -> JStat
zeroApply s =
  mconcat [ TxtI "h$ap_0_0_fast" ||= jLam (enter s r1)
          , closure (ClosureInfo "h$ap_0_0" (CIRegs 0 [PtrV]) "h$ap_0_0" (CILayoutFixed 0 []) CIStackFrame mempty)
            (adjSpN' 1 <> enter s r1)
          , TxtI "h$e" ||= jLam (\c -> (r1 |= c) <> enter s c)
          ]

-- carefully enter a closure that might be a thunk or a function

-- ex may be a local var, but must've been copied to R1 before calling this
enter :: StgToJSConfig -> JExpr -> JStat
enter s ex =
         jVar $ \c ->
                  mconcat [ jwhenS (app "typeof" [ex] .!==. jTyObject) returnStack
                          , c |= ex .^ "f"
                          , jwhenS (c .===. var "h$unbox_e") ((r1 |= ex .^ "d1") <> returnStack)
                          , SwitchStat (c .^ "t")
                            [ (toJExpr Con, mempty)
                            , (toJExpr Fun, mempty)
                            , (toJExpr Pap, returnStack)
                            , (toJExpr Blackhole, push' s [var "h$ap_0_0", ex, var "h$return"]
                                <> returnS (app "h$blockOnBlackhole" [ex]))
                            ] (returnS c)
                          ]

updates :: StgToJSConfig -> JStat
updates s =
  closure (ClosureInfo "h$upd_frame" (CIRegs 0 [PtrV]) "h$upd_frame" (CILayoutFixed 1 [PtrV]) CIStackFrame mempty)
        (jVar $ \updatee waiters ss si sir ->
            mconcat [ updatee |= stack .! (sp - 1)
                    , traceRts s (jString "h$upd_frame updatee alloc: " + updatee .^ "alloc")
                    , -- wake up threads blocked on blackhole
                      waiters |= updatee .^ "d2"
                    , jwhenS (waiters .!==. null_)
                                (loop 0 (.<. waiters .^ "length")
                                   (\i -> appS "h$wakeupThread" [waiters .! i] <> postIncrS i))
                    , -- update selectors
                      jwhenS ((app "typeof" [updatee .^ "m"] .===. jTyObject) .&&. (updatee .^ "m" .^ "sel"))
                      ((ss |= updatee .^ "m" .^ "sel")
                        <> loop 0 (.<. ss .^ "length")
                        (\i -> mconcat [ si |= ss .! i
                                       , sir |= (si .^ "d2") `ApplExpr` [r1]
                                       , ifS (app "typeof" [sir] .===. jTyObject)
                                                (mconcat [ si .^ "f"  |= sir .^ "f"
                                                         , si .^ "d1" |= sir .^ "d1"
                                                         , si .^ "d2" |= sir .^ "d2"
                                                         , si .^ "m"  |= sir .^ "m"
                                                         ])
                                                (mconcat [ si .^ "f"  |= var "h$unbox_e"
                                                         , si .^ "d1" |= sir
                                                         , si .^ "d2" |= null_
                                                         , si .^ "m"  |= 0
                                                         ])
                                       , postIncrS i
                                       ]))
                    , -- overwrite the object
                      ifS (app "typeof" [r1] .===. jTyObject)
                          (mconcat [ traceRts s (jString "$upd_frame: boxed: " + ((r1 .^ "f") .^ "n"))
                                   , updatee .^ "f"  |= r1 .^ "f"
                                   , updatee .^ "d1" |= r1 .^ "d1"
                                   , updatee .^ "d2" |= r1 .^ "d2"
                                   , updatee .^ "m"  |= r1 .^ "m"
                                   , profStat s (updateCC updatee)
                                   ])
                          (mconcat [ updatee .^ "f" |= var "h$unbox_e"
                                   , updatee .^ "d1" |= r1
                                   , updatee .^ "d2" |= null_
                                   , updatee .^ "m" |= 0
                                   , profStat s (updateCC updatee)
                                   ])
                    , adjSpN' 2
                    , traceRts s (jString "h$upd_frame: updating: "
                                  + updatee
                                  + jString " -> "
                                  + r1)
                    , returnStack
                    ])
   <>
   closure (ClosureInfo "h$upd_frame_lne" (CIRegs 0 [PtrV]) "h$upd_frame_lne" (CILayoutFixed 1 [PtrV]) CIStackFrame mempty)
           (jVar $ \updateePos ->
            (updateePos |= stack .! (sp - 1))
            <> (stack .! updateePos |= r1)
            <> adjSpN' 2
            <> traceRts s (jString "h$upd_frame_lne: updating: "
                           + updateePos
                           + jString " -> "
                           + r1)
            <> returnStack)
  where
    updateCC updatee = updatee .^ "cc" |= jCurrentCCS

selectors :: StgToJSConfig -> JStat
selectors s =
  mkSel "1"   (.^ "d1")
  <> mkSel "2a"  (.^ "d2")
  <> mkSel "2b"  (\x -> x .^ "d2" .^ "d1")
  <> mconcat (map mkSelN [3..16])
   where
    mkSelN :: Int -> JStat
    mkSelN x = mkSel (pack $ show x)
                     (\e -> SelExpr (SelExpr (toJExpr e) (TxtI (pack "d2"))){-[je| `toJExpr`.d2 |]-}
                            (TxtI $ pack ("d" ++ show (x-1))))


    mkSel :: ShortText -> (JExpr -> JExpr) -> JStat
    mkSel name sel =
      mconcat [TxtI createName ||= jLam (\r -> traceRts s (toJExpr ("selector create: " <> name <> " for ") + (r .^ "alloc"))
                                        <> ifS (isThunk r .||. isBlackhole r)
                                          (returnS (app "h$mkSelThunk" [r, toJExpr (v entryName), toJExpr (v resName)]))
                                          (returnS (sel r)))
              , TxtI resName ||= jLam (\r -> traceRts s (toJExpr ("selector result: " <> name <> " for ") + (r .^ "alloc"))
                                        <> returnS (sel r))
              , closure (ClosureInfo entryName (CIRegs 0 [PtrV]) ("select " <> name) (CILayoutFixed 1 [PtrV]) CIThunk mempty)
                (jVar $ \tgt ->
                 (tgt |= r1 .^ "d1")
                 <> traceRts s (toJExpr ("selector entry: " <> name <> " for ") + (tgt .^ "alloc"))
                 <> ifS (isThunk tgt .||. isBlackhole tgt)
                 (preIncrS sp
                  <> (stack .! sp |= var frameName)
                  <> returnS (app "h$e" [tgt]))
                  (returnS (app "h$e" [sel tgt])))
              , closure
                (ClosureInfo frameName (CIRegs 0 [PtrV]) ("select " <> name <> " frame") (CILayoutFixed 0 []) CIStackFrame mempty)
                $ mconcat [ traceRts s (toJExpr ("selector frame: " <> name))
                          , postDecrS sp
                          , returnS (app "h$e" [sel r1])
                          ]
              ]

      where
         v x   = JVar (TxtI x)
         n ext =  "h$c_sel_" <> name <> ext
         createName = n ""
         resName    = n "_res"
         entryName  = n "_e"
         frameName  = n "_frame_e"


{-
  Partial applications. There are two different kinds of partial application:
    pap_r contains r registers, pap_gen can contain any number

    layout:
     - d1      = function
     - d2.d1 & 0xff = number of args
       d2.d1 >> 8   = number of registers (r for h$pap_r)
     - d2.d2.. = args (r)
-}
-- arity is the remaining arity after our supplied arguments are applied
mkPap :: StgToJSConfig
      -> Ident   -- ^ id of the pap object
      -> JExpr   -- ^ the function that's called (can be a second pap)
      -> JExpr   -- ^ number of arguments in pap
      -> [JExpr] -- ^ values for the supplied arguments
      -> JStat
mkPap s tgt fun n values =
      traceRts s (toJExpr $ "making pap with: " ++ show (length values) ++ " items")
      `mappend`
      allocDynamic s True tgt (toJExpr entry) (fun:papAr:map toJExpr values')
        (if csProf s then Just jCurrentCCS else Nothing)
  where
    papAr = funOrPapArity fun Nothing - toJExpr (length values * 256) - n

    values' | GHC.Prelude.null values = [null_]
            | otherwise   = values
    entry | length values > numSpecPap = TxtI "h$pap_gen"
          | otherwise                  = TxtI . pack $ "h$pap_" ++ show (length values)

-- specialized (faster) pap generated for [0..numSpecPap]
-- others use h$pap_gen
specPap :: [Int]
specPap = [0..numSpecPap]

numSpecPap :: Int
numSpecPap = 6

pap :: StgToJSConfig
    -> Int
    -> JStat
pap s r = closure (ClosureInfo funcName CIRegsUnknown funcName (CILayoutUnknown (r+2)) CIPap mempty) body
  where
    funcName = pack ("h$pap_" ++ show r)

    body =
      jVar $ \c d f extra ->
          mconcat [ c |= r1 .^ "d1"
                  , d |= r1 .^ "d2"
                  , f |= c  .^ "f"
                  , assertRts s (isFun' f .||. isPap' f) (funcName <> ": expected function or pap")
                  , profStat s (enterCostCentreFun currentCCS)
                  , extra |= (funOrPapArity c (Just f) .>>. 8) - toJExpr r
                  , traceRts s (toJExpr (funcName <> ": pap extra args moving: ") + extra)
                  , moveBy extra
                  , loadOwnArgs d
                  , r1 |= c
                  , returnS f
                  ]
    moveBy extra = SwitchStat extra
                   (reverse $ map moveCase [1..maxReg-r-1]) mempty
    moveCase m = (toJExpr m, toJExpr (intToJSReg (m+r+1)) |= toJExpr (intToJSReg (m+1)))
    loadOwnArgs d = mconcat $ map (\r ->
        toJExpr (intToJSReg (r+1)) |= dField d (r+2)) [1..r]
    dField d n = SelExpr d (TxtI . pack $ ('d':show (n-1)))

-- Construct a generic PAP
papGen :: StgToJSConfig -> JStat
papGen cfg =
   closure (ClosureInfo funcName CIRegsUnknown funcName CILayoutVariable CIPap mempty)
           (jVar $ \c f d pr or r ->
                        mconcat [ c |= r1 .^ "d1"
                                , f |= c  .^ "f"
                                , d |= r1 .^ "d2"
                                , pr |= funOrPapArity c (Just f) .>>. 8
                                , or |= papArity r1 .>>. 8
                                , r |= pr - or
                                , assertRts cfg
                                  (isFun' f .||. isPap' f)
                                  (jString "h$pap_gen: expected function or pap")
                                , profStat cfg (enterCostCentreFun currentCCS)
                                , traceRts cfg (jString "h$pap_gen: generic pap extra args moving: " + or)
                                , appS "h$moveRegs2" [or, r]
                                , loadOwnArgs d r
                                , r1 |= c
                                , returnS f
                                ])


  where
    funcName = "h$pap_gen"
    loadOwnArgs d r =
      let prop n = d .^ ("d" <> pack (show $ n+1))
          loadOwnArg n = (toJExpr n, toJExpr (intToJSReg (n+1)) |= prop n)
      in  SwitchStat r (map loadOwnArg [127,126..1]) mempty

-- general utilities
-- move the first n registers, starting at R2, m places up (do not use with negative m)
-- FIXME (Jeff, 2022/03): pick a better name, e.g., `r2moveRegs`
moveRegs2 :: JStat
moveRegs2 = TxtI "h$moveRegs2" ||= jLam moveSwitch
  where
    moveSwitch n m = SwitchStat ((n .<<. 8) .|. m) switchCases (defaultCase n m)
    -- fast cases
    -- TODO: tune the parameteters for performance and size
    switchCases = [switchCase n m | n <- [1..5], m <- [1..4]]
    switchCase :: Int -> Int -> (JExpr, JStat)
    switchCase n m = (toJExpr $
                      (n `Bits.shiftL` 8) Bits..|. m
                     , mconcat (map (`moveRegFast` m) [n+1,n..2])
                       <> BreakStat Nothing {-[j| break; |]-})
    moveRegFast n m = toJExpr (intToJSReg (n+m)) |= toJExpr (intToJSReg n)
    -- fallback
    defaultCase n m =
      loop n (.>.0) (\i -> appS "h$setReg" [i+1+m, app "h$getReg" [i+1]] `mappend` postDecrS i)


-- Initalize a variable sized object from an array of values
initClosure :: StgToJSConfig -> JExpr -> JExpr -> JExpr -> JExpr
initClosure cfg entry values ccs
  | csProf cfg =
      app "h$init_closure" [ toJExpr (jhFromList [ ("f" , entry)
                                                 , ("d1", null_)
                                                 , ("d2", null_)
                                                 , ("m" , 0)
                                                 , ("cc", ccs)])
                           , values]
  | otherwise =
      app "h$init_closure" [ toJExpr (jhFromList [ ("f" , entry)
                                                 , ("d1", null_)
                                                 , ("d2", null_)
                                                 , ("m" , 0)])
                           , values]


-- FIXME: where to put this
closure :: ClosureInfo -- ^ object being info'd see @ciVar@ in @ClosureInfo@
        -> JStat       -- ^ rhs
        -> JStat
closure ci body = (TxtI (ciVar ci)||= jLam body) `mappend` toStat ci

-- FIXME: where to put this
conClosure :: ShortText -> ShortText -> CILayout -> Int -> JStat
conClosure symbol name layout constr =
  closure (ClosureInfo symbol (CIRegs 0 [PtrV]) name layout (CICon constr) mempty)
          (returnS (stack .! sp))
