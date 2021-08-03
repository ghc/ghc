{-# OPTIONS_GHC -O0 #-}

{-# LANGUAGE OverloadedStrings #-}

module Gen2.Rts where

import           DynFlags

-- import           Control.Lens                     hiding ((||=), (#))

import           Data.Array
import qualified Data.Bits                        as Bits
import           Data.Char                        (toLower, toUpper)
import qualified Data.Map                         as M
import qualified Data.Text                        as T
import qualified Data.Text.Lazy                   as TL

import           Text.PrettyPrint.Leijen.Text     hiding (pretty{-, (<>)-})

import           Compiler.JMacro
import           Compiler.JMacro.Lens
import           Compiler.JMacro.Symbols
import           Compiler.JMacro.Combinators

import           Gen2.ClosureInfo
import           Gen2.Profiling
import           Gen2.Printer
import           Gen2.RtsApply
import           Gen2.RtsTypes
import           Prelude

garbageCollector :: JStat
garbageCollector =
  "h$resetRegisters"  ||= jLam (mconcat $ map resetRegister [minBound..maxBound]) #
  "h$resetResultVars" ||= jLam (mconcat $ map resetResultVar [minBound..maxBound])

resetRegister :: StgReg -> JStat
resetRegister r = e r |= null_

resetResultVar :: StgRet -> JStat
resetResultVar r = e r |= null_

{-
          use h$c1, h$c2, h$c3, ... h$c24 instead of making objects manually
  so layouts and fields can be changed more easily
 -}
closureConstructors :: CgSettings -> JStat
closureConstructors s =
     declClsConstr "h$c"  ["f"] [jsv "f", null_, null_, 0]
  <> declClsConstr "h$c0" ["f"] [jsv "f", null_, null_, 0] -- FIXME: same as h$c, maybe remove one of them?
  <> declClsConstr "h$c1" ["f", "x1"] [jsv "f", jsv "x1", null_, 0]
  <> declClsConstr "h$c2" ["f", "x1", "x2"] [jsv "f", jsv "x1", jsv "x2", 0]
  <> mconcat (map mkClosureCon [3..24])
  <> mconcat (map mkDataFill [1..24])
  where
    prof = csProf s
    addCCArg as = map TxtI $ as ++ if prof then ["cc"] else []
    addCCArg' as = as ++ if prof then [TxtI "cc"] else []
    addCCField fs = jhFromList $ fs ++ if prof then [("cc", jsv "cc")] else []

    declClsConstr i as fs = (TxtI i) ||= ValExpr (JFunc (addCCArg as)
      ( jVar $ \x ->
       checkC #
        x |= e (addCCField $ zip ["f", "d1", "d2", "m"] fs) #
         notifyAlloc x #
          traceAlloc x #
          returnS x
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
      ifS' (var "arguments" .! 0 .!==. "h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e")
                                  (loop 1 (.<. var "arguments" .^ "length")
                                          (\i -> msg |= "warning: undefined or null in argument: "
                                                         + i + " allocating closure: " + (var "arguments" .! 0 .^ "n") #
                                                 appS "h$log" [msg] #
                                                 ifS' (var "console" .&&. (var "console" .^ "trace"))
                                                      (var "console" .^ "trace" |$ [msg]) #
                                                 postIncrS i)

                                  )
           | otherwise = mempty

    -- h$d is never used for JSVal (since it's only for constructors with
    -- at least three fields, so we always warn here
    checkD | csAssertRts s =
                     loop 0 (.<. var "arguments" .^ "length")
                                                            (\i -> ifS' ((var "arguments" .! i .===. null_) .||.
                                                                         (var "arguments" .! i .===. undefined_))
                                                                        (jVar $ \msg ->
                                                                         msg |= "warning: undefined or null in argument: " + i + " allocating fields" #
                                                                         ifS' (var "console" .&&. (var "console" .^ "trace")) (var "console" .^ "trace" |$ [msg])

                                                                        ))

           | otherwise = mempty

    mkClosureCon :: Int -> JStat
    mkClosureCon n = let funName = TxtI $ T.pack ("h$c" ++ show n)
                         vals   = TxtI "f" : addCCArg' (map (TxtI . T.pack . ('x':) . show) [(1::Int)..n])
                         fun    = JFunc vals funBod
                         funBod =
                           jVar $ \x ->
                            checkC #
                            x |= e (addCCField [("f", jsv "f"), ("d1", jsv "x1"), ("d2", toJExpr obj), ("m", 0)]) #
                              notifyAlloc x #
                              traceAlloc x #
                              returnS x

                         obj    = JHash . M.fromList . zip
                                    (map (T.pack . ('d':) . show) [(1::Int)..]) $
                                    (map (toJExpr . TxtI . T.pack . ('x':) . show) [2..n])
                     in funName ||= e fun

    mkDataFill :: Int -> JStat
    mkDataFill n = let funName = TxtI $ T.pack ("h$d" ++ show n)
                       ds      = map (T.pack . ('d':) . show) [(1::Int)..n]
                       obj     = JHash . M.fromList . zip ds $ map (toJExpr . TxtI) ds
                       fun     = JFunc (map TxtI ds) (checkD #  returnS (e obj))
                   in funName ||= e fun

stackManip :: JStat
stackManip = mconcat (map mkPush [1..32]) <>
             mconcat (map mkPpush [1..255])
  where
    mkPush :: Int -> JStat
    mkPush n = let funName = TxtI $ T.pack ("h$p" ++ show n)
                   as      = map (TxtI . T.pack . ('x':) . show) [1..n]
                   fun     = JFunc as $ (sp |= sp + e n #
                             mconcat (zipWith (\i a -> stack .! (sp - e (n-i)) |= e a) [1..] as))
               in funName ||= e fun

    -- | partial pushes, based on bitmap, increases Sp by highest bit
    mkPpush :: Integer -> JStat
    mkPpush sig | sig Bits..&. (sig+1) == 0 = mempty -- already handled by h$p
    mkPpush sig = let funName = TxtI $ T.pack ("h$pp" ++ show sig)
                      bits    = bitsIdx sig
                      n       = length bits
                      h       = last bits
                      args    = map (TxtI . T.pack . ('x':) . show) [1..n]
                      fun     = JFunc args $
                                 sp |= sp + e (h+1) #
                                 mconcat (zipWith (\b a -> stack .! (sp - e (h-b)) |= e a) bits args)
                   in funName ||= e fun

bitsIdx :: Integer -> [Int]
bitsIdx n | n < 0 = error "bitsIdx: negative"
          | otherwise = go n 0
  where
    go 0 _ = []
    go m b | Bits.testBit m b = b : go (Bits.clearBit m b) (b+1)
           | otherwise   = go (Bits.clearBit m b) (b+1)

bhStats :: CgSettings -> Bool -> JStat
bhStats s pushUpd =
  let u = if pushUpd then push' s [toJExpr R1, jsv "h$upd_frame"] else mempty
  in  u # r1 .^ "f"  |= var "h$blackhole" #
          r1 .^ "d1" |= var "h$currentThread" #
          r1 .^ "d2" |= null_ -- will be filled with waiters array

bhLneStats :: CgSettings -> JExpr -> JExpr -> JStat
bhLneStats _s p frameSize =
   jVar $ \v ->
      v |= stack .! p #
      ifS v
          (sp |= sp - frameSize #
           ifS (v .===. var "h$blackhole")
               (returnS $ app "h$throw" [var "h$baseZCControlziExceptionziBasezinonTermination", false_])
               (r1 |= v #
                sp |= sp - frameSize #
                returnStack)


          )
          (stack .! p |= var "h$blackhole" # returnS null_)

updateThunk' :: CgSettings -> JStat
updateThunk' settings =
  if csInlineBlackhole settings
    then bhStats settings True
    else appS "h$bh" []

updateThunk :: C
updateThunk = do
  settings <- use gsSettings
  adjPushStack 2 -- update frame size
  return $ (updateThunk' settings)

{-
  Overwrite a single entry object with a special thunk that behaves like a black hole
  (throws a JS exception when entered) but pretends to be a thunk. Useful for making
  sure that the object is not accidentally entered multiple times
 -}
bhSingleEntry :: CgSettings -> JStat
bhSingleEntry _settings =
   r1 .^ "f"  |= var "h$blackholeTrap" #
   r1 .^ "d1" |= undefined_ #
   r1 .^ "d2" |= undefined_

-- fixme move somewhere else
declRegs :: JStat
-- fixme prevent holes
declRegs =
        "h$regs" ||= e (JList []) #
        mconcat (map declReg (enumFromTo R1 R32)) #
        regGettersSetters #
        loadRegs
    where
      declReg r = (decl . TxtI . T.pack . ("h$"++) . map toLower . show) r <>     BlockStat [(AssignStat (toJExpr r)) (ValExpr (JInt 0))] -- [j| `r` = 0; |]

regGettersSetters :: JStat
regGettersSetters =
  "h$getReg" ||= jLam (\n -> SwitchStat n getRegCases mempty) #
  "h$setReg" ||= jLam (\n v -> SwitchStat n (setRegCases v) mempty)
  where
    getRegCases =
      map (\r -> (e (regNum r), returnS (e r))) (enumFrom R1)
    setRegCases v =
      map (\r -> (e (regNum r), e r |= e v # returnS undefined_)) (enumFrom R1)

loadRegs :: JStat
loadRegs = mconcat $ map mkLoad [1..32]
  where
    mkLoad :: Int -> JStat
    mkLoad n = let args   = map (TxtI . T.pack . ("x"++) . show) [1..n]
                   assign = zipWith (\a r -> e r |= e a)
                              args (reverse $ take n (enumFrom R1))
                   fname  = TxtI $ T.pack ("h$l" ++ show n)
                   fun    = JFunc args (mconcat assign)
               in fname ||= e fun

-- assign registers R1 ... Rn
-- assigns Rn first
assignRegs :: CgSettings -> [JExpr] -> JStat
assignRegs _ [] = mempty
assignRegs s xs
  | l <= 32 && not (csInlineLoadRegs s)
      = ApplStat (ValExpr (JVar $ assignRegs'!l)) (reverse xs)
  | otherwise = mconcat . reverse $
      zipWith (\r ex -> e r |= ex) (take l $ enumFrom R1) xs
  where
    l = length xs

assignRegs' :: Array Int Ident
assignRegs' = listArray (1,32) (map (TxtI . T.pack . ("h$l"++) . show) [(1::Int)..32])

declRets :: JStat
declRets = mconcat $ map (decl . TxtI . T.pack . ("h$"++) . map toLower . show) (enumFrom Ret1)

trace :: ToJExpr a => a -> JStat
trace ex = appS "h$log" [e ex]

closureTypes :: JStat
closureTypes = mconcat (map mkClosureType (enumFromTo minBound maxBound)) <> closureTypeName
  where
    mkClosureType :: CType -> JStat
    mkClosureType c = let s = TxtI . T.pack $ "h$" ++ map toUpper (show c) ++ "_CLOSURE"
                      in  s ||= e c
    closureTypeName :: JStat
    closureTypeName =
      "h$closureTypeName" ||= jLam (\c -> mconcat (map (ifCT c) [minBound..maxBound]) #
                                          returnS "InvalidClosureType"
                                   )

    ifCT :: JExpr -> CType -> JStat
    ifCT arg ct = ifS' (arg .===. e ct) (returnS (e (show ct)))

rtsDeclsText :: TL.Text
rtsDeclsText = (displayT . renderPretty 0.8 150 . pretty $ rtsDecls) <> "\n"

rtsDecls :: JStat
rtsDecls = jsSaturate (Just "h$RTSD") (
                     "h$currentThread" ||= null_ # -- thread state object for current thread
                     "h$stack" ||= null_ # -- stack for the current thread
                     "h$sp" ||= 0 # -- stack pointer for the current thread
                     "h$initStatic" ||= e (JList []) # {- we need delayed initialization for static objects, push functions here
                                                                to be initialized just before haskell runs -}
                     "h$staticThunks" ||= e (jhFromList []) # --  funcName -> heapidx map for srefs
                     "h$staticThunksArr" ||= e (JList []) # -- indices of updatable thunks in static heap
                     -- stg registers
                     declRegs #
                     declRets)

rtsText :: DynFlags -> CgSettings -> TL.Text
rtsText dflags = (<>"\n") . displayT . renderPretty 0.8 150 . pretty . rts dflags

rts :: DynFlags -> CgSettings -> JStat
rts dflags s = jsSaturate (Just "h$RTS") (rts' dflags s)

rts' :: DynFlags -> CgSettings -> JStat
rts' dflags s =
     ( closureConstructors s #
       garbageCollector #
       stackManip #
       -- settings (fixme should be const)
       "h$rts_traceForeign" ||= e (csTraceForeign s) #
       "h$rts_profiling"    ||= e (csProf s) #

       -- closure types (fixme should be const)
       "h$ct_fun"        ||= e Fun #
       "h$ct_con"        ||= e Con #
       "h$ct_thunk"      ||= e Thunk #
       "h$ct_pap"        ||= e Pap #
       "h$ct_blackhole"  ||= e Blackhole #
       "h$ct_stackframe" ||= e StackFrame #

       -- var / closure field types (fixme should be const)
       "h$vt_ptr"        ||= e PtrV #
       "h$vt_void"       ||= e VoidV #
       "h$vt_double"     ||= e IntV #
       "h$vt_long"       ||= e LongV #
       "h$vt_addr"       ||= e AddrV #
       "h$vt_rtsobj"     ||= e RtsObjV #
       "h$vt_obj"        ||= e ObjV #
       "h$vt_arr"        ||= e ArrV #

       "h$bh"     ||= jLam (bhStats s True) #
       "h$bh_lne" ||= jLam (\x frameSize -> bhLneStats s x frameSize) #

       closure (ClosureInfo "h$blackhole" (CIRegs 0 []) "blackhole" (CILayoutUnknown 2) CIBlackhole noStatic)
               (appS "throw" ["oops: entered black hole"]) #

       closure (ClosureInfo "h$blackholeTrap" (CIRegs 0 []) "blackhole" (CILayoutUnknown 2) CIThunk noStatic)
               (appS "throw" ["oops: entered multiple times"]) #

       closure (ClosureInfo "h$done" (CIRegs 0 [PtrV]) "done" (CILayoutUnknown 0) CIStackFrame noStatic)
               (appS "h$finishThread" [var "h$currentThread"] #
                returnS (var "h$reschedule")) #

       closure (ClosureInfo "h$doneMain_e" (CIRegs 0 [PtrV]) "doneMain" (CILayoutUnknown 0) CIStackFrame noStatic)
               (returnS (var "h$doneMain")) #

       conClosure "h$false_e" "GHC.Types.False" (CILayoutFixed 0 []) 1 #
       conClosure "h$true_e"  "GHC.Types.True"  (CILayoutFixed 0 []) 2 #
       conClosure "h$integerzmwiredzminZCGHCziIntegerziTypeziSzh_con_e" "GHC.Integer.Type.S#" (CILayoutFixed 1 [IntV]) 1 #
       conClosure "h$integerzmwiredzminZCGHCziIntegerziTypeziJpzh_con_e" "GHC.Integer.Type.Jp#" (CILayoutFixed 1 [ObjV]) 2 #
       conClosure "h$integerzmwiredzminZCGHCziIntegerziTypeziJnzh_con_e" "GHC.Integer.Type.Jn#" (CILayoutFixed 1 [ObjV]) 3 #

       -- generic data constructor with 1 non-heapobj field
       conClosure "h$data1_e" "data1" (CILayoutFixed 1 [ObjV]) 1 #

       -- generic data constructor with 2 non-heapobj fields
       conClosure "h$data2_e" "data2" (CILayoutFixed 2 [ObjV,ObjV]) 1 #

       closure (ClosureInfo "h$noop_e" (CIRegs 1 [PtrV]) "no-op IO ()" (CILayoutFixed 0 []) (CIFun 1 0) noStatic)
               (returnS (stack .! sp)) #

       "h$noop" ||= (ApplExpr (jsv "h$c0") $ [jsv "h$noop_e"] ++ if csProf s then [jSystemCCS] else []) #

       closure (ClosureInfo "h$catch_e" (CIRegs 0 [PtrV]) "exception handler" (CILayoutFixed 2 [PtrV,IntV]) CIStackFrame noStatic)
               (adjSpN' 3 # returnS (stack .! sp)) #

       closure (ClosureInfo "h$dataToTag_e" (CIRegs 0 [PtrV]) "data to tag" (CILayoutFixed 0 []) CIStackFrame noStatic)
               ( r1 |= if_ (r1 .===. true_) 1 (if_ (typeof r1 .===. "object") (r1 .^ "f" .^ "a" - 1) 0) #
                 adjSpN' 1 #
                 returnS (stack .! sp)
               ) #
       -- function application to one argument
       closure (ClosureInfo "h$ap1_e" (CIRegs 0 [PtrV]) "apply1" (CILayoutFixed 2 [PtrV, PtrV]) CIThunk noStatic)
               (jVar $ \d1 d2 ->
                d1 |= r1 .^ "d1" #
                d2 |= r1 .^ "d2" #
                  appS "h$bh" [] #
                  profStat s enterCostCentreThunk #
                  r1 |= d1 #
                  r2 |= d2 #
                  returnS (app "h$ap_1_1_fast" [])
               ) #
       -- function application to two arguments
       closure (ClosureInfo "h$ap2_e" (CIRegs 0 [PtrV]) "apply2" (CILayoutFixed 3 [PtrV, PtrV, PtrV]) CIThunk noStatic)
               (jVar $ \d1 d2 d3 ->
                d1 |= r1 .^ "d1" #
                d2 |= r1 .^ "d2" .^ "d1" #
                d3 |= r1 .^ "d2" .^ "d2" #
                  appS "h$bh" [] #
                  profStat s enterCostCentreThunk #
                  r1 |= d1 #
                  r2 |= d2 #
                  r3 |= d3 #
                  returnS (app "h$ap_2_2_fast" [])
               ) #
       -- function application to three arguments
       closure (ClosureInfo "h$ap3_e" (CIRegs 0 [PtrV]) "apply3" (CILayoutFixed 4 [PtrV, PtrV, PtrV, PtrV]) CIThunk noStatic)
               (jVar $ \d1 d2 d3 d4 ->
                d1 |= r1 .^ "d1" #
                d2 |= r1 .^ "d2" .^ "d1" #
                d3 |= r1 .^ "d2" .^ "d2" #
                d4 |= r1 .^ "d2" .^ "d3" #
                  appS "h$bh" [] #
                  r1 |= d1 #
                  r2 |= d2 #
                  r3 |= d3 #
                  r4 |= d4 #
                  returnS (app "h$ap_3_3_fast" [])
                  ) #

       -- select first field
       closure (ClosureInfo "h$select1_e" (CIRegs 0 [PtrV]) "select1" (CILayoutFixed 1 [PtrV]) CIThunk noStatic)
               (jVar $ \t ->
                  t |= r1 .^ "d1" #
                  adjSp' 3 #
                  stack .! (sp - 2) |= r1 #
                  stack .! (sp - 1) |= var "h$upd_frame" #
                  stack .! sp |= var "h$select1_ret" #
                  r1 .^ "f" |= var "h$blackhole" #
                  r1 .^ "d1" |= var "h$currentThread" #
                  r1 .^ "d2" |= null_ #
                  r1 |= t #
                  returnS (app "h$ap_0_0_fast" [])
                  ) #

       closure (ClosureInfo "h$select1_ret" (CIRegs 0 [PtrV]) "select1ret" (CILayoutFixed 0 []) CIStackFrame noStatic)
               (r1 |= r1 .^ "d1" #
                adjSpN' 1 #
                returnS (app "h$ap_0_0_fast" [])
               ) #

       -- select second field of a two-field constructor
       closure (ClosureInfo "h$select2_e" (CIRegs 0 [PtrV]) "select2" (CILayoutFixed 1 [PtrV]) CIThunk noStatic)
               (jVar $ \t ->
                  t |= r1 .^ "d1" #
                  adjSp' 3 #
                  stack .! (sp - 2) |= r1 #
                  stack .! (sp - 1) |= var "h$upd_frame" #
                  stack .! sp |= var "h$select2_ret" #
                  r1 .^ "f" |= var "h$blackhole" #
                  r1 .^ "d1" |= var "h$currentThread" #
                  r1 .^ "d2" |= null_ #
                  r1 |= t #
                  returnS (app "h$ap_0_0_fast" [])
                  ) #

       closure (ClosureInfo "h$select2_ret" (CIRegs 0 [PtrV]) "select2ret" (CILayoutFixed 0 []) CIStackFrame noStatic)
               (r1 |= r1 .^ "d2" #
                adjSpN' 1 #
                returnS (app "h$ap_0_0_fast" [])
               ) #

       -- a thunk that just raises a synchronous exception
       closure (ClosureInfo "h$raise_e" (CIRegs 0 [PtrV]) "h$raise_e" (CILayoutFixed 0 []) CIThunk noStatic)
               (returnS (app "h$throw" [r1 .^ "d1", false_])) #

       closure (ClosureInfo "h$raiseAsync_e" (CIRegs 0 [PtrV]) "h$raiseAsync_e" (CILayoutFixed 0 []) CIThunk noStatic)
               (returnS  (app "h$throw" [r1 .^ "d1", true_])) #

       closure (ClosureInfo "h$raiseAsync_frame" (CIRegs 0 []) "h$raiseAsync_frame" (CILayoutFixed 1 []) CIStackFrame noStatic)
               (jVar $ \ex ->
                  ex |= stack .! (sp - 1) #
                  adjSpN' 2 #
                  returnS (app "h$throw" [ex, true_])) #

       {- reduce result if it's a thunk, follow if it's an ind
          add this to the stack if you want the outermost result
          to always be reduced to whnf, and not an ind -}
       closure (ClosureInfo "h$reduce" (CIRegs 0 [PtrV]) "h$reduce" (CILayoutFixed 0 []) CIStackFrame noStatic)
               (ifS (isThunk r1)
                    (returnS (r1 .^ "f"))
                    (adjSpN' 1 # returnS (stack .! sp))
               ) #

       rtsApply dflags s #
       closureTypes #

        closure (ClosureInfo "h$runio_e" (CIRegs 0 [PtrV]) "runio" (CILayoutFixed 1 [PtrV]) CIThunk noStatic)
                 (r1 |= r1 .^ "d1" #
                 stack .! preIncr sp |= var "h$ap_1_0" #
                 returnS (var "h$ap_1_0")
                 ) #

        closure (ClosureInfo "h$flushStdout_e" (CIRegs 0 []) "flushStdout" (CILayoutFixed 0 []) CIThunk noStatic)
                (r1 |= var "h$baseZCGHCziIOziHandlezihFlush" #
                 r2 |= var "h$baseZCGHCziIOziHandleziFDzistdout" #
                 returnS (app "h$ap_1_1_fast" [])) #

        "h$flushStdout" ||= app "h$static_thunk" [var "h$flushStdout_e"] #

        -- the scheduler pushes this frame when suspending a thread that
        -- has not called h$reschedule explicitly
        closure (ClosureInfo "h$restoreThread" (CIRegs 0 []) "restoreThread" CILayoutVariable CIStackFrame noStatic)
                (jVar $ \f frameSize nregs ->

                 f |= stack .! (sp - 2) #
                 frameSize |= stack .! (sp - 1) #
                 nregs |= frameSize - 3 #
                  loop 1 (.<=. nregs) (\i -> appS "h$setReg" [i, stack .! (sp - 2 - i)] #
                                             postIncrS i) #
                  sp |= sp - frameSize #
                  returnS f
                  ) #
        -- return a closure in the stack frame to the next thing on the stack
        closure (ClosureInfo "h$return" (CIRegs 0 []) "return" (CILayoutFixed 1 [PtrV]) CIStackFrame noStatic)
                (r1 |= stack .! (sp - 1) #
                adjSpN' 2 #
                returnS (stack .! sp)) #

        --  return a function in the stack frame for the next call
        closure (ClosureInfo "h$returnf" (CIRegs 0 [PtrV]) "returnf" (CILayoutFixed 1 [ObjV]) CIStackFrame noStatic)
                (jVar $ \r ->
                r |= stack .! (sp - 1) #
                   adjSpN' 2 #
                   returnS r
                  ) #
        -- return this function when the scheduler needs to come into action
        -- (yield, delay etc), returning thread needs to push all relevant
        -- registers to stack frame, thread will be resumed by calling the stack top
        closure (ClosureInfo "h$reschedule" (CIRegs 0 []) "reschedule" (CILayoutFixed 0 []) CIThunk noStatic)
                (returnS $ var "h$reschedule") #

        -- debug thing, insert on stack to dump current result, should be boxed
        closure (ClosureInfo "h$dumpRes" (CIRegs 0 [PtrV]) "dumpRes" (CILayoutFixed 1 [ObjV]) CIThunk noStatic)
                (jVar $ \re ->
                 appS "h$log" ["h$dumpRes result: " + stack .! (sp-1)] #
                 appS "h$log" [r1] #
                 appS "h$log" [app "h$collectProps" [r1]] #
                 ifS' ((r1 .^ "f") .&&. (r1 .^ "f" .^ "n")) (appS "h$log" ["name: " + r1 .^ "f" .^ "n"]) #
                 ifS' (ApplExpr (r1 .^ "hasOwnProperty") ["d1"]) (appS "h$log" ["d1: " + r1 .^ "d1"]) #
                 ifS' (ApplExpr (r1 .^ "hasOwnProperty") ["d2"]) (appS "h$log" ["d2: " + r1 .^ "d2"]) #
                 ifS' (r1 .^ "f")
                      (re |= UOpExpr NewOp (app "RegExp" ["([^\\n]+)\\n(.|\\n)*"]) #
                        appS "h$log" ["function" + ApplExpr ((ApplExpr (("" + r1 .^ "f") .^ "substring") [0, 50]) .^ "replace") [r1, "$1"]]
                        ) #
                  adjSpN' 2 #
                  r1 |= null_ #
                  returnS (stack .! sp)) #

         closure (ClosureInfo "h$resume_e" (CIRegs 0 [PtrV]) "resume" (CILayoutFixed 0 []) CIThunk noStatic)
                  (jVar $ \ss ->
                   ss |= r1 .^ "d1" #
                   updateThunk' s #
                   loop 0 (.<. ss .^ "length") (\i ->
                     stack .! (sp+1+i) |= ss .! i #
                     postIncrS i) #
                   sp |= sp + ss .^ "length" #
                   r1 |= null_ #
                   returnS (stack .! sp)) #

       closure (ClosureInfo "h$unmaskFrame" (CIRegs 0 [PtrV]) "unmask" (CILayoutFixed 0 []) CIStackFrame noStatic)
               (var "h$currentThread" .^ "mask" |= 0 #
                adjSpN' 1 #
                -- back to scheduler to give us async exception if pending
                ifS (var "h$currentThread" .^ "excep" .^ "length" .>. 0)
                    (push' s [r1, var "h$return"] # returnS (var "h$reschedule"))
                    (returnS (stack .! sp))
                ) #

        closure (ClosureInfo "h$maskFrame" (CIRegs 0 [PtrV]) "mask" (CILayoutFixed 0 []) CIStackFrame noStatic)
                (var "h$currentThread" .^ "mask" |= 2 #
                 adjSpN' 1 #
                 returnS (stack .! sp)) #

        closure (ClosureInfo "h$maskUnintFrame" (CIRegs 0 [PtrV]) "maskUnint" (CILayoutFixed 0 []) CIStackFrame noStatic)
                (var "h$currentThread" .^ "mask" |= 1 #
                 adjSpN' 1 #
                 returnS (stack .! sp)
                ) #

       closure (ClosureInfo "h$unboxFFIResult" (CIRegs 0 [PtrV]) "unboxFFI" (CILayoutFixed 0 []) CIStackFrame noStatic)
               (jVar $ \d ->
                  d |= r1 .^ "d1" #
                  loop 0 (.<. d .^ "length") (\i -> appS "h$setReg" [i + 1, d .! i] #
                                                    postIncrS i) #
                  adjSpN' 1 #
                  returnS (stack .! sp)
                  ) #

       closure (ClosureInfo "h$unbox_e" (CIRegs 0 [PtrV]) "unboxed value" (CILayoutFixed 1 [DoubleV]) CIThunk noStatic)
               (r1 |= r1 .^ "d1" #
                returnS (stack .! sp)) #

       closure (ClosureInfo "h$retryInterrupted" (CIRegs 0 [ObjV]) "retry interrupted operation" (CILayoutFixed 1 [ObjV]) CIStackFrame noStatic)
               (jVar $ \a ->
                  a |= stack .! (sp - 1) #
                  adjSpN' 2 #
                  returnS (ApplExpr (a .! 0 .^ "apply") [var "this", ApplExpr (a .^ "slice") [1]])) #

       closure (ClosureInfo "h$atomically_e" (CIRegs 0 [PtrV]) "atomic operation" (CILayoutFixed 1 [PtrV]) CIStackFrame noStatic)
               (ifS (app "h$stmValidateTransaction" [])
                    (appS "h$stmCommitTransaction" [] #
                     adjSpN' 2 #
                     returnS (stack .! sp)
                    )
                    ( push' s [var "h$checkInvariants_e"] #
                      returnS (app "h$stmStartTransaction" [stack .! (sp - 2)]))) #

       closure (ClosureInfo "h$checkInvariants_e" (CIRegs 0 [PtrV]) "check transaction invariants" (CILayoutFixed 0 []) CIStackFrame noStatic)
               (adjSpN' 1 #
                returnS (app "h$stmCheckInvariants" [])) #

                closure (ClosureInfo "h$stmCheckInvariantStart_e" (CIRegs 0 []) "start checking invariant" (CILayoutFixed 2 [ObjV, RtsObjV]) CIStackFrame noStatic)
                        (jVar $ \t inv m t1 ->
                         t |= stack .! (sp - 2) #
                         inv |= stack .! (sp - 1) #
                         m |= var "h$currentThread" .^ "mask" #
                         adjSpN' 3 #
                         t1 |= UOpExpr NewOp (app "h$Transaction" [inv .^ "action", t]) #
                         t1 .^ "checkRead" |= UOpExpr NewOp (app "h$Set" []) #
                         var "h$currentTread" .^ "transaction" |= t1 #
                         push' s [t1, m, var "h$stmInvariantViolatedHandler", var "h$catchStm_e"] #
                         r1 |= inv .^ "action" #
                         returnS (app "h$ap_1_0_fast" [])
                         ) #

                closure (ClosureInfo "h$stmCheckInvariantResult_e" (CIRegs 0 [PtrV]) "finish checking invariant" (CILayoutFixed 1 [ObjV]) CIStackFrame noStatic)
                        (jVar $ \inv ->
                        inv |= stack .! (sp -1) #
                        adjSpN' 2 #
                        appS "h$stmUpdateInvariantDependencies" [inv] #
                        appS "h$stmAbortTransaction" [] #
                        returnS (stack .! sp)
                        ) #

                -- update invariant TVar dependencies and rethrow exception
                -- handler must be pushed above h$stmCheckInvariantResult_e frame

                closure (ClosureInfo "h$stmInvariantViolatedHandler_e" (CIRegs 0 [PtrV]) "finish checking invariant" (CILayoutFixed 0 []) (CIFun 2 1) noStatic)
                        (jVar $ \inv ->
                         ifS' (stack .! sp .===. var "h$stmCheckInvariantResult_e")
                             (appS "throw" ["h$stmInvariantViolatedHandler_e: unexpected value on stack"]) #
                         inv |= stack .! (sp - 2) #
                         adjSpN' 2 #
                         appS "h$stmUpdateInvariantDependencies" [] #
                         appS "h$stmAbortTransaction" [] #
                         returnS (app "h$throw" [r2, false_])
                        ) #

                "h$stmInvariantViolatedHandler" ||= app "h$c" ([var "h$stmInvariantViolatedHandler_e"] ++ if csProf s then [jSystemCCS] else []) #

                closure (ClosureInfo "h$stmCatchRetry_e" (CIRegs 0 [PtrV]) "catch retry" (CILayoutFixed 1 [PtrV]) CIStackFrame noStatic)
                        (adjSpN' 2 #
                        appS "h$stmCommitTransaction" [] #
                        returnS (stack .! sp)) #

               closure (ClosureInfo "h$catchStm_e" (CIRegs 0 [PtrV]) "STM catch" (CILayoutFixed 3 [ObjV,PtrV,ObjV]) CIStackFrame noStatic)
                       (adjSpN' 4 #
                        returnS (stack .! sp)) #

               closure (ClosureInfo "h$stmResumeRetry_e" (CIRegs 0 [PtrV]) "resume retry" (CILayoutFixed 0 []) CIStackFrame noStatic)
                        (jVar $ \blocked ->
                         ifS' (stack .! (sp - 2) .!==. var "h$atomically_e")
                              (appS "throw" ["h$stmResumeRetry_e: unexpected value on stack"]) #
                         blocked |= stack .! (sp - 1) #
                           adjSpN' 2 #
                           push' s [var "h$checkInvariants_e"] #
                           appS "h$stmRemoveBlockedThread" [blocked, var "h$currentThread"] #
                           returnS (app "h$stmStartTransaction" [stack .! (sp - 2)])

                           ) #

                closure (ClosureInfo "h$lazy_e" (CIRegs 0 [PtrV]) "generic lazy value" (CILayoutFixed 0 []) CIThunk noStatic)
                        (jVar $ \x ->
                         x |= ApplExpr (r1 .^ "d1") [] #
                         appS "h$bh" [] #
                         profStat s enterCostCentreThunk #
                         r1 |= x #
                         returnS (stack .! sp))

     ) #
  -- Top-level statements to generate only in profiling mode
  profStat s (
     closure (ClosureInfo "h$setCcs_e" (CIRegs 0 [PtrV]) "set cost centre stack" (CILayoutFixed 1 [ObjV]) CIStackFrame noStatic)
             (appS "h$restoreCCS" [stack .! (sp - 1)] # adjSpN' 2 # returnS (stack .! sp))

  )
