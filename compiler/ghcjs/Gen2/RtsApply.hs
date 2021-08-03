{-# LANGUAGE OverloadedStrings #-}

{- |
  generate various apply functions for the RTS, for speeding up
  function application in the most common cases. The code is generated
  because it contains lots of repeating patterns, and to make it more
  flexible when changing the RTS (for example how arguments are passed)

  The code in here can be a bit hard to read due to all the generated
  low-level access things. Reading rts.js for a compiled program can be
  easier (the file is always the same unless you change low-level RTS
  options)

  - fixme: add selector thunks and let the gc follow them
-}

module Gen2.RtsApply where

import           CostCentre
import           DynFlags

import           Compiler.JMacro
import           Compiler.JMacro.Combinators
import           Compiler.JMacro.Symbols

import           Gen2.RtsAlloc
import           Gen2.RtsTypes
import           Gen2.ClosureInfo
import           Gen2.Profiling

import qualified Data.Bits as Bits
import qualified Data.Text as T
import Prelude

te :: T.Text -> JExpr
te = toJExpr

rtsApply :: DynFlags -> CgSettings -> JStat
rtsApply dflags s =
            mconcat  $ map (uncurry (stackApply dflags s)) applySpec
                    ++ map (uncurry (fastApply dflags s)) applySpec
                    ++ map (pap dflags s) specPap
                    ++ [ mkApplyArr
                       , genericStackApply dflags s
                       , genericFastApply dflags s
                       , zeroApply s
                       , updates dflags s
                       , papGen dflags s
                       , moveRegs2
                       , selectors dflags s
                       ]

-- specialized apply for these
-- make sure that once you are in spec, you stay there
applySpec :: [(Int,Int)] -- regs,arity
applySpec = [ (regs,arity)  | arity <- [1..4], regs <- [max 0 (arity-1)..(arity*2)]]

specApply :: Bool -> Int -> Int -> Maybe JExpr
specApply fast n r
  | (r,n) == (0,0)         = Just (toJExpr . TxtI . T.pack $ ("h$ap_0_0" ++ fastSuff))
  | (r,n) == (0,1)         = Just (toJExpr . TxtI . T.pack $ ("h$ap_1_0" ++ fastSuff))
  | (r,n) `elem` applySpec =
        Just (toJExpr . TxtI . T.pack $ ("h$ap_" ++ show n ++ "_" ++ show r ++ fastSuff))
  | otherwise = Nothing
   where
      fastSuff | fast      = "_fast"
               | otherwise = ""
{-
  Build arrays to quickly lookup apply functions, getting the fast variant when possible
   - h$apply[r << 8 | n] = function application for r regs, n args
   - h$paps[r]           = partial application for r registers (number of args is in the object)
 -}
mkApplyArr :: JStat
mkApplyArr =
   TxtI "h$apply" ||= e (JList []) #
   TxtI "h$paps"  ||= e (JList []) #
   (var "h$initStatic" .^ "push") |$ [
      ValExpr (JFunc [] (
         jVar (\i ->
            i |= 0 #
            WhileStat False (i .<. 65536)
                      (var "h$apply" .! i |= var "h$ap_gen" #
                       UOpStat PreInc i
                      ) #
            i |= 0 #
            WhileStat False (i .<. 128)
                      (var "h$paps" .! i |= var "h$pap_gen" #
                       UOpStat PreInc i
                      ) #
            var "h$apply" .! 0 |= var "h$ap_0_0" #
            mconcat (map assignSpec applySpec) #
            mconcat (map assignPap specPap)
         )
      ))
   ]
  where
    assignSpec :: (Int, Int) -> JStat
    assignSpec (r,n) =
      var "h$apply" .! (e $ Bits.shiftL r 8 Bits..|. n) |=
           e (TxtI . T.pack $ ("h$ap_" ++ show n ++ "_" ++ show r))

    assignPap :: Int -> JStat
    assignPap p = var "h$paps" .! e p |=
                      e (TxtI . T.pack $ ("h$pap_" ++ show p))

-- generic stack apply that can do everything, but less efficiently
-- on stack: tag: (regs << 8 | arity)
-- fixme: set closure info of stack frame
genericStackApply :: DynFlags -> CgSettings -> JStat
genericStackApply dflags s =
   closure (ClosureInfo "h$ap_gen" (CIRegs 0 [PtrV]) "h$ap_gen" CILayoutVariable CIStackFrame noStatic)
           (jVar $ \cf ->
            traceRts s "h$ap_gen" #
            cf |= r1 .^ "f" #
               SwitchStat (cf .^ "t")
                          [(e Thunk, profStat s pushRestoreCCS #
                                     returnS cf)
                          ,(e Fun,   funCase cf (funArity' cf))
                          ,(e Pap,   funCase cf (papArity r1))
                          ,(e Blackhole, push' s [r1, var "h$return"] #
                                         returnS (app "h$blockOnBlackhole" [r1]))
                          ]
                          (appS "throw" ["h$ap_gen: unexpected closure type " + (cf .^ "t")])
            )
  where
    funCase c arity =
      jVar $ \myArity ar myAr myRegs regs newTag newAp p dat ->
      myArity |= stack .! (sp - 1) #
      ar |= arity .&. 0xFF #
      myAr |= myArity .&. 0xFF #
      myRegs |= myArity .>>. 8 #
      traceRts s ("h$ap_gen: args: " + myAr + " regs: " + myRegs) #
      ifS (myAr .===. ar)
          (traceRts s "h$ap_gen: exact" #
          loop 0 (.<. myRegs) (\i -> appS "h$setReg" [i+2, stack .! (sp-2-i)] #
                                     postIncrS i
                              ) #
          sp |= sp - myRegs - 2 #
          returnS c
          )
          (ifS (myAr .>. ar)
                (regs |= arity .>>. 8 #
                traceRts s ("h$ap_gen: oversat: arity: " + ar + " regs: " + regs) #
                loop 0 (.<. regs) (\i -> traceRts s ("h$ap_gen: loading register: " + i) #
                                         appS "h$setReg" [i+2, stack .! (sp-2-i)] #
                                         postIncrS i) #
               newTag |= ((myRegs-regs).<<.8).|.(myAr - ar) #
               newAp |= var "h$apply" .! newTag #
               traceRts s ("h$ap_gen: next: " + (newAp .^ "n")) #
               ifS (newAp .===. var "h$ap_gen")
                   (sp |= sp - regs #
                    stack .! (sp - 1) |= newTag)
                   (sp |= sp - regs - 1) #
               stack .! sp |= newAp #
               profStat s pushRestoreCCS #
               returnS c
               )
               (traceRts s "h$ap_gen: undersat" #
                p |= var "h$paps" .! myRegs #
                dat |= e [r1, (((arity .>>. 8)-myRegs))*256+ar-myAr] #
                loop 0 (.<. myRegs) (\i -> dat .^ "push" |$ [stack .! (sp - i - 2)] #
                                           postIncrS i) #
                sp |= sp - myRegs - 2 #
                r1 |= initClosure dflags p dat jCurrentCCS #
                returnStack
                  ))

{-
  generic fast apply: can handle anything (slowly)
  signature tag in argument
-}
genericFastApply :: DynFlags -> CgSettings -> JStat
genericFastApply dflags s =
   TxtI "h$ap_gen_fast" ||= jLam (\tag -> jVar (\c ->
      traceRts s ("h$ap_gen_fast: " + tag) #
      c |= r1 .^ "f" #
      SwitchStat (c .^ "t")
                 [(e Thunk, traceRts s "h$ap_gen_fast: thunk" #
                            pushStackApply c tag #
                            returnS c )
                  ,(e Fun, jVar (\farity -> farity |= funArity' c #
                          traceRts s ("h$ap_gen_fast: fun " + farity) #
                          funCase c tag farity))
                  ,(e Pap, jVar (\parity -> parity |= papArity r1 #
                          traceRts s ("h$ap_gen_fast: pap " + parity) #
                          funCase c tag parity)
                          )
                  ,(e Con, traceRts s "h$ap_gen_fast: con" #
                           ifS' (tag .!=. 0) (appS "throw" ["h$ap_gen_fast: invalid apply"]) #
                           returnS c)
                  ,(e Blackhole, traceRts s "h$ap_gen_fast: blackhole" #
                                pushStackApply c tag #
                                push' s [r1, var "h$return"] #
                                returnS (app "h$blockOnBlackhole" [r1]))
                  ]
                  (appS "throw" ["h$ap_gen_fast: unexpected closure type: " + c .^ "t"])
      
   ))
  where
     -- thunk: push everything to stack frame, enter thunk first
    pushStackApply :: JExpr -> JExpr -> JStat
    pushStackApply _c tag =
      jVar $ \ap ->
      pushAllRegs tag #
      ap |= var "h$apply" .! tag #
      ifS (ap .===. var "h$ap_gen")
          (sp |= sp + 2 # stack .! (sp-1) |= tag)
          (sp |= sp + 1) #
      stack .! sp |= ap #
      profStat s pushRestoreCCS

    funCase :: JExpr -> JExpr -> JExpr -> JStat
    funCase c tag arity =
      jVar $ \ar myAr myRegs regsStart newTag newAp dat p ->
      ar |= arity .&. 0xFF #
      myAr |= tag .&. 0xFF #
      myRegs |= tag .>>. 8 #
      traceRts s ("h$ap_gen_fast: args: " + myAr + " regs: " + myRegs) #
      ifS (myAr .===. ar)
        -- call the function directly
        (traceRts s "h$ap_gen_fast: exact" #
        returnS c)
        (ifS (myAr .>. ar) 
             (-- push stack frame with remaining args, then call fun
              traceRts s ("h$ap_gen_fast: oversat " + sp) #
              regsStart |= (arity .>>. 8) + 1 #
              sp |= sp + myRegs - regsStart + 1 #
              traceRts s ("h$ap_gen_fast: oversat " + sp) #
              pushArgs regsStart myRegs #
              newTag |= (((myRegs-( arity.>>.8)).<<.8)).|.(myAr-ar) #
              newAp |= var "h$apply" .! newTag #
              ifS (newAp .===. var "h$ap_gen")
                  (sp |= sp + 2 # stack .! (sp - 1) |= newTag)
                  (sp |= sp + 1) #
              stack .! sp |= newAp #
              profStat s pushRestoreCCS #
              returnS c
             )
             (traceRts s ("h$ap_gen_fast: undersat: " + myRegs + " " + tag) #
              ifS' (tag .!=. 0)
                    (p |= var "h$paps" .! myRegs #
                    dat |= (e [r1, (((arity .>>. 8)-myRegs))*256+ar-myAr]) #
                    loop 0 (.<. myRegs) (\i -> dat .^ "push" |$ [app "h$getReg" [i+2]] #
                                               postIncrS i) #
                     r1 |= initClosure dflags p dat jCurrentCCS
                     ) 
                     #
              returnStack))


    pushAllRegs :: JExpr -> JStat
    pushAllRegs tag =
      jVar $ \regs -> 
      regs |= tag .>>. 8 #
      sp |= sp + regs #
      SwitchStat regs (map pushReg [65,64..2]) mempty
      where
        pushReg :: Int -> (JExpr, JStat)
        pushReg r = (e (r-1),  stack .! (sp - (e (r - 2))) |= e (numReg r))

    pushArgs :: JExpr -> JExpr -> JStat
    pushArgs start end =
      loop end (.>=.start) (\i -> traceRts s ("pushing register: " + i) #
                                  stack .! (sp + start - i) |= app "h$getReg" [i+1] #
                                  postDecrS i
                           )

stackApply :: DynFlags
           -> CgSettings
           -> Int         -- ^ number of registers in stack frame
           -> Int         -- ^ number of arguments
           -> JStat
stackApply dflags s r n =
  closure (ClosureInfo funcName (CIRegs 0 [PtrV]) funcName layout CIStackFrame noStatic)
          body
  where
    layout    = CILayoutUnknown r

    funcName = T.pack ("h$ap_" ++ show n ++ "_" ++ show r)

    body =
        jVar $ \c ->
          c |= r1 .^ "f" #
          traceRts s (e funcName + " " + (c .^ "n") + " sp: " + sp + " a: " + (c .^ "a")) #
          SwitchStat (c .^ "t")
                     [(e Thunk, traceRts s (e $ funcName <> ": thunk") #
                       profStat s pushRestoreCCS #
                       returnS c)                           
                     ,(e Fun, traceRts s (e $ funcName <> ": fun") #
                             funCase c)
                     ,(e Pap, traceRts s (e $ funcName <> ": pap") #
                              papCase c)
                     ,(e Blackhole, push' s [r1, var "h$return"] #
                                    returnS (app "h$blockOnBlackhole" [r1]))
                     ]
                     (appS "throw" [e ("panic: " <> funcName <> ", unexpected closure type: ") + (c .^ "t")])         

    funExact c = popSkip' 1 (reverse $ take r (map toJExpr $ enumFrom R2)) #
                 returnS c
    stackArgs = map (\x -> stack .! (sp - e x)) [1..r]

    papCase :: JExpr -> JStat
    papCase c = jVar $ \(ValExpr (JVar pap)) arity0 arity ->
      arity0 |= papArity r1 #
      arity |= arity0 .&. 0xFF #
      traceRts s (e (funcName <> ": found pap, arity: ") + arity) #
      ifS (e n .===. arity)
          (traceRts s (e (funcName <> ": exact")) #
           funExact c)
          (ifS (e n .>. arity)
                (traceRts s (e (funcName <> ": oversat")) #
                 oversatCase c arity0 arity)
                (traceRts s (e (funcName <> ": undersat")) #
                -- fixme do we want double pap?
                 mkPap dflags s pap r1 (e n) stackArgs #
                 sp |= sp - e (r + 1) #
                 r1 |= e pap #
                 returnStack

          )
         )   

    funCase :: JExpr -> JStat
    funCase c = jVar $ \(ValExpr (JVar pap)) ar0 ar ->
               ar0 |= funArity' c #
               ar |= (ar0 .&. 0xFF) #
               ifS (e n .===. ar)
                   (traceRts s (e (funcName <> ": exact")) #
                    funExact c
                   )
                   (ifS (e n .>. ar)
                        (traceRts s (e (funcName <> ": oversat")) #
                        oversatCase c ar0 ar)
                        (traceRts s (e (funcName <> ": undersat")) #
                         mkPap dflags s pap (toJExpr R1) (toJExpr n) stackArgs #
                         sp |= sp - (e (r+1)) #
                         r1 |= e pap #
                         returnStack
                   )
                  )   

    -- oversat: call the function but keep enough on the stack for the next
    oversatCase :: JExpr -- function
                -> JExpr -- the arity tag
                -> JExpr -- real arity (arity & 0xff)
                -> JStat
    oversatCase c arity arity0 =
      jVar (\rs newAp ->
      rs |= (arity .>>. 8) #
      loadRegs rs #
      sp |= sp - rs #
      newAp |= (var "h$apply" .! ((e n-arity0).|.((e r-rs).<<.8))) #
      stack .! sp |= newAp #
      profStat s pushRestoreCCS #
      traceRts s (e (funcName <> ": new stack frame: ") + (newAp .^ "n")) #
      returnS c
         )   

      where
        loadRegs rs = SwitchStat rs switchAlts mempty
          where
            switchAlts = map (\x -> (e x, e (numReg (x+1)) |= stack .! (sp - e x))) [r,r-1..1]

{-
  stg_ap_r_n_fast is entered if a function of unknown arity
  is called, n arguments are already in r registers
-}
fastApply :: DynFlags -> CgSettings -> Int -> Int -> JStat
fastApply dflags s r n = func ||= e (JFunc myFunArgs body)
    where
      funName = T.pack ("h$ap_" ++ show n ++ "_" ++ show r ++ "_fast")
      func    = TxtI funName

      myFunArgs = []

      regArgs = take r (enumFrom R2)

      mkAp :: Int -> Int -> [JExpr]
      mkAp n' r' = [ jsv . T.pack $ "h$ap_" ++ show n' ++ "_" ++ show r' ]

      body = 
        jVar $ \c farity arity ->
        c |= r1 .^ "f" #
        traceRts s (e (funName <> ": sp ") + sp) #
        SwitchStat (c .^ "t")
                   [(e Fun, traceRts s (e (funName <> ": ") + clName c + " (arity: " + (c .^ "a") + ")") #
                            farity |= funArity' c #
                            funCase c farity)
                   ,(e Pap, traceRts s (e (funName <> ": pap")) #
                             arity |= papArity r1 #
                             funCase c arity)
                   ,(e Thunk, traceRts s (e (funName <> ": thunk")) #
                               push' s (reverse (map e $ take r (enumFrom R2)) ++ mkAp n r) #
                               profStat s pushRestoreCCS #
                               returnS c)
                   ,(e Blackhole, traceRts s (e (funName <> ": blackhole")) #
                                  push' s (reverse (map e $ take r (enumFrom R2)) ++ mkAp n r) #
                                  push' s [r1, var "h$return"] #
                                  returnS (app "h$blockOnBlackhole" [r1]))]
                   (appS "throw" [e (funName <> ": unexpected closure type: ") + c .^ "t"])

      funCase :: JExpr -> JExpr -> JStat
      funCase c arity = jVar $ \(ValExpr (JVar pap)) ar ->
               ar |= arity .&. 0xFF #
               ifS (e n .===. ar)
                (traceRts s (e (funName <> ": exact")) #
                 returnS c
                ) 
                (ifS (e n .>. ar)
                     (traceRts s (e (funName <> ": oversat")) #
                      oversatCase c arity
                     )
                     (traceRts s (e (funName <> ": undersat")) #
                      mkPap dflags s pap r1 (e n) (map toJExpr regArgs) #
                      r1 |= e pap #
                      returnStack
                     ))

      oversatCase :: JExpr -> JExpr -> JStat
      oversatCase c arity =
         jVar $ \rs rsRemain ->
            rs |= arity .>>. 8 #
            rsRemain |= e r - rs #
            traceRts s (e (funName <> " regs oversat ") + rs + " remain: " + rsRemain) #
            saveRegs rs #
            sp |= sp + rsRemain + 1 #
            stack .! sp |= var "h$apply" .! ((rsRemain.<<.8).|.(e n-(arity.&.0xFF))) #
            profStat s pushRestoreCCS #
            returnS c
          where
            saveRegs n = SwitchStat n switchAlts mempty
              where
                switchAlts = map (\x -> (e x, stack .! (sp + e (r-x)) |= e (numReg (x+2)))) [0..r-1]

zeroApply :: CgSettings -> JStat
zeroApply s =
      "h$ap_0_0_fast" ||= jLam (enter s r1) #
      closure (ClosureInfo "h$ap_0_0" (CIRegs 0 [PtrV]) "h$ap_0_0" (CILayoutFixed 0 []) CIStackFrame noStatic)
              (adjSpN' 1 # enter s r1) #
      
      "h$e" ||= jLam (\c -> r1 |= c # enter s c)

-- carefully enter a closure that might be a thunk or a function

-- ex may be a local var, but must've been copied to R1 before calling this
enter :: CgSettings -> JExpr -> JStat
enter s ex =
         jVar $ \c ->
         ifS' (app "typeof" [ex] .!==. "object") returnStack #
         c |= ex .^ "f" #
         ifS' (c .===. var "h$unbox_e")
              (r1 |= ex .^ "d1" # returnStack) #
         SwitchStat (c .^ "t")
                    [(e Con, mempty)
                    ,(e Fun, mempty)
                    ,(e Pap, returnStack)
                    ,(e Blackhole, push' s [var "h$ap_0_0", ex, var "h$return"] #
                                   returnS (app "h$blockOnBlackhole" [ex]))]
                    (returnS c)

updates :: DynFlags -> CgSettings -> JStat
updates _dflags s =
  closure (ClosureInfo "h$upd_frame" (CIRegs 0 [PtrV]) "h$upd_frame" (CILayoutFixed 1 [PtrV]) CIStackFrame noStatic)
        (jVar $ \updatee waiters ss si sir ->
      updatee |= stack .! (sp - 1) #
      traceRts s ("h$upd_frame updatee alloc: " + updatee .^ "alloc") #
      -- wake up threads blocked on blackhole
      waiters |= updatee .^ "d2" #
        ifS' (waiters .!==. null_)
      (loop 0 (.<. waiters .^ "length") (\i -> appS "h$wakeupThread" [waiters .! i] #
                                              postIncrS i)) #
      -- update selectors
      ifS' ((app "typeof" [updatee .^ "m"] .===. "object") .&&. (updatee .^ "m" .^ "sel"))
           (ss |= updatee .^ "m" .^ "sel" #
            loop 0 (.<. ss .^ "length")
                   (\i -> si |= ss .! i #
                          sir |= si .^ "d2" .$ [r1] #
                          ifS(app "typeof" [sir] .===. "object")
                             (si .^ "f" |= sir .^ "f" #
                             si .^ "d1" |= sir .^ "d1" #
                             si .^ "d2" |= sir .^ "d2" #
                             si .^ "m" |= sir .^ "m")
                             (si .^ "f" |= var "h$unbox_e" #
                             si .^ "d1" |= sir #
                             si .^ "d2" |= null_ #
                             si .^ "m" |= 0) #
                             postIncrS i
                           ))
             #
      -- overwrite the object
      ifS (app "typeof" [r1] .===. "object")
          (traceRts s ("$upd_frame: boxed: " + ((r1 .^ "f") .^ "n")) #
           updatee .^ "f" |= r1 .^ "f" #
           updatee .^ "d1" |= r1 .^ "d1" #
           updatee .^ "d2" |= r1 .^ "d2" # 
           updatee .^ "m" |= r1 .^ "m" #
           profStat s (updateCC updatee)
         )
         (updatee .^ "f" |= var "h$unbox_e" #
          updatee .^ "d1" |= r1 #
                           updatee .^ "d2" |= null_ #
                           updatee .^ "m" |= 0 #
                           profStat s (updateCC updatee)
                           ) #
         adjSpN' 2 #
         traceRts s ("h$upd_frame: updating: " + updatee + " -> " + r1) #
         returnStack

         )
    #
   closure (ClosureInfo "h$upd_frame_lne" (CIRegs 0 [PtrV]) "h$upd_frame_lne" (CILayoutFixed 1 [PtrV]) CIStackFrame noStatic)
           (jVar $ \updateePos -> 
            updateePos |= stack .! (sp - 1) #
            stack .! updateePos |= r1 #
            adjSpN' 2 #
            traceRts s ("h$upd_frame_lne: updating: " + updateePos + " -> " + r1) #
            returnStack)
  where
    updateCC updatee = updatee .^ "cc" |= jCurrentCCS

selectors :: DynFlags -> CgSettings -> JStat
selectors _df s =
  mkSel "1"   (.^ "d1") <>
  mkSel "2a"  (.^ "d2") <>
  mkSel "2b"  (\x -> x .^ "d2" .^ "d1") <>
  mconcat (map mkSelN [3..16])
   where
    mkSelN :: Int -> JStat
    mkSelN x = mkSel (T.pack $ show x)
                     (\e -> SelExpr ((SelExpr (toJExpr e)) (TxtI (T.pack "d2"))){-[je| `e`.d2 |]-}
                            (TxtI $ T.pack ("d" ++ show (x-1)))
                     )


    mkSel :: T.Text -> (JExpr -> JExpr) -> JStat
    mkSel name sel =
        TxtI createName ||= jLam (\r -> traceRts s (e ("selector create: " <> name <> " for ") + (r .^ "alloc")) #
                                        ifS (isThunk r .||. isBlackhole r)
                                            (returnS (app "h$mkSelThunk" [r, e (v entryName), e (v resName)]))
                                            (returnS (sel r))
                                            

        ) #

        TxtI resName ||= jLam (\r -> traceRts s (e ("selector result: " <> name <> " for ") + (r .^ "alloc")) #
                                returnS (sel r)) #

        closure (ClosureInfo entryName (CIRegs 0 [PtrV]) ("select " <> name) (CILayoutFixed 1 [PtrV]) CIThunk noStatic)
                (jVar $ \tgt -> 
                 tgt |= r1 .^ "d1" #
                 traceRts s (e ("selector entry: " <> name <> " for ") + (tgt .^ "alloc")) #
                 ifS (isThunk tgt .||. isBlackhole tgt)
                     (preIncrS sp #
                      stack .! sp |= var frameName #
                      returnS (app "h$e" [tgt]))
                     (returnS (app "h$e" [sel tgt]))
                  ) #

        closure (ClosureInfo frameName (CIRegs 0 [PtrV]) ("select " <> name <> " frame") (CILayoutFixed 0 []) CIStackFrame noStatic)
                (traceRts s (e ("selector frame: " <> name)) #
                 postDecrS sp #
                 returnS (app "h$e" [sel r1]))

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
mkPap :: DynFlags
      -> CgSettings
      -> Ident   -- ^ id of the pap object
      -> JExpr   -- ^ the function that's called (can be a second pap)
      -> JExpr   -- ^ number of arguments in pap
      -> [JExpr] -- ^ values for the supplied arguments
      -> JStat
mkPap _dflags s tgt fun n values =
      traceRts s (e $ "making pap with: " ++ show (length values) ++ " items") #
      allocDynamic s True tgt (e entry) (fun:papAr:map toJExpr values')
        (if csProf s then Just jCurrentCCS else Nothing)
  where
    papAr = funOrPapArity fun Nothing - e (length values * 256) - n

    values' | null values = [null_]
            | otherwise   = values
    entry | length values > numSpecPap = TxtI "h$pap_gen"
          | otherwise                  = TxtI . T.pack $ "h$pap_" ++ show (length values)

-- specialized (faster) pap generated for [0..numSpecPap]
-- others use h$pap_gen
specPap :: [Int]
specPap = [0..numSpecPap]

numSpecPap :: Int
numSpecPap = 6

pap :: DynFlags
    -> CgSettings
    -> Int
    -> JStat
pap _dflags s r = closure (ClosureInfo funcName CIRegsUnknown funcName (CILayoutUnknown (r+2)) CIPap noStatic) body
  where
    funcName = T.pack ("h$pap_" ++ show r)

    body = 
      (jVar $ \c d f extra ->
         c |= r1 .^ "d1" #
         d |= r1 .^ "d2" #
         f |= c .^ "f" #
         assertRts s (isFun' f .||. isPap' f) (funcName <> ": expected function or pap") #
         profStat s (enterCostCentreFun currentCCS) #
         extra |= (funOrPapArity c (Just f) .>>. 8) - e r #
         traceRts s (te (funcName <> ": pap extra args moving: ") + extra) #
         moveBy extra #
         loadOwnArgs d #
         r1 |= c #
         returnS f
      )
    moveBy extra = SwitchStat extra
                   (reverse $ map moveCase [1..maxReg-r-1]) mempty
    moveCase m = (e m, e (numReg (m+r+1)) |= e (numReg (m+1)))
    loadOwnArgs d = mconcat $ map (\r ->
        e (numReg (r+1)) |= dField d (r+2)) [1..r]
    dField d n = SelExpr d (TxtI . T.pack $ ('d':show (n-1)))

-- generic pap
papGen :: DynFlags -> CgSettings -> JStat
papGen _dflags s =
   closure (ClosureInfo funcName CIRegsUnknown funcName CILayoutVariable CIPap noStatic)
           (jVar $ \c f d pr or r ->
      
                         c |= r1 .^ "d1" #
                         f |= c .^ "f" #
                         d |= r1 .^ "d2" #
                         pr |= funOrPapArity c (Just f) .>>. 8 #
                         or |= papArity r1 .>>. 8 #
                         r |= pr - or #
                         assertRts s (isFun' f .||. isPap' f) (te "h$pap_gen: expected function or pap") #
                         profStat s (enterCostCentreFun currentCCS) #
                         traceRts s (te "h$pap_gen: generic pap extra args moving: " + or) #
                         appS "h$moveRegs2" [or, r] #
                         loadOwnArgs d r #
                         r1 |= c #
                         returnS f
                           )

  where
    funcName = "h$pap_gen"
    loadOwnArgs d r =
      let prop n = d .^ ("d" <> T.pack (show $ n+1))
          loadOwnArg n = (e n, e (numReg (n+1)) |= prop n)
      in  SwitchStat r (map loadOwnArg [127,126..1]) mempty

-- general utilities
-- move the first n registers, starting at R2, m places up (do not use with negative m)
moveRegs2 :: JStat
moveRegs2 = "h$moveRegs2" ||= jLam moveSwitch
  where
    moveSwitch n m = SwitchStat ((n .<<. 8) .|. m) switchCases (defaultCase n m)
    -- fast cases
    switchCases = [switchCase n m | n <- [1..5], m <- [1..4]] -- tune the parameteters for performance and size
    switchCase :: Int -> Int -> (JExpr, JStat)
    switchCase n m = (toJExpr $ (n `Bits.shiftL` 8) Bits..|. m, mconcat (map (\n -> moveRegFast n m) [n+1,n..2]) <> BreakStat Nothing {-[j| break; |]-})
    moveRegFast n m = e (numReg (n+m)) |= e (numReg n)
    -- fallback
    defaultCase n m = 
      loop n (.>.0) (\i -> appS "h$setReg" [i+1+m, app "h$getReg" [i+1]] # postDecrS i)

