{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE BlockArguments    #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.StgToJS.Rts.Rts
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Jeffrey Young  <jeffrey.young@iohk.io>
--                Luite Stegeman <luite.stegeman@iohk.io>
--                Sylvain Henry  <sylvain.henry@iohk.io>
--                Josh Meredith  <josh.meredith@iohk.io>
-- Stability   :  experimental
--
-- Top level driver of the JavaScript Backend RTS. This file is an
-- implementation of the JS RTS for the JS backend written as an EDSL in
-- Haskell. It assumes the existence of pre-generated JS functions, included as
-- js-sources in base. These functions are similarly assumed for non-inline
-- Primops, See 'GHC.StgToJS.Prim'. Most of the elements in this module are
-- constants in Haskell Land which define pieces of the JS RTS.
--
-----------------------------------------------------------------------------

module GHC.StgToJS.Rts.Rts
  ( rts
  , assignRegs
  )
where

import GHC.Prelude

import GHC.JS.JStg.Syntax
import GHC.JS.JStg.Monad
import GHC.JS.Make
import GHC.JS.Ident

import GHC.StgToJS.Apply
import GHC.StgToJS.Closure
import GHC.StgToJS.Heap
import GHC.StgToJS.Profiling
import GHC.StgToJS.Regs
import GHC.StgToJS.Stack
import GHC.StgToJS.Symbols
import GHC.StgToJS.Types

import GHC.Types.Unique.Map

import Data.Array
import Data.Monoid
import qualified Data.Bits          as Bits

-- | The garbageCollector resets registers and result variables.
garbageCollector :: JSM JStgStat
garbageCollector = jBlock
    [ jFunction' hdResetRegisters  (return $ mconcat $ map resetRegister [minBound..maxBound])
    , jFunction' hdResetResultVars (return $ mconcat $ map resetResultVar [minBound..maxBound])
    ]

-- | Reset the register 'r' in JS Land. Note that this "resets" by setting the
-- register to a dummy variable called "null", /not/ by setting to JS's nil
-- value.
resetRegister :: StgReg -> JStgStat
resetRegister r = toJExpr r |= null_

-- | Reset the return variable 'r' in JS Land. Note that this "resets" by
-- setting the register to a dummy variable called "null", /not/ by setting to
-- JS's nil value.
resetResultVar :: StgRet -> JStgStat
resetResultVar r = toJExpr r |= null_

-- | Define closures based on size, these functions are syntactic sugar. Each
-- Closure constructor follows the naming convention h$cN, where N is a natural
-- number. For example, h$c (with the nat omitted) is a JS Land Constructor for
-- a closure which has a single entry function 'f', and no fields; identical to
-- h$c0. h$c1 is a for a closure with an entry function 'f', and a /single/
-- field 'x1', 'Just foo' is an example of this kind of closure. h$c2 is a
-- constructor for a closure with an entry function and two data fields: 'x1'
-- and 'x2'. And so on. Note that this has JIT performance implications; you
-- should use h$c1, h$c2, h$c3, ... h$c24 instead of making objects manually so
-- layouts and fields can be changed more easily and so the JIT can optimize
-- better.
closureConstructors :: StgToJSConfig -> JSM JStgStat
closureConstructors s = do
 closures <- mapM mkClosureCon (Nothing : map Just [0..jsClosureCount])
 fillers  <- mapM mkDataFill [1..jsClosureCount]
 return $ BlockStat $ closures ++ fillers

  where
    prof = csProf s
    (ccArg,ccVal)
      -- the cc argument happens to be named just like the cc field...
      | prof      = ([Var $ name closureCC_], Just (global closureCC_))
      | otherwise = ([], Nothing)

    addCCArg' as = as ++ ccArg

    traceAlloc x | csTraceRts s = appS hdTraceAlloc [x]
                 | otherwise    = mempty

    notifyAlloc x | csDebugAlloc s = appS hdDebugAllocNotifyAlloc [x]
                  | otherwise      = mempty

    -- only JSVal can typically contain undefined or null
    -- although it's possible (and legal) to make other Haskell types
    -- to contain JS refs directly
    -- this can cause false positives here
    checkC :: JSM JStgStat
    checkC | csAssertRts s =
      jVar $ \msg ->
        jwhenS (arguments .! 0 .!==. jString hdGhcInternalJSPrimValConEntryStr)
        <$>
        (loop 1 (.<. arguments .^ lngth)
          (\i ->
              return $
              mconcat [msg |= jString "warning: undefined or null in argument: "
                        + i
                        + jString " allocating closure: " + (arguments .! 0 .^ n)
                      , appS hdLogStr [msg]
                      , jwhenS (console .&&. (console .^ trace)) ((console .^ trace) `ApplStat` [msg])
                      , postIncrS i
                      ]))
           | otherwise = pure mempty

    -- h$d is never used for JSVal (since it's only for constructors with
    -- at least three fields, so we always warn here
    checkD | csAssertRts s =
                     loop 0 (.<. arguments .^ lngth)
                     (\i -> jwhenS ((arguments .! i .===. null_)
                                    .||. (arguments .! i .===. undefined_))
                            <$>
                            (jVar \msg->
                                return $
                                mconcat [ msg |= jString "warning: undefined or null in argument: " + i + jString " allocating fields"
                                        , jwhenS (console .&&. (console .^ trace))
                                                ((console .^ trace) `ApplStat` [msg])
                                        ]))

           | otherwise = pure mempty

    -- special case handler, the key difference is a call to @jFunction@ instead
    -- of @jFunctionSized@
    singleton_closure_con nm = jFunction nm $
      \(MkSolo f) -> do
        chk_c <- checkC
        jVar $ \x ->
          return $ mconcat $
          [ chk_c
          , x |= newClosure (mkClosure f mempty 0 ccVal)
          , notifyAlloc x
          , traceAlloc x
          , returnS x
          ]

    mkClosureCon :: Maybe Int -> JSM JStgStat
    -- the h$c special case
    mkClosureCon Nothing  = singleton_closure_con hdCStr
    -- the h$c0 special case
    mkClosureCon (Just 0) = singleton_closure_con hdC0Str
    -- the rest h$c1 .. h$c24. Note that h$c1 takes 2 arguments, one for the
    -- entry function 'f' and another for the data field 'd1'. Thus the 1 in
    -- h$c1 means 1 data field argument, not just one argument
    mkClosureCon (Just n) = jFunctionSized funName (n + 1) funBod
      where
        funName = name $ clsName n

        funBod [] = pure mempty -- impossible
        funBod (f:vars') = do
          let vars = addCCArg' vars'
          chk_c <- checkC
          jVar $ \x ->
            return $ mconcat $
            [ chk_c
            , x |= newClosure (mkClosure f vars 0 ccVal)
            , notifyAlloc x
            , traceAlloc x
            , returnS x
            ]

    mkDataFill :: Int -> JSM JStgStat
    mkDataFill n = jFunctionSized funName n body
      where
        funName    = name $ dataName n
        ds         = map dataFieldName [1..n]
        extra_args as = ValExpr . JHash
                        . listToUniqMap
                        $ zip ds as
        body :: [JStgExpr] -> JSM JStgStat
        body ids = do
          c <- checkD
          return (c <> returnS (extra_args ids))

-- | JS Payload to perform stack manipulation in the RTS
stackManip :: JSM JStgStat
stackManip = do
  pushes  <- mapM mkPush [1..32]
  ppushes <- mapM mkPpush [1..255]
  return $ mconcat $ pushes ++ ppushes
  where
    mkPush :: Int -> JSM JStgStat
    mkPush n = let funName = pushN ! n
                   body as = return $
                     ((sp |= sp + toJExpr n)
                       <> mconcat (zipWith (\i a -> stack .! (sp - toJExpr (n-i)) |= a)
                                            [1..] as))
               in jFunctionSized funName n body

    -- partial pushes, based on bitmap, increases Sp by highest bit
    mkPpush :: Integer -> JSM JStgStat
    mkPpush sig | sig Bits..&. (sig+1) == 0 = pure mempty -- already handled by h$p
    mkPpush sig = let funName = pushNN ! sig
                      bits    = bitsIdx sig
                      h       = last bits
                      body args = return $
                        mconcat [ sp |= sp + toJExpr (h+1)
                                , mconcat (zipWith (\b a -> stack .! (sp - toJExpr (h-b)) |= a) bits args)
                                ]
                   in jFunctionSized funName (length bits) body

bitsIdx :: Integer -> [Int]
bitsIdx n | n < 0 = error "bitsIdx: negative"
          | otherwise = go n 0
  where
    go 0 _ = []
    go m b | Bits.testBit m b = b : go (Bits.clearBit m b) (b+1)
           | otherwise   = go (Bits.clearBit m b) (b+1)

bhLneStats :: StgToJSConfig -> JStgExpr -> JStgExpr -> JSM JStgStat
bhLneStats _s p frameSize = jVar $ \v ->
  return $ mconcat
  [ v |= stack .! p
  , ifS v
    ((sp |= sp - frameSize)
      <> ifS (v .===. hdBlackHole)
      (returnS $ app hdThrowStr [hdInternalExceptionControlExceptionBaseNonTermination, false_])
      (mconcat [r1 |= v
               , sp |= sp - frameSize
               , returnStack
               ]))
    ((stack .! p |= hdBlackHole) <> returnS null_)
  ]


-- | JS payload to declare the registers
declRegs :: JSM JStgStat
declRegs = do
    getters_setters <- regGettersSetters
    loaders         <- loadRegs
    return $
      mconcat [ hdRegsStr ||= toJExpr (JList [])
              , mconcat (map declReg lowRegs)
              , getters_setters
              , loaders
              ]
    where
      declReg r = decl r <> BlockStat [toJExpr r |= zero_]

-- | JS payload to define getters and setters on the registers.
regGettersSetters :: JSM JStgStat
regGettersSetters =
  do setters <- jFunction (name hdGetRegStr) (\(MkSolo n) -> return $ SwitchStat n getRegCases mempty)
     getters <- jFunction (name hdSetRegStr) (\(n,v)      -> return $ SwitchStat n (setRegCases v) mempty)
     return $ setters <> getters
  where
    getRegCases =
      map (\r -> (toJExpr (jsRegToInt r) , returnS (toJExpr r))) regsFromR1
    setRegCases :: JStgExpr -> [(JStgExpr,JStgStat)]
    setRegCases v =
      map (\r -> (toJExpr (jsRegToInt r), (toJExpr r |= toJExpr v) <> returnS undefined_)) regsFromR1

-- | JS payload that defines the functions to load each register
loadRegs :: JSM JStgStat
loadRegs = mconcat <$> mapM mkLoad [1..32]
  where
    mkLoad :: Int -> JSM JStgStat
    mkLoad n = let body  = \args -> return $ mconcat $
                           zipWith (\a r -> toJExpr r |= a)
                           args (reverse $ take n regsFromR1)
                   fname = hdLoads ! n
               in jFunctionSized fname n body

-- | Assign registers R1 ... Rn in descending order, that is assign Rn first.
-- This function uses the 'hdLoads' array to construct functions which set
-- the registers.

-- | JS payload which defines an array of function symbols that set N registers
-- from M parameters. For example, h$l4 compiles to:
-- @
--    function h$l4(x1, x2, x3, x4) {
--      h$r4 = x1;
--      h$r3 = x2;
--      h$r2 = x3;
--      h$r1 = x4;
--    };
-- @
assignRegs :: StgToJSConfig -> [JStgExpr] -> JStgStat
assignRegs _ [] = mempty
assignRegs s xs
  | l <= 32 && not (csInlineLoadRegs s)
      = ApplStat (ValExpr (JVar $ hdLoads ! l)) (reverse xs)
  | otherwise = mconcat . reverse $
      zipWith (\r ex -> toJExpr r |= ex) (take l regsFromR1) xs
  where
    l = length xs


-- | JS payload to declare return variables.
declRets :: JStgStat
declRets = mconcat $ map decl retRegs

-- | JS payload defining the types closures.
closureTypes :: JSM JStgStat
closureTypes = do
  cls_typ_nm <- closureTypeName
  return $
    mconcat (map mkClosureType (enumFromTo minBound maxBound))
    <> cls_typ_nm
  where
    mkClosureType :: ClosureType -> JStgStat
    mkClosureType c = let s = closureNames ! c
                      in  s ||= toJExpr c
    closureTypeName :: JSM JStgStat
    closureTypeName = jFunction hdClosureTypeNameStr
                      \(MkSolo c) -> return $
                              mconcat (map (ifCT c) [minBound..maxBound])
                              <> returnS (jString "InvalidClosureType")

    ifCT :: JStgExpr -> ClosureType -> JStgStat
    ifCT arg ct = jwhenS (arg .===. toJExpr ct) (returnS (toJExpr (show ct)))

-- | JS payload declaring the RTS functions.
rtsDecls :: JSM JStgStat
rtsDecls = do
  decl_stg_regs <- declRegs
  return $
    mconcat [ hdCurrentThreadStr    ||= null_                   -- thread state object for current thread
            , hdStackStr            ||= null_                   -- stack for the current thread
            , hdStackPtrStr         ||= 0                       -- stack pointer for the current thread
            , hdInitStaticStr       ||= toJExpr (JList [])      -- we need delayed initialization for static objects, push functions here to be initialized just before haskell runs
            ,  hdStaticThunksStr    ||= toJExpr (jhFromList []) --  funcName -> heapidx map for srefs
            ,  hdStaticThunksArrStr ||= toJExpr (JList [])      -- indices of updatable thunks in static heap
            ,  hdCAFsStr            ||= toJExpr (JList [])
            ,  hdCAFsResetStr       ||= toJExpr (JList [])
            -- stg registers
            , decl_stg_regs
            , declRets
            ]

-- | Generated RTS code
rts :: StgToJSConfig -> JSM JStgStat
rts cfg = withTag "h$RTS" $
  do
  rts_      <- rts_gen cfg
  rts_decls <- rtsDecls
  return $  rts_decls <> rts_

-- | JS Payload which defines the embedded RTS.
rts_gen :: StgToJSConfig -> JSM JStgStat
rts_gen s = do
  let decls = [  hdRtsTraceForeign ||= toJExpr (csTraceForeign s)
              ,  hdRtsProfiling    ||= toJExpr (csProf s)
              ,  hdCtFun           ||= toJExpr Fun
              ,  hdCtCon           ||= toJExpr Con
              ,  hdCtThunk         ||= toJExpr Thunk
              ,  hdCtPap           ||= toJExpr Pap
              ,  hdCtBlackhole     ||= toJExpr Blackhole
              ,  hdCtStackFrame    ||= toJExpr StackFrame
              ,  hdCtVtPtr         ||= toJExpr PtrV
              ,  hdVtVoid          ||= toJExpr VoidV
              ,  hdVtInt           ||= toJExpr IntV
              ,  hdVtDouble        ||= toJExpr DoubleV
              ,  hdVtLong          ||= toJExpr LongV
              ,  hdVtAddr          ||= toJExpr AddrV
              ,  hdVtObj           ||= toJExpr ObjV
              ,  hdVtArr           ||= toJExpr ArrV
              ]
  gc           <- garbageCollector
  closure_cons <- closureConstructors s
  stk_manip    <- stackManip
  rest         <- impure
  return $ mconcat $ pure gc <> decls <> [closure_cons, stk_manip] <> rest
  where
    impure = sequence
             [ jFunction' (name hdBhStr) (return $ bhStats s True)
             , jFunction hdBlackHoleLNEStr (\(x, frameSize) -> bhLneStats s x frameSize)
             , closure (ClosureInfo hdBlackHoleStr (CIRegs 0 []) "blackhole" (CILayoutUnknown 2) CIBlackhole mempty)
               (return $ appS throwStr [jString "oops: entered black hole"])
             , closure (ClosureInfo hdBlackHoleTrapStr (CIRegs 0 []) "blackhole" (CILayoutUnknown 2) CIThunk mempty)
               (return $ appS throwStr [jString "oops: entered multiple times"])
             , closure (ClosureInfo hdDone (CIRegs 0 [PtrV]) "done" (CILayoutUnknown 0) CIStackFrame mempty)
               (return $ appS hdFinishedThread [hdCurrentThread] <> returnS hdReschedule)
             , closure (ClosureInfo hdDoneMainEntryStr (CIRegs 0 [PtrV]) "doneMain" (CILayoutUnknown 0) CIStackFrame mempty)
               (return $ returnS hdDoneMain)
             , conClosure hdFalseEntry "GHC.Types.False" (CILayoutFixed 0 []) 1
             , conClosure hdTrueEntry "GHC.Types.True" (CILayoutFixed 0 []) 2
             -- generic data constructor with 1 non-heapobj field
             , conClosure hdData1Entry "data1" (CILayoutFixed 1 [ObjV]) 1
             -- generic data constructor with 2 non-heapobj fields
             , conClosure hdData2Entry "data2" (CILayoutFixed 2 [ObjV,ObjV]) 1
             , closure (ClosureInfo hdNoopEntryStr (CIRegs 1 [PtrV]) "no-op IO ()" (CILayoutFixed 0 []) (CIFun 1 0) mempty)
               $ return (returnS (stack .! sp))
             , pure (hdNoopStr ||= ApplExpr hdC0 (hdNoopEntry : [jSystemCCS | csProf s]))
             , closure (ClosureInfo hdCatchEntryStr (CIRegs 0 [PtrV]) "exception handler" (CILayoutFixed 2 [PtrV,IntV]) CIStackFrame mempty)
                  (return $ adjSpN' 3 <> returnS (stack .! sp))
             , closure (ClosureInfo hdDataToTagEntryStr (CIRegs 0 [PtrV]) "data to tag" (CILayoutFixed 0 []) CIStackFrame mempty)
                   $ return $ mconcat [ r1 |= if_ (r1 .===. true_) 1 (if_ (typeOf r1 .===. jTyObject) (r1 .^ "f" .^ "a" - 1) 0)
                                      , adjSpN' 1
                                      , returnS (stack .! sp)
                                      ]
             -- function application to one argument
             , closure (ClosureInfo hdAp1EntryStr (CIRegs 0 [PtrV]) "apply1" (CILayoutFixed 2 [PtrV, PtrV]) CIThunk mempty)
                  (jVars \(d1, d2) -> return $
                                mconcat [ d1 |= closureField1 r1
                                        , d2 |= closureField2 r1
                                        , appS hdBhStr []
                                        , profStat s enterCostCentreThunk
                                        , r1 |= d1
                                        , r2 |= d2
                                        , returnS (app hdAp11Fast [])
                                        ])
             -- function application to two arguments
             , closure (ClosureInfo hdAp2EntryStr (CIRegs 0 [PtrV]) "apply2" (CILayoutFixed 3 [PtrV, PtrV, PtrV]) CIThunk mempty)
                  (jVars \(d1, d2, d3) -> return $
                                    mconcat [ d1 |= closureField1 r1
                                            , d2 |= closureField2 r1 .^ d1Str
                                            , d3 |= closureField2 r1 .^ d2Str
                                            , appS hdBhStr []
                                            , profStat s enterCostCentreThunk
                                            , r1 |= d1
                                            , r2 |= d2
                                            , r3 |= d3
                                            , returnS (app hdAp22FastStr [])
                                            ])
             -- function application to three arguments
             , closure (ClosureInfo hdAp3EntryStr (CIRegs 0 [PtrV]) "apply3" (CILayoutFixed 4 [PtrV, PtrV, PtrV, PtrV]) CIThunk mempty)
                  (jVars \(d1, d2, d3, d4) -> return $
                                        mconcat [ d1 |= closureField1 r1
                                                , d2 |= closureField2 r1 .^ d1Str
                                                , d3 |= closureField2 r1 .^ d2Str
                                                , d4 |= closureField2 r1 .^ d3Str
                                                , appS hdBhStr []
                                                , r1 |= d1
                                                , r2 |= d2
                                                , r3 |= d3
                                                , r4 |= d4
                                                , returnS (app hdAp33FastStr [])
                                                ])
             , closure (ClosureInfo hdUpdThunkEntryStr (CIRegs 0 [PtrV]) "updatable thunk" (CILayoutFixed 1 [PtrV]) CIThunk mempty)
               (jVar $ \t -> return $
                   mconcat [t |= closureField1 r1
                           , adjSp' 2
                           , stack .! (sp - 1) |= r1
                           , stack .! sp       |= hdUpdFrame
                           , closureInfo   r1  |= hdBlackHole
                           , closureField1 r1  |= hdCurrentThread
                           , closureField2 r1  |= null_
                           , r1 |= t
                           , returnS (app hdAp00FastStr [])
                           ]
                  )
             -- select first field
             , closure (ClosureInfo hdSelect1EntryStr (CIRegs 0 [PtrV]) "select1" (CILayoutFixed 1 [PtrV]) CIThunk mempty)
                  (jVar \t -> return $
                           mconcat [ t |= closureField1 r1
                                   , adjSp' 3
                                   , stack .! (sp - 2) |= r1
                                   , stack .! (sp - 1) |= hdUpdFrame
                                   , stack .! sp       |= hdSelect1Ret
                                   , closureInfo   r1  |= hdBlackHole
                                   , closureField1 r1  |= hdCurrentThread
                                   , closureField2 r1  |= null_
                                   , r1 |= t
                                   , returnS (app hdAp00FastStr [])
                                   ])
             , closure (ClosureInfo hdSelect1RetStr (CIRegs 0 [PtrV]) "select1ret" (CILayoutFixed 0 []) CIStackFrame mempty)
                  (return $
                    (r1 |= closureField1 r1)
                    <> adjSpN' 1
                    <> returnS (app hdAp00FastStr [])
                  )
             -- select second field of a two-field constructor
             , closure (ClosureInfo hdSelect2EntryStr (CIRegs 0 [PtrV]) "select2" (CILayoutFixed 1 [PtrV]) CIThunk mempty)
                  (jVar \t -> return $
                           mconcat [t |= closureField1 r1
                                   , adjSp' 3
                                   , stack .! (sp - 2) |= r1
                                   , stack .! (sp - 1) |= hdUpdFrame
                                   , stack .! sp       |= hdSelect2Return
                                   , closureInfo   r1  |= hdBlackHole
                                   , closureField1 r1  |= hdCurrentThread
                                   , closureField2 r1  |= null_
                                   , r1 |= t
                                   , returnS (app hdAp00FastStr [])
                                   ]
                  )
             , closure (ClosureInfo hdSelect2ReturnStr (CIRegs 0 [PtrV]) "select2ret" (CILayoutFixed 0 []) CIStackFrame mempty)
                           $ return $ mconcat [ r1 |= closureField2 r1
                                              , adjSpN' 1
                                              , returnS (app hdAp00FastStr [])
                                              ]
             , closure (ClosureInfo hdKeepAliveEntryStr (CIRegs 0 [PtrV]) "keepAlive" (CILayoutFixed 1 [PtrV]) CIStackFrame mempty)
                       (return $ mconcat [ adjSpN' 2
                                         , returnS (stack .! sp)
                                         ]
                       )
             -- a thunk that just raises a synchronous exception
             , closure (ClosureInfo hdRaiseEntryStr (CIRegs 0 [PtrV]) (identFS hdRaiseEntryStr) (CILayoutFixed 0 []) CIThunk mempty)
                  (return $ returnS (app hdThrowStr [closureField1 r1, false_]))
             , closure (ClosureInfo hdRaiseAsyncEntryStr (CIRegs 0 [PtrV]) (identFS hdRaiseAsyncEntryStr) (CILayoutFixed 0 []) CIThunk mempty)
                  (return $ returnS  (app hdThrowStr [closureField1 r1, true_]))
             , closure (ClosureInfo hdRaiseAsyncFrameStr (CIRegs 0 []) (identFS hdRaiseAsyncFrameStr) (CILayoutFixed 1 []) CIStackFrame mempty)
                  (jVar \ex -> return $ mconcat [ ex |= stack .! (sp - 1)
                                                , adjSpN' 2
                                                , returnS (app hdThrowStr [ex, true_])
                                                ])
             {- reduce result if it's a thunk, follow if it's an ind
                add this to the stack if you want the outermost result
                to always be reduced to whnf, and not an ind
             -}
             , closure (ClosureInfo hdReduceStr (CIRegs 0 [PtrV]) (identFS hdReduceStr) (CILayoutFixed 0 []) CIStackFrame mempty)
                  (return $
                    ifS (isThunk r1)
                       (returnS (r1 .^ "f"))
                       (adjSpN' 1 <> returnS (stack .! sp))
                  )
             , rtsApply s
             , closureTypes
             , closure (ClosureInfo hdRunIOEntryStr (CIRegs 0 [PtrV]) "runio" (CILayoutFixed 1 [PtrV]) CIThunk mempty)
                           $ return $ mconcat [ r1 |= closureField1 r1
                                              , stack .! PreInc sp |= hdAp10
                                              , returnS hdAp10
                                              ]
             , closure (ClosureInfo hdFlushStdoutEntryStr (CIRegs 0 []) "flushStdout" (CILayoutFixed 0 []) CIThunk mempty)
               $ return $ mconcat [ r1 |= hdGhcInternalIOHandleFlush
                                  , r2 |= hdGhcInternalIOHandleFDStdout
                                  , returnS (app hdAp11Fast [])
                                  ]
             , pure $ hdFlushStdoutStr ||= app hdStaticThunkStr [hdFlushStdoutEntry]
             -- the scheduler pushes this frame when suspending a thread that
             -- has not called h$reschedule explicitly
             , closure (ClosureInfo hdRestoreThreadStr (CIRegs 0 []) "restoreThread" CILayoutVariable CIStackFrame mempty)
               (jVars \(f,frameSize,nregs) ->
                      do set_regs <- loop 1 (.<=. nregs)
                                     (\i -> return $ appS hdSetRegStr [i, stack .! (sp - 2 - i)] <> postIncrS i)
                         return $ mconcat [f |= stack .! (sp - 2)
                                          , frameSize |= stack .! (sp - 1)
                                          , nregs |= frameSize - 3
                                          , set_regs
                                          , sp |= sp - frameSize
                                          , returnS f
                                          ])
             -- return a closure in the stack frame to the next thing on the stack
             , closure (ClosureInfo hdReturnStr (CIRegs 0 []) "return" (CILayoutFixed 1 [PtrV]) CIStackFrame mempty)
                   (return $
                     (r1 |= stack .! (sp - 1))
                     <> adjSpN' 2
                     <> returnS (stack .! sp))
             --  return a function in the stack frame for the next call
             , closure (ClosureInfo hdReturnFStr (CIRegs 0 [PtrV]) "returnf" (CILayoutFixed 1 [ObjV]) CIStackFrame mempty)
                   (jVar \r -> return $
                               mconcat [ r |= stack .! (sp - 1)
                                       , adjSpN' 2
                                       , returnS r
                                       ])
             -- return this function when the scheduler needs to come into action
             -- (yield, delay etc), returning thread needs to push all relevant
             -- registers to stack frame, thread will be resumed by calling the stack top
             , closure (ClosureInfo hdRescheduleStr (CIRegs 0 []) "reschedule" (CILayoutFixed 0 []) CIThunk mempty)
                   (return $ returnS $ hdReschedule)
             -- debug thing, insert on stack to dump current result, should be boxed
             , closure (ClosureInfo hdDumpResStr (CIRegs 0 [PtrV]) "dumpRes" (CILayoutFixed 1 [ObjV]) CIThunk mempty)
                (jVar \re -> return $
                     mconcat [ appS hdLogStr [jString "h$dumpRes result: " + stack .! (sp-1)]
                             , appS hdLogStr [r1]
                             , appS hdLogStr [app hdCollectProps [r1]]
                             , jwhenS ((r1 .^ f) .&&. (r1 .^ f .^ n))
                               (appS hdLogStr [jString "name: " + r1 .^ f .^ n])
                             , jwhenS (ApplExpr (r1 .^ hasOwnProperty) [jString closureField1_])
                               (appS hdLogStr [jString "d1: " + closureField1 r1])
                             , jwhenS (ApplExpr (r1 .^ hasOwnProperty) [jString closureField2_])
                               (appS hdLogStr [jString "d2: " + closureField2 r1])
                             , jwhenS (r1 .^ f) $ mconcat
                               [ re |= New (app "RegExp" [jString "([^\\n]+)\\n(.|\\n)*"])
                               , appS hdLogStr [jString "function"
                                                + ApplExpr (ApplExpr ((jString "" + r1 .^ f) .^ substring) [0, 50] .^ replace) [r1, jString "$1"]]
                               ]
                             , adjSpN' 2
                             , r1 |= null_
                             , returnS (stack .! sp)
                             ])
             , closure (ClosureInfo hdResumeEntryStr (CIRegs 0 [PtrV]) resume (CILayoutFixed 0 []) CIThunk mempty)
                     (jVar \ss ->
                        do update_stk <- loop 0 (.<. ss .^ lngth) (\i -> return $ (stack .! (sp+1+i) |= ss .! i) <> postIncrS i)
                           return $ mconcat [ss |= closureField1 r1
                                            , updateThunk' s
                                            , update_stk
                                            , sp |= sp + ss .^ lngth
                                            , r1 |= null_
                                            , returnS (stack .! sp)
                                            ])
             , closure (ClosureInfo hdUnMaskFrameStr (CIRegs 0 [PtrV]) unMask (CILayoutFixed 0 []) CIStackFrame mempty)
                  (return $
                    (hdCurrentThread .^ mask |= 0)
                    <> adjSpN' 1
                    -- back to scheduler to give us async exception if pending
                    <> ifS (hdCurrentThread .^ excepStr .^ lngth .>. 0)
                    (push' s [r1, hdReturn] <> returnS hdReschedule)
                    (returnS (stack .! sp)))
             , closure (ClosureInfo hdMaskFrameStr (CIRegs 0 [PtrV]) mask (CILayoutFixed 0 []) CIStackFrame mempty)
               (return $
                 (hdCurrentThread .^ mask |= 2)
                 <> adjSpN' 1
                 <> returnS (stack .! sp))
             , closure (ClosureInfo hdMaskUnintFrameStr (CIRegs 0 [PtrV]) "maskUnint" (CILayoutFixed 0 []) CIStackFrame mempty)
               (return $
                 (hdCurrentThread .^ mask |= 1)
                 <> adjSpN' 1
                 <> returnS (stack .! sp))
             , closure (ClosureInfo hdUnboxFFIResultStr (CIRegs 0 [PtrV]) "unboxFFI" (CILayoutFixed 0 []) CIStackFrame mempty)
                  (jVar \d -> do set_regs <- loop 0 (.<. d .^ lngth) (\i -> return $ appS hdSetRegStr [i + 1, d .! i] <> postIncrS i)
                                 return $ mconcat [ d |= closureField1 r1
                                                  , set_regs
                                                  , adjSpN' 1
                                                  , returnS (stack .! sp)
                                                  ])
             , closure (ClosureInfo hdUnboxEntryStr (CIRegs 0 [PtrV]) "unboxed value" (CILayoutFixed 1 [DoubleV]) CIThunk mempty)
                  (return $ (r1 |= closureField1 r1) <> returnS (stack .! sp))
             , closure (ClosureInfo hdRetryInterruptedStr (CIRegs 0 [ObjV]) "retry interrupted operation" (CILayoutFixed 1 [ObjV]) CIStackFrame mempty)
                  (jVar \a -> return $ mconcat [ a |= stack .! (sp - 1)
                                               , adjSpN' 2
                                               , returnS (ApplExpr (a .! 0 .^ apply) [this, ApplExpr (a .^ slice) [1]])
                                               ])
             , closure (ClosureInfo hdAtomicallyEntryStr (CIRegs 0 [PtrV]) "atomic operation" (CILayoutFixed 1 [PtrV]) CIStackFrame mempty)
                  (return $ ifS (app hdStmValidateTransactionStr [])
               (appS hdStmCommitTransactionStr []
                        <> adjSpN' 2
                        <> returnS (stack .! sp))
                       (returnS (app hdStmStartTransactionStr [stack .! (sp - 1)])))

             , closure (ClosureInfo hdStmCatchRetryEntryStr (CIRegs 0 [PtrV]) "catch retry" (CILayoutFixed 1 [PtrV]) CIStackFrame mempty)
                           (return $
                             adjSpN' 2
                             <> appS hdStmCommitTransactionStr []
                             <> returnS (stack .! sp))
             , closure (ClosureInfo hdStmCatchEntryStr (CIRegs 0 [PtrV]) "STM catch" (CILayoutFixed 3 [ObjV,PtrV,ObjV]) CIStackFrame mempty)
               (return $
                 adjSpN' 4
                 <> appS hdStmCommitTransactionStr []
                 <> returnS (stack .! sp))
             , closure (ClosureInfo hdStgResumeRetryEntryStr (CIRegs 0 [PtrV]) "resume retry" (CILayoutFixed 0 []) CIStackFrame mempty)
                           (jVar \blocked ->
                              return $
                               mconcat [ jwhenS (stack .! (sp - 2) .!==. hdAtomicallyEntry)
                                                    (appS throwStr [jString "h$stmResumeRetry_e: unexpected value on stack"])
                                       , blocked |= stack .! (sp - 1)
                                       , adjSpN' 2
                                       , appS hdStmRemoveBlockedThreadStr [blocked, hdCurrentThread]
                                       , returnS (app hdStmStartTransactionStr [stack .! (sp - 1)])
                                       ])
             , closure (ClosureInfo hdLazyEntryStr (CIRegs 0 [PtrV]) "generic lazy value" (CILayoutFixed 0 []) CIThunk mempty)
                           (jVar \x ->
                              return $
                               mconcat [x |= ApplExpr (closureField1 r1) []
                                       , appS hdBhStr []
                                       , profStat s enterCostCentreThunk
                                       , r1 |= x
                                       , returnS (stack .! sp)
                                       ])
             , closure (ClosureInfo hdReportHeapOverflowStr (CIRegs 0 [PtrV]) (identFS hdReportHeapOverflowStr) (CILayoutFixed 0 []) CIStackFrame mempty)
                  (return $ (appS throwStr [jString "h$reportHeapOverflow: Heap Overflow!"]))
             , closure (ClosureInfo hdReportStackOverflowStr (CIRegs 0 [PtrV]) "h$reportStackOverflow" (CILayoutFixed 0 []) CIStackFrame mempty)
                  (return $ (appS throwStr [jString "h$reportStackOverflow: Stack Overflow!"]))
             -- Top-level statements to generate only in profiling mode
             , fmap (profStat s) $ (closure (ClosureInfo hdSetCcsEntryStr (CIRegs 0 [PtrV]) "set cost centre stack" (CILayoutFixed 1 [ObjV]) CIStackFrame mempty)
                           (return $
                             appS hdRestoreCCSStr [ stack .! (sp - 1)]
                             <> adjSpN' 2
                             <> returnS (stack .! sp)))
             ]
