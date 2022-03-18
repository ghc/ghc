{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.StgToJS.Rts.Apply
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Jeffrey Young  <jeffrey.young@iohk.io>
--                Luite Stegeman <luite.stegeman@iohk.io>
--                Sylvain Henry  <sylvain.henry@iohk.io>
-- Stability   :  experimental
--
--  Code generator for STG applications. Use specialized "apply" functions for
--  common cases when possible.
--
--  This module also contains code to generate these various apply functions
--  for the RTS, for speeding up function application in the most common cases.
--  The code is generated because it contains lots of repeating patterns, and to
--  make it more flexible when changing the RTS (for example how arguments are
--  passed)
--
--  The code in here can be a bit hard to read due to all the generated
--  low-level access things. Reading rts.js for a compiled program can be
--  easier (the file is always the same unless you change low-level RTS
--  options)
--
--  FIXME: add selector thunks and let the gc follow them
-----------------------------------------------------------------------------

module GHC.StgToJS.Apply
  ( genApp
  , mkApplyArr
  , rtsApply
  )
where

import GHC.Prelude hiding ((.|.))

import GHC.JS.Syntax
import GHC.JS.Make

import GHC.StgToJS.Arg
import GHC.StgToJS.Closure
import GHC.StgToJS.DataCon
import GHC.StgToJS.Heap
import GHC.StgToJS.Monad
import GHC.StgToJS.Types
import GHC.StgToJS.Profiling
import GHC.StgToJS.Regs
import GHC.StgToJS.CoreUtils
import GHC.StgToJS.Utils
import GHC.StgToJS.Rts.Types

import GHC.Types.Literal
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.Unique.Set
import GHC.Types.Unique.FM
import GHC.Types.CostCentre

import GHC.Stg.Syntax

import GHC.Builtin.Names

import GHC.Core.TyCon
import GHC.Core.DataCon
import GHC.Core.Type hiding (typeSize)

import GHC.Utils.Encoding
import GHC.Utils.Misc
import GHC.Utils.Monad
import GHC.Utils.Panic
import GHC.Utils.Outputable (vcat, ppr)
import qualified GHC.Data.ShortText as ST
import GHC.Data.ShortText (ShortText)

import qualified Data.Bits as Bits
import Data.Monoid
import Data.Array


-- | Generate an application of some args to an Id
genApp
  :: HasDebugCallStack
  => ExprCtx
  -> Id
  -> [StgArg]
  -> G (JStat, ExprResult)
genApp ctx i args

-- FIXME (sylvain 2022/02): what's our new equivalent of this?
--  -- special cases for JSString literals
--  -- we could handle unpackNBytes# here, but that's probably not common
--  -- enough to warrant a special case
--  | [StgVarArg v] <- args
--  , [top] <- concatMap snd (ctxTarget ctx)
--  -- , Just (Lit (MachStr bs)) <- expandUnfolding_maybe (idUnfolding v)
--  -- , Just t <- decodeModifiedUTF8 bs -- unpackFS fs -- Just t <- decodeModifiedUTF8 bs
--  , matchVarName "ghcjs-prim" "GHCJS.Prim" "unsafeUnpackJSStringUtf8##" i =
--     (,ExprInline Nothing) . (|=) top . app "h$decodeUtf8z" <$> genIds v

    | [StgLitArg (LitString bs), x] <- args
    , [top] <- concatMap typex_expr (ctxTarget ctx)
    , getUnique i == unpackCStringAppendIdKey
    -- , Just d <- decodeModifiedUTF8 bs
    , d <- utf8DecodeByteString bs
        -- FIXME (Sylvain, 2022/02): we assume that it decodes but it may not (e.g. embedded file)
    = do
        -- fixme breaks assumption in codegen if bs doesn't decode
        prof <- csProf <$> getSettings
        let profArg = if prof then [jCafCCS] else []
        a <- genArg x
        return (top |= app "h$appendToHsStringA" ([toJExpr d, toJExpr a] ++ profArg)
               ,ExprInline Nothing)

    -- let-no-escape
    | Just n <- lookupUFM (ctxLneFrameBs ctx) i
    = do
      as'      <- concatMapM genArg args
      ei       <- jsEntryId i
      let ra = mconcat . reverse $
                 zipWith (\r a -> toJExpr r |= a) [R1 ..] as'
      p <- pushLneFrame n ctx
      a <- adjSp 1 -- for the header (which will only be written when the thread is suspended)
      return (ra <> p <> a <> returnS ei, ExprCont)

    | [] <- args
    , isUnboxedTupleType (idType i) || isStrictType (idType i)
    = do
      a <- assignCoerce1 (ctxTarget ctx) . (alignIdExprs i) <$> genIds i
      return (a, ExprInline Nothing)

    | [] <- args
    , [vt] <- idVt i
    , isUnboxable vt
    , i `elementOfUniqSet` (ctxEval ctx)
    = do
      let c = head (concatMap typex_expr $ ctxTarget ctx)
      is <- genIds i
      case is of
        [i'] ->
          return ( c |= if_ (isObject i') (closureField1 i') i'
                 , ExprInline Nothing
                 )
        _ -> panic "genApp: invalid size"

    | [] <- args
    , i `elementOfUniqSet` (ctxEval ctx) || isStrictId i
    = do
      a <- assignCoerce1 (ctxTarget ctx) . (alignIdExprs i) <$> genIds i
      settings <- getSettings
      let ww = case concatMap typex_expr (ctxTarget ctx) of
                 [t] | csAssertRts settings ->
                         ifS (isObject t .&&. isThunk t)
                             (appS "throw" [String "unexpected thunk"]) -- yuck
                             mempty
                 _   -> mempty
      return (a `mappend` ww, ExprInline Nothing)

    | DataConWrapId dc <- idDetails i
    , isNewTyCon (dataConTyCon dc)
    = do
      as <- concatMapM genArg args
      case as of
        [ai] -> do
          let t = head (concatMap typex_expr (ctxTarget ctx))
              a' = case args of
                [StgVarArg a'] -> a'
                _              -> panic "genApp: unexpected arg"
          if isStrictId a' || a' `elementOfUniqSet` (ctxEval ctx)
            then return (t |= ai, ExprInline Nothing)
            else return (returnS (app "h$e" [ai]), ExprCont)
        _ -> panic "genApp: invalid size"

    | [] <- args
    , idFunRepArity i == 0
    , not (might_be_a_function (idType i))
    = do
      ii <- enterId
      return (returnS (app "h$e" [ii]), ExprCont)

    | n <- length args
    , n /= 0
    , idFunRepArity i == n
    , not (isLocalId i)
    , isStrictId i
    = do
      as' <- concatMapM genArg args
      jmp <- jumpToII i as' =<< r1
      return (jmp, ExprCont)

    | idFunRepArity i < length args
    , isStrictId i
    , idFunRepArity i > 0
    = do
      let (reg,over) = splitAt (idFunRepArity i) args
      reg' <- concatMapM genArg reg
      pc   <- pushCont over
      jmp  <- jumpToII i reg' =<< r1
      return (pc <> jmp, ExprCont)

    | otherwise
    = do
      jmp <- jumpToFast args =<< r1
      return (jmp, ExprCont)
  where
    enterId :: G JExpr
    enterId = genArg (StgVarArg i) >>=
                \case
                   [x] -> return x
                   xs  -> pprPanic "genApp: unexpected multi-var argument"
                            (vcat [ppr (length xs), ppr i])

    r1 :: G JStat
    r1 = do
      ids <- genIds i
      return $ mconcat $ zipWith (\r u -> toJExpr r |= toJExpr u) (enumFrom R1) ids

-- avoid one indirection for global ids
-- fixme in many cases we can also jump directly to the entry for local?
jumpToII :: Id -> [JExpr] -> JStat -> G JStat
jumpToII i args afterLoad
  | isLocalId i = do
     ii <- jsId i
     return $ mconcat
      [ ra
      , afterLoad
      , returnS (closureEntry ii)
      ]
  | otherwise   = do
     ei <- jsEntryId i
     return $ mconcat
      [ ra
      , afterLoad
      , returnS ei
      ]
  where
    ra = mconcat . reverse $ zipWith (\r a -> toJExpr r |= a) (enumFrom R2) args

jumpToFast :: HasDebugCallStack => [StgArg] -> JStat -> G JStat
jumpToFast as afterLoad = do
  regs <- concatMapM genArg as
  (fun, spec) <- selectApply True (as,regs)
  pure $ mconcat
    [ mconcat (ra regs)
    , afterLoad
    , if spec
        then returnS (ApplExpr fun [])
        else returnS (ApplExpr fun [toJExpr (mkTag regs as)])
    ]
    where
      ra regs   = reverse $ zipWith (\r ex -> toJExpr r |= ex) (enumFrom R2) regs
      mkTag rs as = (length rs `Bits.shiftL` 8) Bits..|. length as

-- find a specialized application path if there is one
selectApply
  :: Bool                -- ^ true for fast apply, false for stack apply
  -> ([StgArg], [JExpr]) -- ^ arguments
  -> G (JExpr, Bool)     -- ^ the function to call, true if specialized path
selectApply fast (args, as) =
  case specApply fast (length args) (length as) of
    Just e  -> return (e, True)
    Nothing -> return (var $ "h$ap_gen" <> fastSuff, False)
  where
    fastSuff | fast      = "_fast"
             | otherwise = ""



-- specialized apply for these
-- make sure that once you are in spec, you stay there
applySpec :: [(Int,Int)] -- regs,arity
applySpec = [ (regs,arity)  | arity <- [1..4], regs <- [max 0 (arity-1)..(arity*2)]]

specApply :: Bool -> Int -> Int -> Maybe JExpr
specApply fast n r
  | (r,n) == (0,0)         = Just (var . ST.pack $ ("h$ap_0_0" ++ fastSuff))
  | (r,n) == (0,1)         = Just (var . ST.pack $ ("h$ap_1_0" ++ fastSuff))
  | (r,n) `elem` applySpec = Just (var . ST.pack $ ("h$ap_" ++ show n ++ "_" ++ show r ++ fastSuff))
  | otherwise              = Nothing
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
  [ TxtI "h$apply" ||= toJExpr (JList [])
  , TxtI "h$paps"  ||= toJExpr (JList [])
  , ApplStat (var "h$initStatic" .^ "push")
    [ ValExpr $ JFunc [] $ jVar \i -> mconcat
        [ i |= zero_
        , WhileStat False (i .<. Int 65536) $ mconcat
            [ var "h$apply" .! i |= var "h$ap_gen"
            , preIncrS i
            ]
        , i |= zero_
        , WhileStat False (i .<. Int 128) $ mconcat
            [ var "h$paps" .! i |= var "h$pap_gen"
            , preIncrS i
            ]
        , var "h$apply" .! zero_ |= var "h$ap_0_0"
        , mconcat (map assignSpec applySpec)
        , mconcat (map assignPap specPap)
        ]
    ]
  ]
  where
    assignSpec :: (Int, Int) -> JStat
    assignSpec (r,n) =
      var "h$apply" .! (toJExpr $ Bits.shiftL r 8 Bits..|. n) |=
           (var (ST.pack ("h$ap_" ++ show n ++ "_" ++ show r)))

    assignPap :: Int -> JStat
    assignPap p = var "h$paps" .! toJExpr p |=
                      (var (ST.pack $ ("h$pap_" ++ show p)))

pushCont :: HasDebugCallStack
         => [StgArg]
         -> G JStat
pushCont as = do
  as' <- concatMapM genArg as
  (app, spec) <- selectApply False (as,as')
  if spec
    then push $ reverse $ app : as'
    else push $ reverse $ app : mkTag as' as : as'
  where
    mkTag rs ns = toJExpr ((length rs `Bits.shiftL` 8) Bits..|. length ns)

rtsApply :: StgToJSConfig -> JStat
rtsApply cfg = BlockStat $
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

-- generic stack apply that can do everything, but less efficiently
-- on stack: tag: (regs << 8 | arity)
-- fixme: set closure info of stack frame
genericStackApply :: StgToJSConfig -> JStat
genericStackApply s =
   closure (ClosureInfo "h$ap_gen" (CIRegs 0 [PtrV]) "h$ap_gen" CILayoutVariable CIStackFrame mempty)
           (jVar \cf ->
               [ traceRts s (jString "h$ap_gen")
               , cf |= closureEntry r1
               , SwitchStat (entryClosureType cf)
                 [ (toJExpr Thunk, profStat s pushRestoreCCS <> returnS cf)
                 , (toJExpr Fun,   funCase cf (funArity' cf))
                 , (toJExpr Pap,   funCase cf (papArity r1))
                 , (toJExpr Blackhole, push' s [r1, var "h$return"]
                     <> returnS (app "h$blockOnBlackhole" [r1]))
                 ] (appS "throw" [jString "h$ap_gen: unexpected closure type " + (entryClosureType cf)])
               ]
            )
  where
    funCase c arity = jVar \myArity ar myAr myRegs regs newTag newAp p dat ->
      [ myArity |= stack .! (sp - 1)
      , ar |= mask8 arity
      , myAr |= mask8 myArity
      , myRegs |= myArity .>>. 8
      , traceRts s (jString "h$ap_gen: args: " + myAr
                    + jString " regs: " + myRegs)
      , ifBlockS (myAr .===. ar)
        -- then
        [ traceRts s (jString "h$ap_gen: exact")
        , loop 0 (.<. myRegs)
                         (\i -> appS "h$setReg" [i+2, stack .! (sp-2-i)]
                           <> postIncrS i)
        , (sp |= sp - myRegs - 2)
        , returnS c
        ]
        -- else
        [ ifBlockS (myAr .>. ar)
            --then
            [ regs |= arity .>>. 8
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
            ]
            -- else
            [ traceRts s (jString "h$ap_gen: undersat")
            , p   |= var "h$paps" .! myRegs
            , dat |= toJExpr [r1, ((arity .>>. 8)-myRegs)*256+ar-myAr]
            ,  loop 0 (.<. myRegs)
               (\i -> (dat .^ "push") `ApplStat` [stack .! (sp - i - 2)]
                      <> postIncrS i)
            , sp  |= sp - myRegs - 2
            , r1  |= initClosure s p dat jCurrentCCS
            , returnStack
            ]
        ]
      ]

{-
  generic fast apply: can handle anything (slowly)
  signature tag in argument
-}
genericFastApply :: StgToJSConfig -> JStat
genericFastApply s =
   TxtI "h$ap_gen_fast" ||= jLam \tag -> jVar \c ->
      [traceRts s (jString "h$ap_gen_fast: " + tag)
      , c |= closureEntry r1
      , SwitchStat (entryClosureType c)
        [ (toJExpr Thunk, traceRts s (jString "h$ap_gen_fast: thunk")
           <> pushStackApply c tag
           <> returnS c)
        , (toJExpr Fun, jVar \farity ->
                               [ farity |= funArity' c
                               , traceRts s (jString "h$ap_gen_fast: fun " + farity)
                               , funCase c tag farity
                               ])
        , (toJExpr Pap, jVar \parity ->
                               [ parity |= papArity r1
                               , traceRts s (jString "h$ap_gen_fast: pap " + parity)
                               , funCase c tag parity
                               ])
        , (toJExpr Con, traceRts s (jString "h$ap_gen_fast: con")
            <> jwhenS (tag .!=. 0)
                (appS "throw" [jString "h$ap_gen_fast: invalid apply"])
                        <> returnS c)
        , (toJExpr Blackhole, traceRts s (jString "h$ap_gen_fast: blackhole")
            <> pushStackApply c tag
            <> push' s [r1, var "h$return"]
            <> returnS (app "h$blockOnBlackhole" [r1]))
        ] $ appS "throw" [jString "h$ap_gen_fast: unexpected closure type: " + entryClosureType c]
      ]

  where
     -- thunk: push everything to stack frame, enter thunk first
    pushStackApply :: JExpr -> JExpr -> JStat
    pushStackApply _c tag =
      jVar \ap ->
        [ pushAllRegs tag
        , ap |= var "h$apply" .! tag
        , ifS (ap .===. var "h$ap_gen")
                ((sp |= sp + 2) <> (stack .! (sp-1) |= tag))
                (sp |= sp + 1)
        , stack .! sp |= ap
        , profStat s pushRestoreCCS
        ]

    funCase :: JExpr -> JExpr -> JExpr -> JStat
    funCase c tag arity =
      jVar \ar myAr myRegs regsStart newTag newAp dat p ->
        [ ar     |= mask8 arity
        , myAr   |= mask8 tag
        , myRegs |= tag .>>. 8
        , traceRts s (jString "h$ap_gen_fast: args: " + myAr
                      + jString " regs: "             + myRegs)
        , ifS (myAr .===. ar)
        -- call the function directly
          (traceRts s (jString "h$ap_gen_fast: exact") <> returnS c)
          (ifBlockS (myAr .>. ar)
          -- push stack frame with remaining args, then call fun
           [ traceRts s (jString "h$ap_gen_fast: oversat " + sp)
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
           ]
          -- else
           [traceRts s (jString "h$ap_gen_fast: undersat: " + myRegs + jString " " + tag)
           , jwhenS (tag .!=. 0) $ mconcat
               [ p |= var "h$paps" .! myRegs
               , dat |= toJExpr [r1, ((arity .>>. 8)-myRegs)*256+ar-myAr]
               , loop 0 (.<. myRegs)
                 (\i -> (dat .^ "push")
                   `ApplStat` [app "h$getReg" [i+2]] <> postIncrS i)
               , r1 |= initClosure s p dat jCurrentCCS
               ]
           , returnStack
           ])
        ]


    pushAllRegs :: JExpr -> JStat
    pushAllRegs tag =
      jVar \regs ->
        [ regs |= tag .>>. 8
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

    funcName = ST.pack ("h$ap_" ++ show n ++ "_" ++ show r)

    body = jVar \c ->
             [ c |= closureEntry r1
             , traceRts s (toJExpr funcName
                           + jString " "
                           + (c .^ "n")
                           + jString " sp: " + sp
                           + jString " a: "  + (c .^ "a"))
             , SwitchStat (entryClosureType c)
               [ (toJExpr Thunk, traceRts s (toJExpr $ funcName <> ": thunk") <> profStat s pushRestoreCCS <> returnS c)
               , (toJExpr Fun, traceRts s (toJExpr $ funcName <> ": fun") <> funCase c)
               , (toJExpr Pap, traceRts s (toJExpr $ funcName <> ": pap") <> papCase c)
               , (toJExpr Blackhole, push' s [r1, var "h$return"] <> returnS (app "h$blockOnBlackhole" [r1]))
               ] (appS "throw" [toJExpr ("panic: " <> funcName <> ", unexpected closure type: ") + (entryClosureType c)])
             ]

    funExact c = popSkip' 1 (reverse $ take r (map toJExpr $ enumFrom R2)) <> returnS c
    stackArgs = map (\x -> stack .! (sp - toJExpr x)) [1..r]

    papCase :: JExpr -> JStat
    papCase c = jVar \expr arity0 arity ->
      case expr of
        ValExpr (JVar pap) -> [ arity0 |= papArity r1
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
    funCase c = jVar \expr ar0 ar ->
      case expr of
        ValExpr (JVar pap) -> [ ar0 |= funArity' c
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
      jVar \rs newAp ->
        [ rs |= (arity .>>. 8)
        , loadRegs rs
        , sp |= sp - rs
        , newAp |= (var "h$apply" .! (toJExpr n-arity0.|.((toJExpr r-rs).<<.8)))
        , stack .! sp |= newAp
        , profStat s pushRestoreCCS
        , traceRts s (toJExpr (funcName <> ": new stack frame: ") + (newAp .^ "n"))
        , returnS c
        ]
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
      funName = ST.pack ("h$ap_" ++ show n ++ "_" ++ show r ++ "_fast")
      func    = TxtI funName

      myFunArgs = []

      regArgs = take r (enumFrom R2)

      mkAp :: Int -> Int -> [JExpr]
      mkAp n' r' = [ var . ST.pack $ "h$ap_" ++ show n' ++ "_" ++ show r' ]

      body =
        jVar \c farity arity ->
          [ c |= closureEntry r1
          , traceRts s (toJExpr (funName <> ": sp ") + sp)
          -- TODO: Jeff (2022,03): factor our and dry out this code
          , SwitchStat (entryClosureType c)
             [(toJExpr Fun, traceRts s (toJExpr (funName <> ": ")
                                        + clName c
                                        + jString " (arity: " + (c .^ "a") + jString ")")
                            <> (farity |= funArity' c)
                            <> funCase c farity)
             ,(toJExpr Pap, traceRts s (toJExpr (funName <> ": pap")) <> (arity |= papArity r1) <> funCase c arity)
             ,(toJExpr Thunk, traceRts s (toJExpr (funName <> ": thunk")) <> push' s (reverse (map toJExpr $ take r (enumFrom R2)) ++ mkAp n r) <> profStat s pushRestoreCCS <> returnS c)
             ,(toJExpr Blackhole, traceRts s (toJExpr (funName <> ": blackhole")) <> push' s (reverse (map toJExpr $ take r (enumFrom R2)) ++ mkAp n r) <> push' s [r1, var "h$return"] <> returnS (app "h$blockOnBlackhole" [r1]))]
             (appS "throw" [toJExpr (funName <> ": unexpected closure type: ") + entryClosureType c])
          ]

      funCase :: JExpr -> JExpr -> JStat
      funCase c arity = jVar \arg ar -> case arg of
          ValExpr (JVar pap) -> [ ar |= mask8 arity
                                ,  ifS (toJExpr n .===. ar)
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
                                ]
          _             -> mempty -- FIXME: Jeff (2022,03), just quieting non-exhaustive
                                  -- patterns. That the code wants to do this
                                  -- means we should be encoding that funCase is
                                  -- only callable on ValExpr (JVar pap)'s in
                                  -- the type system, perhaps with a GADT or
                                  -- phantom

      oversatCase :: JExpr -> JExpr -> JStat
      oversatCase c arity =
         jVar \rs rsRemain ->
           [ rs |= arity .>>. 8
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
zeroApply s = mconcat
  [ TxtI "h$ap_0_0_fast" ||= jLam (enter s r1)
  , closure (ClosureInfo "h$ap_0_0" (CIRegs 0 [PtrV]) "h$ap_0_0" (CILayoutFixed 0 []) CIStackFrame mempty)
    (adjSpN' 1 <> enter s r1)
  , TxtI "h$e" ||= jLam (\c -> (r1 |= c) <> enter s c)
  ]

-- carefully enter a closure that might be a thunk or a function

-- ex may be a local var, but must've been copied to R1 before calling this
enter :: StgToJSConfig -> JExpr -> JStat
enter s ex = jVar \c ->
  [ jwhenS (app "typeof" [ex] .!==. jTyObject) returnStack
  , c |= closureEntry ex
  , jwhenS (c .===. var "h$unbox_e") ((r1 |= closureField1 ex) <> returnStack)
  , SwitchStat (entryClosureType c)
    [ (toJExpr Con, mempty)
    , (toJExpr Fun, mempty)
    , (toJExpr Pap, returnStack)
    , (toJExpr Blackhole, push' s [var "h$ap_0_0", ex, var "h$return"]
        <> returnS (app "h$blockOnBlackhole" [ex]))
    ] (returnS c)
  ]

updates :: StgToJSConfig -> JStat
updates s = BlockStat
  [ closure
      (ClosureInfo "h$upd_frame" (CIRegs 0 [PtrV]) "h$upd_frame" (CILayoutFixed 1 [PtrV]) CIStackFrame mempty)
      $ jVar \updatee waiters ss si sir ->
            let unbox_closure = Closure
                  { clEntry  = var "h$unbox_e"
                  , clField1 = sir
                  , clField2 = null_
                  , clMeta   = 0
                  , clCC     = Nothing
                  }
                updateCC updatee = closureCC updatee |= jCurrentCCS
            in [ updatee |= stack .! (sp - 1)
               , traceRts s (jString "h$upd_frame updatee alloc: " + updatee .^ "alloc")
               , -- wake up threads blocked on blackhole
                 waiters |= closureField2 updatee
               , jwhenS (waiters .!==. null_)
                           (loop 0 (.<. waiters .^ "length")
                              (\i -> appS "h$wakeupThread" [waiters .! i] <> postIncrS i))
               , -- update selectors
                 jwhenS ((app "typeof" [closureMeta updatee] .===. jTyObject) .&&. (closureMeta updatee .^ "sel"))
                 ((ss |= closureMeta updatee .^ "sel")
                   <> loop 0 (.<. ss .^ "length") \i -> mconcat
                        [ si |= ss .! i
                        , sir |= (closureField2 si) `ApplExpr` [r1]
                        , ifS (app "typeof" [sir] .===. jTyObject)
                            (copyClosure DontCopyCC si sir)
                            (assignClosure si unbox_closure)
                        , postIncrS i
                        ])
               , -- overwrite the object
                 ifS (app "typeof" [r1] .===. jTyObject)
                     (mconcat [ traceRts s (jString "$upd_frame: boxed: " + ((closureEntry r1) .^ "n"))
                              , copyClosure DontCopyCC updatee r1
                              ])
                     (assignClosure updatee unbox_closure)
               , profStat s (updateCC updatee)
               , adjSpN' 2
               , traceRts s (jString "h$upd_frame: updating: "
                             + updatee
                             + jString " -> "
                             + r1)
               , returnStack
               ]

   , closure
      (ClosureInfo "h$upd_frame_lne" (CIRegs 0 [PtrV]) "h$upd_frame_lne" (CILayoutFixed 1 [PtrV]) CIStackFrame mempty)
      $ jVar \updateePos ->
          [ updateePos |= stack .! (sp - 1)
          , (stack .! updateePos |= r1)
          , adjSpN' 2
          , traceRts s (jString "h$upd_frame_lne: updating: "
                         + updateePos
                         + jString " -> "
                         + r1)
          , returnStack
          ]
  ]

selectors :: StgToJSConfig -> JStat
selectors s =
  mkSel "1"      closureField1
  <> mkSel "2a"  closureField2
  <> mkSel "2b"  (closureField1 . closureField2)
  <> mconcat (map mkSelN [3..16])
   where
    mkSelN :: Int -> JStat
    mkSelN x = mkSel (ST.pack $ show x)
                     (\e -> SelExpr (closureField2 (toJExpr e))
                            (TxtI $ ST.pack ("d" ++ show (x-1))))


    mkSel :: ShortText -> (JExpr -> JExpr) -> JStat
    mkSel name sel = mconcat
      [TxtI createName ||= jLam \r -> mconcat
          [ traceRts s (toJExpr ("selector create: " <> name <> " for ") + (r .^ "alloc"))
          , ifS (isThunk r .||. isBlackhole r)
              (returnS (app "h$mkSelThunk" [r, toJExpr (v entryName), toJExpr (v resName)]))
              (returnS (sel r))
          ]
      , TxtI resName ||= jLam \r -> mconcat
          [ traceRts s (toJExpr ("selector result: " <> name <> " for ") + (r .^ "alloc"))
          , returnS (sel r)
          ]
      , closure
        (ClosureInfo entryName (CIRegs 0 [PtrV]) ("select " <> name) (CILayoutFixed 1 [PtrV]) CIThunk mempty)
        (jVar \tgt ->
          [ tgt |= closureField1 r1
          , traceRts s (toJExpr ("selector entry: " <> name <> " for ") + (tgt .^ "alloc"))
          , ifS (isThunk tgt .||. isBlackhole tgt)
              (preIncrS sp
               <> (stack .! sp |= var frameName)
               <> returnS (app "h$e" [tgt]))
              (returnS (app "h$e" [sel tgt]))
          ])
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
          | otherwise                  = specPapIdents ! length values

-- | Number of specialized PAPs (pre-generated for a given number of args)
numSpecPap :: Int
numSpecPap = 6

-- specialized (faster) pap generated for [0..numSpecPap]
-- others use h$pap_gen
specPap :: [Int]
specPap = [0..numSpecPap]

-- | Cache of specialized PAP idents
specPapIdents :: Array Int Ident
specPapIdents = listArray (0,numSpecPap) $ map (TxtI . ST.pack . ("h$pap_"++) . show) specPap

pap :: StgToJSConfig
    -> Int
    -> JStat
pap s r = closure (ClosureInfo funcName CIRegsUnknown funcName (CILayoutUnknown (r+2)) CIPap mempty) body
  where
    funcName = ST.pack ("h$pap_" ++ show r)

    body = jVar \c d f extra ->
             [ c |= closureField1 r1
             , d |= closureField2 r1
             , f |= closureEntry  c
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
    dField d n = SelExpr d (TxtI . ST.pack $ ('d':show (n-1)))

-- Construct a generic PAP
papGen :: StgToJSConfig -> JStat
papGen cfg =
   closure (ClosureInfo funcName CIRegsUnknown funcName CILayoutVariable CIPap mempty)
           (jVar \c f d pr or r ->
              [ c |= closureField1 r1
              , d |= closureField2 r1
              , f |= closureEntry  c
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
      let prop n = d .^ ("d" <> ST.pack (show $ n+1))
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
initClosure cfg entry values ccs =
  let cc | csProf cfg = Just ccs
         | otherwise  = Nothing
  in app "h$init_closure" [ newClosure $ Closure
                              { clEntry  = entry
                              , clField1 = null_
                              , clField2 = null_
                              , clMeta   = 0
                              , clCC     = cc
                              }
                          , values
                          ]

