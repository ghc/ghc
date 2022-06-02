{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module GHC.StgToJS.Apply
  ( genApp
  , mkApplyArr
  )
where

import GHC.Prelude

import GHC.JS.Syntax
import GHC.JS.Make

import GHC.StgToJS.Arg
import GHC.StgToJS.Heap
import GHC.StgToJS.Monad
import GHC.StgToJS.Types
import GHC.StgToJS.Profiling
import GHC.StgToJS.Regs
import GHC.StgToJS.CoreUtils
import GHC.StgToJS.Utils

import GHC.Types.Literal
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.Unique.Set
import GHC.Types.Unique.FM
import GHC.Types.RepType

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

import Data.Bits as Bits
import Data.Monoid


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
          return ( c |= if_ (isObject i') (i' .^ closureExtra1_) i'
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
      , returnS (ii .^ "f")
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

-- specialized (faster) pap generated for [0..numSpecPap]
-- others use h$pap_gen
specPap :: [Int]
specPap = [0..numSpecPap]

numSpecPap :: Int
numSpecPap = 6

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

-- | Return False only if we are *sure* it's a data type
-- Look through newtypes etc as much as possible
might_be_a_function :: HasDebugCallStack => Type -> Bool
might_be_a_function ty
  | [LiftedRep] <- typePrimRep ty
  , Just tc <- tyConAppTyCon_maybe (unwrapType ty)
  , isDataTyCon tc
  = False
  | otherwise
  = True
