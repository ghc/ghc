{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module GHC.StgToJS.FFI
  ( genPrimCall
  , genForeignCall
  , saturateFFI
  )
where

import GHC.Prelude

import GHC.JS.Unsat.Syntax
import GHC.JS.Make
import GHC.JS.Transform
import qualified GHC.JS.Syntax as Sat

import GHC.StgToJS.Arg
import GHC.StgToJS.ExprCtx
import GHC.StgToJS.Monad
import GHC.StgToJS.Types
import GHC.StgToJS.Literal
import GHC.StgToJS.Regs
import GHC.StgToJS.CoreUtils
import GHC.StgToJS.Ids

import GHC.Types.RepType
import GHC.Types.ForeignCall
import GHC.Types.Unique.Map

import GHC.Stg.Syntax

import GHC.Builtin.PrimOps
import GHC.Builtin.Types.Prim

import GHC.Core.Type hiding (typeSize)

import GHC.Utils.Misc
import GHC.Utils.Outputable (renderWithContext, defaultSDocContext, ppr)
import GHC.Data.FastString

import Data.Char
import Data.Monoid
import qualified Data.List as L

genPrimCall :: ExprCtx -> PrimCall -> [StgArg] -> Type -> G (JStat, ExprResult)
genPrimCall ctx (PrimCall lbl _) args t = do
  j <- parseFFIPattern False False False ("h$" ++ unpackFS lbl) t (concatMap typex_expr $ ctxTarget ctx) args
  return (j, ExprInline Nothing)

-- | generate the actual call
{-
  parse FFI patterns:
   "&value         -> value
  1. "function"      -> ret = function(...)
  2. "$r = $1.f($2)  -> r1 = a1.f(a2)

  arguments, $1, $2, $3 unary arguments
     $1_1, $1_2, for a binary argument

  return type examples
  1. $r                      unary return
  2. $r1, $r2                binary return
  3. $r1, $r2, $r3_1, $r3_2  unboxed tuple return
 -}
parseFFIPattern :: Bool  -- ^ catch exception and convert them to haskell exceptions
                -> Bool  -- ^ async (only valid with javascript calling conv)
                -> Bool  -- ^ using javascript calling convention
                -> String
                -> Type
                -> [JExpr]
                -> [StgArg]
                -> G JStat
parseFFIPattern catchExcep async jscc pat t es as
  | catchExcep = do
      c <- parseFFIPatternA async jscc pat t es as
      -- Generate:
      --  try {
      --    `c`;
      --  } catch(except) {
      --    return h$throwJSException(except);
      --  }
      let ex = TxtI "except"
      return (TryStat c ex (ReturnStat (ApplExpr (var "h$throwJSException") [toJExpr ex])) mempty)
  | otherwise  = parseFFIPatternA async jscc pat t es as

parseFFIPatternA :: Bool  -- ^ async
                 -> Bool  -- ^ using JavaScript calling conv
                 -> String
                 -> Type
                 -> [JExpr]
                 -> [StgArg]
                 -> G JStat
-- async calls get an extra callback argument
-- call it with the result
parseFFIPatternA True True pat t es as  = do
  cb <- freshIdent
  x  <- freshIdent
  d  <- freshIdent
  stat <- parseFFIPattern' (Just (toJExpr cb)) True pat t es as
  return $ mconcat
    [ x  ||= (toJExpr (jhFromList [("mv", null_)]))
    , cb ||= ApplExpr (var "h$mkForeignCallback") [toJExpr x]
    , stat
    , IfStat (InfixExpr StrictEqOp (toJExpr x .^ "mv") null_)
          (mconcat
            [ toJExpr x .^ "mv" |= UOpExpr NewOp (ApplExpr (var "h$MVar") [])
            , sp |= Add sp one_
            , (IdxExpr stack sp) |= var "h$unboxFFIResult"
            , ReturnStat $ ApplExpr (var "h$takeMVar") [toJExpr x .^ "mv"]
            ])
          (mconcat
            [ d ||= toJExpr x .^ "mv"
            , copyResult (toJExpr d)
            ])
    ]
    where nrst = typeSize t
          copyResult d = assignAllEqual es (map (IdxExpr d . toJExpr) [0..nrst-1])
parseFFIPatternA _async javascriptCc pat t es as =
  parseFFIPattern' Nothing javascriptCc pat t es as

-- parseFFIPatternA _ _ _ _ _ _ = error "parseFFIPattern: non-JavaScript pattern must be synchronous"

parseFFIPattern' :: Maybe JExpr -- ^ Nothing for sync, Just callback for async
                 -> Bool        -- ^ javascript calling convention used
                 -> String      -- ^ pattern called
                 -> Type        -- ^ return type
                 -> [JExpr]     -- ^ expressions to return in (may be more than necessary)
                 -> [StgArg]    -- ^ arguments
                 -> G JStat
parseFFIPattern' callback javascriptCc pat t ret args
  | not javascriptCc = mkApply pat
  | otherwise = mkApply pat
  where
    tgt = take (typeSize t) ret
    -- automatic apply, build call and result copy
    mkApply f
      | Just cb <- callback = do
         (stats, as) <- unzip <$> mapM (genFFIArg javascriptCc) args
         cs <- getSettings
         return $ traceCall cs as <> mconcat stats <> ApplStat f' (concat as++[cb])
      | {-ts@-}
        (t:ts') <- tgt = do
         (stats, as) <- unzip <$> mapM (genFFIArg javascriptCc) args
         cs <- getSettings
         return $ traceCall cs as
                <> mconcat stats
                <> (t |= ApplExpr f' (concat as) )
                <> copyResult ts'
           -- _ -> error "mkApply: empty list"
      | otherwise = do
         (stats, as) <- unzip <$> mapM (genFFIArg javascriptCc) args
         cs <- getSettings
         return $ traceCall cs as <> mconcat stats <> ApplStat f' (concat as)
        where f' = toJExpr (TxtI $ mkFastString f)
    copyResult rs = mconcat $ zipWith (\t r -> toJExpr r |= toJExpr t) (enumFrom Ret1) rs

    traceCall cs as
        | csTraceForeign cs = ApplStat (var "h$traceForeign") [toJExpr pat, toJExpr as]
        | otherwise         = mempty

-- generate arg to be passed to FFI call, with marshalling JStat to be run
-- before the call
genFFIArg :: Bool -> StgArg -> G (JStat, [JExpr])
genFFIArg _isJavaScriptCc (StgLitArg l) = (mempty,) <$> genLit l
genFFIArg isJavaScriptCc a@(StgVarArg i)
    | not isJavaScriptCc &&
      (tycon == byteArrayPrimTyCon || tycon == mutableByteArrayPrimTyCon) =
        (\x -> (mempty,[x, zero_])) <$> varForId i
    | isVoid r                  = return (mempty, [])
--    | Just x <- marshalFFIArg a = x
    | isMultiVar r              = (mempty,) <$> mapM (varForIdN i) [1..varSize r]
    | otherwise                 = (\x -> (mempty,[x])) <$> varForId i
   where
     tycon  = tyConAppTyCon (unwrapType arg_ty)
     arg_ty = stgArgType a
     r      = uTypeVt arg_ty

saturateFFI :: Int -> JStat -> Sat.JStat
saturateFFI u = satJStat (Just . mkFastString $ "ghcjs_ffi_sat_" ++ show u)

genForeignCall :: HasDebugCallStack
               => ExprCtx
               -> ForeignCall
               -> Type
               -> [JExpr]
               -> [StgArg]
               -> G (JStat, ExprResult)
genForeignCall _ctx
               (CCall (CCallSpec (StaticTarget _ tgt Nothing True)
                                   JavaScriptCallConv
                                   PlayRisky))
               _t
               [obj]
               args
  | tgt == fsLit "h$buildObject"
  , Just pairs <- getObjectKeyValuePairs args = do
      pairs' <- mapM (\(k,v) -> genArg v >>= \vs -> return (k, head vs)) pairs
      return ( (|=) obj (ValExpr (JHash $ listToUniqMap pairs'))
             , ExprInline Nothing
             )

genForeignCall ctx (CCall (CCallSpec ccTarget cconv safety)) t tgt args = do
  emitForeign (ctxSrcSpan ctx) (mkFastString lbl) safety cconv (map showArgType args) (showType t)
  (,exprResult) <$> parseFFIPattern catchExcep async isJsCc lbl t tgt' args
  where
    isJsCc = cconv == JavaScriptCallConv

    lbl | (StaticTarget _ clbl _mpkg _isFunPtr) <- ccTarget
            = let clbl' = unpackFS clbl
              in  if | isJsCc -> clbl'
                     | wrapperPrefix `L.isPrefixOf` clbl' ->
                         ("h$" ++ (drop 2 $ dropWhile isDigit $ drop (length wrapperPrefix) clbl'))
                     | otherwise -> "h$" ++ clbl'
        | otherwise = "h$callDynamic"

    exprResult | async     = ExprCont
               | otherwise = ExprInline Nothing

    catchExcep = (cconv == JavaScriptCallConv) &&
                 playSafe safety || playInterruptible safety

    async | isJsCc    = playInterruptible safety
          | otherwise = playInterruptible safety || playSafe safety

    tgt'  | async     = take (length tgt) jsRegsFromR1
          | otherwise = tgt

    wrapperPrefix = "ghczuwrapperZC"

getObjectKeyValuePairs :: [StgArg] -> Maybe [(FastString, StgArg)]
getObjectKeyValuePairs [] = Just []
getObjectKeyValuePairs (k:v:xs)
  | Just t <- argJSStringLitUnfolding k =
      fmap ((t,v):) (getObjectKeyValuePairs xs)
getObjectKeyValuePairs _ = Nothing

argJSStringLitUnfolding :: StgArg -> Maybe FastString
argJSStringLitUnfolding (StgVarArg _v) = Nothing -- fixme
argJSStringLitUnfolding _              = Nothing

showArgType :: StgArg -> FastString
showArgType a = showType (stgArgType a)

showType :: Type -> FastString
showType t
  | Just tc <- tyConAppTyCon_maybe (unwrapType t) =
      mkFastString (renderWithContext defaultSDocContext (ppr tc))
  | otherwise = "<unknown>"
