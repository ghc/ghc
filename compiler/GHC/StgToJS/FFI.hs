{-# LANGUAGE LambdaCase #-}
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

import GHC.JS.Syntax
import GHC.JS.Make
import GHC.JS.Transform

import GHC.StgToJS.Arg
import GHC.StgToJS.Monad
import GHC.StgToJS.Types
import GHC.StgToJS.Literal
import GHC.StgToJS.Regs
import GHC.StgToJS.CoreUtils

import GHC.Types.RepType
import GHC.Types.ForeignCall

import GHC.Stg.Syntax

import GHC.Builtin.PrimOps
import GHC.Builtin.Types.Prim

import GHC.Core.Type hiding (typeSize)

import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Utils.Outputable (renderWithContext, defaultSDocContext, ppr, vcat, text)
import GHC.Data.FastString
import qualified GHC.Data.ShortText as ST
import GHC.Data.ShortText (ShortText)

import Data.Char
import Data.Monoid
import Data.Maybe
import qualified Data.List as L
import qualified Data.Map as M
import Control.Monad
import Control.Applicative
import qualified Text.ParserCombinators.ReadP as P

-- FIXME: what if the call returns a thunk?
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
  cb <- makeIdent
  x  <- makeIdent
  d  <- makeIdent
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
            [ DeclStat d
            , toJExpr d |= toJExpr x .^ "mv"
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
  | otherwise = do
      u <- freshUnique
      case parseFfiJME pat u of
        Right (ValExpr (JVar (TxtI _ident))) -> mkApply pat
        Right expr | not async && length tgt < 2 -> do
          (statPre, ap) <- argPlaceholders javascriptCc args
          let rp  = resultPlaceholders async t ret
              env = M.fromList (rp ++ ap)
          if length tgt == 1
            then return $ statPre <> (mapStatIdent (replaceIdent env) (var "$r" |= expr))
            else return $ statPre <> (mapStatIdent (replaceIdent env) (toStat expr))
        Right _ -> p $ "invalid expression FFI pattern. Expression FFI patterns can only be used for synchronous FFI " ++
                       " imports with result size 0 or 1.\n" ++ pat
        Left _ -> case parseFfiJM pat u of
          Left err -> p (show err)
          Right stat -> do
            let rp = resultPlaceholders async t ret
            let cp = callbackPlaceholders callback
            (statPre, ap) <- argPlaceholders javascriptCc args
            let env = M.fromList (rp ++ ap ++ cp)
            return $ statPre <> (mapStatIdent (replaceIdent env) stat) -- fixme trace?
  where
    async = isJust callback
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
        where f' = toJExpr (TxtI $ ST.pack f)
    copyResult rs = mconcat $ zipWith (\t r -> toJExpr r |= toJExpr t) (enumFrom Ret1) rs
    p e = error ("Parse error in FFI pattern: " ++ pat ++ "\n" ++ e)
    replaceIdent :: M.Map Ident JExpr -> Ident -> JExpr
    replaceIdent env i
      | isFFIPlaceholder i = fromMaybe err (M.lookup i env)
      | otherwise = ValExpr (JVar i)
        where
          (TxtI i') = i
          err = pprPanic "parseFFIPattern': invalid placeholder, check function type"
                  (vcat [ppr pat, text (ST.unpack i'), ppr args, ppr t])
    traceCall cs as
        | csTraceForeign cs = ApplStat (var "h$traceForeign") [toJExpr pat, toJExpr as]
        | otherwise         = mempty

-- ident is $N, $N_R, $rN, $rN_R or $r or $c
isFFIPlaceholder :: Ident -> Bool
isFFIPlaceholder (TxtI x) = not (null (P.readP_to_S parser (ST.unpack x)))
  where
    digit = P.satisfy (`elem` ("0123456789" :: String))
    parser = void (P.string "$r" >> P.eof) <|>
             void (P.string "$c" >> P.eof) <|> do
      _ <- P.char '$'
      P.optional (P.char 'r')
      _ <- P.many1 digit
      P.optional (P.char '_' >> P.many1 digit)
      P.eof

-- generate arg to be passed to FFI call, with marshalling JStat to be run
-- before the call
genFFIArg :: Bool -> StgArg -> G (JStat, [JExpr])
genFFIArg _isJavaScriptCc (StgLitArg l) = (mempty,) <$> genLit l
genFFIArg isJavaScriptCc a@(StgVarArg i)
    | not isJavaScriptCc &&
      (tycon == byteArrayPrimTyCon || tycon == mutableByteArrayPrimTyCon) =
        (\x -> (mempty,[x, zero_])) <$> jsId i
    | isVoid r                  = return (mempty, [])
--    | Just x <- marshalFFIArg a = x
    | isMultiVar r              = (mempty,) <$> mapM (jsIdN i) [1..varSize r]
    | otherwise                 = (\x -> (mempty,[x])) <$> jsId i
   where
     tycon  = tyConAppTyCon (unwrapType arg_ty)
     arg_ty = stgArgType a
     r      = uTypeVt arg_ty

-- $1, $2, $3 for single, $1_1, $1_2 etc for dual
-- void args not counted
argPlaceholders :: Bool -> [StgArg] -> G (JStat, [(Ident,JExpr)])
argPlaceholders isJavaScriptCc args = do
  (stats, idents0) <- unzip <$> mapM (genFFIArg isJavaScriptCc) args
  let idents = filter (not . null) idents0
  return $ (mconcat stats, concat
    (zipWith (\is n -> mkPlaceholder True ("$"++show n) is) idents [(1::Int)..]))

mkPlaceholder :: Bool -> String -> [JExpr] -> [(Ident, JExpr)]
mkPlaceholder undersc prefix aids =
      case aids of
             []       -> []
             [x]      -> [(TxtI . ST.pack $ prefix, x)]
             xs@(x:_) -> (TxtI . ST.pack $ prefix, x) :
                zipWith (\x m -> (TxtI . ST.pack $ prefix ++ u ++ show m,x)) xs [(1::Int)..]
   where u = if undersc then "_" else ""

-- $r for single, $r1,$r2 for dual
-- $r1, $r2, etc for ubx tup, void args not counted
resultPlaceholders :: Bool -> Type -> [JExpr] -> [(Ident,JExpr)] -- ident, replacement
resultPlaceholders True _ _ = [] -- async has no direct resuls, use callback
resultPlaceholders False t rs =
  case typeVt (unwrapType t) of
    [t'] -> mkUnary (varSize t')
    uts ->
      let sizes = filter (>0) (map varSize uts)
          f _ 0 = []
          f n 1 = [["$r" ++ show n]]
          f n k = ["$r" ++ sn, "$r" ++ sn ++ "_1"] : map (\x -> ["$r" ++ sn ++ "_" ++ show x]) [2..k]
            where sn = show n
          phs   = zipWith (\size n -> f n size) sizes [(1::Int)..]
      in case sizes of
           [n] -> mkUnary n
           _   -> concat $ zipWith (\phs' r -> map (\i -> (TxtI (ST.pack i), r)) phs') (concat phs) rs
  where
    mkUnary 0 = []
    mkUnary 1 = [(TxtI "$r",head rs)] -- single
    mkUnary n = [(TxtI "$r",head rs),(TxtI "$r1", head rs)] ++
       zipWith (\n r -> (TxtI . ST.pack $ "$r" ++ show n, toJExpr r)) [2..n] (tail rs)

callbackPlaceholders :: Maybe JExpr -> [(Ident,JExpr)]
callbackPlaceholders Nothing  = []
callbackPlaceholders (Just e) = [((TxtI "$c"), e)]

parseFfiJME :: String -> Int -> Either String JExpr
parseFfiJME _xs _u =  Left "parseFfiJME not yet implemented"
  -- FIXME: removed temporarily for the codegen merge (sylvain)

parseFfiJM :: String -> Int -> Either String JStat
parseFfiJM _xs _u = Left "parseFfiJM not yet implemented"
  -- FIXME: removed temporarily for the codegen merge (sylvain)

saturateFFI :: JMacro a => Int -> a -> a
saturateFFI u = jsSaturate (Just . ST.pack $ "ghcjs_ffi_sat_" ++ show u)

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
      return ( (|=) obj (ValExpr (JHash $ M.fromList pairs'))
             , ExprInline Nothing
             )

genForeignCall ctx (CCall (CCallSpec ccTarget cconv safety)) t tgt args = do
  emitForeign (ctxSrcSpan ctx) (ST.pack lbl) safety cconv (map showArgType args) (showType t)
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

    tgt'  | async     = take (length tgt) (map toJExpr $ enumFrom R1)
          | otherwise = tgt

    wrapperPrefix = "ghczuwrapperZC"

getObjectKeyValuePairs :: [StgArg] -> Maybe [(ShortText, StgArg)]
getObjectKeyValuePairs [] = Just []
getObjectKeyValuePairs (k:v:xs)
  | Just t <- argJSStringLitUnfolding k =
      fmap ((t,v):) (getObjectKeyValuePairs xs)
getObjectKeyValuePairs _ = Nothing

argJSStringLitUnfolding :: StgArg -> Maybe ShortText
argJSStringLitUnfolding (StgVarArg _v) = Nothing -- fixme
argJSStringLitUnfolding _              = Nothing

showArgType :: StgArg -> ShortText
showArgType a = showType (stgArgType a)

showType :: Type -> ShortText
showType t
  | Just tc <- tyConAppTyCon_maybe (unwrapType t) =
      ST.pack (renderWithContext defaultSDocContext (ppr tc))
  | otherwise = "<unknown>"
