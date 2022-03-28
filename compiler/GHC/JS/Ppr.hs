{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Pretty-printing JavaScript
module GHC.JS.Ppr
  ( renderJs
  , renderJs'
  , renderPrefixJs
  , renderPrefixJs'
  , JsToDoc(..)
  , defaultRenderJs
  , RenderJs(..)
  , jsToDoc
  , pprStringLit
  )
where

import GHC.Prelude

import GHC.JS.Syntax
import GHC.JS.Transform


import Data.Function
import Data.Char (isControl, ord)
import qualified Data.Map as M

import Numeric(showHex)

import GHC.Utils.Ppr as PP
import qualified GHC.Data.ShortText as ST
import GHC.Data.ShortText (ShortText)

($$$) :: Doc -> Doc -> Doc
--x $$$ y = align (nest 2 $ x $+$ y) -- FIXME (Sylvain, 2022/02)
x $$$ y = nest 2 $ x $+$ y

-- | Render a syntax tree as a pretty-printable document
-- (simply showing the resultant doc produces a nice,
-- well formatted String).
renderJs :: (JsToDoc a, JMacro a) => a -> Doc
renderJs = renderJs' defaultRenderJs

renderJs' :: (JsToDoc a, JMacro a) => RenderJs -> a -> Doc
renderJs' r = jsToDocR r . jsSaturate Nothing

data RenderJs = RenderJs
  { renderJsS :: RenderJs -> JStat -> Doc
  , renderJsE :: RenderJs -> JExpr -> Doc
  , renderJsV :: RenderJs -> JVal  -> Doc
  , renderJsI :: RenderJs -> Ident -> Doc
  }

defaultRenderJs :: RenderJs
defaultRenderJs = RenderJs defRenderJsS defRenderJsE defRenderJsV defRenderJsI

jsToDoc :: JsToDoc a => a -> Doc
jsToDoc = jsToDocR defaultRenderJs

-- | Render a syntax tree as a pretty-printable document, using a given prefix
-- to all generated names. Use this with distinct prefixes to ensure distinct
-- generated names between independent calls to render(Prefix)Js.
renderPrefixJs :: (JsToDoc a, JMacro a) => ShortText -> a -> Doc
renderPrefixJs pfx = renderPrefixJs' defaultRenderJs pfx

renderPrefixJs' :: (JsToDoc a, JMacro a) => RenderJs -> ShortText -> a -> Doc
renderPrefixJs' r pfx = jsToDocR r . jsSaturate (Just $ "jmId_" `mappend` pfx)

braceNest :: Doc -> Doc
braceNest x = char '{' <+> nest 2 x $$ char '}'

braceNest' :: Doc -> Doc
braceNest' x = nest 2 (char '{' $+$ x) $$ char '}'

class JsToDoc a where jsToDocR :: RenderJs -> a -> Doc
instance JsToDoc JStat where jsToDocR r = renderJsS r r
instance JsToDoc JExpr where jsToDocR r = renderJsE r r
instance JsToDoc JVal  where jsToDocR r = renderJsV r r
instance JsToDoc Ident where jsToDocR r = renderJsI r r
instance JsToDoc [JExpr] where
    jsToDocR r = vcat . map ((<> semi) . jsToDocR r)
instance JsToDoc [JStat] where
    jsToDocR r = vcat . map ((<> semi) . jsToDocR r)

defRenderJsS :: RenderJs -> JStat -> Doc
defRenderJsS r = \case
  IfStat cond x y -> text "if" <> parens (jsToDocR r cond) $$ braceNest' (jsToDocR r x) $$ mbElse
        where mbElse | y == BlockStat []  = PP.empty
                     | otherwise = text "else" $$ braceNest' (jsToDocR r y)
  DeclStat x          -> text "var" <+> jsToDocR r x
  WhileStat False p b -> text "while" <> parens (jsToDocR r p) $$ braceNest' (jsToDocR r b)
  WhileStat True  p b -> (text "do" $$ braceNest' (jsToDocR r b)) $+$ text "while" <+> parens (jsToDocR r p)
  UnsatBlock e        -> jsToDocR r $ pseudoSaturate e
  BreakStat l         -> maybe (text "break") (((<+>) `on` stext) "break") l
  ContinueStat l      -> maybe (text "continue") (((<+>) `on` stext) "continue") l
  LabelStat l s       -> stext l <> char ':' $$ printBS s
        where
          printBS (BlockStat ss) = vcat $ interSemi $ flattenBlocks ss
          printBS x = jsToDocR r x
          interSemi [x] = [jsToDocR r x]
          interSemi [] = []
          interSemi (x:xs) = (jsToDocR r x <> semi) : interSemi xs

  ForInStat each i e b -> text txt <> parens (jsToDocR r i <+> text "in" <+> jsToDocR r e) $$ braceNest' (jsToDocR r b)
        where txt | each = "for each"
                  | otherwise = "for"
  SwitchStat e l d     -> text "switch" <+> parens (jsToDocR r e) $$ braceNest' cases
        where l' = map (\(c,s) -> (text "case" <+> parens (jsToDocR r c) <> char ':') $$$ (jsToDocR r s)) l ++ [text "default:" $$$ (jsToDocR r d)]
              cases = vcat l'
  ReturnStat e      -> text "return" <+> jsToDocR r e
  ApplStat e es     -> jsToDocR r e <> (parens . hsep . punctuate comma $ map (jsToDocR r) es)
  TryStat s i s1 s2 -> text "try" $$ braceNest' (jsToDocR r s) $$ mbCatch $$ mbFinally
        where mbCatch | s1 == BlockStat [] = PP.empty
                      | otherwise = text "catch" <> parens (jsToDocR r i) $$ braceNest' (jsToDocR r s1)
              mbFinally | s2 == BlockStat [] = PP.empty
                        | otherwise = text "finally" $$ braceNest' (jsToDocR r s2)
  AssignStat i x    -> jsToDocR r i <+> char '=' <+> jsToDocR r x
  UOpStat op x
    | isPre op && isAlphaOp op -> stext (uOpText op) <+> optParens r x
    | isPre op                 -> stext (uOpText op) <> optParens r x
    | otherwise                -> optParens r x <> stext (uOpText op)
  BlockStat xs -> jsToDocR r (flattenBlocks xs)

flattenBlocks :: [JStat] -> [JStat]
flattenBlocks = \case
  BlockStat y:ys -> flattenBlocks y ++ flattenBlocks ys
  y:ys           -> y : flattenBlocks ys
  []             -> []

optParens :: RenderJs -> JExpr -> Doc
optParens r x = case x of
  UOpExpr _ _ -> parens (jsToDocR r x)
  _           -> jsToDocR r x

defRenderJsE :: RenderJs -> JExpr -> Doc
defRenderJsE r = \case
  ValExpr x         -> jsToDocR r x
  SelExpr x y       -> cat [jsToDocR r x <> char '.', jsToDocR r y]
  IdxExpr x y       -> jsToDocR r x <> brackets (jsToDocR r y)
  IfExpr x y z      -> parens (jsToDocR r x <+> char '?' <+> jsToDocR r y <+> char ':' <+> jsToDocR r z)
  InfixExpr op x y  -> parens $ hsep [jsToDocR r x, stext (opText op), jsToDocR r y]
  UOpExpr op x
    | isPre op && isAlphaOp op -> stext (uOpText op) <+> optParens r x
    | isPre op                 -> stext (uOpText op) <> optParens r x
    | otherwise                -> optParens r x <> stext (uOpText op)
  ApplExpr je xs -> jsToDocR r je <> (parens . hsep . punctuate comma $ map (jsToDocR r) xs)
  UnsatExpr e    -> jsToDocR r $ pseudoSaturate e

defRenderJsV :: RenderJs -> JVal -> Doc
defRenderJsV r = \case
  JVar i    -> jsToDocR r i
  JList xs  -> brackets . hsep . punctuate comma $ map (jsToDocR r) xs
  JDouble (SaneDouble d)
    | d < 0 || isNegativeZero d -> parens (double d)
    | otherwise                 -> double d
  JInt i
    | i < 0     -> parens (integer i)
    | otherwise -> integer i
  JStr s    -> pprStringLit s
  JRegEx s  -> hcat [char '/',stext s, char '/']
  JHash m
    | M.null m  -> text "{}"
    | otherwise -> braceNest . hsep . punctuate comma .
                          map (\(x,y) -> squotes (stext x) <> colon <+> jsToDocR r y) $ M.toList m
  JFunc is b -> parens $ text "function" <> parens (hsep . punctuate comma . map (jsToDocR r) $ is) $$ braceNest' (jsToDocR r b)
  UnsatVal f -> jsToDocR r $ pseudoSaturate f

defRenderJsI :: RenderJs -> Ident -> Doc
defRenderJsI _ (TxtI t) = stext t


pprStringLit :: ShortText -> Doc
pprStringLit s = hcat [char '\"',encodeJson s, char '\"']

encodeJson :: ShortText -> Doc
encodeJson xs = hcat (map encodeJsonChar (ST.unpack xs))

encodeJsonChar :: Char -> Doc
encodeJsonChar = \case
  '/'  -> text "\\/"
  '\b' -> text "\\b"
  '\f' -> text "\\f"
  '\n' -> text "\\n"
  '\r' -> text "\\r"
  '\t' -> text "\\t"
  '"'  -> text "\\\""
  '\\' -> text "\\\\"
  c
    | not (isControl c) && ord c <= 127 -> char c
    | ord c <= 0xff   -> hexxs "\\x" 2 (ord c)
    | ord c <= 0xffff -> hexxs "\\u" 4 (ord c)
    | otherwise      -> let cp0 = ord c - 0x10000 -- output surrogate pair
                        in hexxs "\\u" 4 ((cp0 `shiftR` 10) + 0xd800) <>
                           hexxs "\\u" 4 ((cp0 .&. 0x3ff) + 0xdc00)
    where hexxs prefix pad cp =
            let h = showHex cp ""
            in  text (prefix ++ replicate (pad - length h) '0' ++ h)

uOpText :: JUOp -> ShortText
uOpText = \case
  NotOp     -> "!"
  BNotOp    -> "~"
  NegOp     -> "-"
  PlusOp    -> "+"
  NewOp     -> "new"
  TypeofOp  -> "typeof"
  DeleteOp  -> "delete"
  YieldOp   -> "yield"
  VoidOp    -> "void"
  PreIncOp  -> "++"
  PostIncOp -> "++"
  PreDecOp  -> "--"
  PostDecOp -> "--"

opText :: JOp -> ShortText
opText = \case
  EqOp          -> "=="
  StrictEqOp    -> "==="
  NeqOp         -> "!="
  StrictNeqOp   -> "!=="
  GtOp          -> ">"
  GeOp          -> ">="
  LtOp          -> "<"
  LeOp          -> "<="
  AddOp         -> "+"
  SubOp         -> "-"
  MulOp         -> "*"
  DivOp         -> "/"
  ModOp         -> "%"
  LeftShiftOp   -> "<<"
  RightShiftOp  -> ">>"
  ZRightShiftOp -> ">>>"
  BAndOp        -> "&"
  BOrOp         -> "|"
  BXorOp        -> "^"
  LAndOp        -> "&&"
  LOrOp         -> "||"
  InstanceofOp  -> "instanceof"
  InOp          -> "in"


isPre :: JUOp -> Bool
isPre = \case
  PostIncOp -> False
  PostDecOp -> False
  _         -> True

isAlphaOp :: JUOp -> Bool
isAlphaOp = \case
  NewOp    -> True
  TypeofOp -> True
  DeleteOp -> True
  YieldOp  -> True
  VoidOp   -> True
  _        -> False
