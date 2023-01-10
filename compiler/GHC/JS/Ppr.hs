{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BlockArguments #-}

-- For Outputable instances for JS syntax
{-# OPTIONS_GHC -Wno-orphans #-}

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
  , flattenBlocks
  , braceNest
  , hangBrace
  )
where

import GHC.Prelude

import GHC.JS.Syntax
import GHC.JS.Transform


import Data.Char (isControl, ord)
import Data.List (sortOn)

import Numeric(showHex)

import GHC.Utils.Outputable (Outputable (..), docToSDoc)
import GHC.Utils.Ppr as PP
import GHC.Data.FastString
import GHC.Types.Unique.Map

instance Outputable JExpr where
  ppr = docToSDoc . renderJs

instance Outputable JVal where
  ppr = docToSDoc . renderJs


($$$) :: Doc -> Doc -> Doc
x $$$ y = nest 2 $ x $+$ y

-- | Render a syntax tree as a pretty-printable document
-- (simply showing the resultant doc produces a nice,
-- well formatted String).
renderJs :: (JsToDoc a) => a -> Doc
renderJs = renderJs' defaultRenderJs

renderJs' :: (JsToDoc a) => RenderJs -> a -> Doc
renderJs' r = jsToDocR r

data RenderJs = RenderJs
  { renderJsS :: !(RenderJs -> JStat -> Doc)
  , renderJsE :: !(RenderJs -> JExpr -> Doc)
  , renderJsV :: !(RenderJs -> JVal  -> Doc)
  , renderJsI :: !(RenderJs -> Ident -> Doc)
  }

defaultRenderJs :: RenderJs
defaultRenderJs = RenderJs defRenderJsS defRenderJsE defRenderJsV defRenderJsI

jsToDoc :: JsToDoc a => a -> Doc
jsToDoc = jsToDocR defaultRenderJs

-- | Render a syntax tree as a pretty-printable document, using a given prefix
-- to all generated names. Use this with distinct prefixes to ensure distinct
-- generated names between independent calls to render(Prefix)Js.
renderPrefixJs :: (JsToDoc a, JMacro a) => a -> Doc
renderPrefixJs = renderPrefixJs' defaultRenderJs

renderPrefixJs' :: (JsToDoc a, JMacro a) => RenderJs -> a -> Doc
renderPrefixJs' r = jsToDocR r

braceNest :: Doc -> Doc
braceNest x = char '{' <+> nest 2 x $$ char '}'

-- | Hang with braces:
--
--  hdr {
--    body
--  }
hangBrace :: Doc -> Doc -> Doc
hangBrace hdr body = sep [ hdr <> char ' ' <> char '{', nest 2 body, char '}' ]

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
  IfStat cond x y -> hangBrace (text "if" <> parens (jsToDocR r cond))
                      (jsToDocR r x)
                      $$ mbElse
        where mbElse | y == BlockStat []  = PP.empty
                     | otherwise = hangBrace (text "else") (jsToDocR r y)
  DeclStat x Nothing  -> text "var" <+> jsToDocR r x
  DeclStat x (Just e) -> text "var" <+> jsToDocR r x <+> char '=' <+> jsToDocR r e
  WhileStat False p b -> hangBrace (text "while" <> parens (jsToDocR r p)) (jsToDocR r b)
  WhileStat True  p b -> (hangBrace (text "do") (jsToDocR r b)) $+$ text "while" <+> parens (jsToDocR r p)
  BreakStat l         -> maybe (text "break")    (\(LexicalFastString s) -> (text "break"    <+> ftext s)) l
  ContinueStat l      -> maybe (text "continue") (\(LexicalFastString s) -> (text "continue" <+> ftext s)) l
  LabelStat (LexicalFastString l) s -> ftext l <> char ':' $$ printBS s
        where
          printBS (BlockStat ss) = vcat $ interSemi $ flattenBlocks ss
          printBS x = jsToDocR r x
          interSemi [x] = [jsToDocR r x]
          interSemi [] = []
          interSemi (x:xs) = (jsToDocR r x <> semi) : interSemi xs

  ForInStat each i e b -> hangBrace (text txt <> parens (jsToDocR r i <+> text "in" <+> jsToDocR r e)) (jsToDocR r b)
        where txt | each = "for each"
                  | otherwise = "for"
  SwitchStat e l d     -> hangBrace (text "switch" <+> parens (jsToDocR r e)) cases
        where l' = map (\(c,s) -> (text "case" <+> parens (jsToDocR r c) <> char ':') $$$ (jsToDocR r s)) l ++ [text "default:" $$$ (jsToDocR r d)]
              cases = vcat l'
  ReturnStat e      -> text "return" <+> jsToDocR r e
  ApplStat e es     -> jsToDocR r e <> (parens . hsep . punctuate comma $ map (jsToDocR r) es)
  TryStat s i s1 s2 -> hangBrace (text "try") (jsToDocR r s) $$ mbCatch $$ mbFinally
        where mbCatch | s1 == BlockStat [] = PP.empty
                      | otherwise = hangBrace (text "catch" <> parens (jsToDocR r i)) (jsToDocR r s1)
              mbFinally | s2 == BlockStat [] = PP.empty
                        | otherwise = hangBrace (text "finally") (jsToDocR r s2)
  AssignStat i x    -> case x of
    -- special treatment for functions, otherwise there is too much left padding
    -- (more than the length of the expression assigned to). E.g.
    --
    --    var long_variable_name = (function()
    --                               {
    --                               ...
    --                             });
    --
    ValExpr (JFunc is b) -> sep [jsToDocR r i <+> text "= function" <> parens (hsep . punctuate comma . map (jsToDocR r) $ is) <> char '{', nest 2 (jsToDocR r b), text "}"]
    _                      -> jsToDocR r i <+> char '=' <+> jsToDocR r x
  UOpStat op x
    | isPre op && isAlphaOp op -> ftext (uOpText op) <+> optParens r x
    | isPre op                 -> ftext (uOpText op) <> optParens r x
    | otherwise                -> optParens r x <> ftext (uOpText op)
  BlockStat xs -> jsToDocR r (flattenBlocks xs)

flattenBlocks :: [JStat] -> [JStat]
flattenBlocks = \case
  BlockStat y:ys -> flattenBlocks y ++ flattenBlocks ys
  y:ys            -> y : flattenBlocks ys
  []              -> []

optParens :: RenderJs -> JExpr -> Doc
optParens r x = case x of
  UOpExpr _ _ -> parens (jsToDocR r x)
  _           -> jsToDocR r x

defRenderJsE :: RenderJs -> JExpr -> Doc
defRenderJsE r = \case
  ValExpr x         -> jsToDocR r x
  SelExpr x y       -> jsToDocR r x <> char '.' <> jsToDocR r y
  IdxExpr x y       -> jsToDocR r x <> brackets (jsToDocR r y)
  IfExpr x y z      -> parens (jsToDocR r x <+> char '?' <+> jsToDocR r y <+> char ':' <+> jsToDocR r z)
  InfixExpr op x y  -> parens $ hsep [jsToDocR r x, ftext (opText op), jsToDocR r y]
  UOpExpr op x
    | isPre op && isAlphaOp op -> ftext (uOpText op) <+> optParens r x
    | isPre op                 -> ftext (uOpText op) <> optParens r x
    | otherwise                -> optParens r x <> ftext (uOpText op)
  ApplExpr je xs -> jsToDocR r je <> (parens . hsep . punctuate comma $ map (jsToDocR r) xs)

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
  JStr   s -> pprStringLit s
  JRegEx s -> hcat [char '/',ftext s, char '/']
  JHash m
    | isNullUniqMap m  -> text "{}"
    | otherwise -> braceNest . hsep . punctuate comma .
                          map (\(x,y) -> squotes (ftext x) <> colon <+> jsToDocR r y)
                          -- nonDetKeysUniqMap doesn't introduce non-determinism here
                          -- because we sort the elements lexically
                          $ sortOn (LexicalFastString . fst) (nonDetUniqMapToList m)
  JFunc is b -> parens $ hangBrace (text "function" <> parens (hsep . punctuate comma . map (jsToDocR r) $ is)) (jsToDocR r b)

defRenderJsI :: RenderJs -> Ident -> Doc
defRenderJsI _ (TxtI t) = ftext t


pprStringLit :: FastString -> Doc
pprStringLit s = hcat [char '\"',encodeJson s, char '\"']

encodeJson :: FastString -> Doc
encodeJson xs = hcat (map encodeJsonChar (unpackFS xs))

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

uOpText :: UOp -> FastString
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

opText :: Op -> FastString
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


isPre :: UOp -> Bool
isPre = \case
  PostIncOp -> False
  PostDecOp -> False
  _         -> True

isAlphaOp :: UOp -> Bool
isAlphaOp = \case
  NewOp    -> True
  TypeofOp -> True
  DeleteOp -> True
  YieldOp  -> True
  VoidOp   -> True
  _        -> False
