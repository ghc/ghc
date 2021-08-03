{-# LANGUAGE OverloadedStrings #-}
{-
  Custom prettyprinter for JMacro AST
  - uses the jmacro prettyprinter for most of the work
  - fixme: need a better way to make a customized prettyprinter, without duplicating 5 cases
-}
module Gen2.Printer where

import           Data.Char                    (isAlpha, isDigit)
import qualified Data.Map                     as M
import qualified Data.Text.Lazy               as TL
import qualified Data.Text                    as T
import Prelude

import           Text.PrettyPrint.Leijen.Text (Doc, align, char, comma,
                                               fillSep, hcat, nest, parens,
                                               punctuate, text, vcat, (<+>))
import qualified Text.PrettyPrint.Leijen.Text as PP

import           Compiler.JMacro              (Ident, JExpr(..), JStat(..), JOp(..),
                                               JVal(..), jsToDocR, RenderJs(..), defaultRenderJs)


pretty :: JStat -> Doc
pretty = jsToDocR ghcjsRenderJs

ghcjsRenderJs :: RenderJs
ghcjsRenderJs = defaultRenderJs { renderJsV = ghcjsRenderJsV
                                , renderJsS = ghcjsRenderJsS
                                }

-- attempt to resugar some of the common constructs
ghcjsRenderJsS :: RenderJs -> JStat -> Doc
ghcjsRenderJsS r (BlockStat xs) = prettyBlock r (flattenBlocks xs)
ghcjsRenderJsS r s              = renderJsS defaultRenderJs r s

-- don't quote keys in our object literals, so closure compiler works
ghcjsRenderJsV :: RenderJs -> JVal -> Doc
ghcjsRenderJsV r (JHash m)
  | M.null m  = text "{}"
  | otherwise = braceNest . fillSep . punctuate comma .
                          map (\(x,y) -> quoteIfRequired x <> PP.colon <+> jsToDocR r y) $ M.toList m
  where
    quoteIfRequired x
      | isUnquotedKey x = text (TL.fromStrict x)
      | otherwise       = PP.squotes (text (TL.fromStrict x))

    isUnquotedKey x | T.null x        = False
                    | T.all isDigit x = True
                    | otherwise       = validFirstIdent (T.head x) && T.all validOtherIdent (T.tail x)

    -- fixme, this will quote some idents that don't really need to be quoted
    validFirstIdent c = c == '_' || c == '$' || isAlpha c
    validOtherIdent c = isAlpha c || isDigit c
ghcjsRenderJsV r v = renderJsV defaultRenderJs r v

prettyBlock :: RenderJs -> [JStat] -> Doc
prettyBlock r xs = vcat $ map addSemi (prettyBlock' r xs)

-- recognize common patterns in a block and convert them to more idiomatic/concise javascript
prettyBlock' :: RenderJs -> [JStat] -> [Doc]
-- resugar for loops with/without var declaration
prettyBlock' r ( (DeclStat i)
              : (AssignStat (ValExpr (JVar i')) v0)
              : (WhileStat False p (BlockStat bs))
              : xs
              )
     | i == i' && not (null flat) && isForUpdStat (last flat)
     = mkFor r True i v0 p (last flat) (init flat) : prettyBlock' r xs
        where
          flat = flattenBlocks bs
prettyBlock' r ( (AssignStat (ValExpr (JVar i)) v0)
               : (WhileStat False p (BlockStat bs))
               : xs
               )
     | not (null flat) && isForUpdStat (last flat)
     = mkFor r False i v0 p (last flat) (init flat) : prettyBlock' r xs
        where
          flat = flattenBlocks bs

-- global function (does not preserve semantics but works for GHCJS)
prettyBlock' r ( (DeclStat i)
               : (AssignStat (ValExpr (JVar i')) (ValExpr (JFunc is b)))
               : xs
               )
      | i == i' = (text "function" <+> jsToDocR r i
                   <> parens (fillSep . punctuate comma . map (jsToDocR r) $ is)
                   $$ braceNest' (jsToDocR r b)
                  ) : prettyBlock' r xs
-- declare/assign
prettyBlock' r ( (DeclStat i)
               : (AssignStat (ValExpr (JVar i')) v)
               : xs
               )
      | i == i' = (text "var" <+> jsToDocR r i <+> char '=' <+> jsToDocR r v) : prettyBlock' r xs

-- modify/assign operators (fixme this should be more general, but beware of side effects like PPostExpr)
prettyBlock' r ( (AssignStat (ValExpr (JVar i)) (InfixExpr AddOp (ValExpr (JVar i')) (ValExpr (JInt 1))))
               : xs
               )
      | i == i' = ("++" <> jsToDocR r i) : prettyBlock' r xs
prettyBlock' r ( (AssignStat (ValExpr (JVar i)) (InfixExpr SubOp (ValExpr (JVar i')) (ValExpr (JInt 1))))
               : xs
               )
      | i == i' = ("--" <> jsToDocR r i) : prettyBlock' r xs
prettyBlock' r ( (AssignStat (ValExpr (JVar i)) (InfixExpr AddOp (ValExpr (JVar i')) e))
               : xs
               )
      | i == i' = (jsToDocR r i <+> text "+=" <+> jsToDocR r e) : prettyBlock' r xs
prettyBlock' r ( (AssignStat (ValExpr (JVar i)) (InfixExpr SubOp (ValExpr (JVar i')) e))
               : xs
               )
      | i == i' = (jsToDocR r i <+> text "-=" <+> jsToDocR r e) : prettyBlock' r xs


prettyBlock' r (x:xs) = jsToDocR r x : prettyBlock' r xs
prettyBlock' _ [] = []

-- build the for block
mkFor :: RenderJs -> Bool -> Ident -> JExpr -> JExpr -> JStat -> [JStat] -> Doc
mkFor r decl i v0 p s1 sb = text "for" <> forCond <+> braceNest'' (jsToDocR r $ BlockStat sb)
    where
      c0 | decl      = text "var" <+> jsToDocR r i <+> char '=' <+> jsToDocR r v0
         | otherwise =                jsToDocR r i <+> char '=' <+> jsToDocR r v0
      forCond = parens $ hcat $ interSemi
                            [ c0
                            , jsToDocR r p
                            , parens (jsToDocR r s1)
                            ]

-- check if a statement is suitable to be converted to something in the for(;;x) position
isForUpdStat :: JStat -> Bool
isForUpdStat UOpStat {}    = True
isForUpdStat AssignStat {} = True
isForUpdStat ApplStat {}   = True
isForUpdStat _             = False

interSemi :: [Doc] -> [Doc]
interSemi [] = [PP.empty]
interSemi [s] = [s]
interSemi (x:xs) = x <> text ";" : interSemi xs

addSemi :: Doc -> Doc
addSemi x = x <> text ";"

-- stuff below is from jmacro
infixl 5 $$, $+$
($+$), ($$), ($$$) :: Doc -> Doc -> Doc
x $+$ y = x PP.<$> y
x $$ y  = align (x $+$ y)
x $$$ y = align (nest 2 $ x $+$ y)

flattenBlocks :: [JStat] -> [JStat]
flattenBlocks (BlockStat y:ys) = flattenBlocks y ++ flattenBlocks ys
flattenBlocks (y:ys) = y : flattenBlocks ys
flattenBlocks [] = []

braceNest :: Doc -> Doc
braceNest x = char '{' <+> nest 2 x $$ char '}'

braceNest' :: Doc -> Doc
braceNest' x = nest 2 (char '{' $+$ x) $$ char '}'

-- somewhat more compact (egyptian style) braces
braceNest'' :: Doc -> Doc
braceNest'' x = nest 2 (char '{' PP.<$> x) PP.<$> char '}'

