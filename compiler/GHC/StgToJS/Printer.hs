{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.StgToJS.Printer
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Jeffrey Young  <jeffrey.young@iohk.io>
--                Luite Stegeman <luite.stegeman@iohk.io>
--                Sylvain Henry  <sylvain.henry@iohk.io>
-- Stability   :  experimental
--
-- Custom prettyprinter for JS AST uses the JS PPr module for most of
-- the work
--
--
-----------------------------------------------------------------------------
module GHC.StgToJS.Printer
  ( pretty
  , ghcjsRenderJs
  , prettyBlock
  ) where


import GHC.JS.Syntax
import GHC.JS.Ppr

import qualified GHC.Data.ShortText as T
import           GHC.Utils.Ppr      as PP

import qualified Data.Map           as M

import Data.Char (isAlpha,isDigit)

import GHC.Prelude

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
  | otherwise = braceNest . PP.fsep . punctuate comma .
                          map (\(x,y) -> quoteIfRequired x <> PP.colon <+> jsToDocR r y) $ M.toList m
  where
    quoteIfRequired :: T.ShortText -> Doc
    quoteIfRequired x
      | isUnquotedKey x' = text x'
      | otherwise        = PP.squotes (text x')
    -- FIXME: Jeff (2022,03): remove the deserialization to String. We are only
    -- converting from ShortText to String here to call @all@ and @tail@.
      where x' = T.unpack x

    isUnquotedKey :: String -> Bool
    isUnquotedKey x | null x        = False
                    | all isDigit x = True
                    | otherwise     = validFirstIdent (head x)
                                      && all validOtherIdent (tail x)


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
                   <> parens (fsep . punctuate comma . map (jsToDocR r) $ is)
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
      | i == i' = (text "++" <> jsToDocR r i) : prettyBlock' r xs
prettyBlock' r ( (AssignStat (ValExpr (JVar i)) (InfixExpr SubOp (ValExpr (JVar i')) (ValExpr (JInt 1))))
               : xs
               )
      | i == i' = (text "--" <> jsToDocR r i) : prettyBlock' r xs
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
