
{------------------------------------------------------------------------}
{--- Generic stuff for all architectures.                 Generics.hs ---}
{------------------------------------------------------------------------}

{- 
   This file is part of Cacheprof, a profiling tool for finding
   sources of cache misses in programs.

   Copyright (C) 1999 Julian Seward (jseward@acm.org) 
   Home page: http://www.cacheprof.org

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file LICENSE.
-}

module Generics where

internal msg
   = error ("\ncacheann: Internal error: " ++ msg ++ "\n")
incomplete msg
   = error ("\ncacheann: Unhandled instruction set artefact:\n   " 
            ++ msg ++ "\n")
inputerr msg
   = error ("\ncacheann: Bad input: " ++ msg ++ "\n")

{-----------------------------------------------------------}
{--- A data type for lexemes                             ---}
{-----------------------------------------------------------}

{- In here, LReg, LLiteral and LName are arch/syntax
   specific, but I don't think this matters, so long
   as the arch-specific lexer produces the Right Things.
   Note that lexers themselves are arch/syntax
   specific.
-}
data Lex
   = LReg     String  -- a register name eg "%eax"
   | LNum     String  -- a number           "456"
   | LLiteral String  -- a literal value    "$12"
   | LName    String  -- a name             "fprintf"
   | LLabel   String  -- a label            ".L3345"
   | LComma
   | LLParen
   | LRParen
   | LPlus
   | LMinus
   | LStar
   | LDollar
     deriving (Show, Eq)


unLReg (LReg s) = s
isLReg lx = case lx of { LReg _ -> True; _ -> False }

unLNum (LNum s) = s
isLNum lx = case lx of { LNum _ -> True; _ -> False }

unLLiteral (LLiteral s) = s
isLLiteral lx = case lx of { LLiteral _ -> True; _ -> False }

unLName (LName s) = s
isLName lx = case lx of { LName _ -> True; _ -> False }

unLLabel (LLabel s) = s
isLLabel lx = case lx of { LLabel _ -> True; _ -> False }



{-----------------------------------------------------------}
{--- Combinator parser generics -- building blocks for   ---}
{--- parsers                                             ---}
{-----------------------------------------------------------}

data PResult a
   = PFail
   | POk a [Lex]
     deriving Show

type Parser a = [Lex] -> PResult a

pEmpty :: a -> Parser a
pEmpty x ts = POk x ts

pSat :: (Lex -> Bool) -> Parser Lex
pSat p []     = PFail
pSat p (t:ts) = if p t then POk t ts else PFail

pApply :: (a -> b) -> Parser a -> Parser b
pApply f p ts
   = case p ts of
        PFail -> PFail
        POk x uu -> POk (f x) uu
   

pName :: String -> a -> Parser a
pName w x ((LName w2):lxs)
   = if w == w2 then POk x lxs else PFail
pName w x _ = PFail

p2 :: (a -> b -> c) 
      -> Parser a -> Parser b -> Parser c
p2 f p1 p2 ts1
   = case p1 ts1 of { PFail -> PFail ; POk x1 uu1 ->
     case p2 uu1 of { PFail -> PFail ; POk x2 uu2 ->
     POk (f x1 x2) uu2
     }}

p3 :: (a -> b -> c -> d) 
      -> Parser a -> Parser b -> Parser c -> Parser d
p3 f p1 p2 p3 ts1
   = case p1 ts1 of { PFail -> PFail ; POk x1 uu1 ->
     case p2 uu1 of { PFail -> PFail ; POk x2 uu2 ->
     case p3 uu2 of { PFail -> PFail ; POk x3 uu3 ->
     POk (f x1 x2 x3) uu3
     }}}

pStar :: Parser a -> Parser [a]
pStar p ts
   = case p ts of
        PFail     -> POk [] ts
        POk x uu1 -> case pStar p uu1 of
                        POk xs uu2 -> POk (x:xs) uu2
                        PFail      -> internal "pStar failed"

pPlus :: Parser a -> Parser [a]
pPlus p = p2 (:) p (pStar p)

pAlt2 :: Parser a -> Parser a -> Parser a
pAlt2 p1 p2 ts
   = case p1 ts of
        POk x1 uu -> POk x1 uu
        PFail     -> p2 ts

pAlts :: [Parser a] -> Parser a
pAlts = foldl1 pAlt2

pOpt :: Parser a -> Parser (Maybe a)
pOpt p ts
   = case p ts of
        PFail    -> POk Nothing ts
        POk x uu -> POk (Just x) uu

pStarComma p
   = pAlts [
        p2 (\xs y -> xs++[y]) (pPlus (p2 (\x y -> x) p pLComma)) p,
        pApply (\x -> [x]) p,
        pEmpty []
     ]

pLComma  = pSat (== LComma)
pLMinus  = pSat (== LMinus)
pLPlus   = pSat (== LPlus)
pLLParen = pSat (== LLParen)
pLRParen = pSat (== LRParen)
pLStar   = pSat (== LStar)
pLDollar = pSat (== LDollar)

pInParens p    = p3 (\_ r _ -> r) pLLParen p pLRParen
pPreComma p    = p2 (\_ r -> r) pLComma p
pPreCommaOpt p = p2 (\_ r -> r) (pOpt pLComma) p



{------------------------------------------------------------------------}
{--- end                                                  Generics.hs ---}
{------------------------------------------------------------------------}
