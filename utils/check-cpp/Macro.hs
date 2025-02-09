module Macro where

-- From https://gcc.gnu.org/onlinedocs/cpp/Macros.html

{-

A macro is a fragment of code which has been given a name. Whenever
the name is used, it is replaced by the contents of the macro. There
are two kinds of macros. They differ mostly in what they look like
when they are used. Object-like macros resemble data objects when
used, function-like macros resemble function calls.

... the preprocessor operator `defined` can never be defined as a macro

If the expansion of a macro contains its own name, either directly or
via intermediate macros, it is not expanded again when the expansion
is examined for more macros. See
https://gcc.gnu.org/onlinedocs/cpp/Self-Referential-Macros.html for
details

-}

-- TODO: Parse tokens with original locations in them.

import qualified Data.Map as Map
import Data.Maybe

import Eval
import ParsePP
import Parser
import ParserM
import State

-- ---------------------------------------------------------------------

process :: PpState -> Input -> (PpState, Output)
process s str = (s0, o)
  where
    o = case parseDirective str of
        Left _ -> undefined
        Right r -> r
    s0 = case o of
        CppDefine name args rhs -> define s name args rhs
        CppInclude _ -> undefined
        CppIfdef name -> ifdef s name
        CppIf ifstr -> cppIf s ifstr
        CppIfndef name -> ifndef s name
        CppElse -> cppElse s
        CppEndif -> popScope' s
        CppDumpState -> undefined

-- ---------------------------------------------------------------------

define :: PpState -> String -> Maybe ([String]) -> MacroDef -> PpState
define s name args toks = addDefine' s (MacroName name args) toks

ifdef :: PpState -> String -> PpState
ifdef s name = pushAccepting' s (ppIsDefined' s (MacroName name Nothing))

ifndef :: PpState -> String -> PpState
ifndef s name = pushAccepting' s (not $ ppIsDefined' s (MacroName name Nothing))

--    We evaluate to an Int, which we convert to a bool
cppIf :: PpState -> String -> PpState
cppIf s str = pushAccepting' s (toBool v)
  where
    expanded = expand s str
    v = case Parser.parseExpr expanded of
        Left err -> error $ "parseExpr:" ++ show (err, expanded)
        Right tree -> eval tree

cppElse :: PpState -> PpState
cppElse s = setAccepting' s (not $ getAccepting' s)

-- ---------------------------------------------------------------------

expand :: PpState -> String -> String
expand s str = expanded
  where
    -- TODO: repeat until re-expand or fixpoint
    toks = case cppLex str of
        Left err -> error $ "expand:" ++ show (err, str)
        Right tks -> tks
    expanded = combineToks $ map t_str $ expandToks s toks

expandToks :: PpState -> [Token] -> [Token]
expandToks _ [] = []
expandToks s (TIdentifier n : ts) = expanded ++ expandToks s ts
  where
    expanded = case Map.lookup n (pp_defines s) of
        Nothing -> [TIdentifier n]
        Just defs -> r
          where
            -- Assume no args to start with
            r = fromMaybe [TIdentifier n] (Map.lookup Nothing defs)
expandToks s (t : ts) = t : expandToks s ts

-- ---------------------------------------------------------------------

m0 :: (PpState, Output)
m0 = do
    let (s0, _) = process initPpState "#define FOO 3"
    let (s1, _) = process s0 "#ifdef FOO"
    process s1 "# if FOO == 4"

-- ---------------------------------------------------------------------

m1 :: Either String [Token]
m1 = cppLex "`"

m2 :: Either String [Token]
m2 = cppLex "hello(5)"

m3 :: Either String [Token]
m3 = cppLex "#define FOO(m1,m2,m) ((m1) <  1 || (m1) == 1 && (m2) <  7 || (m1) == 1 && (m2) == 7 && (m) <= 0)"

-- Right [THash {t_str = "#"}
--       ,TDefine {t_str = "define"}
--       ,TUpperName {t_str = "FOO"}
--       ,TOpenParen {t_str = "("}
--       ,TLowerName {t_str = "m1"}
--       ,TComma {t_str = ","}
--       ,TLowerName {t_str = "m2"}
--       ,TComma {t_str = ","}
--       ,TLowerName {t_str = "m"}
--       ,TCloseParen {t_str = ")"}
--       ,TOther {t_str = " ((m1) <  1 || (m1) == 1 && (m2) <  7 || (m1) == 1 && (m2) == 7 && (m) <= 0)"}
--       ]

m4 :: Either String [Token]
m4 = cppLex "#if (m < 1)"
