module GHC.Parser.PreProcess.Macro where

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

import Data.Map qualified as Map
import Data.Maybe

import GHC.Parser.PreProcess.Eval
import GHC.Parser.PreProcess.ParsePP
import GHC.Parser.PreProcess.Parser qualified as Parser
import GHC.Parser.PreProcess.ParserM
import GHC.Parser.PreProcess.Types
import GHC.Prelude

-- ---------------------------------------------------------------------

process :: PpState -> Input -> (PpState, Output)
process s str = (s0, o)
  where
    o = case parseDirective str of
        Left _ -> undefined
        Right r -> r
    s0 = case o of
        CppDefine name rhs -> define s name rhs
        CppInclude _ -> undefined
        CppIfdef name -> ifdef s name
        CppIf ifstr -> cppIf s ifstr
        CppIfndef name -> ifndef s name
        CppElse -> undefined
        CppEndif -> undefined

-- ---------------------------------------------------------------------

define :: PpState -> String -> MacroDef -> PpState
define s name toks = s{pp_defines = Map.insert (MacroName name Nothing) toks (pp_defines s)}

ifdef :: PpState -> String -> PpState
ifdef s name =
    case Map.lookup (MacroName name Nothing) (pp_defines s) of
        Just _ -> s{pp_accepting = True}
        _ -> s{pp_accepting = False}

ifndef :: PpState -> String -> PpState
ifndef s name =
    case Map.lookup (MacroName name Nothing) (pp_defines s) of
        Just _ -> s{pp_accepting = False}
        _ -> s{pp_accepting = True}

cppIf :: PpState -> String -> PpState
cppIf s str = r
  where
    expanded = expand s str
    -- toks0 = cppLex expanded
    -- r = error (show toks0)
    v = case Parser.parseExpr expanded of
        Left err -> error $ show err
        Right tree -> eval tree
    --    We evaluate to an Int, which we convert to a bool
    r = s{pp_accepting = toBool v}

-- ---------------------------------------------------------------------

expand :: PpState -> String -> String
expand s str = expanded
  where
    -- TODO: repeat until re-expand or fixpoint
    toks = case cppLex str of
        Left err -> error err
        Right tks -> tks
    expanded = concatMap (expandOne s) toks

expandOne :: PpState -> Token -> String
expandOne s tok = r
  where
    -- TODO: protect against looking up `define`
    r =
        fromMaybe
            (t_str tok)
            (Map.lookup (MacroName (t_str tok) Nothing) (pp_defines s))

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
