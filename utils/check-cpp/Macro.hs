module Macro (
    process,
    cppIf,
    -- get rid of warnings for tests
    m0,
    m1,
    m2,
    m3,
    m4,
    m5,
    tt,
) where

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

import Debug.Trace

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

{-
https://timsong-cpp.github.io/cppwp/n4140/cpp#replace-11

The sequence of preprocessing tokens bounded by the outside-most
matching parentheses forms the list of arguments for the function-like
macro. The individual arguments within the list are separated by comma
preprocessing tokens, but comma preprocessing tokens between matching
inner parentheses do not separate arguments.
-}

{- | Look for possible arguments to a macro expansion.
The only thing we look for are commas, open parens, and close parens.
-}
getExpandArgs :: [Token] -> (Maybe [[Token]], [Token])
getExpandArgs ts =
    case pArgs ts of
        Left err -> error $ err
        Right r -> r

pArgs :: [Token] -> Either String (Maybe [[Token]], [Token])
pArgs (TOpenParen _ : ts) = do
    (args, rest) <- pArgsList ts
    case rest of
        [] -> return (args, rest)
        TCloseParen _ : rest' -> return (args, rest')
        _ -> Left $ "expected ')', got: " ++ show rest
pArgs ts = Right (Nothing, ts)

pArgsList :: [Token] -> Either String (Maybe [[Token]], [Token])
pArgsList ts = do
    (arg, rest) <- pArg ts
    case rest of
        [] -> return (Just [arg], rest)
        TCloseParen _ : _ -> return (Just [arg], rest)
        TComma _ : rest1 -> do
            (args, rest2) <- pArgsList rest1
            return (Just [arg] <> args, rest2)
        _ -> Left $ "expected ',' or ')', got: " ++ show rest

-- An arg is
--  sequence of non-comma tokens, ending with ',' or ')'
--  within that, (', anything, ')', possibly nested
pArg :: [Token] -> Either String ([Token], [Token])
pArg ts = do
    (frag, rest) <- pa_frag ts
    case rest of
        [] -> return (frag, rest)
        TCloseParen _ : _ -> return (frag, rest)
        TComma _ : _ -> return (frag, rest)
        (t@TOpenParen{}) : ts' -> do
            (frag', rest') <- inside_parens 1 [t] ts'
            return (frag <> frag', rest')
        _ -> do
            (frag', rest') <- pa_frag rest
            return (frag <> frag', rest')

pa_frag :: [Token] -> Either String ([Token], [Token])
pa_frag [] = return ([], [])
pa_frag (t : ts)
    | isOther t = return $ pOtherRest [t] ts
pa_frag (t : ts) =
    case t of
        TOpenParen _ -> do
            inside_parens 1 [t] ts
        _ -> return ([], (t : ts))

-- Process the part in an argument starting with parens
inside_parens :: Int -> [Token] -> [Token] -> Either String ([Token], [Token])
inside_parens pc acc [] =
    if pc == 0
        then return (reverse acc, [])
        else Left $ "Unexpected end of input in arg, at: " ++ show (map t_str $ reverse acc)
inside_parens pc acc (t : ts) =
    case t of
        TComma _ ->
            if pc == 0
                then return (reverse acc, t : ts)
                else inside_parens pc (t : acc) ts
        TOpenParen _ -> inside_parens (pc + 1) (t : acc) ts
        TCloseParen _ ->
            if pc > 0
                then inside_parens (pc - 1) (t : acc) ts
                else return (reverse acc, t : ts)
        _ -> inside_parens pc (t : acc) ts

pOtherRest :: [Token] -> [Token] -> ([Token], [Token])
pOtherRest acc (t : ts)
    | isOther t = pOtherRest (t : acc) ts
pOtherRest acc ts = (reverse acc, ts)

isOther :: Token -> Bool
isOther (TComma _) = False
isOther (TOpenParen _) = False
isOther (TCloseParen _) = False
isOther _ = True

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

m5 :: Either String (Maybe [[Token]], [Token])
m5 = do
    -- toks <- cppLex "(43,foo(a)) some other stuff"
    toks <- cppLex "( ff(bar(),baz), 4 )"
    return $ getExpandArgs toks

tt = case m5 of
    Left err -> Left err
    Right (Just a, b) -> Right (map (\k -> concatMap t_str k) a, concatMap t_str b)
    Right (Nothing, _) -> error "oops"
