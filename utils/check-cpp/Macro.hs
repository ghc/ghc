module Macro (
    -- process,
    cppCond,
    -- get rid of warnings for tests
    -- m0,
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

-- ---------------------------------------------------------------------

--    We evaluate to an Int, which we convert to a bool
cppCond :: String -> PP Bool
cppCond str = do
  s <- getPpState
  let
    expanded = expand (pp_defines s) str
    v = case Parser.parseExpr expanded of
        Left err -> error $ "parseExpr:" ++ show (err, expanded)
        Right tree -> eval tree
  return (toBool v)

-- ---------------------------------------------------------------------

expand :: MacroDefines -> String -> String
expand s str = expanded
  where
    -- TODO: repeat until re-expand or fixpoint
    toks = case cppLex False str of
        Left err -> error $ "expand:" ++ show (err, str)
        Right tks -> tks
    expanded = combineToks $ map t_str $ expandToks maxExpansions s toks

maxExpansions :: Int
maxExpansions = 15

expandToks :: Int -> MacroDefines -> [Token] -> [Token]
expandToks 0 _ ts = error $ "macro_expansion limit (" ++ show maxExpansions ++ ") hit, aborting. ts=" ++ show ts
expandToks cnt s ts =
    let
        (!expansionDone, !r) = doExpandToks False s ts
    in
        if expansionDone
            then expandToks (cnt -1) s r
            else r

doExpandToks :: Bool -> MacroDefines -> [Token] -> (Bool, [Token])
doExpandToks ed _ [] = (ed, [])
doExpandToks ed s (TIdentifierLParen n: ts) =
  -- TIdentifierLParen has no meaning here (only in a #define), so
  -- restore it to its constituent tokens
  doExpandToks ed s (TIdentifier (init n):TOpenParen "(":ts)
doExpandToks _  s (TIdentifier "defined" : ts) = (True, rest)
  -- See Note: [defined unary operator] below
  where
    rest = case getExpandArgs ts of
      (Just [[TIdentifier macro_name]], rest0) ->
        case Map.lookup macro_name s of
          Nothing -> TInteger "0" : rest0
          Just _ ->TInteger "1" : rest0
      (Nothing, TIdentifier macro_name:ts0) ->
        case Map.lookup macro_name s of
          Nothing -> TInteger "0" : ts0
          Just _ ->TInteger "1" : ts0
      (Nothing,_) -> error $ "defined: expected an identifier, got:" ++ show ts
      (Just args,_) -> error $ "defined: expected a single arg, got:" ++ show args
doExpandToks ed s (TIdentifier n : ts) = (ed'', expanded ++ rest)
  where
    (ed', expanded, ts') = case Map.lookup n s of
        Nothing -> (ed, [TIdentifier n], ts)
        Just defs -> (ed0, r, rest1)
          where
            (args, rest0) = getExpandArgs ts
            fallbackArgs = fromMaybe (Nothing, [TIdentifier n]) (Map.lookup Nothing defs)
            (m_args, rhs) = fromMaybe fallbackArgs (Map.lookup (arg_arity args) defs)
            (ed0, r, rest1) = case m_args of
                Nothing -> (True, rhs, ts)
                Just _ -> (True, replace_args args m_args rhs, rest0)
    (ed'', rest) = doExpandToks ed' s ts'
doExpandToks ed s (t : ts) = (ed', t : r)
  where
    (ed', r) = doExpandToks ed s ts

{-
Note: [defined unary operator]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

From https://timsong-cpp.github.io/cppwp/n4140/cpp#cond-1

  unary operator expressions of the form

    defined identifier

  or

    defined ( identifier )

  which evaluate to 1 if the identifier is currently defined as a macro
  name (that is, if it is predefined or if it has been the subject of a
  #define preprocessing directive without an intervening #undef
  directive with the same subject identifier), 0 if it is not.

Also, may not change the meaning of `defined`

https://timsong-cpp.github.io/cppwp/n4140/cpp#predefined-4

  If any of the pre-defined macro names in this subclause, or the
  identifier defined, is the subject of a #define or a #undef
  preprocessing directive, the behavior is undefined

Empirical tests show that the presence or absence of arguments to the
macro definition does not matter.
-}

-- ---------------------------------------------------------------------

replace_args ::
    Maybe [[Token]] ->
    Maybe [String] ->
    [Token] ->
    [Token]
replace_args Nothing Nothing rhs = rhs
replace_args (Just args) (Just m_args) rhs = rhs'
  where
    -- At this point, the surrounding context should guarantee that the
    -- args and m_args have the same arity
    rhs' = foldl' (\acc (arg, m_arg) -> replace_arg arg m_arg acc) rhs (zip args m_args)
replace_args args margs _ = error $ "replace_args: impossible, mismatch between: " ++ show (args, margs)

-- The spec (https://timsong-cpp.github.io/cppwp/n4140/cpp#replace-10)
-- says an arg can only be an identifier
-- replace_arg arg m_arg acc = acc
replace_arg :: [Token] -> String -> [Token] -> [Token]
replace_arg _ _ [] = []
replace_arg a ma (TIdentifier t : ts)
    | ma == t = a ++ replace_arg a ma ts
replace_arg a ma (t : ts) = t : replace_arg a ma ts

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
            return (frag ++ frag', rest')
        _ -> do
            (frag', rest') <- pa_frag rest
            return (frag ++ frag', rest')

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

m1 :: Either String [Token]
m1 = cppLex False "`"

m2 :: Either String [Token]
m2 = cppLex False "hello(5)"

m3 :: Either String [Token]
m3 = cppLex True "#define FOO(m1,m2,m) ((m1) <  1 || (m1) == 1 && (m2) <  7 || (m1) == 1 && (m2) == 7 && (m) <= 0)"

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
m4 = cppLex True "#if (m < 1)"

m5 :: Either String (Maybe [[Token]], [Token])
m5 = do
    -- toks <- cppLex "(43,foo(a)) some other stuff"
    toks <- cppLex False "( ff(bar(),baz), 4 )"
    return $ getExpandArgs toks

tt :: Either String ([[Char]], [Char])
tt = case m5 of
    Left err -> Left err
    Right (Just a, b) -> Right (map (\k -> concatMap t_str k) a, concatMap t_str b)
    Right (Nothing, _) -> error "oops"

m6 = do

-- #define MIN_VERSION_GLASGOW_HASKELL(ma,mi,pl1,pl2) (
--   ( ( ma ) * 100 + ( mi ) ) < 913

-- ||

--   ( ( ma ) * 100 + ( mi ) ) == 913
--         && ( pl1 ) < 20250412

-- ||

--   ( ( ma ) * 100 + ( mi ) ) == 913
--         && ( pl1 ) == 20250412
--         && ( pl2 ) <= 0 )
  let foo =
       -- ma = 19
       -- mi = 13
       -- pl1 = 20250101
       -- pl2 = 0
       -- [ ""
       -- , "  ( ( 19 ) * 100 + ( 13 ) ) < 913"
       -- , ""
       -- , "||"
       -- , ""
       -- , "  ( ( 19 ) * 100 + ( 13 ) ) == 913"
       -- , "        && ( 20250101 ) < 20250412"
       -- , ""
       -- , "||"
       -- , ""
       -- , "  ( ( 19 ) * 100 + ( 13 ) ) == 913"
       -- , "        && ( 20250101 ) == 20250412"
       -- , "        && ( 0 ) <= 0 "
       -- ]
       [ ""
       , "  ( ( 19 ) * 100 + ( 13 ) ) < 913"
       , ""
       , "||"
       , ""
       , "  ( ( 19 ) * 100 + ( 13 ) ) == 913"
       , "        && ( 20250101 ) < 20250412"
       , ""
       , "||"
       , ""
       , "  ( ( 19 ) * 100 + ( 13 ) ) == 913"
       , "        && ( 20250101 ) == 20250412"
       , "        && ( 0 ) <= 0 "
       ]
  case Parser.parseExpr (concat foo) of
    Right v -> eval v
    Left err -> error err


-- Right
--   (Logic
--      LogicalOr
--        (Logic LogicalOr
--           (Comp CmpLt (Plus (Times (IntVal 19) (IntVal 100)) (IntVal 13)) (IntVal 913))
--           (Logic LogicalAnd
--              (Comp CmpEqual
--                 (Plus (Times (IntVal 19) (IntVal 100))
--                       (IntVal 13))
--                 (IntVal 913))
--              (Comp CmpLt (IntVal 20250101) (IntVal 20250412))))
--        (Logic LogicalAnd
--               (Logic LogicalAnd
--                  (Comp CmpEqual (Plus (Times (IntVal 19) (IntVal 100)) (IntVal 13)) (IntVal 913))
--                  (Comp CmpEqual (IntVal 20250101) (IntVal 20250412)))
--               (Comp CmpLtE (IntVal 0) (IntVal 0)))
--   )
