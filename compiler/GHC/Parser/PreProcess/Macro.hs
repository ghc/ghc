module GHC.Parser.PreProcess.Macro (
    -- process,
    cppCond,
    -- get rid of warnings for tests
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

import Data.List (intercalate)
import Data.Map qualified as Map
import Data.Maybe
import Data.Semigroup qualified as S

import GHC.Base
import GHC.Parser.Lexer (P (..), PState (..), ParseResult (..))
import GHC.Parser.PreProcess.ParsePP
import GHC.Parser.PreProcess.Parser qualified as Parser
import GHC.Parser.PreProcess.ParserM
import GHC.Parser.PreProcess.State
import GHC.Prelude
import GHC.Types.SrcLoc
import GHC.Utils.Outputable
import GHC.Utils.Panic (panic)

-- ---------------------------------------------------------------------

-- We evaluate to an Int, which we convert to a bool
cppCond :: SrcSpan -> String -> PP Bool
cppCond loc str = do
    r <- runPM $ cppCond' loc str
    return $ fromMaybe False r

cppCond' :: SrcSpan -> String -> PPM Bool
cppCond' loc str = do
    s <- liftPM getPpState
    expanded <- expand loc (pp_defines s) str
    v <- case Parser.parseExpr expanded of
        Left err -> do
            let detail =
                    if str == expanded || expanded == ""
                        then
                            [text str]
                        else
                            [ text expanded
                            , text "expanded from:"
                            , text str
                            ]
            liftPM $
                addGhcCPPError'
                    loc
                    "Error evaluating CPP condition:"
                    ( text err
                        <+> text "of"
                        $+$ vcat detail
                    )
            return 0
        Right tree -> return (eval tree)
    return (toBool v)

-- ---------------------------------------------------------------------

expand :: SrcSpan -> MacroDefines -> String -> PPM String
expand loc s str = do
    case cppLex False str of
        Left err -> do
            liftPM $
                addGhcCPPError'
                    loc
                    "Error evaluating CPP condition:"
                    (text err <+> text "of" $+$ text str)
            failPM
        Right tks -> do
            toks <- expandToks loc maxExpansions s tks
            return $ combineToks $ map t_str toks

maxExpansions :: Int
maxExpansions = 15

expandToks :: SrcSpan -> Int -> MacroDefines -> [Token] -> PPM [Token]
expandToks loc 0 _ ts = do
    liftPM $
        addGhcCPPError'
            loc
            "CPP macro expansion limit hit:"
            (text (combineToks $ map t_str ts))
    failPM
expandToks loc cnt s ts = do
    (!expansionDone, !r) <- doExpandToks loc False s ts
    if expansionDone
        then expandToks loc (cnt - 1) s r
        else return r

doExpandToks :: SrcSpan -> Bool -> MacroDefines -> [Token] -> PPM (Bool, [Token])
doExpandToks _loc ed _ [] = return (ed, [])
doExpandToks loc ed s (TIdentifierLParen n : ts) =
    -- TIdentifierLParen has no meaning here (only in a #define), so
    -- restore it to its constituent tokens
    doExpandToks loc ed s (TIdentifier (init n) : TOpenParen "(" : ts)
doExpandToks loc _ s (TIdentifier "defined" : ts) = do
    -- See Note: ['defined' unary operator] below
    expandedArgs <- getExpandArgs loc ts
    case expandedArgs of
        (Just [[TIdentifier macro_name]], rest0) ->
            case Map.lookup macro_name s of
                Nothing -> return (True, TInteger "0" : rest0)
                Just _ -> return (True, TInteger "1" : rest0)
        (Nothing, TIdentifier macro_name : ts0) ->
            case Map.lookup macro_name s of
                Nothing -> return (True, TInteger "0" : ts0)
                Just _ -> return (True, TInteger "1" : ts0)
        (Nothing, _) -> do
            liftPM $
                addGhcCPPError'
                    loc
                    "CPP defined: expected an identifier, got:"
                    (text (concatMap t_str ts))
            failPM -- TODO:AZ make part of addGhcCPPError'?
        (Just args, _) -> do
            liftPM $
                addGhcCPPError'
                    loc
                    "CPP defined: expected a single arg, got:"
                    (text (intercalate "," (map (concatMap t_str) args)))
            failPM -- TODO:AZ make part of addGhcCPPError'?
doExpandToks loc ed s (TIdentifier n : ts) = do
    (args, rest0) <- getExpandArgs loc ts
    let
        (ed', expanded, ts') = case Map.lookup n s of
            Nothing -> (ed, [TIdentifier n], ts)
            Just defs -> (ed0, r, rest1)
              where
                fallbackArgs = fromMaybe (Nothing, [TIdentifier n]) (Map.lookup Nothing defs)
                (m_args, rhs) = fromMaybe fallbackArgs (Map.lookup (arg_arity args) defs)
                (ed0, r, rest1) = case m_args of
                    Nothing -> (True, rhs, ts)
                    Just _ -> (True, replace_args args m_args rhs, rest0)
    (ed'', rest) <- doExpandToks loc ed' s ts'
    return (ed'', expanded ++ rest)
doExpandToks loc ed s (t : ts) = do
    (ed', r) <- doExpandToks loc ed s ts
    return (ed', t : r)

{-
Note: ['defined' unary operator]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
replace_args args margs _ = panic $ "replace_args: impossible, mismatch between: " ++ show (args, margs)

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
getExpandArgs :: SrcSpan -> [Token] -> PPM (Maybe [[Token]], [Token])
getExpandArgs loc ts =
    case pArgs ts of
        Left err -> do
            liftPM $
                addGhcCPPError'
                    loc
                    "CPP: cannot expand macro arguments:"
                    (text err <+> text "in" $+$ text (concatMap t_str ts))
            return (Nothing, ts)
        Right r -> return r

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
            return (Just [arg] S.<> args, rest2)
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

-- | Wrapper around P Monad to include a Maybe result
type PPM = PM PpState
-- See Note [PPM Monad]

newtype PM p a = PM {unPM :: PState p -> ParseResult p (Maybe a)}

instance Functor (PM p) where
    fmap = liftM

instance Applicative (PM p) where
    pure = returnP
    (<*>) = ap

instance Monad (PM p) where
    (>>=) = thenP

returnP :: a -> PM p a
returnP a = a `seq` (PM $ \s -> POk s (Just a))

failPM :: PM p a
failPM = PM $ \s -> POk s Nothing

thenP :: PM p a -> (a -> PM p b) -> PM p b
(PM m) `thenP` k = PM $ \s ->
    case m s of
        POk s1 Nothing -> POk s1 Nothing
        POk s1 (Just a) -> (unPM (k a)) s1
        PFailed s1 -> PFailed s1

runPM :: PM p a -> P p (Maybe a)
runPM m = P $ \s -> (unPM m) s

liftPM :: P p a -> PM p a
liftPM m = PM $ \s -> case (unP m) s of
    POk s1 a -> POk s1 (Just a)
    PFailed s1 -> PFailed s1

{-
Note [PPM Monad]
~~~~~~~~~~~~~~~~

The PPM monad is a combination of the Lexer P monad and the Maybe
monad.

It is used when processing GHC_CPP macro conditionals, where we may
find an error processing the condition, such as a CPP parse error,
undefined macro, or similar.

In this case we do not want to fail the overall GHC Parse, and simply
reporting the error and continuing is not enough, as the CPP
processing needs to be aware that there was a failure in the
processing, and not continue. In particular, this prevents reporting
additional CPP errors after the originating cause, and simplifies the
coding to not have to match on returned Maybe results
-}

-- ---------------------------------------------------------------------
--  Evaluate a CPP expression
-- ---------------------------------------------------------------------

eval :: Expr -> Int
eval (Parens e) = eval e
eval (Not e) = fromBool $ not (toBool $ eval e)
eval (Var _) = 0 -- Spec says remaining identifiers are replaces with zero
eval (IntVal i) = i
eval (Plus e1 e2) = (eval e1) + (eval e2)
eval (Minus e1 e2) = (eval e1) - (eval e2)
eval (Times e1 e2) = (eval e1) * (eval e2)
eval (Logic op e1 e2) = evalLogicOp op (eval e1) (eval e2)
eval (Comp op e1 e2) = evalCompOp op (eval e1) (eval e2)

evalLogicOp :: LogicOp -> Int -> Int -> Int
evalLogicOp LogicalOr e1 e2 = fromBool $ (toBool e1) || (toBool e2)
evalLogicOp LogicalAnd e1 e2 = fromBool $ (toBool e1) && (toBool e2)

evalCompOp :: CompOp -> Int -> Int -> Int
evalCompOp CmpEqual e1 e2 = fromBool $ e1 == e2
evalCompOp CmpNotEqual e1 e2 = fromBool $ e1 /= e2
evalCompOp CmpGt e1 e2 = fromBool $ e1 > e2
evalCompOp CmpGtE e1 e2 = fromBool $ e1 >= e2
evalCompOp CmpLt e1 e2 = fromBool $ e1 < e2
evalCompOp CmpLtE e1 e2 = fromBool $ e1 <= e2

toBool :: Int -> Bool
toBool 0 = False
toBool _ = True

fromBool :: Bool -> Int
fromBool False = 0
fromBool True = 1

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
    pArgs toks

tt :: Either String ([[Char]], [Char])
tt = case m5 of
    Left err -> Left err
    Right (Just a, b) -> Right (map (\k -> concatMap t_str k) a, concatMap t_str b)
    Right (Nothing, _) -> error "oops"
