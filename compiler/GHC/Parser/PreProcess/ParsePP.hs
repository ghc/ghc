module GHC.Parser.PreProcess.ParsePP (
    parseDirective,
    cppLex,
    combineToks,
    -- Reduce warnings so long
    t0,
    t1,
    t2,
    t3,
    t4,
) where

import Data.List (intercalate)
import GHC.Parser.Errors.Ppr ()
import GHC.Parser.PreProcess.Lexer
import GHC.Parser.PreProcess.ParserM (Token (..), init_state, St(..))
import GHC.Parser.PreProcess.State
import GHC.Prelude

-- =====================================================================
-- First parse to CPP tokens, using a C++-like language spec
-- https://gcc.gnu.org/onlinedocs/cpp/Tokenization.html

-- | Parse a CPP directive, using tokens from the CPP lexer
parseDirective :: String -> Either String CppDirective
parseDirective s =
    case cppLex True s of
        Left e -> Left e
        Right toks ->
            case map deComment toks of
                (THash "#" : TIdentifier "define" : ts) -> cppDefine ts
                (THash "#" : TIdentifier "undef" : ts) -> Right $ cppUndef (map t_str ts)
                (THash "#" : TIdentifier "if" : ts) -> Right $ cppIf (map t_str ts)
                (THash "#" : TIdentifier "ifndef" : ts) -> Right $ cppIfndef (map t_str ts)
                (THash "#" : TIdentifier "ifdef" : ts) -> Right $ cppIfdef (map t_str ts)
                (THash "#" : TIdentifier "else" : ts) -> Right $ cppElse (map t_str ts)
                (THash "#" : TIdentifier "elif" : ts) -> Right $ cppElIf (map t_str ts)
                (THash "#" : TIdentifier "endif" : ts) -> Right $ cppEndif (map t_str ts)
                (THash "#" : TIdentifier "dumpghccpp" : ts) -> Right $ cppDumpState (map t_str ts)
                _ -> Left ("unexpected directive: " ++ (show toks))

{- | Comply with the CPP requirement to not combine adjacent tokens.
This will automatically insert a space in place of a comment, as
comments cannot occur within a token.
-}
combineToks :: [String] -> String
combineToks ss = intercalate " " ss

cppDefine :: [Token] -> Either String CppDirective
cppDefine [] = Left "error:empty #define directive"
cppDefine (TIdentifierLParen n : ts) = Right $ CppDefine (init n) args def
  where
    (args, def) = getArgs ts
cppDefine (TIdentifier n : ts) = Right $ CppDefine n Nothing ts
cppDefine (t : _) = Left $ "#define: expecting an identifier, got :" ++ show t

cppUndef :: [String] -> CppDirective
cppUndef ts = CppUndef (combineToks ts)

cppIf :: [String] -> CppDirective
cppIf ts = CppIf (combineToks ts)

cppIfdef :: [String] -> CppDirective
cppIfdef ts = CppIfdef (combineToks ts)

cppIfndef :: [String] -> CppDirective
cppIfndef ts = CppIfndef (combineToks ts)

cppElse :: [String] -> CppDirective
cppElse _ts = CppElse

cppElIf :: [String] -> CppDirective
cppElIf ts = CppElIf (combineToks ts)

cppEndif :: [String] -> CppDirective
cppEndif _ts = CppEndif

cppDumpState :: [String] -> CppDirective
cppDumpState _ts = CppDumpState

-- ---------------------------------------------------------------------

-- Crack out the arguments to a #define. This is of the form of
-- comma-separated identifiers between parens, where we have already
-- seen the opening paren.
getArgs :: [Token] -> (Maybe [String], [Token])
getArgs [] = (Nothing, [])
getArgs ts =
    case parseDefineArgs [] ts of
        Left err -> error err
        Right (args, rest) -> (Just (reverse args), rest)

parseDefineArgs ::
    [String] ->
    [Token] ->
    Either String ([String], [Token])
parseDefineArgs acc [] = Right (acc, [])
parseDefineArgs acc (TCloseParen _ : ts) = Right (acc, ts)
parseDefineArgs acc (TIdentifier n : TCloseParen _ : ts) = Right (n : acc, ts)
parseDefineArgs acc (TIdentifier n : TComma _ : ts) = parseDefineArgs (n : acc) ts
parseDefineArgs acc ts = Left $ "malformed macro args, expecting identifier followed by comma or ')', got:" ++ show ts ++ " after getting " ++ show (reverse acc)

-- ---------------------------------------------------------------------

-- TODO: give this a better name
cppLex :: Bool -> String -> Either String [Token]
cppLex sd s = case lexCppTokenStream s (init_state {scanning_directive = sd}) of
    Left err -> Left err
    Right (_inp, _st, toks) -> Right toks

-- Each comment is replaced with a space
-- https://timsong-cpp.github.io/cppwp/n4140/lex#phases-1.3
deComment :: Token -> Token
deComment (TComment _) = TComment " "
deComment t = t


-- ---------------------------------------------------------------------

doATest :: String -> Either String CppDirective
doATest str = parseDirective str

-- doATest str = parseDirectiveOld str

t0 :: Either String CppDirective
t0 = doATest "#define FOO(m1,m2,m) ((m1) <  1 || (m1) == 1 && (m2) <  7 || (m1) == 1 && (m2) == 7 && (m) <= 0)"

-- Right (CppDefine "FOO(m1,m2,m)" ["((m1)","<","1","||","(m1)","==","1","&&","(m2)","<","7","||","(m1)","==","1","&&","(m2)","==","7","&&","(m)","<=","0)"])

t1 :: Either String CppDirective
t1 = doATest "#if (m < 1)"

t2 :: Either String CppDirective
t2 = doATest "# if ((m1) <  1 || (m1) == 1 && (m2) <  7 || (m1) == 1 && (m2) == 7 && (m) <= 0)"

-- Right (CppIf
--        (Logic LogicalOr
--         (Logic LogicalOr
--          (Comp CmpLt (Var "m1") (IntVal 1))
--          (Logic LogicalAnd
--           (Comp CmpEqual (Var "m1") (IntVal 1))
--           (Comp CmpLt (Var "m2") (IntVal 7))))
--         (Logic LogicalAnd
--           (Logic LogicalAnd
--            (Comp CmpEqual (Var "m1") (IntVal 1))
--            (Comp CmpEqual (Var "m2") (IntVal 7)))
--           (Comp CmpLtE (Var "m") (IntVal 0)))))

t3 :: Either String CppDirective
t3 = parseDirective "# if FOO == 4"

t4 :: Either String [Token]
t4 = cppLex True "#define foo(X) X"
