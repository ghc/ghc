module ParsePP (
    parseDirective,
    cppLex,
    -- Reduce warnings so long
    t0,
    t1,
    t2,
    t3,
) where

import Data.List
import GHC.Parser.Errors.Ppr ()
import Lexer
import ParserM (Token (..), init_state)
import State

-- import Debug.Trace

-- =====================================================================
-- First parse to CPP tokens, using a C++-like language spec
-- https://gcc.gnu.org/onlinedocs/cpp/Tokenization.html

-- Parse a CPP directive, using tokens from the CPP lexer
parseDirective :: String -> Either String CppDirective
parseDirective s =
    case cppLex s of
        Left e -> Left e
        Right toks ->
            case map t_str toks of
                ("#" : "define" : ts) -> cppDefine ts
                ("#" : "include" : ts) -> Right $ cppInclude ts
                ("#" : "if" : ts) -> Right $ cppIf ts
                ("#" : "ifndef" : ts) -> Right $ cppIfndef ts
                ("#" : "ifdef" : ts) -> Right $ cppIfdef ts
                ("#" : "else" : ts) -> Right $ cppElse ts
                ("#" : "endif" : ts) -> Right $ cppEndif ts
                ("#" : "dumpghccpp" : ts) -> Right $ cppDumpState ts
                other -> Left ("unexpected directive: " ++ (combineToks other))

{- | Comply with the CPP requirement to not combine adjacent tokens.
This will automatically insert a space in place of a comment, as
comments cannot occur within a token.
-}
combineToks :: [String] -> String
combineToks ss = intercalate " " ss

cppDefine :: [String] -> Either String CppDirective
cppDefine [] = Left "error:empty #define directive"
cppDefine (n : ts) = Right $ CppDefine n (combineToks ts)

cppInclude :: [String] -> CppDirective
cppInclude ts = CppInclude (combineToks ts)

cppIf :: [String] -> CppDirective
cppIf ts = CppIf (combineToks ts)

cppIfdef :: [String] -> CppDirective
cppIfdef ts = CppIfdef (combineToks ts)

cppIfndef :: [String] -> CppDirective
cppIfndef ts = CppIfndef (combineToks ts)

cppElse :: [String] -> CppDirective
cppElse _ts = CppElse

cppEndif :: [String] -> CppDirective
cppEndif _ts = CppEndif

cppDumpState :: [String] -> CppDirective
cppDumpState _ts = CppDumpState

-- ---------------------------------------------------------------------

cppLex :: String -> Either String [Token]
cppLex s = case lexCppTokenStream s init_state of
    Left err -> Left err
    Right (_inp, _st, toks) -> Right toks

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
