-----------------------------------------------------------------------------
-- $Id: Main.hs,v 1.18 2001/02/13 15:09:02 rrt Exp $
--
-- (originally "GlueHsc.hs" by Marcin 'Qrczak' Kowalczyk)
--
-- Program for converting .hsc files to .hs files, by converting the
-- file into a C program which is run to generate the Haskell source.
-- Certain items known only to the C compiler can then be used in
-- the Haskell module; for example #defined constants, byte offsets
-- within structures, etc.
--
-- See the documentation in the Users' Guide for more details.

import GetOpt
import System        (getProgName, getArgs, ExitCode(..), exitWith, exitFailure)
import KludgedSystem (system)
import Directory     (removeFile)
import Parsec
import ParsecError
import Monad         (liftM, liftM2, when)
import Char          (ord, intToDigit, isSpace, isAlpha, isAlphaNum, toUpper)
import List          (intersperse)

version :: String
version = "hsc2hs-0.64"

data Flag
    = Help
    | Version
    | Template String
    | Compiler String
    | Linker   String
    | CompFlag String
    | LinkFlag String
    | Include  String

include :: String -> Flag
include s@('\"':_) = Include s
include s@('<' :_) = Include s
include s          = Include ("\""++s++"\"")

options :: [OptDescr Flag]
options = [
    Option "t" ["template"] (ReqArg Template   "FILE") "template file",
    Option ""  ["cc"]       (ReqArg Compiler   "PROG") "C compiler to use",
    Option ""  ["ld"]       (ReqArg Linker     "PROG") "linker to use",
    Option ""  ["cflag"]    (ReqArg CompFlag   "FLAG") "flag to pass to the C compiler",
    Option "I" []           (ReqArg (CompFlag . ("-I"++))
                                               "DIR")  "passed to the C compiler",
    Option ""  ["lflag"]    (ReqArg LinkFlag   "FLAG") "flag to pass to the linker",
    Option ""  ["include"]  (ReqArg include    "FILE") "as if placed in the source",
    Option ""  ["help"]     (NoArg  Help)              "display this help and exit",
    Option ""  ["version"]  (NoArg  Version)           "output version information and exit"]

main :: IO ()
main = do
    prog <- getProgName
    let header = "Usage: "++prog++" [OPTIONS] INPUT.hsc [...]"
    args <- getArgs
    case getOpt Permute options args of
        (flags, _, _)
            | any isHelp    flags -> putStrLn (usageInfo header options)
            | any isVersion flags -> putStrLn version
            where
            isHelp    Help    = True; isHelp    _ = False
            isVersion Version = True; isVersion _ = False
        (_,     [],    [])   -> putStrLn (prog++": No input files")
        (flags, files, [])   -> mapM_ (processFile flags) files
        (_,     _,     errs) -> do
            mapM_ putStrLn errs
            putStrLn (usageInfo header options)
            exitFailure

processFile :: [Flag] -> String -> IO ()
processFile flags name = do
    parsed <- parseFromFile parser name
    case parsed of
        Left err   -> do print err; exitFailure
        Right toks -> output flags name toks

data Token
    = Text    SourcePos String
    | Special SourcePos String String

parser :: Parser [Token]
parser = many (text <|> special)

text :: Parser Token
text = do
    pos <- getPosition
    liftM (Text pos . concat) $ many1
        (   many1 (satisfy (\ch -> not (isAlpha ch || ch `elem` "\"#'-_{")))
        <|> (do a <- satisfy (\ch -> isAlpha ch || ch == '_')
                b <- many (satisfy (\ch -> isAlphaNum ch || ch == '_' || ch == '\''))
                return (a:b))
        <|> (do char '\"'; a <- hsString '\"'; char '\"'; return ("\""++a++"\""))
        <|> (do try (string "##"); return "#")
        <|> (do char '\''; a <- hsString '\''; char '\''; return ("\'"++a++"\'"))
        <|> (do try (string "--"); a <- many (satisfy (/= '\n')); return ("--"++a))
        <|> string "-"
        <|> (do try (string "{-#"); optional (try linePragma); a <- hsComment; return ("{-#"++a))
        <|> (do try (string "{-"); a <- hsComment; return ("{-"++a))
        <|> string "{"
        <?> "Haskell source")

linePragma :: Parser ()
linePragma = do
    state <- getState
    spaces
    string "LINE"
    skipMany1 space
    line <- many1 digit
    skipMany1 space
    char '\"'
    file <- many (satisfy (/= '\"'))
    char '\"'
    spaces
    string "#-}"
    setState state
    setPosition (newPos file (read line - 1) 1)

hsComment :: Parser String
hsComment =
    (   (do a <- many1 (noneOf "-{"); b <- hsComment; return (a++b))
    <|> try (string "-}")
    <|> (do char '-'; b <- hsComment; return ('-':b))
    <|> (do try (string "{-"); a <- hsComment; b <- hsComment; return ("{-"++a++b))
    <|> (do char '{'; b <- hsComment; return ('{':b))
    <?> "Haskell comment")

hsString :: Char -> Parser String
hsString quote =
    liftM concat $ many
    (   many1 (noneOf (quote:"\n\\"))
    <|> (do char '\\'; a <- escape; return ('\\':a))
    <?> "Haskell character or string")
    where
    escape = (do a <- many1 (satisfy isSpace); char '\\'; return (a++"\\"))
         <|> (do a <- anyChar; return [a])

special :: Parser Token
special = do
    pos <- getPosition
    char '#'
    skipMany (oneOf " \t")
    key <- liftM2 (:) (letter <|> char '_') (many (alphaNum <|> char '_'))
        <?> "hsc directive"
    skipMany (oneOf " \t")
    arg <- argument pzero
    return (Special pos key arg)

argument :: Parser String -> Parser String
argument eol =
    liftM concat $ many
    (   many1 (noneOf "\n\"\'()/[\\]{}")
    <|> eol
    <|> (do char '\"'; a <- cString '\"'; char '\"'; return ("\""++a++"\""))
    <|> (do char '\''; a <- cString '\''; char '\''; return ("\'"++a++"\'"))
    <|> (do char '('; a <- nested; char ')'; return ("("++a++")"))
    <|> (do try (string "/*"); cComment; return " ")
    <|> (do try (string "//"); skipMany (satisfy (/= '\n')); return " ")
    <|> string "/"
    <|> (do char '['; a <- nested; char ']'; return ("["++a++"]"))
    <|> (do char '\\'; a <- anyChar; return $ if a == '\n' then [] else ['\\',a])
    <|> (do char '{'; a <- nested; char '}'; return ("{"++a++"}"))
    <?> "C expression")
    where nested = argument (string "\n")

cComment :: Parser ()
cComment =
    (   (do skipMany1 (noneOf "*"); cComment)
    <|> (do try (string "*/"); return ())
    <|> (do char '*'; cComment)
    <?> "C comment")

cString :: Char -> Parser String
cString quote =
    liftM concat $ many
    (   many1 (noneOf (quote:"\n\\"))
    <|> (do char '\\'; a <- anyChar; return ['\\',a])
    <?> "C character or string")

output :: [Flag] -> String -> [Token] -> IO ()
output flags name toks = let
    baseName = case reverse name of
        'c':base -> reverse base
        _        -> name++".hs"
    cProgName = baseName++"c_make_hs.c"
    oProgName = baseName++"c_make_hs.o"
    progName  = baseName++"c_make_hs"
    outHsName = baseName
    outHName  = baseName++".h"
    outCName  = baseName++".c"
    
    execProgName = case progName of
        '/':_ -> progName
        _     -> "./"++progName
    
    specials = [(pos, key, arg) | Special pos key arg <- toks]
    
    needsC = any (\(_, key, _) -> key == "def") specials
    needsH = needsC
    
    includeGuard = map fixChar outHName
        where
        fixChar c | isAlphaNum c = toUpper c
                  | otherwise    = '_'
    
    in do
    
    compiler <- case [c | Compiler c <- flags] of
        []  -> return "ghc"
        [c] -> return c
        _   -> onlyOne "compiler"
    linker <- case [l | Linker l <- flags] of
#ifndef mingw32_TARGET_OS
        []  -> return "gcc"
#else
        []  -> return "gcc -mno-cygwin"
#endif
        [l] -> return l
        _   -> onlyOne "linker"
        
    writeFile cProgName $
        concat ["#include \""++t++"\"\n" | Template t <- flags]++
        concat ["#include "++f++"\n"     | Include  f <- flags]++
        outHeaderCProg specials++
        "\nint main (void)\n{\n"++
        outHeaderHs flags (if needsH then Just outHName else Nothing) specials++
        outHsLine (newPos name 0 1)++
        concatMap outTokenHs toks++
        "    return 0;\n}\n"
    
    compilerStatus <- system $
        compiler++
        " -c"++
        concat [" "++f | CompFlag f <- flags]++
        " "++cProgName++
        " -o "++oProgName
    case compilerStatus of
        e@(ExitFailure _) -> exitWith e
        _                 -> return ()
    removeFile cProgName
    
    linkerStatus <- system $
        linker++
        concat [" "++f | LinkFlag f <- flags]++
        " "++oProgName++
        " -o "++progName
    case linkerStatus of
        e@(ExitFailure _) -> exitWith e
        _                 -> return ()
    removeFile oProgName
    
    system (execProgName++" >"++outHsName)
    removeFile progName
    
    when needsH $ writeFile outHName $
        "#ifndef "++includeGuard++"\n\
        \#define "++includeGuard++"\n\
        \#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ < 409\n\
        \#include <Rts.h>\n\
        \#endif\n\
        \#include <HsFFI.h>\n\
        \#if __NHC__\n\
        \#undef HsChar\n\
        \#define HsChar int\n\
        \#endif\n"++
        concat ["#include "++n++"\n" | Include n <- flags]++
        concatMap outTokenH specials++
        "#endif\n"
    
    when needsC $ writeFile outCName $
        "#include \""++outHName++"\"\n"++
        concatMap outTokenC specials

onlyOne :: String -> IO a
onlyOne what = do
    putStrLn ("Only one "++what++" may be specified")
    exitFailure

outHeaderCProg :: [(SourcePos, String, String)] -> String
outHeaderCProg =
    concatMap $ \(pos, key, arg) -> case key of
        "include"           -> outCLine pos++"#include "++arg++"\n"
        "define"            -> outCLine pos++"#define "++arg++"\n"
        "undef"             -> outCLine pos++"#undef "++arg++"\n"
        "def"               -> case arg of
            's':'t':'r':'u':'c':'t':' ':_ -> outCLine pos++arg++"\n"
            't':'y':'p':'e':'d':'e':'f':' ':_ -> outCLine pos++arg++"\n"
            _ -> ""
        _ | conditional key -> outCLine pos++"#"++key++" "++arg++"\n"
        "let"               -> case break (== '=') arg of
            (_,      "")     -> ""
            (header, _:body) -> case break isSpace header of
                (name, args) ->
                    outCLine pos++
                    "#define hsc_"++name++"("++dropWhile isSpace args++") \
                    \printf ("++joinLines body++");\n"
        _ -> ""
    where
    joinLines = concat . intersperse " \\\n" . lines

outHeaderHs :: [Flag] -> Maybe String -> [(SourcePos, String, String)] -> String
outHeaderHs flags inH toks =
    "#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ < 409\n\
    \    printf (\"{-# OPTIONS -optc-D__GLASGOW_HASKELL__=%d #-}\\n\", \
    \__GLASGOW_HASKELL__);\n\
    \#endif\n"++
    includeH++
    concatMap outSpecial toks
    where
    outSpecial (pos, key, arg) = case key of
        "include" -> case inH of
            Nothing -> outOption ("-#include "++arg)
            Just _  -> ""
        "define" -> case inH of
            Nothing | goodForOptD arg -> outOption ("-optc-D"++toOptD arg)
            _ -> ""
        _ | conditional key -> outCLine pos++"#"++key++" "++arg++"\n"
        _ -> ""
    goodForOptD arg = case arg of
        ""              -> True
        c:_ | isSpace c -> True
        '(':_           -> False
        _:s             -> goodForOptD s
    toOptD arg = case break isSpace arg of
        (name, "")      -> name
        (name, _:value) -> name++'=':dropWhile isSpace value
    includeH = concat [
        outOption ("-#include "++name++"")
        | name <- case inH of
            Nothing   -> [name | Include name <- flags]
            Just name -> ["\""++name++"\""]]
    outOption s = "    printf (\"{-# OPTIONS %s #-}\\n\", \""++
                  showCString s++"\");\n"

outTokenHs :: Token -> String
outTokenHs (Text pos text) =
    case break (== '\n') text of
        (all, [])       -> outText all
        (first, _:rest) ->
            outText (first++"\n")++
            outHsLine pos++
            outText rest
    where
    outText s = "    fputs (\""++showCString s++"\", stdout);\n"
outTokenHs (Special pos key arg) =
    case key of
        "include"           -> ""
        "define"            -> ""
        "undef"             -> ""
        "def"               -> ""
        _ | conditional key -> outCLine pos++"#"++key++" "++arg++"\n"
        "let"               -> ""
        "enum"              -> outCLine pos++outEnum arg
        _                   -> outCLine pos++"    hsc_"++key++" ("++arg++");\n"

outEnum :: String -> String
outEnum arg =
    case break (== ',') arg of
        (_, [])        -> ""
        (t, _:afterT) -> case break (== ',') afterT of
            (f, afterF) -> let
                enums []    = ""
                enums (_:s) = case break (== ',') s of
                    (enum, rest) -> let
                        this = case break (== '=') $ dropWhile isSpace enum of
                            (name, []) ->
                                "    hsc_enum ("++t++", "++f++", \
                                \hsc_haskellize (\""++name++"\"), "++
                                name++");\n"
                            (hsName, _:cName) ->
                                "    hsc_enum ("++t++", "++f++", \
                                \printf (\"%s\", \""++hsName++"\"), "++
                                cName++");\n"
                        in this++enums rest
                in enums afterF

outTokenH :: (SourcePos, String, String) -> String
outTokenH (pos, key, arg) =
    case key of
        "include" -> outCLine pos++"#include "++arg++"\n"
        "define"  -> outCLine pos++"#define " ++arg++"\n"
        "undef"   -> outCLine pos++"#undef "  ++arg++"\n"
        "def"     -> outCLine pos++case arg of
            's':'t':'r':'u':'c':'t':' ':_ -> arg++"\n"
            't':'y':'p':'e':'d':'e':'f':' ':_ -> arg++"\n"
            'i':'n':'l':'i':'n':'e':' ':_ ->
                "#ifdef __GNUC__\n\
                \extern\n\
                \#endif\n"++
                arg++"\n"
            _ -> "extern "++header++";\n"
            where header = takeWhile (\c -> c /= '{' && c /= '=') arg
        _ | conditional key -> outCLine pos++"#"++key++" "++arg++"\n"
        _ -> ""

outTokenC :: (SourcePos, String, String) -> String
outTokenC (pos, key, arg) =
    case key of
        "def" -> case arg of
            's':'t':'r':'u':'c':'t':' ':_ -> ""
            't':'y':'p':'e':'d':'e':'f':' ':_ -> ""
            'i':'n':'l':'i':'n':'e':' ':_ ->
                outCLine pos++
                "#ifndef __GNUC__\n\
                \extern\n\
                \#endif\n"++
                header++
                "\n#ifndef __GNUC__\n\
                \;\n\
                \#else\n"++
                body++
                "\n#endif\n"
            _ -> outCLine pos++arg++"\n"
            where (header, body) = span (\c -> c /= '{' && c /= '=') arg
        _ | conditional key -> outCLine pos++"#"++key++" "++arg++"\n"
        _ -> ""

conditional :: String -> Bool
conditional "if"      = True
conditional "ifdef"   = True
conditional "ifndef"  = True
conditional "elif"    = True
conditional "else"    = True
conditional "endif"   = True
conditional "error"   = True
conditional "warning" = True
conditional _         = False

sourceFileName :: SourcePos -> String
sourceFileName pos = fileName (sourceName pos)
    where
    fileName s = case break (== '/') s of
        (name, [])      -> name
        (_,     _:rest) -> fileName rest

outCLine :: SourcePos -> String
outCLine pos =
    "# "++show (sourceLine pos)++
    " \""++showCString (sourceFileName pos)++"\"\n"

outHsLine :: SourcePos -> String
outHsLine pos =
    "    hsc_line ("++
    show (sourceLine pos + 1)++", \""++
    showCString (sourceFileName pos)++"\");\n"

showCString :: String -> String
showCString = concatMap showCChar
    where
    showCChar '\"' = "\\\""
    showCChar '\'' = "\\\'"
    showCChar '?'  = "\\?"
    showCChar '\\' = "\\\\"
    showCChar c | c >= ' ' && c <= '~' = [c]
    showCChar '\a' = "\\a"
    showCChar '\b' = "\\b"
    showCChar '\f' = "\\f"
    showCChar '\n' = "\\n\"\n           \""
    showCChar '\r' = "\\r"
    showCChar '\t' = "\\t"
    showCChar '\v' = "\\v"
    showCChar c    = ['\\',
                      intToDigit (ord c `quot` 64),
                      intToDigit (ord c `quot` 8 `mod` 8),
                      intToDigit (ord c          `mod` 8)]
