module C where

{-
The standard mode for hsc2hs: generates a C file which is
compiled and run; the output of that program is the .hs file.
-}

import Data.Char                ( isSpace, intToDigit, ord )
import Data.List                ( intersperse )
import System.FilePath          ( splitFileName )

import HSCParser                ( SourcePos(..), Token(..) )
import Flags

outTemplateHeaderCProg :: FilePath -> String
outTemplateHeaderCProg template = "#include \"" ++ template ++ "\"\n"

outFlagHeaderCProg :: Flag -> String
outFlagHeaderCProg (Include  f)          = "#include "++f++"\n"
outFlagHeaderCProg (Define   n Nothing)  = "#define "++n++" 1\n"
outFlagHeaderCProg (Define   n (Just v)) = "#define "++n++" "++v++"\n"
outFlagHeaderCProg _                     = ""

outHeaderCProg :: (SourcePos, String, String) -> String
outHeaderCProg (pos, key, arg) = case key of
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
                "#define hsc_"++name++"("++dropWhile isSpace args++") " ++
                "hsc_printf ("++joinLines body++");\n"
    _ -> ""
   where
    joinLines = concat . intersperse " \\\n" . lines

outHeaderHs :: [Flag] -> Maybe String -> [(SourcePos, String, String)] -> String
outHeaderHs flags inH toks =
    case inH of
        Nothing -> concatMap outFlag flags++concatMap outSpecial toks
        Just _  -> ""
    where
    outFlag (Define  n Nothing)  = outOption ("-optc-D"++n)
    outFlag (Define  n (Just v)) = outOption ("-optc-D"++n++"="++v)
    outFlag _                    = ""
    outSpecial (pos, key, arg) = case key of
        "define" | goodForOptD arg -> outOption ("-optc-D"++toOptD arg)
                 | otherwise       -> ""
        _ | conditional key        -> outCLine pos++"#"++key++" "++arg++"\n"
        _                          -> ""
    goodForOptD arg = case arg of
        ""              -> True
        c:_ | isSpace c -> True
        '(':_           -> False
        _:s             -> goodForOptD s
    toOptD arg = case break isSpace arg of
        (name, "")      -> name
        (name, _:value) -> name++'=':dropWhile isSpace value
    outOption s =
        "    hsc_printf (\"{-# OPTIONS_GHC %s #-}\\n\", \""++
                  showCString s++"\");\n"

outTokenHs :: Bool                      -- ^ enable COLUMN pragmas?
           -> (ShowS, (Bool, Bool))
           -> Token
           -> (ShowS, (Bool, Bool))
outTokenHs enableCol (out, state) (Text pos txt) =
    (out . showString str, state')
    where
    (str, state') = outTextHs state pos txt outText outHsLine
                              (if enableCol then outHsColumn else const "")
    outText s = "    hsc_fputs (\""++showCString s++"\", hsc_stdout());\n"
outTokenHs _ (out, (rowSync, colSync)) (Special pos key arg) =
    (out . showString str, (rowSync && null str, colSync && null str))
    where
    str = case key of
        "include"           -> ""
        "define"            -> ""
        "undef"             -> ""
        "def"               -> ""
        _ | conditional key -> outCLine pos++"#"++key++" "++arg++"\n"
        "let"               -> ""
        "enum"              -> outCLine pos++outEnum arg
        _                   -> outCLine pos++"    hsc_"++key++" ("++arg++");\n"

-- | Output a 'Text' 'Token' literally, making use of the three given output
-- functions.  The state contains @(lineSync, colSync)@, which indicate
-- whether the line number and column number in the input are synchronized
-- with those of the output.
outTextHs :: (Bool, Bool)               -- ^ state @(lineSync, colSync)@
          -> SourcePos                  -- ^ original position of the token
          -> String                     -- ^ text of the token
          -> (String -> String)         -- ^ output text
          -> (SourcePos -> String)      -- ^ output LINE pragma
          -> (Int -> String)            -- ^ output COLUMN pragma
          -> (String, (Bool, Bool))
outTextHs (lineSync, colSync) pos@(SourcePos _ _ col) txt
          outText outLine outColumn =
    -- Ensure COLUMN pragmas are always inserted right before an identifier.
    -- They are never inserted in the middle of whitespace, as that could ruin
    -- the indentation.
    case break (== '\n') spaces of
        (_, "") ->
            case break (== '\n') rest of
                ("", _) ->
                    ( outText spaces
                    , (lineSync, colSync) )
                (_, "") ->
                    ( (outText spaces++
                       updateCol++
                       outText rest)
                    , (lineSync, True) )
                (firstRest, nl:restRest) ->
                    ( (outText spaces++
                       updateCol++
                       outText (firstRest++[nl])++
                       updateLine++
                       outText restRest)
                    , (True, True) )
        (firstSpaces, nl:restSpaces) ->
            ( (outText (firstSpaces++[nl])++
               updateLine++
               outText (restSpaces++rest))
            , (True, True) )
    where
    (spaces, rest) = span isSpace txt
    updateLine | lineSync   = ""
               | otherwise = outLine pos
    updateCol | colSync   = ""
              | otherwise = outColumn (col + length spaces)

parseEnum :: String -> Maybe (String,String,[(Maybe String,String)])
parseEnum arg =
    case break (== ',') arg of
        (_, [])        -> Nothing
        (t, _:afterT) -> case break (== ',') afterT of
            (f, afterF) -> let
                enums []    = []
                enums (_:s) = case break (== ',') s of
                    (enum, rest) -> let
                        this = case break (== '=') $ dropWhile isSpace enum of
                            (name, []) -> (Nothing, name)
                            (hsName, _:cName) -> (Just hsName, cName)
                        in this:enums rest
                in Just (t, f, enums afterF)

outEnum :: String -> String
outEnum arg = case parseEnum arg of
    Nothing -> ""
    Just (t,f,enums) ->
        flip concatMap enums $ \(maybeHsName, cName) ->
            case maybeHsName of
               Nothing ->
                    "    hsc_enum ("++t++", "++f++", " ++
                    "hsc_haskellize (\""++cName++"\"), "++
                    cName++");\n"
               Just hsName ->
                    "    hsc_enum ("++t++", "++f++", " ++
                    "hsc_printf (\"%s\", \""++hsName++"\"), "++
                    cName++");\n"

outFlagH :: Flag -> String
outFlagH (Include  f)          = "#include "++f++"\n"
outFlagH (Define   n Nothing)  = "#define "++n++" 1\n"
outFlagH (Define   n (Just v)) = "#define "++n++" "++v++"\n"
outFlagH _                     = ""

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
                "#ifdef __GNUC__\n" ++
                "extern\n" ++
                "#endif\n"++
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
            'i':'n':'l':'i':'n':'e':' ':arg' ->
                case span (\c -> c /= '{' && c /= '=') arg' of
                (header, body) ->
                    outCLine pos++
                    "#ifndef __GNUC__\n" ++
                    "extern inline\n" ++
                    "#endif\n"++
                    header++
                    "\n#ifndef __GNUC__\n" ++
                    ";\n" ++
                    "#else\n"++
                    body++
                    "\n#endif\n"
            _ -> outCLine pos++arg++"\n"
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

outCLine :: SourcePos -> String
outCLine (SourcePos name line _) =
    "#line "++show line++" \""++showCString (snd (splitFileName name))++"\"\n"

outHsLine :: SourcePos -> String
outHsLine (SourcePos name line _) =
    "    hsc_line ("++show (line + 1)++", \""++
    (showCString . showCString) name ++ "\");\n"

outHsColumn :: Int -> String
outHsColumn column =
    "    hsc_column ("++show column++");\n"

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
