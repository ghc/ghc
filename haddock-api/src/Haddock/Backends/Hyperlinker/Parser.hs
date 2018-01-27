module Haddock.Backends.Hyperlinker.Parser (parse) where

import Data.Either         ( isRight, isLeft )
import Data.List           ( foldl', isPrefixOf, isSuffixOf )
import Data.Maybe          ( maybeToList )

import GHC                 ( DynFlags, addSourceToTokens )
import SrcLoc
import FastString          ( mkFastString )
import StringBuffer        ( stringToStringBuffer )
import Lexer               ( Token(..) )
import qualified Lexer as L

import Haddock.Backends.Hyperlinker.Types as T


-- | Turn source code string into a stream of more descriptive tokens.
--
-- Result should retain original file layout (including comments, whitespace,
-- etc.), i.e. the following "law" should hold:
--
-- prop> concat . map tkValue . parse = id
--
-- (In reality, this only holds for input not containing '\r', '\t', '\f', '\v',
-- characters, since GHC transforms those into ' ' and '\n')
parse :: DynFlags -> FilePath -> String -> [T.Token]
parse dflags fp s = ghcToks (processCPP dflags fp s)


-- | Parse the source into tokens using the GHC lexer.
--
--   * CPP lines are removed and reinserted as line-comments
--   * top-level file pragmas are parsed as block comments (see the
--     'ITblockComment' case of 'classify' for more details)
--
processCPP :: DynFlags    -- ^ GHC's flags
           -> FilePath    -- ^ source file name (for position information)
           -> String      -- ^ source file contents
           -> [(Located L.Token, String)]
processCPP dflags fpath s = addSrc . go start . splitCPP $ s
  where
    start = mkRealSrcLoc (mkFastString fpath) 1 1
    addSrc = addSourceToTokens start (stringToStringBuffer s)

    -- Transform a list of Haskell/CPP lines into a list of tokens
    go :: RealSrcLoc -> [Either String String] -> [Located L.Token]
    go _   [] = []
    go pos ls =
      let (hLinesRight,  ls')  = span isRight ls
          (cppLinesLeft, rest) = span isLeft ls'

          hSrc   = concat [ hLine   | Right hLine  <- hLinesRight  ]
          cppSrc = concat [ cppLine | Left cppLine <- cppLinesLeft ]

      in case L.lexTokenStream (stringToStringBuffer hSrc) pos dflags of

           -- Stuff that fails to lex gets turned into comments
           L.PFailed _ _ss _msg ->
             let (src_pos, failed) = mkToken ITunknown pos hSrc
                 (new_pos, cpp)    = mkToken ITlineComment src_pos cppSrc
             in failed : cpp : go new_pos rest

           -- Successfully lexed
           L.POk ss toks ->
             let (new_pos, cpp) = mkToken ITlineComment (L.loc ss) cppSrc
             in toks ++ [cpp] ++ go new_pos rest

    -- Manually make a token from a 'String', advancing the cursor position
    mkToken tok start' str =
      let end = foldl' advanceSrcLoc start' str
      in (end, L (RealSrcSpan $ mkRealSrcSpan start' end) (tok str))


-- | Split apart the initial file into Haskell source lines ('Left' entries) and
-- CPP lines ('Right' entries).
--
-- All characters in the input are present in the output:
--
-- prop> concat . map (either id id) . splitCPP = id
splitCPP :: String -> [Either String String]
splitCPP "" = []
splitCPP s | isCPPline s = Left l : splitCPP rest
           | otherwise =  Right l : splitCPP rest
  where
    ~(l, rest) = spanToNewline 0 s


-- | Heuristic to decide if a line is going to be a CPP line. This should be a
-- cheap operation since it is going to be run on every line being processed.
--
-- Right now it just checks if the first non-whitespace character in the first
-- five characters of the line is a '#':
--
-- >>> isCPPline "#define FOO 1"
-- True
--
-- >>> isCPPline "\t\t  #ifdef GHC"
-- True
--
-- >>> isCPPline "       #endif"
-- False
--
isCPPline :: String -> Bool
isCPPline = isPrefixOf "#" . dropWhile (`elem` " \t") . take 5


-- | Split a "line" off the front of a string, supporting newline escapes.
--
-- By "line", we understand: the shortest substring ending in a '\n' that is not
--
--   1. immediately preceded by a '\\'
--   2. not inside some (possibly nested) block comment
--
-- All characters in the input are present in the output:
--
-- prop> curry (++) . spanToNewLine 0 = id
spanToNewline :: Int                 -- ^ open '{-'
              -> String              -- ^ input
              -> (String, String)
spanToNewline _ [] = ([], [])
spanToNewline n ('\n':str) | n <= 0 = ("\n", str)
spanToNewline n ('\\':'\n':str) =
    let (str', rest) = spanToNewline n str
    in ('\\':'\n':str', rest)
spanToNewline n ('{':'-':str) =
    let (str', rest) = spanToNewline (n+1) str
    in ('{':'-':str', rest)
spanToNewline n ('-':'}':str) =
    let (str', rest) = spanToNewline (n-1) str
    in ('-':'}':str', rest)
spanToNewline n (c:str) =
    let (str', rest) = spanToNewline n str
    in (c:str', rest)


-- | Turn a list of GHC's 'L.Token' (and their source 'String') into a list of
-- Haddock's 'T.Token'.
ghcToks :: [(Located L.Token, String)] -> [T.Token]
ghcToks = reverse . (\(_,ts,_) -> ts) . foldl' go (start, [], False)
  where
    start = mkRealSrcLoc (mkFastString "lexing") 1 1

    go :: (RealSrcLoc, [T.Token], Bool)
       -- ^ current position, tokens accumulated, currently in pragma (or not)
       
       -> (Located L.Token, String)
       -- ^ next token, its content
       
       -> (RealSrcLoc, [T.Token], Bool)
       -- ^ new position, new tokens accumulated, currently in pragma (or not)

    go (pos, toks, in_prag) (L l tok, raw) =
        ( next_pos
        , classifiedTok ++ maybeToList white ++ toks
        , inPragma in_prag tok
        )
       where
         (next_pos, white) = mkWhitespace pos l
         
         classifiedTok = [ Token (classify' tok) raw rss
                         | RealSrcSpan rss <- [l]
                         , not (null raw)
                         ]
         
         classify' | in_prag = const TkPragma
                   | otherwise = classify


-- | Find the correct amount of whitespace between tokens.
mkWhitespace :: RealSrcLoc -> SrcSpan -> (RealSrcLoc, Maybe T.Token)
mkWhitespace prev spn =
  case spn of
    UnhelpfulSpan _ -> (prev,Nothing)
    RealSrcSpan s | null wsstring -> (end, Nothing)
                  | otherwise -> (end, Just (Token TkSpace wsstring wsspan))
      where
        start = realSrcSpanStart s
        end = realSrcSpanEnd s
        wsspan = mkRealSrcSpan prev start
        nls = srcLocLine start - srcLocLine prev
        spaces = if nls == 0 then srcLocCol start - srcLocCol prev
                             else srcLocCol start - 1
        wsstring = replicate nls '\n' ++ replicate spaces ' '


-- | Classify given tokens as appropriate Haskell token type.
classify :: L.Token -> TokenType
classify tok =
  case tok of
    ITas                   -> TkKeyword
    ITcase                 -> TkKeyword
    ITclass                -> TkKeyword
    ITdata                 -> TkKeyword
    ITdefault              -> TkKeyword
    ITderiving             -> TkKeyword
    ITdo                   -> TkKeyword
    ITelse                 -> TkKeyword
    IThiding               -> TkKeyword
    ITforeign              -> TkKeyword
    ITif                   -> TkKeyword
    ITimport               -> TkKeyword
    ITin                   -> TkKeyword
    ITinfix                -> TkKeyword
    ITinfixl               -> TkKeyword
    ITinfixr               -> TkKeyword
    ITinstance             -> TkKeyword
    ITlet                  -> TkKeyword
    ITmodule               -> TkKeyword
    ITnewtype              -> TkKeyword
    ITof                   -> TkKeyword
    ITqualified            -> TkKeyword
    ITthen                 -> TkKeyword
    ITtype                 -> TkKeyword
    ITwhere                -> TkKeyword

    ITforall            {} -> TkKeyword
    ITexport               -> TkKeyword
    ITlabel                -> TkKeyword
    ITdynamic              -> TkKeyword
    ITsafe                 -> TkKeyword
    ITinterruptible        -> TkKeyword
    ITunsafe               -> TkKeyword
    ITstdcallconv          -> TkKeyword
    ITccallconv            -> TkKeyword
    ITcapiconv             -> TkKeyword
    ITprimcallconv         -> TkKeyword
    ITjavascriptcallconv   -> TkKeyword
    ITmdo                  -> TkKeyword
    ITfamily               -> TkKeyword
    ITrole                 -> TkKeyword
    ITgroup                -> TkKeyword
    ITby                   -> TkKeyword
    ITusing                -> TkKeyword
    ITpattern              -> TkKeyword
    ITstatic               -> TkKeyword
    ITstock                -> TkKeyword
    ITanyclass             -> TkKeyword

    ITunit                 -> TkKeyword
    ITsignature            -> TkKeyword
    ITdependency           -> TkKeyword
    ITrequires             -> TkKeyword

    ITinline_prag       {} -> TkPragma
    ITspec_prag         {} -> TkPragma
    ITspec_inline_prag  {} -> TkPragma
    ITsource_prag       {} -> TkPragma
    ITrules_prag        {} -> TkPragma
    ITwarning_prag      {} -> TkPragma
    ITdeprecated_prag   {} -> TkPragma
    ITline_prag         {} -> TkPragma
    ITcolumn_prag       {} -> TkPragma
    ITscc_prag          {} -> TkPragma
    ITgenerated_prag    {} -> TkPragma
    ITcore_prag         {} -> TkPragma
    ITunpack_prag       {} -> TkPragma
    ITnounpack_prag     {} -> TkPragma
    ITann_prag          {} -> TkPragma
    ITcomplete_prag     {} -> TkPragma
    ITclose_prag           -> TkPragma
    IToptions_prag      {} -> TkPragma
    ITinclude_prag      {} -> TkPragma
    ITlanguage_prag        -> TkPragma
    ITvect_prag         {} -> TkPragma
    ITvect_scalar_prag  {} -> TkPragma
    ITnovect_prag       {} -> TkPragma
    ITminimal_prag      {} -> TkPragma
    IToverlappable_prag {} -> TkPragma
    IToverlapping_prag  {} -> TkPragma
    IToverlaps_prag     {} -> TkPragma
    ITincoherent_prag   {} -> TkPragma
    ITctype             {} -> TkPragma

    ITdotdot               -> TkGlyph
    ITcolon                -> TkGlyph
    ITdcolon            {} -> TkGlyph
    ITequal                -> TkGlyph
    ITlam                  -> TkGlyph
    ITlcase                -> TkGlyph
    ITvbar                 -> TkGlyph
    ITlarrow            {} -> TkGlyph
    ITrarrow            {} -> TkGlyph
    ITat                   -> TkGlyph
    ITtilde                -> TkGlyph
    ITtildehsh             -> TkGlyph
    ITdarrow            {} -> TkGlyph
    ITminus                -> TkGlyph
    ITbang                 -> TkGlyph
    ITdot                  -> TkOperator
    ITtypeApp              -> TkGlyph

    ITbiglam               -> TkGlyph

    ITocurly               -> TkSpecial
    ITccurly               -> TkSpecial
    ITvocurly              -> TkSpecial
    ITvccurly              -> TkSpecial
    ITobrack               -> TkSpecial
    ITopabrack             -> TkSpecial
    ITcpabrack             -> TkSpecial
    ITcbrack               -> TkSpecial
    IToparen               -> TkSpecial
    ITcparen               -> TkSpecial
    IToubxparen            -> TkSpecial
    ITcubxparen            -> TkSpecial
    ITsemi                 -> TkSpecial
    ITcomma                -> TkSpecial
    ITunderscore           -> TkIdentifier
    ITbackquote            -> TkSpecial
    ITsimpleQuote          -> TkSpecial

    ITvarid             {} -> TkIdentifier
    ITconid             {} -> TkIdentifier
    ITvarsym            {} -> TkOperator
    ITconsym            {} -> TkOperator
    ITqvarid            {} -> TkIdentifier
    ITqconid            {} -> TkIdentifier
    ITqvarsym           {} -> TkOperator
    ITqconsym           {} -> TkOperator

    ITdupipvarid        {} -> TkUnknown
    ITlabelvarid        {} -> TkUnknown

    ITchar              {} -> TkChar
    ITstring            {} -> TkString
    ITinteger           {} -> TkNumber
    ITrational          {} -> TkNumber

    ITprimchar          {} -> TkChar
    ITprimstring        {} -> TkString
    ITprimint           {} -> TkNumber
    ITprimword          {} -> TkNumber
    ITprimfloat         {} -> TkNumber
    ITprimdouble        {} -> TkNumber

    ITopenExpQuote      {} -> TkSpecial
    ITopenPatQuote         -> TkSpecial
    ITopenDecQuote         -> TkSpecial
    ITopenTypQuote         -> TkSpecial
    ITcloseQuote        {} -> TkSpecial
    ITopenTExpQuote     {} -> TkSpecial
    ITcloseTExpQuote       -> TkSpecial
    ITidEscape          {} -> TkUnknown
    ITparenEscape          -> TkSpecial
    ITidTyEscape        {} -> TkUnknown
    ITparenTyEscape        -> TkSpecial
    ITtyQuote              -> TkSpecial
    ITquasiQuote        {} -> TkUnknown
    ITqQuasiQuote       {} -> TkUnknown

    ITproc                 -> TkKeyword
    ITrec                  -> TkKeyword
    IToparenbar         {} -> TkGlyph
    ITcparenbar         {} -> TkGlyph
    ITlarrowtail        {} -> TkGlyph
    ITrarrowtail        {} -> TkGlyph
    ITLarrowtail        {} -> TkGlyph
    ITRarrowtail        {} -> TkGlyph

    ITunknown           {} -> TkUnknown
    ITeof                  -> TkUnknown

    -- Line comments are only supposed to start with '--'. Starting with '#'
    -- means that this was probably a CPP.
    ITlineComment s
      | isCPPline s        -> TkCpp
      | otherwise          -> TkComment

    ITdocCommentNext    {} -> TkComment
    ITdocCommentPrev    {} -> TkComment
    ITdocCommentNamed   {} -> TkComment
    ITdocSection        {} -> TkComment
    ITdocOptions        {} -> TkComment

    -- The lexer considers top-level pragmas as comments (see `pragState` in
    -- the GHC lexer for more), so we have to manually reverse this. The
    -- following is a hammer: it smashes _all_ pragma-like block comments into
    -- pragmas.
    ITblockComment c
      | isPrefixOf "{-#" c
      , isSuffixOf "#-}" c -> TkPragma
      | otherwise          -> TkComment

-- | Classify given tokens as beginning pragmas (or not).
inPragma :: Bool     -- ^ currently in pragma
         -> L.Token  -- ^ current token
         -> Bool     -- ^ new information about whether we are in a pragma
inPragma _ ITclose_prag = False
inPragma True _ = True
inPragma False tok =
  case tok of
    ITinline_prag       {} -> True
    ITspec_prag         {} -> True
    ITspec_inline_prag  {} -> True
    ITsource_prag       {} -> True
    ITrules_prag        {} -> True
    ITwarning_prag      {} -> True
    ITdeprecated_prag   {} -> True
    ITline_prag         {} -> True
    ITcolumn_prag       {} -> True
    ITscc_prag          {} -> True
    ITgenerated_prag    {} -> True
    ITcore_prag         {} -> True
    ITunpack_prag       {} -> True
    ITnounpack_prag     {} -> True
    ITann_prag          {} -> True
    ITcomplete_prag     {} -> True
    IToptions_prag      {} -> True
    ITinclude_prag      {} -> True
    ITlanguage_prag        -> True
    ITvect_prag         {} -> True
    ITvect_scalar_prag  {} -> True
    ITnovect_prag       {} -> True
    ITminimal_prag      {} -> True
    IToverlappable_prag {} -> True
    IToverlapping_prag  {} -> True
    IToverlaps_prag     {} -> True
    ITincoherent_prag   {} -> True
    ITctype             {} -> True

    _                      -> False

