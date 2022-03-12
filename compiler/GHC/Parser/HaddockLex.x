{
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module GHC.Parser.HaddockLex (lexHsDoc, lexStringLiteral) where

import GHC.Prelude

import GHC.Data.FastString
import GHC.Hs.Doc
import GHC.Parser.Lexer
import GHC.Parser.Annotation
import GHC.Types.SrcLoc
import GHC.Types.SourceText
import GHC.Data.StringBuffer
import qualified GHC.Data.Strict as Strict
import GHC.Types.Name.Reader
import GHC.Utils.Outputable
import GHC.Utils.Error
import GHC.Utils.Encoding
import GHC.Hs.Extension

import qualified GHC.Data.EnumSet as EnumSet

import Data.Maybe
import Data.Word

import Data.ByteString ( ByteString )
import qualified Data.ByteString as BS

import qualified GHC.LanguageExtensions as LangExt
}

-- -----------------------------------------------------------------------------
-- Alex "Character set macros"
-- Copied from GHC/Parser/Lexer.x

-- NB: The logic behind these definitions is also reflected in "GHC.Utils.Lexeme"
-- Any changes here should likely be reflected there.
$unispace    = \x05 -- Trick Alex into handling Unicode. See Note [Unicode in Alex].
$nl          = [\n\r\f]
$whitechar   = [$nl\v\ $unispace]
$white_no_nl = $whitechar # \n -- TODO #8424
$tab         = \t

$ascdigit  = 0-9
$unidigit  = \x03 -- Trick Alex into handling Unicode. See Note [Unicode in Alex].
$decdigit  = $ascdigit -- exactly $ascdigit, no more no less.
$digit     = [$ascdigit $unidigit]

$special   = [\(\)\,\;\[\]\`\{\}]
$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\:]
$unisymbol = \x04 -- Trick Alex into handling Unicode. See Note [Unicode in Alex].
$symbol    = [$ascsymbol $unisymbol] # [$special \_\"\']

$unilarge  = \x01 -- Trick Alex into handling Unicode. See Note [Unicode in Alex].
$asclarge  = [A-Z]
$large     = [$asclarge $unilarge]

$unismall  = \x02 -- Trick Alex into handling Unicode. See Note [Unicode in Alex].
$ascsmall  = [a-z]
$small     = [$ascsmall $unismall \_]

$uniidchar = \x07 -- Trick Alex into handling Unicode. See Note [Unicode in Alex].
$idchar    = [$small $large $digit $uniidchar \']

$unigraphic = \x06 -- Trick Alex into handling Unicode. See Note [Unicode in Alex].
$graphic   = [$small $large $symbol $digit $idchar $special $unigraphic \"\']

$alpha     = [$small $large]

-- The character sets marked "TODO" are mostly overly inclusive
-- and should be defined more precisely once alex has better
-- support for unicode character sets (see
-- https://github.com/simonmar/alex/issues/126).

@id = $alpha $idchar* \#* | $symbol+
@modname = $large $idchar*
@qualid = (@modname \.)* @id

:-
  \' @qualid \' | \` @qualid \` { getIdentifier 1 }
  \'\` @qualid \`\' | \'\( @qualid \)\' | \`\( @qualid \)\` { getIdentifier 2 }
  [. \n] ;

{
data AlexInput = AlexInput
  { alexInput_position     :: !RealSrcLoc
  , alexInput_string       :: !ByteString
  }

-- NB: As long as we don't use a left-context we don't need to track the
-- previous input character.
alexInputPrevChar :: AlexInput -> Word8
alexInputPrevChar = error "Left-context not supported"

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (AlexInput p s) = case utf8UnconsByteString s of
  Nothing -> Nothing
  Just (c,bs) -> Just (adjustChar c, AlexInput (advanceSrcLoc p c) bs)

alexScanTokens :: RealSrcLoc -> ByteString -> [(RealSrcSpan, ByteString)]
alexScanTokens start str0 = go (AlexInput start str0)
  where go inp@(AlexInput pos str) =
          case alexScan inp 0 of
            AlexSkip  inp' _ln          -> go inp'
            AlexToken inp'@(AlexInput _ str') _ act -> act pos (BS.length str - BS.length str') str : go inp'
            AlexEOF                     -> []
            AlexError (AlexInput p _) -> error $ "lexical error at " ++ show p

--------------------------------------------------------------------------------

-- | Extract identifier from Alex state.
getIdentifier :: Int -- ^ adornment length
              -> RealSrcLoc
              -> Int
                 -- ^ Token length
              -> ByteString
                 -- ^ The remaining input beginning with the found token
              -> (RealSrcSpan, ByteString)
getIdentifier !i !loc0 !len0 !s0 =
    (mkRealSrcSpan loc1 loc2, ident)
  where
    (adornment, s1) = BS.splitAt i s0
    ident = BS.take (len0 - 2*i) s1
    loc1 = advanceSrcLocBS loc0 adornment
    loc2 = advanceSrcLocBS loc1 ident

advanceSrcLocBS :: RealSrcLoc -> ByteString -> RealSrcLoc
advanceSrcLocBS !loc bs = case utf8UnconsByteString bs of
  Nothing -> loc
  Just (c, bs') -> advanceSrcLocBS (advanceSrcLoc loc c) bs'

-- | Lex 'StringLiteral' for warning messages
lexStringLiteral :: P (LocatedN RdrName) -- ^ A precise identifier parser
                 -> Located StringLiteral
                 -> Located (WithHsDocIdentifiers StringLiteral GhcPs)
lexStringLiteral identParser (L l sl@(StringLiteral _ fs _))
  = L l (WithHsDocIdentifiers sl idents)
  where
    bs = bytesFS fs

    idents = mapMaybe (uncurry (validateIdentWith identParser)) plausibleIdents

    plausibleIdents :: [(SrcSpan,ByteString)]
    plausibleIdents = case l of
      RealSrcSpan span _ -> [(RealSrcSpan span' Strict.Nothing, tok) | (span', tok) <- alexScanTokens (realSrcSpanStart span) bs]
      UnhelpfulSpan reason -> [(UnhelpfulSpan reason, tok) | (_, tok) <- alexScanTokens fakeLoc bs]

    fakeLoc = mkRealSrcLoc (mkFastString "") 0 0

-- | Lex identifiers from a docstring.
lexHsDoc :: P (LocatedN RdrName)      -- ^ A precise identifier parser
         -> HsDocString
         -> HsDoc GhcPs
lexHsDoc identParser doc =
    WithHsDocIdentifiers doc idents
  where
    docStrings = docStringChunks doc
    idents = concat [mapMaybe maybeDocIdentifier (plausibleIdents doc) | doc <- docStrings]

    maybeDocIdentifier :: (SrcSpan, ByteString) -> Maybe (Located RdrName)
    maybeDocIdentifier = uncurry (validateIdentWith identParser)

    plausibleIdents :: LHsDocStringChunk -> [(SrcSpan,ByteString)]
    plausibleIdents (L (RealSrcSpan span _) (HsDocStringChunk s))
      = [(RealSrcSpan span' Strict.Nothing, tok) | (span', tok) <- alexScanTokens (realSrcSpanStart span) s]
    plausibleIdents (L (UnhelpfulSpan reason) (HsDocStringChunk s))
      = [(UnhelpfulSpan reason, tok) | (_, tok) <- alexScanTokens fakeLoc s] -- preserve the original reason

    fakeLoc = mkRealSrcLoc (mkFastString "") 0 0

validateIdentWith :: P (LocatedN RdrName) -> SrcSpan -> ByteString -> Maybe (Located RdrName)
validateIdentWith identParser mloc str0 =
  let -- These ParserFlags should be as "inclusive" as possible, allowing
      -- identifiers defined with any language extension.
      pflags = mkParserOpts
                 (EnumSet.fromList [LangExt.MagicHash])
                 dopts
                 []
                 False False False False
      dopts = DiagOpts
        { diag_warning_flags = EnumSet.empty
          , diag_fatal_warning_flags = EnumSet.empty
          , diag_warn_is_error = False
          , diag_reverse_errors = False
          , diag_max_errors = Nothing
          , diag_ppr_ctx = defaultSDocContext
        }
      buffer = stringBufferFromByteString str0
      realSrcLc = case mloc of
        RealSrcSpan loc _ -> realSrcSpanStart loc
        UnhelpfulSpan _ -> mkRealSrcLoc (mkFastString "") 0 0
      pstate = initParserState pflags buffer realSrcLc
  in case unP identParser pstate of
    POk _ name -> Just $ case mloc of
       RealSrcSpan _ _ -> reLoc name
       UnhelpfulSpan _ -> L mloc (unLoc name) -- Preserve the original reason
    _ -> Nothing
}
