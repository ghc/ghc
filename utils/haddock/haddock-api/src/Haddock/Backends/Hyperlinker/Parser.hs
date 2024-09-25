{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haddock.Backends.Hyperlinker.Parser (parse) where

import Control.Applicative (Alternative (..))
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import qualified Data.ByteString as BS
import Data.List (isPrefixOf, isSuffixOf)
import GHC.Data.Bag (bagToList)
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer (StringBuffer, atEnd)
import GHC.Parser.Errors.Ppr ()
import GHC.Parser.Lexer as Lexer
  ( P (..)
  , PState (..)
  , ParseResult (..)
  , ParserOpts
  , Token (..)
  , getPsErrorMessages
  , initParserState
  , lexer
  )
import qualified GHC.Types.Error as E
import GHC.Types.SourceText
import GHC.Types.SrcLoc
import GHC.Utils.Error (pprLocMsgEnvelopeDefault)
import GHC.Utils.Outputable (SDocContext, text, ($$))
import qualified GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic (panic)

import Haddock.Backends.Hyperlinker.Types as T
import Haddock.GhcUtils

-- | Turn source code string into a stream of more descriptive tokens.
--
-- Result should retain original file layout (including comments,
-- whitespace, and CPP).
parse
  :: ParserOpts
  -> SDocContext
  -> FilePath
  -- ^ Path to the source of this module
  -> BS.ByteString
  -- ^ Raw UTF-8 encoded source of this module
  -> [T.Token]
parse parserOpts sDocContext fpath bs = case unP (go False []) initState of
  POk _ toks -> reverse toks
  PFailed pst ->
    let err : _ = bagToList (E.getMessages $ getPsErrorMessages pst)
     in panic $
          Outputable.renderWithContext sDocContext $
            text "Hyperlinker parse error:" $$ pprLocMsgEnvelopeDefault err
  where
    initState = initParserState parserOpts buf start
    buf = stringBufferFromByteString bs
    start = mkRealSrcLoc (mkFastString fpath) 1 1
    go
      :: Bool
      -- \^ are we currently in a pragma?
      -> [T.Token]
      -- \^ tokens accumulated so far (in reverse)
      -> P [T.Token]
    go inPrag toks = do
      (b, _) <- getInput
      if not (atEnd b)
        then do
          mtok <- runMaybeT (parseCppLine <|> parsePlainTok inPrag)
          (newToks, inPrag') <- case mtok of
            Nothing -> unknownLine
            Just a -> pure a
          go inPrag' (newToks ++ toks)
        else pure toks

    -- \| Like 'Lexer.lexer', but slower, with a better API, and filtering out empty tokens
    wrappedLexer :: P (RealLocated Lexer.Token)
    wrappedLexer = Lexer.lexer False andThen
      where
        andThen (L (RealSrcSpan s _) t)
          | srcSpanStartLine s /= srcSpanEndLine s
              || srcSpanStartCol s /= srcSpanEndCol s =
              pure (L s t)
        andThen (L (RealSrcSpan s _) ITeof) = pure (L s ITeof)
        andThen _ = wrappedLexer

    -- \| Try to parse a CPP line (can fail)
    parseCppLine :: MaybeT P ([T.Token], Bool)
    parseCppLine = MaybeT $ do
      (b, l) <- getInput
      case tryCppLine l b of
        Just (cppBStr, l', b') ->
          let cppTok =
                T.Token
                  { tkType = TkCpp
                  , tkValue = cppBStr
                  , tkSpan = mkRealSrcSpan l l'
                  }
           in setInput (b', l') *> pure (Just ([cppTok], False))
        _ -> return Nothing

    -- \| Try to parse a regular old token (can fail)
    parsePlainTok :: Bool -> MaybeT P ([T.Token], Bool) -- return list is only ever 0-2 elements
    parsePlainTok inPrag = do
      (bInit, lInit) <- lift getInput
      L sp tok <- tryP (Lexer.lexer False return)
      (bEnd, _) <- lift getInput
      case sp of
        UnhelpfulSpan _ -> pure ([], False) -- pretend the token never existed
        RealSrcSpan rsp _ -> do
          let typ = if inPrag then TkPragma else classify tok
              RealSrcLoc lStart _ = srcSpanStart sp -- safe since @sp@ is real
              (spaceBStr, bStart) = spanPosition lInit lStart bInit
              inPragDef = inPragma inPrag tok

          (bEnd', inPrag') <- case tok of
            -- Update internal line + file position if this is a LINE pragma
            ITline_prag _ -> tryOrElse (bEnd, inPragDef) $ do
              L _ (ITinteger (IL{il_value = line})) <- tryP wrappedLexer
              L _ (ITstring _ file) <- tryP wrappedLexer
              L spF ITclose_prag <- tryP wrappedLexer

              let newLoc = mkRealSrcLoc file (fromIntegral line - 1) (srcSpanEndCol spF)
              (bEnd'', _) <- lift getInput
              lift $ setInput (bEnd'', newLoc)

              pure (bEnd'', False)

            -- Update internal column position if this is a COLUMN pragma
            ITcolumn_prag _ -> tryOrElse (bEnd, inPragDef) $ do
              L _ (ITinteger (IL{il_value = col})) <- tryP wrappedLexer
              L spF ITclose_prag <- tryP wrappedLexer

              let newLoc = mkRealSrcLoc (srcSpanFile spF) (srcSpanEndLine spF) (fromIntegral col)
              (bEnd'', _) <- lift getInput
              lift $ setInput (bEnd'', newLoc)

              pure (bEnd'', False)
            _ -> pure (bEnd, inPragDef)

          let tokBStr = splitStringBuffer bStart bEnd'
              plainTok =
                T.Token
                  { tkType = typ
                  , tkValue = tokBStr
                  , tkSpan = rsp
                  }
              spaceTok =
                T.Token
                  { tkType = TkSpace
                  , tkValue = spaceBStr
                  , tkSpan = mkRealSrcSpan lInit lStart
                  }

          pure (plainTok : [spaceTok | not (BS.null spaceBStr)], inPrag')

    -- \| Parse whatever remains of the line as an unknown token (can't fail)
    unknownLine :: P ([T.Token], Bool)
    unknownLine = do
      (b, l) <- getInput
      let (unkBStr, l', b') = spanLine l b
          unkTok =
            T.Token
              { tkType = TkUnknown
              , tkValue = unkBStr
              , tkSpan = mkRealSrcSpan l l'
              }
      setInput (b', l')
      pure ([unkTok], False)

-- | Get the input
getInput :: P (StringBuffer, RealSrcLoc)
getInput = P $ \p@PState{buffer = buf, loc = srcLoc} -> POk p (buf, psRealLoc srcLoc)

-- | Set the input
setInput :: (StringBuffer, RealSrcLoc) -> P ()
setInput (buf, srcLoc) =
  P $ \p@PState{loc = PsLoc _ buf_loc} ->
    POk (p{buffer = buf, loc = PsLoc srcLoc buf_loc}) ()

tryP :: P a -> MaybeT P a
tryP (P f) = MaybeT $ P $ \s -> case f s of
  POk s' a -> POk s' (Just a)
  PFailed _ -> POk s Nothing

tryOrElse :: Alternative f => a -> f a -> f a
tryOrElse x p = p <|> pure x

-- | Classify given tokens as appropriate Haskell token type.
classify :: Lexer.Token -> TokenType
classify tok =
  case tok of
    ITas -> TkKeyword
    ITcase -> TkKeyword
    ITclass -> TkKeyword
    ITdata -> TkKeyword
    ITdefault -> TkKeyword
    ITderiving -> TkKeyword
    ITdo{} -> TkKeyword
    ITelse -> TkKeyword
    IThiding -> TkKeyword
    ITforeign -> TkKeyword
    ITif -> TkKeyword
    ITimport -> TkKeyword
    ITin -> TkKeyword
    ITinfix -> TkKeyword
    ITinfixl -> TkKeyword
    ITinfixr -> TkKeyword
    ITinstance -> TkKeyword
    ITlet -> TkKeyword
    ITmodule -> TkKeyword
    ITnewtype -> TkKeyword
    ITof -> TkKeyword
    ITqualified -> TkKeyword
    ITthen -> TkKeyword
    ITtype -> TkKeyword
    ITvia -> TkKeyword
    ITwhere -> TkKeyword
    ITforall{} -> TkKeyword
    ITexport -> TkKeyword
    ITlabel -> TkKeyword
    ITdynamic -> TkKeyword
    ITsafe -> TkKeyword
    ITinterruptible -> TkKeyword
    ITunsafe -> TkKeyword
    ITstdcallconv -> TkKeyword
    ITccallconv -> TkKeyword
    ITcapiconv -> TkKeyword
    ITprimcallconv -> TkKeyword
    ITjavascriptcallconv -> TkKeyword
    ITmdo{} -> TkKeyword
    ITfamily -> TkKeyword
    ITrole -> TkKeyword
    ITgroup -> TkKeyword
    ITby -> TkKeyword
    ITusing -> TkKeyword
    ITpattern -> TkKeyword
    ITstatic -> TkKeyword
    ITstock -> TkKeyword
    ITanyclass -> TkKeyword
    ITunit -> TkKeyword
    ITsignature -> TkKeyword
    ITdependency -> TkKeyword
    ITrequires -> TkKeyword
    ITinline_prag{} -> TkPragma
    ITopaque_prag{} -> TkPragma
    ITspec_prag{} -> TkPragma
    ITspec_inline_prag{} -> TkPragma
    ITsource_prag{} -> TkPragma
    ITrules_prag{} -> TkPragma
    ITwarning_prag{} -> TkPragma
    ITdeprecated_prag{} -> TkPragma
    ITline_prag{} -> TkPragma
    ITcolumn_prag{} -> TkPragma
    ITscc_prag{} -> TkPragma
    ITunpack_prag{} -> TkPragma
    ITnounpack_prag{} -> TkPragma
    ITann_prag{} -> TkPragma
    ITcomplete_prag{} -> TkPragma
    ITclose_prag -> TkPragma
    IToptions_prag{} -> TkPragma
    ITinclude_prag{} -> TkPragma
    ITlanguage_prag -> TkPragma
    ITminimal_prag{} -> TkPragma
    IToverlappable_prag{} -> TkPragma
    IToverlapping_prag{} -> TkPragma
    IToverlaps_prag{} -> TkPragma
    ITincoherent_prag{} -> TkPragma
    ITctype{} -> TkPragma
    ITdotdot -> TkGlyph
    ITcolon -> TkGlyph
    ITdcolon{} -> TkGlyph
    ITequal -> TkGlyph
    ITlam -> TkGlyph
    ITlcase -> TkGlyph
    ITlcases -> TkGlyph
    ITvbar -> TkGlyph
    ITlarrow{} -> TkGlyph
    ITrarrow{} -> TkGlyph
    ITlolly{} -> TkGlyph
    ITat -> TkGlyph
    ITtilde -> TkGlyph
    ITdarrow{} -> TkGlyph
    ITminus -> TkGlyph
    ITprefixminus -> TkGlyph
    ITbang -> TkGlyph
    ITdot -> TkOperator
    ITproj{} -> TkOperator
    ITstar{} -> TkOperator
    ITtypeApp -> TkGlyph
    ITpercent -> TkGlyph
    ITbiglam -> TkGlyph
    ITocurly -> TkSpecial
    ITccurly -> TkSpecial
    ITvocurly -> TkSpecial
    ITvccurly -> TkSpecial
    ITobrack -> TkSpecial
    ITopabrack -> TkSpecial
    ITcpabrack -> TkSpecial
    ITcbrack -> TkSpecial
    IToparen -> TkSpecial
    ITcparen -> TkSpecial
    IToubxparen -> TkSpecial
    ITcubxparen -> TkSpecial
    ITsemi -> TkSpecial
    ITcomma -> TkSpecial
    ITunderscore -> TkIdentifier
    ITbackquote -> TkSpecial
    ITsimpleQuote -> TkSpecial
    ITvarid{} -> TkIdentifier
    ITconid{} -> TkIdentifier
    ITvarsym{} -> TkOperator
    ITconsym{} -> TkOperator
    ITqvarid{} -> TkIdentifier
    ITqconid{} -> TkIdentifier
    ITqvarsym{} -> TkOperator
    ITqconsym{} -> TkOperator
    ITdupipvarid{} -> TkUnknown
    ITlabelvarid{} -> TkUnknown
    ITchar{} -> TkChar
    ITstring{} -> TkString
    ITstringMulti{} -> TkString
    ITinteger{} -> TkNumber
    ITrational{} -> TkNumber
    ITprimchar{} -> TkChar
    ITprimstring{} -> TkString
    ITprimint{} -> TkNumber
    ITprimword{} -> TkNumber
    ITprimint8{} -> TkNumber
    ITprimint16{} -> TkNumber
    ITprimint32{} -> TkNumber
    ITprimint64{} -> TkNumber
    ITprimword8{} -> TkNumber
    ITprimword16{} -> TkNumber
    ITprimword32{} -> TkNumber
    ITprimword64{} -> TkNumber
    ITprimfloat{} -> TkNumber
    ITprimdouble{} -> TkNumber
    ITopenExpQuote{} -> TkSpecial
    ITopenPatQuote -> TkSpecial
    ITopenDecQuote -> TkSpecial
    ITopenTypQuote -> TkSpecial
    ITcloseQuote{} -> TkSpecial
    ITopenTExpQuote{} -> TkSpecial
    ITcloseTExpQuote -> TkSpecial
    ITdollar -> TkSpecial
    ITdollardollar -> TkSpecial
    ITtyQuote -> TkSpecial
    ITquasiQuote{} -> TkUnknown
    ITqQuasiQuote{} -> TkUnknown
    ITproc -> TkKeyword
    ITrec -> TkKeyword
    IToparenbar{} -> TkGlyph
    ITcparenbar{} -> TkGlyph
    ITlarrowtail{} -> TkGlyph
    ITrarrowtail{} -> TkGlyph
    ITLarrowtail{} -> TkGlyph
    ITRarrowtail{} -> TkGlyph
    ITcomment_line_prag -> TkUnknown
    ITunknown{} -> TkUnknown
    ITeof -> TkUnknown
    ITlineComment{} -> TkComment
    ITdocComment{} -> TkComment
    ITdocOptions{} -> TkComment
    -- The lexer considers top-level pragmas as comments (see `pragState` in
    -- the GHC lexer for more), so we have to manually reverse this. The
    -- following is a hammer: it smashes _all_ pragma-like block comments into
    -- pragmas.
    ITblockComment c _
      | isPrefixOf "{-#" c
      , isSuffixOf "#-}" c ->
          TkPragma
      | otherwise -> TkComment

-- | Classify given tokens as beginning pragmas (or not).
inPragma
  :: Bool
  -- ^ currently in pragma
  -> Lexer.Token
  -- ^ current token
  -> Bool
  -- ^ new information about whether we are in a pragma
inPragma _ ITclose_prag = False
inPragma True _ = True
inPragma False tok =
  case tok of
    ITinline_prag{} -> True
    ITopaque_prag{} -> True
    ITspec_prag{} -> True
    ITspec_inline_prag{} -> True
    ITsource_prag{} -> True
    ITrules_prag{} -> True
    ITwarning_prag{} -> True
    ITdeprecated_prag{} -> True
    ITline_prag{} -> True
    ITcolumn_prag{} -> True
    ITscc_prag{} -> True
    ITunpack_prag{} -> True
    ITnounpack_prag{} -> True
    ITann_prag{} -> True
    ITcomplete_prag{} -> True
    IToptions_prag{} -> True
    ITinclude_prag{} -> True
    ITlanguage_prag -> True
    ITminimal_prag{} -> True
    IToverlappable_prag{} -> True
    IToverlapping_prag{} -> True
    IToverlaps_prag{} -> True
    ITincoherent_prag{} -> True
    ITctype{} -> True
    _ -> False
