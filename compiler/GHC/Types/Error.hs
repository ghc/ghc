{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module GHC.Types.Error
   ( -- * Messages
     Messages
   , WarningMessages
   , ErrorMessages
   , mkMessages
   , emptyMessages
   , isEmptyMessages
   , addMessage
   , unionMessages
   , MsgEnvelope (..)
   , WarnMsg
   , SDoc
   , DecoratedSDoc (unDecorated)
   , Severity (..)
   , RenderableDiagnostic (..)
   , pprMessageBag
   , mkDecorated
   , mkLocMessage
   , mkLocMessageAnn
   , getSeverityColour
   , getCaretDiagnostic
   , makeIntoWarning
   -- * Constructing individual errors
   , mkMsgEnvelope
   , mkPlainMsgEnvelope
   , mkErr
   , mkLongMsgEnvelope
   , mkWarnMsg
   , mkPlainWarnMsg
   , mkLongWarnMsg
   -- * Queries
   , isErrorMessage
   , isWarningMessage
   , getErrorMessages
   , getWarningMessages
   , partitionMessages
   , errorsFound
   )
where

import GHC.Prelude

import GHC.Driver.Flags

import GHC.Data.Bag
import GHC.Utils.Outputable as Outputable
import qualified GHC.Utils.Ppr.Colour as Col
import GHC.Types.SrcLoc as SrcLoc
import GHC.Data.FastString (unpackFS)
import GHC.Data.StringBuffer (atLine, hGetStringBuffer, len, lexemeToString)
import GHC.Utils.Json

import System.IO.Error  ( catchIOError )

{-
Note [Messages]
~~~~~~~~~~~~~~~

We represent the 'Messages' as a single bag of warnings and errors.

The reason behind that is that there is a fluid relationship between errors and warnings and we want to
be able to promote or demote errors and warnings based on certain flags (e.g. -Werror, -fdefer-type-errors
or -XPartialTypeSignatures). For now we rely on the 'Severity' to distinguish between a warning and an
error, although the 'Severity' can be /more/ than just 'SevWarn' and 'SevError', and as such it probably
shouldn't belong to an 'MsgEnvelope' to begin with, as it might potentially lead to the construction of
"impossible states" (e.g. a waning with 'SevInfo', for example).

'WarningMessages' and 'ErrorMessages' are for now simple type aliases to retain backward compatibility, but
in future iterations these can be either parameterised over an 'e' message type (to make type signatures
a bit more declarative) or removed altogether.
-}

-- | A collection of messages emitted by GHC during error reporting. A diagnostic message is typically
-- a warning or an error. See Note [Messages].
newtype Messages e = Messages (Bag (MsgEnvelope e))

instance Functor Messages where
  fmap f (Messages xs) = Messages (mapBag (fmap f) xs)

emptyMessages :: Messages e
emptyMessages = Messages emptyBag

mkMessages :: Bag (MsgEnvelope e) -> Messages e
mkMessages = Messages

isEmptyMessages :: Messages e -> Bool
isEmptyMessages (Messages msgs) = isEmptyBag msgs

addMessage :: MsgEnvelope e -> Messages e -> Messages e
addMessage x (Messages xs) = Messages (x `consBag` xs)

-- | Joins two collections of messages together.
unionMessages :: Messages e -> Messages e -> Messages e
unionMessages (Messages msgs1) (Messages msgs2) = Messages (msgs1 `unionBags` msgs2)

type WarningMessages = Bag (MsgEnvelope DecoratedSDoc)
type ErrorMessages   = Bag (MsgEnvelope DecoratedSDoc)

type WarnMsg         = MsgEnvelope DecoratedSDoc

-- | A 'DecoratedSDoc' is isomorphic to a '[SDoc]' but it carries the invariant that the input '[SDoc]'
-- needs to be rendered /decorated/ into its final form, where the typical case would be adding bullets
-- between each elements of the list.
-- The type of decoration depends on the formatting function used, but in practice GHC uses the
-- 'formatBulleted'.
newtype DecoratedSDoc = Decorated { unDecorated :: [SDoc] }

-- | Creates a new 'DecoratedSDoc' out of a list of 'SDoc'.
mkDecorated :: [SDoc] -> DecoratedSDoc
mkDecorated = Decorated

{-
Note [Rendering Messages]
~~~~~~~~~~~~~~~~~~~~~~~~~

Turning 'Messages' into something that renders nicely for the user is one of the last steps, and it
happens typically at the application boundaries (i.e. from the 'Driver' upwards).

For now (see #18516) this class is very boring as it has only one instance, but the idea is that as
the more domain-specific types are defined, the more instances we would get. For example, given something like:

data TcRnMessage
  = TcRnOutOfScope ..
  | ..

We could then define how a 'TcRnMessage' is displayed to the user. Rather than scattering pieces of
'SDoc' around the codebase, we would write once for all:

instance RenderableDiagnostic TcRnMessage where
  renderDiagnostic = \case
    TcRnOutOfScope .. -> Decorated [text "Out of scope error ..."]
    ...

This way, we can easily write generic rendering functions for errors that all they care about is the
knowledge that a given type 'e' has a 'RenderableDiagnostic' constraint.

-}

-- | A class for types (typically errors and warnings) which can be \"rendered\" into an opaque 'DecoratedSDoc'.
-- For more information, see Note [Rendering Messages].
class RenderableDiagnostic a where
  renderDiagnostic :: a -> DecoratedSDoc

-- | An envelope for GHC's facts about a running program, parameterised over the
-- /domain-specific/ (i.e. parsing, typecheck-renaming, etc) diagnostics.
--
-- To say things differently, GHC emits /diagnostics/ about the running program, each of which is wrapped
-- into a 'MsgEnvelope' that carries specific information like where the error happened, its severity, etc.
-- Finally, multiple 'MsgEnvelope's are aggregated into 'Messages' that are returned to the user.
data MsgEnvelope e = MsgEnvelope
   { errMsgSpan        :: SrcSpan
      -- ^ The SrcSpan is used for sorting errors into line-number order
   , errMsgContext     :: PrintUnqualified
   , errMsgDiagnostic  :: e
   , errMsgSeverity    :: Severity
   , errMsgReason      :: WarnReason
   } deriving Functor

instance RenderableDiagnostic DecoratedSDoc where
  renderDiagnostic = id

data Severity
  = SevOutput
  | SevFatal
  | SevInteractive

  | SevDump
    -- ^ Log message intended for compiler developers
    -- No file\/line\/column stuff

  | SevInfo
    -- ^ Log messages intended for end users.
    -- No file\/line\/column stuff.

  | SevWarning
  | SevError
    -- ^ SevWarning and SevError are used for warnings and errors
    --   o The message has a file\/line\/column heading,
    --     plus "warning:" or "error:",
    --     added by mkLocMessags
    --   o Output is intended for end users
  deriving (Eq, Show)


instance ToJson Severity where
  json s = JSString (show s)

instance Show (MsgEnvelope DecoratedSDoc) where
    show = showMsgEnvelope

-- | Shows an 'MsgEnvelope'.
showMsgEnvelope :: RenderableDiagnostic a => MsgEnvelope a -> String
showMsgEnvelope err =
  renderWithContext defaultSDocContext (vcat (unDecorated . renderDiagnostic $ errMsgDiagnostic err))

pprMessageBag :: Bag SDoc -> SDoc
pprMessageBag msgs = vcat (punctuate blankLine (bagToList msgs))

-- | Make an unannotated error message with location info.
mkLocMessage :: Severity -> SrcSpan -> SDoc -> SDoc
mkLocMessage = mkLocMessageAnn Nothing

-- | Make a possibly annotated error message with location info.
mkLocMessageAnn
  :: Maybe String                       -- ^ optional annotation
  -> Severity                           -- ^ severity
  -> SrcSpan                            -- ^ location
  -> SDoc                             -- ^ message
  -> SDoc
  -- Always print the location, even if it is unhelpful.  Error messages
  -- are supposed to be in a standard format, and one without a location
  -- would look strange.  Better to say explicitly "<no location info>".
mkLocMessageAnn ann severity locn msg
    = sdocOption sdocColScheme $ \col_scheme ->
      let locn' = sdocOption sdocErrorSpans $ \case
                     True  -> ppr locn
                     False -> ppr (srcSpanStart locn)

          sevColour = getSeverityColour severity col_scheme

          -- Add optional information
          optAnn = case ann of
            Nothing -> text ""
            Just i  -> text " [" <> coloured sevColour (text i) <> text "]"

          -- Add prefixes, like    Foo.hs:34: warning:
          --                           <the warning message>
          header = locn' <> colon <+>
                   coloured sevColour sevText <> optAnn

      in coloured (Col.sMessage col_scheme)
                  (hang (coloured (Col.sHeader col_scheme) header) 4
                        msg)

  where
    sevText =
      case severity of
        SevWarning -> text "warning:"
        SevError   -> text "error:"
        SevFatal   -> text "fatal:"
        _          -> empty

getSeverityColour :: Severity -> Col.Scheme -> Col.PprColour
getSeverityColour SevWarning = Col.sWarning
getSeverityColour SevError   = Col.sError
getSeverityColour SevFatal   = Col.sFatal
getSeverityColour _          = const mempty

getCaretDiagnostic :: Severity -> SrcSpan -> IO SDoc
getCaretDiagnostic _ (UnhelpfulSpan _) = pure empty
getCaretDiagnostic severity (RealSrcSpan span _) =
  caretDiagnostic <$> getSrcLine (srcSpanFile span) row
  where
    getSrcLine fn i =
      getLine i (unpackFS fn)
        `catchIOError` \_ ->
          pure Nothing

    getLine i fn = do
      -- StringBuffer has advantages over readFile:
      -- (a) no lazy IO, otherwise IO exceptions may occur in pure code
      -- (b) always UTF-8, rather than some system-dependent encoding
      --     (Haskell source code must be UTF-8 anyway)
      content <- hGetStringBuffer fn
      case atLine i content of
        Just at_line -> pure $
          case lines (fix <$> lexemeToString at_line (len at_line)) of
            srcLine : _ -> Just srcLine
            _           -> Nothing
        _ -> pure Nothing

    -- allow user to visibly see that their code is incorrectly encoded
    -- (StringBuffer.nextChar uses \0 to represent undecodable characters)
    fix '\0' = '\xfffd'
    fix c    = c

    row = srcSpanStartLine span
    rowStr = show row
    multiline = row /= srcSpanEndLine span

    caretDiagnostic Nothing = empty
    caretDiagnostic (Just srcLineWithNewline) =
      sdocOption sdocColScheme$ \col_scheme ->
      let sevColour = getSeverityColour severity col_scheme
          marginColour = Col.sMargin col_scheme
      in
      coloured marginColour (text marginSpace) <>
      text ("\n") <>
      coloured marginColour (text marginRow) <>
      text (" " ++ srcLinePre) <>
      coloured sevColour (text srcLineSpan) <>
      text (srcLinePost ++ "\n") <>
      coloured marginColour (text marginSpace) <>
      coloured sevColour (text (" " ++ caretLine))

      where

        -- expand tabs in a device-independent manner #13664
        expandTabs tabWidth i s =
          case s of
            ""        -> ""
            '\t' : cs -> replicate effectiveWidth ' ' ++
                         expandTabs tabWidth (i + effectiveWidth) cs
            c    : cs -> c : expandTabs tabWidth (i + 1) cs
          where effectiveWidth = tabWidth - i `mod` tabWidth

        srcLine = filter (/= '\n') (expandTabs 8 0 srcLineWithNewline)

        start = srcSpanStartCol span - 1
        end | multiline = length srcLine
            | otherwise = srcSpanEndCol span - 1
        width = max 1 (end - start)

        marginWidth = length rowStr
        marginSpace = replicate marginWidth ' ' ++ " |"
        marginRow   = rowStr ++ " |"

        (srcLinePre,  srcLineRest) = splitAt start srcLine
        (srcLineSpan, srcLinePost) = splitAt width srcLineRest

        caretEllipsis | multiline = "..."
                      | otherwise = ""
        caretLine = replicate start ' ' ++ replicate width '^' ++ caretEllipsis

makeIntoWarning :: WarnReason -> MsgEnvelope e -> MsgEnvelope e
makeIntoWarning reason err = err
    { errMsgSeverity = SevWarning
    , errMsgReason = reason }

--
-- Creating MsgEnvelope(s)
--

mk_err_msg
  :: Severity -> SrcSpan -> PrintUnqualified -> e -> MsgEnvelope e
mk_err_msg sev locn print_unqual err
 = MsgEnvelope { errMsgSpan = locn
               , errMsgContext = print_unqual
               , errMsgDiagnostic = err
               , errMsgSeverity = sev
               , errMsgReason = NoReason }

mkErr :: SrcSpan -> PrintUnqualified -> e -> MsgEnvelope e
mkErr = mk_err_msg SevError

mkLongMsgEnvelope, mkLongWarnMsg   :: SrcSpan -> PrintUnqualified -> SDoc -> SDoc -> MsgEnvelope DecoratedSDoc
-- ^ A long (multi-line) error message
mkMsgEnvelope, mkWarnMsg           :: SrcSpan -> PrintUnqualified -> SDoc         -> MsgEnvelope DecoratedSDoc
-- ^ A short (one-line) error message
mkPlainMsgEnvelope, mkPlainWarnMsg :: SrcSpan ->                     SDoc         -> MsgEnvelope DecoratedSDoc
-- ^ Variant that doesn't care about qualified/unqualified names

mkLongMsgEnvelope   locn unqual msg extra = mk_err_msg SevError   locn unqual        (mkDecorated [msg,extra])
mkMsgEnvelope       locn unqual msg       = mk_err_msg SevError   locn unqual        (mkDecorated [msg])
mkPlainMsgEnvelope  locn        msg       = mk_err_msg SevError   locn alwaysQualify (mkDecorated [msg])
mkLongWarnMsg       locn unqual msg extra = mk_err_msg SevWarning locn unqual        (mkDecorated [msg,extra])
mkWarnMsg           locn unqual msg       = mk_err_msg SevWarning locn unqual        (mkDecorated [msg])
mkPlainWarnMsg      locn        msg       = mk_err_msg SevWarning locn alwaysQualify (mkDecorated [msg])

--
-- Queries
--

isErrorMessage :: MsgEnvelope e -> Bool
isErrorMessage = (== SevError) . errMsgSeverity

isWarningMessage :: MsgEnvelope e -> Bool
isWarningMessage = not . isErrorMessage

errorsFound :: Messages e -> Bool
errorsFound (Messages msgs) = any isErrorMessage msgs

getWarningMessages :: Messages e -> Bag (MsgEnvelope e)
getWarningMessages (Messages xs) = fst $ partitionBag isWarningMessage xs

getErrorMessages :: Messages e -> Bag (MsgEnvelope e)
getErrorMessages (Messages xs) = fst $ partitionBag isErrorMessage xs

-- | Partitions the 'Messages' and returns a tuple which first element are the warnings, and the
-- second the errors.
partitionMessages :: Messages e -> (Bag (MsgEnvelope e), Bag (MsgEnvelope e))
partitionMessages (Messages xs) = partitionBag isWarningMessage xs
