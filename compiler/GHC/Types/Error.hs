{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}

module GHC.Types.Error
   ( Messages
   , WarningMessages(..)
   , ErrorMessages(..)
   , ErrMsg (..)
   , WarnMsg
   , ErrDoc (..)
   , MsgDoc
   , Severity (..)
   , RenderableDiagnostic (..)
   , showErrMsg
   , mapMessages
   , unionMessages
   , errDoc
   , mapErrDoc
   , pprMessageBag
   , mkLocMessage
   , mkLocMessageAnn
   , getErrorMessages
   , getWarningMessages
   , getSeverityColour
   , getCaretDiagnostic

   -- * Promoting and demoting a single warning/error
   , makeIntoWarning
   , makeIntoError

   -- * Promoting and demoting a warnings/errors
   , promoteWarningsToErrors
   )
where

import Data.Coerce (coerce)
import Data.Monoid

import GHC.Prelude

import GHC.Driver.Flags

import GHC.Data.Bag
import GHC.Utils.Outputable as Outputable hiding ((<>))
import qualified GHC.Utils.Outputable as Outputable
import qualified GHC.Utils.Ppr.Colour as Col
import GHC.Types.SrcLoc as SrcLoc
import GHC.Data.FastString (unpackFS)
import GHC.Data.StringBuffer (atLine, hGetStringBuffer, len, lexemeToString)
import GHC.Utils.Json

import System.IO.Error  ( catchIOError )

type Messages         w e = (WarningMessages w, ErrorMessages e)
newtype WarningMessages w = WarningMessages { getWarningMessages :: Bag (ErrMsg w) }
newtype ErrorMessages   e = ErrorMessages   { getErrorMessages   :: Bag (ErrMsg e) }
type MsgDoc               = SDoc
type WarnMsg              = ErrMsg ErrDoc

instance Functor WarningMessages where
  fmap f (WarningMessages xs) = WarningMessages (mapBag (fmap f) xs)

instance Semigroup (WarningMessages w) where
  (WarningMessages a) <> (WarningMessages b) = WarningMessages $ a `unionBags` b

instance Monoid (WarningMessages w) where
  mempty  = WarningMessages emptyBag
  mappend = (<>)

instance Functor ErrorMessages where
  fmap f (ErrorMessages xs) = ErrorMessages (mapBag (fmap f) xs)

instance Semigroup (ErrorMessages w) where
  (ErrorMessages a) <> (ErrorMessages b) = ErrorMessages $ a `unionBags` b

instance Monoid (ErrorMessages w) where
  mempty  = ErrorMessages emptyBag
  mappend = (<>)

-- | A class for types (typically errors and warnings) which can be \"rendered\" into a opaque 'ErrDoc'.
class RenderableDiagnostic a where
  renderDiagnostic :: a -> ErrDoc

-- | The main 'GHC' error type, parameterised over the /domain-specific/ error /or/ warning 'a'.
-- There is deliberately no 'Show' instance for an 'ErrMsg', see Note [Showing ErrMsg].
data ErrMsg a = ErrMsg
   { errMsgSpan        :: SrcSpan
      -- ^ The SrcSpan is used for sorting errors into line-number order
   , errMsgContext     :: PrintUnqualified
   , errMsgDiagnostic  :: a
   , errMsgSeverity    :: Severity
   , errMsgReason      :: WarnReason
   } deriving Functor

-- | Categorise error msgs by their importance.  This is so each section can
-- be rendered visually distinct.  See Note [Error report] for where these come
-- from.
data ErrDoc = ErrDoc {
        -- | Primary error msg.
        errDocImportant     :: [MsgDoc],
        -- | Context e.g. \"In the second argument of ...\".
        errDocContext       :: [MsgDoc],
        -- | Supplementary information, e.g. \"Relevant bindings include ...\".
        errDocSupplementary :: [MsgDoc]
        }

instance RenderableDiagnostic ErrDoc where
  renderDiagnostic = id


mapMessages :: (e -> e') -> Messages w e -> Messages w e'
mapMessages f (ws, es) = bimapMessages id f (ws, es)

bimapMessages :: (w -> w') -> (e -> e') -> Messages w e -> Messages w' e'
bimapMessages f g (ws, es) = (fmap f ws, fmap g es)

unionMessages :: Messages w e -> Messages w e -> Messages w e
unionMessages (coerce -> warns1, coerce -> errs1) (coerce -> warns2, coerce -> errs2) =
  (WarningMessages $ warns1 `unionBags` warns2, ErrorMessages $ errs1 `unionBags` errs2)

errDoc :: [MsgDoc] -> [MsgDoc] -> [MsgDoc] -> ErrDoc
errDoc = ErrDoc

mapErrDoc :: (MsgDoc -> MsgDoc) -> ErrDoc -> ErrDoc
mapErrDoc f (ErrDoc a b c) = ErrDoc (map f a) (map f b) (map f c)

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
  deriving Show


instance ToJson Severity where
  json s = JSString (show s)

-- NOTE [Showing ErrMsg]
-- Showing an 'ErrMsg' via its 'Show' instance is something that doesn't make much sense.
-- To begin with, we can't write a meaningful 'Show' instance such that 'read . show === id', but
-- /properly/ showing an 'ErrMsg' is done via the 'RenderableError' machinery and all the pretty-printing,
-- so having a 'Show' instance feels like duplicating efforts.
-- Furthermore, the 'errMsgShortString' function was added to an 'ErrMsg' with the sole purpose of
-- writing the instance below:
--
-- instance Show (ErrMsg e) where
--   show em = errMsgShortString em
--
-- However, errMsgShortString required 'DynFlags' to be passed as input for rendering correctly the 'SDoc'
-- making things even more coupled.
-- For all these reasons we remove the 'Show' instance for 'ErrMsg' and we offer a top-level function for
-- convenience, i.e. 'showErrMsg'.

-- | Shows an 'ErrMsg'. See NOTE [Showing ErrMsg]. Use this function only for debugging and testing
-- purposes.
showErrMsg :: RenderableDiagnostic a => ErrMsg a -> String
showErrMsg err =
  renderWithContext defaultSDocContext (vcat (errDocImportant $ renderDiagnostic $ errMsgDiagnostic err))

pprMessageBag :: Bag MsgDoc -> SDoc
pprMessageBag msgs = vcat (punctuate blankLine (bagToList msgs))

-- | Make an unannotated error message with location info.
mkLocMessage :: Severity -> SrcSpan -> MsgDoc -> MsgDoc
mkLocMessage = mkLocMessageAnn Nothing

-- | Make a possibly annotated error message with location info.
mkLocMessageAnn
  :: Maybe String                       -- ^ optional annotation
  -> Severity                           -- ^ severity
  -> SrcSpan                            -- ^ location
  -> MsgDoc                             -- ^ message
  -> MsgDoc
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
            Just i  -> text " [" Outputable.<> coloured sevColour (text i) Outputable.<> text "]"

          -- Add prefixes, like    Foo.hs:34: warning:
          --                           <the warning message>
          header = locn' Outputable.<> colon <+>
                   coloured sevColour sevText Outputable.<> optAnn

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

getCaretDiagnostic :: Severity -> SrcSpan -> IO MsgDoc
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
      coloured marginColour (text marginSpace) Outputable.<>
      text ("\n") Outputable.<>
      coloured marginColour (text marginRow) Outputable.<>
      text (" " ++ srcLinePre) Outputable.<>
      coloured sevColour (text srcLineSpan) Outputable.<>
      text (srcLinePost ++ "\n") Outputable.<>
      coloured marginColour (text marginSpace) Outputable.<>
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

makeIntoWarning :: WarnReason -> ErrMsg e -> ErrMsg e
makeIntoWarning reason err = err
    { errMsgSeverity = SevWarning
    , errMsgReason = reason }

makeIntoError :: WarnReason -> ErrMsg e -> ErrMsg e
makeIntoError reason err = err
    { errMsgSeverity = SevError
    , errMsgReason = reason }

-- | \"Promotes\" plain 'WarningMessages' into 'ErrorMessages', by applying the input
-- function. The typical case when you want to do that is when \"-Werror\" is enabled,
-- and therefore warnings need to be treated as errors.
promoteWarningsToErrors :: (w -> e) -> WarningMessages w -> ErrorMessages e
promoteWarningsToErrors toError (WarningMessages warns) = ErrorMessages (mapBag (fmap toError) warns)

