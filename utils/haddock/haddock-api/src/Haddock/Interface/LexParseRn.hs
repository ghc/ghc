{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wwarn #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Haddock.Interface.LexParseRn
-- Copyright   :  (c) Isaac Dupree 2009,
--                    Mateusz Kowalczyk 2013
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
module Haddock.Interface.LexParseRn
  ( processDocString
  , processDocStringParas
  , processDocStringsParas
  , processModuleHeader
  ) where

import Control.Arrow
import Control.Monad
import Control.Monad.State.Strict
import Data.Functor
import Data.List (maximumBy, (\\))
import Data.Ord
import qualified Data.Set as Set
import GHC
import GHC.Data.EnumSet as EnumSet
import GHC.Data.FastString (unpackFS)
import GHC.Driver.Session
import qualified GHC.LanguageExtensions as LangExt
import GHC.Parser.Lexer (ParserOpts)
import GHC.Parser.PostProcess
import GHC.Types.Avail
import GHC.Types.Name
import GHC.Types.Name.Reader
import GHC.Types.Name.Set
import GHC.Utils.Misc ((<||>))
import GHC.Utils.Outputable (Outputable (ppr), SDocContext, renderWithContext)
import qualified GHC.Utils.Outputable as Outputable
import Haddock.Interface.ParseModuleHeader
import Haddock.Parser
import Haddock.Types

processDocStringsParas
  :: MonadIO m
  => ParserOpts
  -> SDocContext
  -> Maybe Package
  -> [HsDoc GhcRn]
  -> IfM m (MDoc Name)
processDocStringsParas parserOpts sDocContext pkg hdss =
  overDocF (rename sDocContext $ hsDocRenamer hds) $ parseParas parserOpts pkg (renderHsDocStrings $ hsDocString hds)
  where
    hds :: WithHsDocIdentifiers [HsDocString] GhcRn
    hds = WithHsDocIdentifiers (map hsDocString hdss) (concatMap hsDocIdentifiers hdss)

processDocStringParas
  :: MonadIO m
  => ParserOpts
  -> SDocContext
  -> Maybe Package
  -> HsDoc GhcRn
  -> IfM m (MDoc Name)
processDocStringParas parserOpts sDocContext pkg hds =
  overDocF (rename sDocContext $ hsDocRenamer hds) $ parseParas parserOpts pkg (renderHsDocString $ hsDocString hds)

processDocString
  :: MonadIO m
  => ParserOpts
  -> SDocContext
  -> HsDoc GhcRn
  -> IfM m (Doc Name)
processDocString parserOpts sDocContext hds =
  rename sDocContext (hsDocRenamer hds) $ parseString parserOpts (renderHsDocString $ hsDocString hds)

processModuleHeader
  :: MonadIO m
  => Maybe Language
  -> ParserOpts
  -> SDocContext
  -> Maybe Package
  -> SafeHaskellMode
  -> Maybe Language
  -> EnumSet LangExt.Extension
  -> Maybe (HsDoc GhcRn)
  -> IfM m (HaddockModInfo Name, Maybe (MDoc Name))
processModuleHeader mLanguage parserOpts sDocContext pkgName safety mayLang extSet mayStr = do
  (hmi, doc) <-
    case mayStr of
      Nothing -> return failure
      Just hsDoc -> do
        let str = renderHsDocString $ hsDocString hsDoc
            (hmi, doc) = parseModuleHeader parserOpts pkgName str
            renamer = hsDocRenamer hsDoc
        !descr <- case hmi_description hmi of
          Just hmi_descr -> Just <$> rename sDocContext renamer hmi_descr
          Nothing -> pure Nothing
        let hmi' = hmi{hmi_description = descr}
        doc' <- overDocF (rename sDocContext renamer) doc
        return (hmi', Just doc')

  let flags :: [LangExt.Extension]
      -- We remove the flags implied by the language setting and we display the language instead
      flags = EnumSet.toList extSet \\ languageExtensions mayLang
  return
    ( hmi
        { hmi_safety = Just $ Outputable.renderWithContext sDocContext (Outputable.ppr safety)
        , hmi_language = mLanguage
        , hmi_extensions = flags
        }
    , doc
    )
  where
    failure = (emptyHaddockModInfo, Nothing)

traverseSnd :: (Traversable t, Applicative f) => (a -> f b) -> t (x, a) -> f (t (x, b))
traverseSnd f =
  traverse
    ( \(x, a) ->
        (\b -> (x, b)) <$> f a
    )

-- | Takes a 'GlobalRdrEnv' which (hopefully) contains all the
-- definitions and a parsed comment and we attempt to make sense of
-- where the identifiers in the comment point to. We're in effect
-- trying to convert 'RdrName's to 'Name's, with some guesswork and
-- fallbacks in case we can't locate the identifiers.
--
-- See the comments in the source for implementation commentary.
rename
  :: MonadIO m
  => SDocContext
  -> Renamer
  -> Doc NsRdrName
  -> IfM m (Doc Name)
rename sDocContext renamer = rn
  where
    rn :: MonadIO m => Doc NsRdrName -> IfM m (Doc Name)
    rn d = case d of
      DocAppend a b -> DocAppend <$> rn a <*> rn b
      DocParagraph p -> DocParagraph <$> rn p
      DocIdentifier i -> do
        let NsRdrName ns x = unwrap i
            occ = rdrNameOcc x
        let valueNsChoices
              | isDataOcc occ = isDataConNameSpace
              | otherwise = isTermVarOrFieldNameSpace
            typeNsChoices
              | isDataOcc occ = isTcClsNameSpace
              | otherwise = isTvNameSpace
        -- Generate the choices for the possible kind of thing this
        -- is. We narrow down the possibilities with the namespace (if
        -- there is one).
        let choices = case ns of
              Value -> valueNsChoices
              Type -> typeNsChoices
              None -> valueNsChoices <||> typeNsChoices
        case renamer (Outputable.renderWithContext sDocContext (Outputable.ppr x)) choices of
          [] -> case ns of
            Type -> outOfScope sDocContext ns (i $> setRdrNameSpace x tcName)
            _ -> outOfScope sDocContext ns (i $> x)
          [a] -> pure (DocIdentifier $ i $> a)
          -- There are multiple names available.
          names -> ambiguous sDocContext i names
      DocWarning dw -> DocWarning <$> rn dw
      DocEmphasis de -> DocEmphasis <$> rn de
      DocBold db -> DocBold <$> rn db
      DocMonospaced dm -> DocMonospaced <$> rn dm
      DocUnorderedList docs -> DocUnorderedList <$> traverse rn docs
      DocOrderedList docs -> DocOrderedList <$> traverseSnd rn docs
      DocDefList list -> DocDefList <$> traverse (\(a, b) -> (,) <$> rn a <*> rn b) list
      DocCodeBlock dcb -> DocCodeBlock <$> rn dcb
      DocIdentifierUnchecked x -> pure (DocIdentifierUnchecked x)
      DocModule (ModLink m l) -> DocModule . ModLink m <$> traverse rn l
      DocHyperlink (Hyperlink u l) -> DocHyperlink . Hyperlink u <$> traverse rn l
      DocPic str -> pure (DocPic str)
      DocMathInline str -> pure (DocMathInline str)
      DocMathDisplay str -> pure (DocMathDisplay str)
      DocAName str -> pure (DocAName str)
      DocProperty p -> pure (DocProperty p)
      DocExamples e -> pure (DocExamples e)
      DocEmpty -> pure (DocEmpty)
      DocString str -> pure (DocString str)
      DocHeader (Header l t) -> DocHeader . Header l <$> rn t
      DocTable t -> DocTable <$> traverse rn t

-- | Wrap an identifier that's out of scope (i.e. wasn't found in
-- 'GlobalReaderEnv' during 'rename') in an appropriate doc. Currently
-- we simply monospace the identifier in most cases except when the
-- identifier is qualified: if the identifier is qualified then we can
-- still try to guess and generate anchors accross modules but the
-- users shouldn't rely on this doing the right thing. See tickets
-- #253 and #375 on the confusion this causes depending on which
-- default we pick in 'rename'.
outOfScope :: MonadIO m => SDocContext -> Namespace -> Wrap RdrName -> IfM m (Doc a)
outOfScope sDocContext ns x =
  case unwrap x of
    Unqual occ -> warnAndMonospace (x $> occ)
    Qual mdl occ -> pure (DocIdentifierUnchecked (x $> (mdl, occ)))
    Orig _ occ -> warnAndMonospace (x $> occ)
    Exact name -> warnAndMonospace (x $> name) -- Shouldn't happen since x is out of scope
  where
    prefix =
      case ns of
        Value -> "the value "
        Type -> "the type "
        None -> ""

    warnAndMonospace :: (MonadIO m, Outputable a) => Wrap a -> IfM m (DocH mod id)
    warnAndMonospace a = do
      let a' = showWrapped (renderWithContext sDocContext . Outputable.ppr) a

      -- If we have already warned for this identifier, don't warn again
      firstWarn <- Set.notMember a' <$> gets ifeOutOfScopeNames
      when firstWarn $ do
        warn $
          "Warning: "
            ++ prefix
            ++ "'"
            ++ a'
            ++ "' is out of scope.\n"
            ++ "    If you qualify the identifier, haddock can try to link it anyway."
        modify' (\env -> env{ifeOutOfScopeNames = Set.insert a' (ifeOutOfScopeNames env)})

      pure (monospaced a')
    monospaced = DocMonospaced . DocString

-- | Handle ambiguous identifiers.
--
-- Prefers local names primarily and type constructors or class names secondarily.
--
-- Emits a warning if the 'GlobalRdrElts's don't belong to the same type or class.
ambiguous
  :: MonadIO m
  => SDocContext
  -> Wrap NsRdrName
  -> [Name]
  -- ^ More than one @gre@s sharing the same `RdrName` above.
  -> IfM m (Doc Name)
ambiguous sDocContext x names = do
  let noChildren = map availName (nubAvails (map Avail names))
      dflt = maximumBy (comparing (isLocalName &&& isTyConName)) noChildren
      nameStr = showNsRdrName sDocContext x
      msg =
        "Warning: "
          ++ nameStr
          ++ " is ambiguous. It is defined\n"
          ++ concatMap (\n -> "    * " ++ defnLoc n ++ "\n") names
          ++ "    You may be able to disambiguate the identifier by qualifying it or\n"
          ++ "    by specifying the type/value namespace explicitly.\n"
          ++ "    Defaulting to the one defined "
          ++ defnLoc dflt

  -- TODO: Once we have a syntax for namespace qualification (#667) we may also
  -- want to emit a warning when an identifier is a data constructor for a type
  -- of the same name, but not the only constructor.
  -- For example, for @data D = C | D@, someone may want to reference the @D@
  -- constructor.

  -- If we have already warned for this name, do not warn again
  firstWarn <- Set.notMember nameStr <$> gets ifeAmbiguousNames
  when (length noChildren > 1 && firstWarn) $ do
    warn msg
    modify' (\env -> env{ifeAmbiguousNames = Set.insert nameStr (ifeAmbiguousNames env)})

  pure (DocIdentifier (x $> dflt))
  where
    isLocalName (nameSrcLoc -> RealSrcLoc{}) = True
    isLocalName _ = False
    defnLoc = Outputable.renderWithContext sDocContext . pprNameDefnLoc

-- | Printable representation of a wrapped and namespaced name
showNsRdrName :: SDocContext -> Wrap NsRdrName -> String
showNsRdrName sDocContext = (\p i -> p ++ "'" ++ i ++ "'") <$> prefix <*> ident
  where
    ident = showWrapped (Outputable.renderWithContext sDocContext . ppr . rdrName)
    prefix = renderNs . namespace . unwrap

hsDocRenamer :: WithHsDocIdentifiers a GhcRn -> Renamer
hsDocRenamer hsDoc = \s cands -> nameSetElemsStable $ filterNameSet (nameMatches s cands) env
  where
    !env = hsDocIds hsDoc
    nameMatches s ok_ns n =
      let occ = occName n
       in ok_ns (occNameSpace occ) && s == unpackFS (occNameFS occ)
