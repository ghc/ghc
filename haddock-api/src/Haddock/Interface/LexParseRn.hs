{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
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
-----------------------------------------------------------------------------
module Haddock.Interface.LexParseRn
  ( processDocString
  , processDocStringParas
  , processDocStrings
  , processModuleHeader
  ) where

import Avail
import Control.Arrow
import Control.Monad
import Data.List
import Data.Ord
import Documentation.Haddock.Doc (metaDocConcat)
import DynFlags (languageExtensions)
import qualified GHC.LanguageExtensions as LangExt
import GHC
import Haddock.Interface.ParseModuleHeader
import Haddock.Parser
import Haddock.Types
import Name
import Outputable ( showPpr, showSDoc )
import RdrName
import EnumSet
import RnEnv (dataTcOccs)

processDocStrings :: DynFlags -> Maybe Package -> GlobalRdrEnv -> [HsDocString]
                  -> ErrMsgM (Maybe (MDoc Name))
processDocStrings dflags pkg gre strs = do
  mdoc <- metaDocConcat <$> traverse (processDocStringParas dflags pkg gre) strs
  case mdoc of
    -- We check that we don't have any version info to render instead
    -- of just checking if there is no comment: there may not be a
    -- comment but we still want to pass through any meta data.
    MetaDoc { _meta = Meta Nothing Nothing, _doc = DocEmpty } -> pure Nothing
    x -> pure (Just x)

processDocStringParas :: DynFlags -> Maybe Package -> GlobalRdrEnv -> HsDocString -> ErrMsgM (MDoc Name)
processDocStringParas dflags pkg gre hds =
  overDocF (rename dflags gre) $ parseParas dflags pkg (unpackHDS hds)

processDocString :: DynFlags -> GlobalRdrEnv -> HsDocString -> ErrMsgM (Doc Name)
processDocString dflags gre hds =
  rename dflags gre $ parseString dflags (unpackHDS hds)

processModuleHeader :: DynFlags -> Maybe Package -> GlobalRdrEnv -> SafeHaskellMode -> Maybe LHsDocString
                    -> ErrMsgM (HaddockModInfo Name, Maybe (MDoc Name))
processModuleHeader dflags pkgName gre safety mayStr = do
  (hmi, doc) <-
    case mayStr of
      Nothing -> return failure
      Just (L _ hds) -> do
        let str = unpackHDS hds
            (hmi, doc) = parseModuleHeader dflags pkgName str
        !descr <- case hmi_description hmi of
                    Just hmi_descr -> Just <$> rename dflags gre hmi_descr
                    Nothing        -> pure Nothing
        let hmi' = hmi { hmi_description = descr }
        doc'  <- overDocF (rename dflags gre) doc
        return (hmi', Just doc')

  let flags :: [LangExt.Extension]
      -- We remove the flags implied by the language setting and we display the language instead
      flags = EnumSet.toList (extensionFlags dflags) \\ languageExtensions (language dflags)
  return (hmi { hmi_safety = Just $ showPpr dflags safety
              , hmi_language = language dflags
              , hmi_extensions = flags
              } , doc)
  where
    failure = (emptyHaddockModInfo, Nothing)

-- | Takes a 'GlobalRdrEnv' which (hopefully) contains all the
-- definitions and a parsed comment and we attempt to make sense of
-- where the identifiers in the comment point to. We're in effect
-- trying to convert 'RdrName's to 'Name's, with some guesswork and
-- fallbacks in case we can't locate the identifiers.
--
-- See the comments in the source for implementation commentary.
rename :: DynFlags -> GlobalRdrEnv -> Doc RdrName -> ErrMsgM (Doc Name)
rename dflags gre = rn
  where
    rn d = case d of
      DocAppend a b -> DocAppend <$> rn a <*> rn b
      DocParagraph doc -> DocParagraph <$> rn doc
      DocIdentifier x -> do
        -- Generate the choices for the possible kind of thing this
        -- is.
        let choices = dataTcOccs x

        -- Lookup any GlobalRdrElts that match the choices.
        case concatMap (\c -> lookupGRE_RdrName c gre) choices of
          -- We found no names in the env so we start guessing.
          [] ->
            case choices of
              -- This shouldn't happen as 'dataTcOccs' always returns at least its input.
              [] -> pure (DocMonospaced (DocString (showPpr dflags x)))

              -- There was nothing in the environment so we need to
              -- pick some default from what's available to us. We
              -- diverge here from the old way where we would default
              -- to type constructors as we're much more likely to
              -- actually want anchors to regular definitions than
              -- type constructor names (such as in #253). So now we
              -- only get type constructor links if they are actually
              -- in scope.
              a:_ -> outOfScope dflags a

          -- There is only one name in the environment that matches so
          -- use it.
          [a] -> pure (DocIdentifier (gre_name a))

          -- There are multiple names available.
          gres -> ambiguous dflags x gres

      DocWarning doc -> DocWarning <$> rn doc
      DocEmphasis doc -> DocEmphasis <$> rn doc
      DocBold doc -> DocBold <$> rn doc
      DocMonospaced doc -> DocMonospaced <$> rn doc
      DocUnorderedList docs -> DocUnorderedList <$> traverse rn docs
      DocOrderedList docs -> DocOrderedList <$> traverse rn docs
      DocDefList list -> DocDefList <$> traverse (\(a, b) -> (,) <$> rn a <*> rn b) list
      DocCodeBlock doc -> DocCodeBlock <$> rn doc
      DocIdentifierUnchecked x -> pure (DocIdentifierUnchecked x)
      DocModule str -> pure (DocModule str)
      DocHyperlink l -> pure (DocHyperlink l)
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
outOfScope :: DynFlags -> RdrName -> ErrMsgM (Doc a)
outOfScope dflags x =
  case x of
    Unqual occ -> warnAndMonospace occ
    Qual mdl occ -> pure (DocIdentifierUnchecked (mdl, occ))
    Orig _ occ -> warnAndMonospace occ
    Exact name -> warnAndMonospace name  -- Shouldn't happen since x is out of scope
  where
    warnAndMonospace a = do
      tell ["Warning: '" ++ showPpr dflags a ++ "' is out of scope.\n" ++
            "    If you qualify the identifier, haddock can try to link it\n" ++
            "    it anyway."]
      pure (monospaced a)
    monospaced a = DocMonospaced (DocString (showPpr dflags a))

-- | Handle ambiguous identifiers.
--
-- Prefers local names primarily and type constructors or class names secondarily.
--
-- Emits a warning if the 'GlobalRdrElts's don't belong to the same type or class.
ambiguous :: DynFlags
          -> RdrName
          -> [GlobalRdrElt] -- ^ More than one @gre@s sharing the same `RdrName` above.
          -> ErrMsgM (Doc Name)
ambiguous dflags x gres = do
  let noChildren = map availName (gresToAvailInfo gres)
      dflt = maximumBy (comparing (isLocalName &&& isTyConName)) noChildren
      msg = "Warning: " ++ x_str ++ " is ambiguous. It is defined\n" ++
            concatMap (\n -> "    * " ++ defnLoc n ++ "\n") (map gre_name gres) ++
            "    You may be able to disambiguate the identifier by qualifying it or\n" ++
            "    by hiding some imports.\n" ++
            "    Defaulting to " ++ x_str ++ " defined " ++ defnLoc dflt
  -- TODO: Once we have a syntax for namespace qualification (#667) we may also
  -- want to emit a warning when an identifier is a data constructor for a type
  -- of the same name, but not the only constructor.
  -- For example, for @data D = C | D@, someone may want to reference the @D@
  -- constructor.
  when (length noChildren > 1) $ tell [msg]
  pure (DocIdentifier dflt)
  where
    isLocalName (nameSrcLoc -> RealSrcLoc {}) = True
    isLocalName _ = False
    x_str = '\'' : showPpr dflags x ++ "'"
    defnLoc = showSDoc dflags . pprNameDefnLoc
