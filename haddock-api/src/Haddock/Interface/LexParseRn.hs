{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE BangPatterns #-}
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

import Data.List
import Documentation.Haddock.Doc (metaDocConcat)
import DynFlags (languageExtensions)
import qualified GHC.LanguageExtensions as LangExt
import FastString
import GHC
import Haddock.Interface.ParseModuleHeader
import Haddock.Parser
import Haddock.Types
import Name
import Outputable ( showPpr )
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

processDocStringParas :: DynFlags -> Maybe Package -> GlobalRdrEnv -> HsDocString
                      -> ErrMsgM (MDoc Name)
processDocStringParas dflags pkg gre (HsDocString fs) =
  overDocF (rename dflags gre) $ parseParas dflags pkg (unpackFS fs)

processDocString :: DynFlags -> GlobalRdrEnv -> HsDocString -> ErrMsgM (Doc Name)
processDocString dflags gre (HsDocString fs) =
  rename dflags gre $ parseString dflags (unpackFS fs)

processModuleHeader :: DynFlags -> Maybe Package -> GlobalRdrEnv -> SafeHaskellMode -> Maybe LHsDocString
                    -> ErrMsgM (HaddockModInfo Name, Maybe (MDoc Name))
processModuleHeader dflags pkgName gre safety mayStr = do
  (hmi, doc) <-
    case mayStr of
      Nothing -> return failure
      Just (L _ (HsDocString fs)) -> do
        let str = unpackFS fs
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
        -- Try to look up all the names in the GlobalRdrEnv that match
        -- the names.
        let names = concatMap (\c -> map gre_name (lookupGRE_RdrName c gre)) choices

        case names of
          -- We found no names in the env so we start guessing.
          [] ->
            case choices of
              [] -> pure (DocMonospaced (DocString (showPpr dflags x)))
              -- There was nothing in the environment so we need to
              -- pick some default from what's available to us. We
              -- diverge here from the old way where we would default
              -- to type constructors as we're much more likely to
              -- actually want anchors to regular definitions than
              -- type constructor names (such as in #253). So now we
              -- only get type constructor links if they are actually
              -- in scope.
              a:_ -> pure (outOfScope dflags a)

          -- There is only one name in the environment that matches so
          -- use it.
          [a] -> pure (DocIdentifier a)
          -- But when there are multiple names available, default to
          -- type constructors: somewhat awfully GHC returns the
          -- values in the list positionally.
          a:b:_ | isTyConName a -> pure (DocIdentifier a)
                | otherwise -> pure (DocIdentifier b)

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
outOfScope :: DynFlags -> RdrName -> Doc a
outOfScope dflags x =
  case x of
    Unqual occ -> monospaced occ
    Qual mdl occ -> DocIdentifierUnchecked (mdl, occ)
    Orig _ occ -> monospaced occ
    Exact name -> monospaced name  -- Shouldn't happen since x is out of scope
  where
    monospaced a = DocMonospaced (DocString (showPpr dflags a))
