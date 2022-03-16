{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
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

import Control.Arrow
import Control.Monad
import Data.Functor
import Data.List ((\\), maximumBy)
import Data.Ord
import Documentation.Haddock.Doc (metaDocConcat)
import GHC.Driver.Session (languageExtensions)
import qualified GHC.LanguageExtensions as LangExt
import GHC
import Haddock.Interface.ParseModuleHeader
import Haddock.Parser
import Haddock.Types
import GHC.Types.Name
import GHC.Types.Avail ( availName )
import GHC.Parser.PostProcess
import GHC.Driver.Ppr ( showPpr, showSDoc )
import GHC.Types.Name.Reader
import GHC.Data.EnumSet as EnumSet

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

processModuleHeader :: DynFlags -> Maybe Package -> GlobalRdrEnv -> SafeHaskellMode -> Maybe HsDocString
                    -> ErrMsgM (HaddockModInfo Name, Maybe (MDoc Name))
processModuleHeader dflags pkgName gre safety mayStr = do
  (hmi, doc) <-
    case mayStr of
      Nothing -> return failure
      Just hds -> do
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
rename :: DynFlags -> GlobalRdrEnv -> Doc NsRdrName -> ErrMsgM (Doc Name)
rename dflags gre = rn
  where
    rn d = case d of
      DocAppend a b -> DocAppend <$> rn a <*> rn b
      DocParagraph doc -> DocParagraph <$> rn doc
      DocIdentifier i -> do
        let NsRdrName ns x = unwrap i
            occ = rdrNameOcc x
            isValueName = isDataOcc occ || isVarOcc occ

        let valueNsChoices | isValueName = [x]
                           | otherwise   = [] -- is this ever possible?
            typeNsChoices  | isValueName = [setRdrNameSpace x tcName]
                           | otherwise   = [x]

        -- Generate the choices for the possible kind of thing this
        -- is. We narrow down the possibilities with the namespace (if
        -- there is one).
        let choices = case ns of
                        Value -> valueNsChoices
                        Type  -> typeNsChoices
                        None  -> valueNsChoices ++ typeNsChoices

        -- Lookup any GlobalRdrElts that match the choices.
        case concatMap (\c -> lookupGRE_RdrName c gre) choices of
          -- We found no names in the env so we start guessing.
          [] ->
            case choices of
              -- The only way this can happen is if a value namespace was
              -- specified on something that cannot be a value.
              [] -> invalidValue dflags i

              -- There was nothing in the environment so we need to
              -- pick some default from what's available to us. We
              -- diverge here from the old way where we would default
              -- to type constructors as we're much more likely to
              -- actually want anchors to regular definitions than
              -- type constructor names (such as in #253). So now we
              -- only get type constructor links if they are actually
              -- in scope.
              a:_ -> outOfScope dflags ns (i $> a)

          -- There is only one name in the environment that matches so
          -- use it.
          [a] -> pure $ DocIdentifier (i $> greMangledName a)

          -- There are multiple names available.
          gres -> ambiguous dflags i gres

      DocWarning doc -> DocWarning <$> rn doc
      DocEmphasis doc -> DocEmphasis <$> rn doc
      DocBold doc -> DocBold <$> rn doc
      DocMonospaced doc -> DocMonospaced <$> rn doc
      DocUnorderedList docs -> DocUnorderedList <$> traverse rn docs
      DocOrderedList docs -> DocOrderedList <$> traverse rn docs
      DocDefList list -> DocDefList <$> traverse (\(a, b) -> (,) <$> rn a <*> rn b) list
      DocCodeBlock doc -> DocCodeBlock <$> rn doc
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
-- still try to guess and generate anchors across modules but the
-- users shouldn't rely on this doing the right thing. See tickets
-- #253 and #375 on the confusion this causes depending on which
-- default we pick in 'rename'.
outOfScope :: DynFlags -> Namespace -> Wrap RdrName -> ErrMsgM (Doc a)
outOfScope dflags ns x =
  case unwrap x of
    Unqual occ -> warnAndMonospace (x $> occ)
    Qual mdl occ -> pure (DocIdentifierUnchecked (x $> (mdl, occ)))
    Orig _ occ -> warnAndMonospace (x $> occ)
    Exact name -> warnAndMonospace (x $> name)  -- Shouldn't happen since x is out of scope
  where
    prefix = case ns of
               Value -> "the value "
               Type -> "the type "
               None -> ""

    warnAndMonospace a = do
      let a' = showWrapped (showPpr dflags) a
      tell ["Warning: " ++ prefix ++ "'" ++ a' ++ "' is out of scope.\n" ++
            "    If you qualify the identifier, haddock can try to link it anyway."]
      pure (monospaced a')
    monospaced = DocMonospaced . DocString

-- | Handle ambiguous identifiers.
--
-- Prefers local names primarily and type constructors or class names secondarily.
--
-- Emits a warning if the 'GlobalRdrElts's don't belong to the same type or class.
ambiguous :: DynFlags
          -> Wrap NsRdrName
          -> [GlobalRdrElt] -- ^ More than one @gre@s sharing the same `RdrName` above.
          -> ErrMsgM (Doc Name)
ambiguous dflags x gres = do
  let noChildren = map availName (gresToAvailInfo gres)
      dflt = maximumBy (comparing (isLocalName &&& isTyConName)) noChildren
      msg = "Warning: " ++ showNsRdrName dflags x ++ " is ambiguous. It is defined\n" ++
            concatMap (\n -> "    * " ++ defnLoc n ++ "\n") (map greMangledName gres) ++
            "    You may be able to disambiguate the identifier by qualifying it or\n" ++
            "    by specifying the type/value namespace explicitly.\n" ++
            "    Defaulting to the one defined " ++ defnLoc dflt
  -- TODO: Once we have a syntax for namespace qualification (#667) we may also
  -- want to emit a warning when an identifier is a data constructor for a type
  -- of the same name, but not the only constructor.
  -- For example, for @data D = C | D@, someone may want to reference the @D@
  -- constructor.
  when (length noChildren > 1) $ tell [msg]
  pure (DocIdentifier (x $> dflt))
  where
    isLocalName (nameSrcLoc -> RealSrcLoc {}) = True
    isLocalName _ = False
    defnLoc = showSDoc dflags . pprNameDefnLoc

-- | Handle value-namespaced names that cannot be for values.
--
-- Emits a warning that the value-namespace is invalid on a non-value identifier.
invalidValue :: DynFlags -> Wrap NsRdrName -> ErrMsgM (Doc a)
invalidValue dflags x = do
  tell ["Warning: " ++ showNsRdrName dflags x ++ " cannot be value, yet it is\n" ++
            "    namespaced as such. Did you mean to specify a type namespace\n" ++
            "    instead?"]
  pure (DocMonospaced (DocString (showNsRdrName dflags x)))

-- | Printable representation of a wrapped and namespaced name
showNsRdrName :: DynFlags -> Wrap NsRdrName -> String
showNsRdrName dflags = (\p i -> p ++ "'" ++ i ++ "'") <$> prefix <*> ident
  where
    ident = showWrapped (showPpr dflags . rdrName)
    prefix = renderNs . namespace . unwrap
