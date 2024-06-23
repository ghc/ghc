-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Haddock.Backends.Html.Names
-- Copyright   :  (c) Simon Marlow   2003-2006,
--                    David Waern    2006-2009,
--                    Mark Lentczner 2010
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
module Haddock.Backends.Xhtml.Names
  ( ppName
  , ppDocName
  , ppLDocName
  , ppRdrName
  , ppUncheckedLink
  , ppBinder
  , ppBinderInfix
  , ppBinder'
  , ppModule
  , ppModuleRef
  , ppIPName
  , linkId
  , Notation (..)
  , ppWrappedDocName
  , ppWrappedName
  ) where

import Data.List (stripPrefix)
import GHC hiding (LexicalFixity (..), anchor)
import GHC.Data.FastString (unpackFS)
import GHC.Types.Name
import GHC.Types.Name.Reader
import Text.XHtml hiding (name, p, quote)

import Haddock.Backends.Xhtml.Utils
import Haddock.GhcUtils
import Haddock.Types
import Haddock.Utils

-- | Indicator of how to render a 'DocName' into 'Html'
data Notation
  = -- | Render as-is.
    Raw
  | -- | Render using infix notation.
    Infix
  | -- | Render using prefix notation.
    Prefix
  deriving (Eq, Show)

ppOccName :: OccName -> Html
ppOccName = toHtml . occNameString

ppRdrName :: RdrName -> Html
ppRdrName = ppOccName . rdrNameOcc

ppIPName :: HsIPName -> Html
ppIPName = toHtml . ('?' :) . unpackFS . hsIPNameFS

ppUncheckedLink :: Qualification -> Wrap (ModuleName, OccName) -> Html
ppUncheckedLink _ x = linkIdOcc' mdl (Just occ) << occHtml
  where
    (mdl, occ) = unwrap x
    occHtml = toHtml (showWrapped (occNameString . snd) x) -- TODO: apply ppQualifyName

-- The Bool indicates if it is to be rendered in infix notation
ppLDocName :: Qualification -> Notation -> GenLocated l DocName -> Html
ppLDocName qual notation (L _ d) = ppDocName qual notation True d

ppDocName :: Qualification -> Notation -> Bool -> DocName -> Html
ppDocName qual notation insertAnchors docName =
  case docName of
    Documented name mdl ->
      linkIdOcc mdl (Just (nameOccName name)) insertAnchors
        << ppQualifyName qual notation name mdl
    Undocumented name
      | isExternalName name || isWiredInName name ->
          ppQualifyName qual notation name (nameModule name)
      | otherwise -> ppName notation name

ppWrappedDocName :: Qualification -> Notation -> Bool -> Wrap DocName -> Html
ppWrappedDocName qual notation insertAnchors docName = case docName of
  Unadorned n -> ppDocName qual notation insertAnchors n
  Parenthesized n -> ppDocName qual Prefix insertAnchors n
  Backticked n -> ppDocName qual Infix insertAnchors n

ppWrappedName :: Notation -> Wrap Name -> Html
ppWrappedName notation docName = case docName of
  Unadorned n -> ppName notation n
  Parenthesized n -> ppName Prefix n
  Backticked n -> ppName Infix n

-- | Render a name depending on the selected qualification mode
ppQualifyName :: Qualification -> Notation -> Name -> Module -> Html
ppQualifyName qual notation name mdl =
  case qual of
    NoQual -> ppName notation name
    FullQual -> ppFullQualName notation mdl name
    LocalQual localmdl ->
      if moduleString mdl == moduleString localmdl
        then ppName notation name
        else ppFullQualName notation mdl name
    RelativeQual localmdl ->
      case stripPrefix (moduleString localmdl) (moduleString mdl) of
        -- local, A.x -> x
        Just [] -> ppName notation name
        -- sub-module, A.B.x -> B.x
        Just ('.' : m) -> toHtml $ m ++ '.' : getOccString name
        -- some module with same prefix, ABC.x -> ABC.x
        Just _ -> ppFullQualName notation mdl name
        -- some other module, D.x -> D.x
        Nothing -> ppFullQualName notation mdl name

ppFullQualName :: Notation -> Module -> Name -> Html
ppFullQualName notation mdl name = wrapInfix notation (getOccName name) qname
  where
    qname = toHtml $ moduleString mdl ++ '.' : getOccString name

ppName :: Notation -> Name -> Html
ppName notation name =
  case m_pun of
    Just str -> toHtml (unpackFS str) -- use the punned form
    Nothing ->
      wrapInfix notation (getOccName name) $
        toHtml (getOccString name) -- use the original identifier
  where
    m_pun = case notation of
      Raw -> namePun_maybe name
      Prefix -> namePun_maybe name
      Infix -> Nothing

ppBinder :: Bool -> OccName -> Html
ppBinder = ppBinderWith Prefix

ppBinderInfix :: Bool -> OccName -> Html
ppBinderInfix = ppBinderWith Infix

ppBinderWith :: Notation -> Bool -> OccName -> Html
-- 'isRef' indicates whether this is merely a reference from another part of
-- the documentation or is the actual definition; in the latter case, we also
-- set the 'id' and 'class' attributes.
ppBinderWith notation isRef n =
  makeAnchor << ppBinder' notation n
  where
    name = nameAnchorId n
    makeAnchor
      | isRef = linkedAnchor name
      | otherwise = namedAnchor name ! [theclass "def"]

ppBinder' :: Notation -> OccName -> Html
ppBinder' notation n = wrapInfix notation n $ ppOccName n

wrapInfix :: Notation -> OccName -> Html -> Html
wrapInfix notation n = case notation of
  Infix | not is_sym -> quote
  Prefix | is_sym -> parens
  _ -> id
  where
    is_sym = isSymOcc n

linkId :: Module -> Maybe Name -> Html -> Html
linkId mdl mbName = linkIdOcc mdl (fmap nameOccName mbName) True

linkIdOcc :: Module -> Maybe OccName -> Bool -> Html -> Html
linkIdOcc mdl mbName insertAnchors =
  if insertAnchors
    then anchor ! [href url, title ttl]
    else id
  where
    ttl = moduleNameString (moduleName mdl)
    url = case mbName of
      Nothing -> moduleUrl mdl
      Just name -> moduleNameUrl mdl name

linkIdOcc' :: ModuleName -> Maybe OccName -> Html -> Html
linkIdOcc' mdl mbName = anchor ! [href url, title ttl]
  where
    ttl = moduleNameString mdl
    url = case mbName of
      Nothing -> moduleHtmlFile' mdl
      Just name -> moduleNameUrl' mdl name

ppModule :: Module -> Html
ppModule mdl =
  anchor
    ! [href (moduleUrl mdl)]
    << toHtml (moduleString mdl)

ppModuleRef :: Maybe Html -> ModuleName -> String -> Html
ppModuleRef Nothing mdl ref =
  anchor
    ! [href (moduleHtmlFile' mdl ++ ref)]
    << toHtml (moduleNameString mdl)
ppModuleRef (Just lbl) mdl ref =
  anchor
    ! [href (moduleHtmlFile' mdl ++ ref)]
    << lbl

-- NB: The ref parameter already includes the '#'.
-- This function is only called from markupModule expanding a
-- DocModule, which doesn't seem to be ever be used.
