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
-----------------------------------------------------------------------------
module Haddock.Backends.Xhtml.Names (
  ppName, ppDocName, ppLDocName, ppRdrName, ppUncheckedLink,
  ppBinder, ppBinderInfix, ppBinder',
  ppModule, ppModuleRef, ppIPName, linkId, Notation(..)
) where


import Haddock.Backends.Xhtml.Utils
import Haddock.GhcUtils
import Haddock.Types
import Haddock.Utils

import Text.XHtml hiding ( name, title, p, quote )
import qualified Data.Map as M
import qualified Data.List as List

import GHC
import Name
import RdrName
import FastString (unpackFS)


-- | Indicator of how to render a 'DocName' into 'Html'
data Notation = Raw -- ^ Render as-is.
              | Infix -- ^ Render using infix notation.
              | Prefix -- ^ Render using prefix notation.
                deriving (Eq, Show)

ppOccName :: OccName -> Html
ppOccName = toHtml . occNameString


ppRdrName :: RdrName -> Html
ppRdrName = ppOccName . rdrNameOcc

ppIPName :: HsIPName -> Html
ppIPName = toHtml . unpackFS . hsIPNameFS


ppUncheckedLink :: Qualification -> (ModuleName, OccName) -> Html
ppUncheckedLink _ (mdl, occ) = linkIdOcc' mdl (Just occ) << ppOccName occ -- TODO: apply ppQualifyName


-- The Bool indicates if it is to be rendered in infix notation
ppLDocName :: Qualification -> Notation -> Located DocName -> Html
ppLDocName qual notation (L _ d) = ppDocName qual notation d

ppDocName :: Qualification -> Notation -> DocName -> Html
ppDocName qual notation docName =
  case docName of
    Documented name mdl ->
      linkIdOcc mdl (Just (nameOccName name)) << ppQualifyName qual notation name mdl
    Undocumented name
      | isExternalName name || isWiredInName name ->
          ppQualifyName qual notation name (nameModule name)
      | otherwise -> ppName notation name


-- | Render a name depending on the selected qualification mode
ppQualifyName :: Qualification -> Notation -> Name -> Module -> Html
ppQualifyName qual notation name mdl =
  case qual of
    NoQual   -> ppName notation name
    FullQual -> ppFullQualName notation mdl name
    LocalQual localmdl ->
      if moduleString mdl == moduleString localmdl
        then ppName notation name
        else ppFullQualName notation mdl name
    RelativeQual localmdl ->
      case List.stripPrefix (moduleString localmdl) (moduleString mdl) of
        -- local, A.x -> x
        Just []      -> ppName notation name
        -- sub-module, A.B.x -> B.x
        Just ('.':m) -> toHtml $ m ++ '.' : getOccString name
        -- some module with same prefix, ABC.x -> ABC.x
        Just _       -> ppFullQualName notation mdl name
        -- some other module, D.x -> D.x
        Nothing      -> ppFullQualName notation mdl name
    AliasedQual aliases localmdl ->
      case (moduleString mdl == moduleString localmdl,
            M.lookup mdl aliases) of
        (False, Just alias) -> ppQualName notation alias name
        _ -> ppName notation name


ppFullQualName :: Notation -> Module -> Name -> Html
ppFullQualName notation mdl name = wrapInfix notation (getOccName name) qname
  where
    qname = toHtml $ moduleString mdl ++ '.' : getOccString name

ppQualName :: Notation -> ModuleName -> Name -> Html
ppQualName notation mdlName name = wrapInfix notation (getOccName name) qname
  where
    qname = toHtml $ moduleNameString mdlName ++ '.' : getOccString name

ppName :: Notation -> Name -> Html
ppName notation name = wrapInfix notation (getOccName name) $ toHtml (getOccString name)


ppBinder :: Bool -> OccName -> Html
-- The Bool indicates whether we are generating the summary, in which case
-- the binder will be a link to the full definition.
ppBinder True n = linkedAnchor (nameAnchorId n) << ppBinder' Prefix n
ppBinder False n = namedAnchor (nameAnchorId n) ! [theclass "def"]
                        << ppBinder' Prefix n

ppBinderInfix :: Bool -> OccName -> Html
ppBinderInfix True n = linkedAnchor (nameAnchorId n) << ppBinder' Infix n
ppBinderInfix False n = namedAnchor (nameAnchorId n) ! [theclass "def"]
                             << ppBinder' Infix n

ppBinder' :: Notation -> OccName -> Html
ppBinder' notation n = wrapInfix notation n $ ppOccName n

wrapInfix :: Notation -> OccName -> Html -> Html
wrapInfix notation n = case notation of
  Infix | is_star_kind -> id
        | not is_sym -> quote
  Prefix | is_star_kind -> id
         | is_sym -> parens
  _ -> id
  where
    is_sym = isSymOcc n
    is_star_kind = isTcOcc n && occNameString n == "*"

linkId :: Module -> Maybe Name -> Html -> Html
linkId mdl mbName = linkIdOcc mdl (fmap nameOccName mbName)


linkIdOcc :: Module -> Maybe OccName -> Html -> Html
linkIdOcc mdl mbName = anchor ! [href url]
  where
    url = case mbName of
      Nothing   -> moduleUrl mdl
      Just name -> moduleNameUrl mdl name


linkIdOcc' :: ModuleName -> Maybe OccName -> Html -> Html
linkIdOcc' mdl mbName = anchor ! [href url]
  where
    url = case mbName of
      Nothing   -> moduleHtmlFile' mdl
      Just name -> moduleNameUrl' mdl name


ppModule :: Module -> Html
ppModule mdl = anchor ! [href (moduleUrl mdl)]
               << toHtml (moduleString mdl)


ppModuleRef :: ModuleName -> String -> Html
ppModuleRef mdl ref = anchor ! [href (moduleHtmlFile' mdl ++ ref)]
                      << toHtml (moduleNameString mdl)
    -- NB: The ref parameter already includes the '#'.
    -- This function is only called from markupModule expanding a
    -- DocModule, which doesn't seem to be ever be used.
