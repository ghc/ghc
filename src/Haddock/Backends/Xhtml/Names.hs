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
  ppModule, ppModuleRef,
  ppIPName,
  linkId
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


ppOccName :: OccName -> Html
ppOccName = toHtml . occNameString


ppRdrName :: RdrName -> Html
ppRdrName = ppOccName . rdrNameOcc

ppIPName :: HsIPName -> Html
ppIPName = toHtml . unpackFS . hsIPNameFS


ppUncheckedLink :: Qualification -> (ModuleName, OccName) -> Html
ppUncheckedLink _ (mdl, occ) = linkIdOcc' mdl (Just occ) << ppOccName occ -- TODO: apply ppQualifyName


-- The Bool indicates if it is to be rendered in infix notation
ppLDocName :: Qualification -> Maybe Bool -> Located DocName -> Html
ppLDocName qual is_infix (L _ d) = ppDocName qual is_infix d


-- The Bool indicates if it is to be rendered in infix notation
-- Nothing means print it raw, i.e. don't force it to either infix or prefix
-- TODO: instead of Maybe Bool, add a bespoke datatype
ppDocName :: Qualification -> Maybe Bool -> DocName -> Html
ppDocName qual is_infix docName =
  case docName of
    Documented name mdl ->
      linkIdOcc mdl (Just (nameOccName name)) << ppQualifyName qual is_infix name mdl
    Undocumented name
      | isExternalName name || isWiredInName name ->
          ppQualifyName qual is_infix name (nameModule name)
      | otherwise -> ppName is_infix name


-- | Render a name depending on the selected qualification mode
ppQualifyName :: Qualification -> Maybe Bool -> Name -> Module -> Html
ppQualifyName qual is_infix name mdl =
  case qual of
    NoQual   -> ppName is_infix name
    FullQual -> ppFullQualName is_infix mdl name
    LocalQual localmdl ->
      if moduleString mdl == moduleString localmdl
        then ppName is_infix name
        else ppFullQualName is_infix mdl name
    RelativeQual localmdl ->
      case List.stripPrefix (moduleString localmdl) (moduleString mdl) of
        -- local, A.x -> x
        Just []      -> ppName is_infix name
        -- sub-module, A.B.x -> B.x
        Just ('.':m) -> toHtml $ m ++ '.' : getOccString name
        -- some module with same prefix, ABC.x -> ABC.x
        Just _       -> ppFullQualName is_infix mdl name
        -- some other module, D.x -> D.x
        Nothing      -> ppFullQualName is_infix mdl name
    AliasedQual aliases localmdl ->
      case (moduleString mdl == moduleString localmdl,
            M.lookup mdl aliases) of
        (False, Just alias) -> ppQualName is_infix alias name
        _ -> ppName is_infix name


ppFullQualName :: Maybe Bool -> Module -> Name -> Html
ppFullQualName is_infix mdl name = wrapInfix is_infix (getOccName name) qname
  where
    qname = toHtml $ moduleString mdl ++ '.' : getOccString name

ppQualName :: Maybe Bool -> ModuleName -> Name -> Html
ppQualName is_infix mdlName name = wrapInfix is_infix (getOccName name) qname
  where
    qname = toHtml $ moduleNameString mdlName ++ '.' : getOccString name

ppName :: Maybe Bool -> Name -> Html
ppName is_infix name = wrapInfix is_infix (getOccName name) $ toHtml (getOccString name)


ppBinder :: Bool -> OccName -> Html
-- The Bool indicates whether we are generating the summary, in which case
-- the binder will be a link to the full definition.
ppBinder True n = linkedAnchor (nameAnchorId n) << ppBinder' False n
ppBinder False n = namedAnchor (nameAnchorId n) ! [theclass "def"]
                        << ppBinder' False n

ppBinderInfix :: Bool -> OccName -> Html
ppBinderInfix True n = linkedAnchor (nameAnchorId n) << ppBinder' True n
ppBinderInfix False n = namedAnchor (nameAnchorId n) ! [theclass "def"]
                             << ppBinder' True n

ppBinder' :: Bool -> OccName -> Html
-- The Bool indicates if it is to be rendered in infix notation
ppBinder' is_infix n = wrapInfix (Just is_infix) n $ ppOccName n

wrapInfix :: Maybe Bool -> OccName -> Html -> Html
wrapInfix Nothing _ = id
wrapInfix (Just is_infix) n | is_star_kind = id
                            | is_infix && not is_sym = quote
                            | not is_infix && is_sym = parens
                            | otherwise = id
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
