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


ppLDocName :: Qualification -> Located DocName -> Html
ppLDocName qual (L _ d) = ppDocName qual d


ppDocName :: Qualification -> DocName -> Html
ppDocName qual docName =
  case docName of
    Documented name mdl ->
      linkIdOcc mdl (Just (nameOccName name)) << ppQualifyName qual name mdl
    Undocumented name
      | isExternalName name || isWiredInName name ->
          ppQualifyName qual name (nameModule name)
      | otherwise -> ppName name


-- | Render a name depending on the selected qualification mode
ppQualifyName :: Qualification -> Name -> Module -> Html
ppQualifyName qual name mdl =
  case qual of
    NoQual   -> ppName name
    FullQual -> ppFullQualName mdl name
    LocalQual localmdl ->
      if moduleString mdl == moduleString localmdl
        then ppName name
        else ppFullQualName mdl name
    RelativeQual localmdl ->
      case List.stripPrefix (moduleString localmdl) (moduleString mdl) of
        -- local, A.x -> x
        Just []      -> ppName name
        -- sub-module, A.B.x -> B.x
        Just ('.':m) -> toHtml $ m ++ '.' : getOccString name
        -- some module with same prefix, ABC.x -> ABC.x
        Just _       -> ppFullQualName mdl name
        -- some other module, D.x -> D.x
        Nothing      -> ppFullQualName mdl name
    AliasedQual aliases localmdl ->
      case (moduleString mdl == moduleString localmdl,
            M.lookup mdl aliases) of
        (False, Just alias) -> ppQualName alias name
        _ -> ppName name


ppFullQualName :: Module -> Name -> Html
ppFullQualName mdl name = toHtml $ moduleString mdl ++ '.' : getOccString name

ppQualName :: ModuleName -> Name -> Html
ppQualName mdlName name =
  toHtml $ moduleNameString mdlName ++ '.' : getOccString name

ppName :: Name -> Html
ppName name = toHtml (getOccString name)


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
ppBinder' is_infix n = wrap $ ppOccName n
  where
    wrap | is_infix && not is_sym = quote
         | not is_infix && is_sym = parens
         | otherwise = id
    is_sym = isVarSym n || isConSym n

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
