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
  ppBinder, ppBinder',
  ppModule, ppModuleRef,
  linkId
) where


import Haddock.Backends.Xhtml.Utils
import Haddock.GhcUtils
import Haddock.Types
import Haddock.Utils

import Text.XHtml hiding ( name, title, p, quote )
import qualified Data.List as List

import GHC
import Name
import RdrName


ppOccName :: OccName -> Html
ppOccName = toHtml . occNameString


ppRdrName :: RdrName -> Html
ppRdrName = ppOccName . rdrNameOcc


ppUncheckedLink :: Qualification -> (ModuleName, OccName) -> Html
ppUncheckedLink _ (mdl, occ) = linkIdOcc' mdl (Just occ) << ppOccName occ -- TODO: apply ppQualifyName


ppLDocName :: Qualification -> Located DocName -> Html
ppLDocName qual (L _ d) = ppDocName qual d


ppDocName :: Qualification -> DocName -> Html
ppDocName qual docName =
  case docName of
    Documented name mdl ->
      linkIdOcc mdl (Just (nameOccName name)) << ppQualifyName qual name mdl
    Undocumented name -> ppQualifyName qual name (nameModule name)


-- | Render a name depending on the selected qualification mode
ppQualifyName :: Qualification -> Name -> Module -> Html
ppQualifyName qual name mdl =
  case qual of
    NoQual   -> ppName name
    FullQual -> ppFullQualName mdl name
    -- this is just in case, it should never happen
    LocalQual Nothing -> ppQualifyName FullQual name mdl
    LocalQual (Just localmdl)
      | moduleString mdl == moduleString localmdl -> ppName name
      | otherwise -> ppFullQualName mdl name
    -- again, this never happens
    RelativeQual Nothing -> ppQualifyName FullQual name mdl
    RelativeQual (Just localmdl) ->
      case List.stripPrefix (moduleString localmdl) (moduleString mdl) of
        -- local, A.x -> x
        Just []      -> ppQualifyName NoQual name mdl
        -- sub-module, A.B.x -> B.x
        Just ('.':m) -> toHtml $ m ++ '.' : getOccString name
        -- some module with same prefix, ABC.x -> ABC.x
        Just _       -> ppQualifyName FullQual name mdl
        -- some other module, D.x -> D.x
        Nothing      -> ppQualifyName FullQual name mdl


ppFullQualName :: Module -> Name -> Html
ppFullQualName mdl name = toHtml $ moduleString mdl ++ '.' : getOccString name


ppName :: Name -> Html
ppName name = toHtml (getOccString name)


ppBinder :: Bool -> OccName -> Html
-- The Bool indicates whether we are generating the summary, in which case
-- the binder will be a link to the full definition.
ppBinder True n = linkedAnchor (nameAnchorId n) << ppBinder' n
ppBinder False n = namedAnchor (nameAnchorId n) ! [theclass "def"]
                        << ppBinder' n


ppBinder' :: OccName -> Html
ppBinder' n
  | isVarSym n = parens $ ppOccName n
  | otherwise  = ppOccName n


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
