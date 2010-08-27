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
  ppName, ppDocName, ppLDocName, ppRdrName,
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


ppLDocName :: Qualification -> Located DocName -> Html
ppLDocName quali (L _ d) = ppDocName quali d

-- | Render a name depending on the selected qualification mode
qualifyName :: Qualification -> DocName -> Html
qualifyName quali docName@(Documented name mdl) = case quali of
    NoQuali   -> ppName name
    FullQuali -> ppFullQualName mdl name
    -- this is just in case, it should never happen
    LocalQuali Nothing -> qualifyName FullQuali docName
    LocalQuali (Just localmdl) ->
        if (moduleString mdl == moduleString localmdl)
            then ppName name
            else ppFullQualName mdl name
    -- again, this never happens
    RelativeQuali Nothing -> qualifyName FullQuali docName
    RelativeQuali (Just localmdl) ->
        case List.stripPrefix (moduleString localmdl) (moduleString mdl) of
            -- local, A.x -> x
            Just []      -> qualifyName NoQuali docName
            -- sub-module, A.B.x -> B.x
            Just ('.':m) -> toHtml $ m ++ '.' : getOccString name
            -- some module with same prefix, ABC.x -> ABC.x
            Just _       -> qualifyName FullQuali docName
            -- some other module, D.x -> D.x
            Nothing      -> qualifyName FullQuali docName

-- this is just for exhaustiveness, but already handled by ppDocName
qualifyName _ (Undocumented name) = ppName name

ppDocName :: Qualification -> DocName -> Html
ppDocName quali docName@(Documented name mdl) =
  linkIdOcc mdl (Just occName) << qualifyName quali docName
    where occName = nameOccName name

ppDocName _ (Undocumented name) = ppName name

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


ppModule :: Module -> Html
ppModule mdl = anchor ! [href (moduleUrl mdl)]
               << toHtml (moduleString mdl)


ppModuleRef :: Module -> String -> Html
ppModuleRef mdl ref = anchor ! [href (moduleUrl mdl ++ ref)]
                      << toHtml (moduleString mdl)
    -- NB: The ref parameter already includes the '#'.
    -- This function is only called from markupModule expanding a
    -- DocModule, which doesn't seem to be ever be used.
