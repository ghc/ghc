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
module Haddock.Backends.Xhtml.Names where

import Haddock.Backends.Xhtml.Util
import Haddock.GhcUtils
import Haddock.Types
import Haddock.Utils

import Text.XHtml hiding ( name, title, p, quote )

import GHC
import Name
import RdrName

ppOccName :: OccName -> Html
ppOccName = toHtml . occNameString

ppRdrName :: RdrName -> Html
ppRdrName = ppOccName . rdrNameOcc

ppLDocName :: Located DocName -> Html
ppLDocName (L _ d) = ppDocName d

ppDocName :: DocName -> Html
ppDocName (Documented name mdl) = 
  linkIdOcc mdl (Just occName) << ppOccName occName
    where occName = nameOccName name
ppDocName (Undocumented name) = toHtml (getOccString name)

linkTarget :: OccName -> Html
linkTarget n = namedAnchor (anchorNameStr n) << toHtml "" 

ppName :: Name -> Html
ppName name = toHtml (getOccString name)


ppBinder :: Bool -> OccName -> Html
-- The Bool indicates whether we are generating the summary, in which case
-- the binder will be a link to the full definition.
ppBinder True n = linkedAnchor (anchorNameStr n) << ppBinder' n
ppBinder False n = linkTarget n +++ bold << ppBinder' n


ppBinder' :: OccName -> Html
ppBinder' n
  | isVarSym n = parens $ ppOccName n
  | otherwise  = ppOccName n


linkId :: Module -> Maybe Name -> Html -> Html
linkId mdl mbName = linkIdOcc mdl (fmap nameOccName mbName)


linkIdOcc :: Module -> Maybe OccName -> Html -> Html
linkIdOcc mdl mbName = anchor ! [href uri]
  where 
    uri = case mbName of
      Nothing   -> moduleHtmlFile mdl
      Just name -> nameHtmlRef mdl name

ppModule :: Module -> String -> Html
ppModule mdl ref = anchor ! [href ((moduleHtmlFile mdl) ++ ref)] 
                   << toHtml (moduleString mdl)

