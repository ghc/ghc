-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Backends.Html.Types
-- Copyright   :  (c) Simon Marlow   2003-2006,
--                    David Waern    2006-2009,
--                    Mark Lentczner 2010
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

module Haddock.Backends.Xhtml.Types where


-- the base, module and entity URLs for the source code and wiki links.
type SourceURLs = (Maybe String, Maybe String, Maybe String)
type WikiURLs = (Maybe String, Maybe String, Maybe String)

-- The URL for source and wiki links, and the current module
type LinksInfo = (SourceURLs, WikiURLs)
