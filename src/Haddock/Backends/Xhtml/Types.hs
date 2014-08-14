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
module Haddock.Backends.Xhtml.Types (
  SourceURLs, WikiURLs,
  LinksInfo,
  Splice,
  Unicode,
) where


import Data.Map
import GHC


-- the base, module and entity URLs for the source code and wiki links.
type SourceURLs = (Maybe FilePath, Maybe FilePath, Map PackageId FilePath, Map PackageId FilePath)
type WikiURLs = (Maybe FilePath, Maybe FilePath, Maybe FilePath)


-- The URL for source and wiki links
type LinksInfo = (SourceURLs, WikiURLs)

-- Whether something is a splice or not
type Splice = Bool

-- Whether unicode syntax is to be used
type Unicode = Bool
