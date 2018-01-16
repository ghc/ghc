{-# LANGUAGE DeriveGeneric #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.BuildReports.Types
-- Copyright   :  (c) Duncan Coutts 2009
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Types related to build reporting
--
-----------------------------------------------------------------------------
module Distribution.Client.BuildReports.Types (
    ReportLevel(..),
  ) where

import qualified Distribution.Text as Text
         ( Text(..) )

import qualified Distribution.Compat.ReadP as Parse
         ( pfail, munch1 )
import qualified Text.PrettyPrint as Disp
         ( text )

import Data.Char as Char
         ( isAlpha, toLower )
import GHC.Generics (Generic)
import Distribution.Compat.Binary (Binary)


data ReportLevel = NoReports | AnonymousReports | DetailedReports
  deriving (Eq, Ord, Enum, Show, Generic)

instance Binary ReportLevel

instance Text.Text ReportLevel where
  disp NoReports        = Disp.text "none"
  disp AnonymousReports = Disp.text "anonymous"
  disp DetailedReports  = Disp.text "detailed"
  parse = do
    name <- Parse.munch1 Char.isAlpha
    case lowercase name of
      "none"       -> return NoReports
      "anonymous"  -> return AnonymousReports
      "detailed"   -> return DetailedReports
      _            -> Parse.pfail

lowercase :: String -> String
lowercase = map Char.toLower
