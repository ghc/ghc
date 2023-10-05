{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.StgToJS.Linker.Types
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Jeffrey Young  <jeffrey.young@iohk.io>
--                Luite Stegeman <luite.stegeman@iohk.io>
--                Sylvain Henry  <sylvain.henry@iohk.io>
--                Josh Meredith  <josh.meredith@iohk.io>
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module GHC.StgToJS.Linker.Types
  ( JSLinkConfig (..)
  , defaultJSLinkConfig
  , LinkedObj (..)
  , LinkPlan (..)
  )
where

import GHC.StgToJS.Object

import GHC.Unit.Types
import GHC.Utils.Outputable (hsep,Outputable(..),text,ppr, hang, IsDoc (vcat), IsLine (hcat))

import Data.Map.Strict      (Map)
import Data.Set             (Set)
import qualified Data.Set        as S

import System.IO

import Prelude

--------------------------------------------------------------------------------
-- Linker Config
--------------------------------------------------------------------------------

data JSLinkConfig = JSLinkConfig
  { lcNoJSExecutables    :: !Bool -- ^ Dont' build JS executables
  , lcNoHsMain           :: !Bool -- ^ Don't generate Haskell main entry
  , lcNoRts              :: !Bool -- ^ Don't dump the generated RTS
  , lcNoStats            :: !Bool -- ^ Disable .stats file generation
  , lcForeignRefs        :: !Bool -- ^ Dump .frefs (foreign references) files
  , lcCombineAll         :: !Bool -- ^ Generate all.js (combined js) + wrappers
  }

-- | Default linker configuration
defaultJSLinkConfig :: JSLinkConfig
defaultJSLinkConfig = JSLinkConfig
  { lcNoJSExecutables = False
  , lcNoHsMain        = False
  , lcNoRts           = False
  , lcNoStats         = False
  , lcCombineAll      = True
  , lcForeignRefs     = True
  }

data LinkPlan = LinkPlan
  { lkp_block_info :: Map Module LocatedBlockInfo
      -- ^ Block information

  , lkp_dep_blocks :: Set BlockRef
      -- ^ Blocks to link

  , lkp_archives   :: Set FilePath
      -- ^ Archives to load JS sources from

  , lkp_extra_js   :: Set FilePath
      -- ^ Extra JS files to link
  }

instance Outputable LinkPlan where
  ppr s = hang (text "LinkPlan") 2 $ vcat
            -- Hidden because it's too verbose and it's not really part of the
            -- plan, just meta info used to retrieve actual block contents
            -- [ hcat [ text "Block info: ", ppr (lkp_block_info s)]
            [ hcat [ text "Blocks: ", ppr (S.size (lkp_dep_blocks s))]
            , hang (text "JS files from archives:") 2 (vcat (fmap text (S.toList (lkp_archives s))))
            , hang (text "Extra JS:") 2 (vcat (fmap text (S.toList (lkp_extra_js s))))
            ]

--------------------------------------------------------------------------------
-- Linker Environment
--------------------------------------------------------------------------------

-- | An object file that's either already in memory (with name) or on disk
data LinkedObj
  = ObjFile   FilePath      -- ^ load from this file
  | ObjLoaded String Object -- ^ already loaded: description and payload

instance Outputable LinkedObj where
  ppr = \case
    ObjFile fp    -> hsep [text "ObjFile", text fp]
    ObjLoaded s o -> hsep [text "ObjLoaded", text s, ppr (objModuleName o)]
