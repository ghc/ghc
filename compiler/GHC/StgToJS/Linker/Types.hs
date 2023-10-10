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
  , LinkPlan (..)
  )
where

import GHC.StgToJS.Object

import GHC.Unit.Types
import GHC.Utils.Outputable (Outputable(..),text,ppr, hang, IsDoc (vcat), IsLine (hcat))

import Data.Map.Strict      (Map)
import Data.Set             (Set)
import qualified Data.Set        as S

import System.IO

import Prelude

--------------------------------------------------------------------------------
-- Linker Config
--------------------------------------------------------------------------------

data JSLinkConfig = JSLinkConfig
  { lcNoJSExecutables :: !Bool         -- ^ Dont' build JS executables
  , lcNoHsMain        :: !Bool         -- ^ Don't generate Haskell main entry
  , lcNoRts           :: !Bool         -- ^ Don't dump the generated RTS
  , lcNoStats         :: !Bool         -- ^ Disable .stats file generation
  , lcForeignRefs     :: !Bool         -- ^ Dump .frefs (foreign references) files
  , lcCombineAll      :: !Bool         -- ^ Generate all.js (combined js) + wrappers
  , lcForceEmccRts    :: !Bool
      -- ^ Force the link with the emcc rts. Use this if you plan to dynamically
      -- load wasm modules made from C files (e.g. in iserv).
  , lcLinkCsources    :: !Bool
      -- ^ Link C sources (compiled to JS/Wasm) with Haskell code compiled to
      -- JS. This implies the use of the Emscripten RTS to load this code.
  }

data LinkPlan = LinkPlan
  { lkp_block_info :: Map Module LocatedBlockInfo
      -- ^ Block information

  , lkp_dep_blocks :: Set BlockRef
      -- ^ Blocks to link

  , lkp_archives   :: !(Set FilePath)
      -- ^ Archives to load JS and Cc sources from (JS code corresponding to
      -- Haskell code is handled with blocks above)

  , lkp_objs_js   :: !(Set FilePath)
      -- ^ JS objects to link

  , lkp_objs_cc   :: !(Set FilePath)
      -- ^ Cc objects to link
  }

instance Outputable LinkPlan where
  ppr s = hang (text "LinkPlan") 2 $ vcat
            -- Hidden because it's too verbose and it's not really part of the
            -- plan, just meta info used to retrieve actual block contents
            -- [ hcat [ text "Block info: ", ppr (lkp_block_info s)]
            [ hcat [ text "Blocks: ", ppr (S.size (lkp_dep_blocks s))]
            , hang (text "Archives:") 2 (vcat (fmap text (S.toList (lkp_archives s))))
            , hang (text "Extra JS objects:") 2 (vcat (fmap text (S.toList (lkp_objs_js s))))
            , hang (text "Extra Cc objects:") 2 (vcat (fmap text (S.toList (lkp_objs_cc s))))
            ]
