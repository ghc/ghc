{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module GHC.Toolchain.Tools.Readelf (Readelf(..), findReadelf) where

import Control.Monad

import GHC.Toolchain.Prelude
import GHC.Toolchain.Program

data Readelf = Readelf { readelfProgram :: Program
                       }
    deriving (Show, Read)

-- | Readelf is only needed by 'GHC.Toolchain.Tools.Link.checkBfdCopyBug'.
findReadelf :: ProgOpt -> M Readelf
findReadelf progOpt = checking "for 'readelf'" $ do
    readelfProgram <- findProgram "readelf utility" progOpt ["readelf"]
    return Readelf {..}

