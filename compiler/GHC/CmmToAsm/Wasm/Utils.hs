{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module GHC.CmmToAsm.Wasm.Utils
  ( widthMax,
    detEltsUFM,
    detEltsUniqMap,
    builderCommas,
  )
where

import Data.ByteString.Builder
import Data.List (intersperse, sortOn)
import GHC.Cmm
import GHC.Prelude
import GHC.Types.Unique.FM
import GHC.Types.Unique.Map

widthMax :: Width -> Integer
widthMax w = (1 `shiftL` widthInBits w) - 1

detEltsUFM :: Ord k => UniqFM k0 (k, a) -> [(k, a)]
detEltsUFM = sortOn fst . nonDetEltsUFM

detEltsUniqMap :: Ord k => UniqMap k a -> [(k, a)]
detEltsUniqMap = sortOn fst . nonDetUniqMapToList

builderCommas :: (a -> Builder) -> [a] -> Builder
builderCommas f xs = mconcat (intersperse ", " (map f xs))
