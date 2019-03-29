{-# LANGUAGE TopLevelKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module TLKS_027 where

import Data.Kind

$([d| type U :: Type
      data U = MkU
    |])
