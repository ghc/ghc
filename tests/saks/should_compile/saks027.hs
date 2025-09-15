{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module SAKS_027 where

import Data.Kind

$([d| type U :: Type
      data U = MkU
    |])
