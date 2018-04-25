{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module T5883 where

import Language.Haskell.TH

$( [d|
        data Unit = Unit
        instance Show Unit where
          show _ = ""
          {-# INLINE show #-}
  |])
