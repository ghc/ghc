{-# OPTIONS_GHC -ddump-rn -ddump-splices #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module T7532 where

import Language.Haskell.TH
import T7532a

instance C Bool where
  data D Bool = MkD

$(bang)
