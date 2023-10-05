{-# OPTIONS_GHC -Wno-missing-fields#-}
module A where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

aquoter :: QuasiQuoter
aquoter = QuasiQuoter {quoteType = conT . mkName }
