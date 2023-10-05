{-# LANGUAGE ViewPatterns #-}
module A where

import Language.Haskell.TH.Quote
import Language.Haskell.TH

foo :: QuasiQuoter
foo = QuasiQuoter{quotePat = \s -> viewP [|(*2)|] (varP . mkName $ s)}

bar :: QuasiQuoter
bar = QuasiQuoter{quotePat = \_ -> [p|((*3) -> fixed_var)|] }
