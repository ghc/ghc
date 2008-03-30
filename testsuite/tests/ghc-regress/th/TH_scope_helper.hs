
module TH_scope_helper where

import Language.Haskell.TH

wibble :: Q [Dec] -> Q Exp
wibble _ = [| 'a' |]
