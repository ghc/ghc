module Settings.Builders.GenPrimopCode (genPrimopCodeArgs) where

import Expression
import Predicates (builder)

-- Stdin/stdout are handled in a special way. See Rules/Actions.hs.
genPrimopCodeArgs :: Args
genPrimopCodeArgs = builder GenPrimopCode ? arg "--make-haskell-wrappers"
