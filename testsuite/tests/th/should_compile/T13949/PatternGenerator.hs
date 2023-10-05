module PatternGenerator where

import Tree

import Language.Haskell.TH

templateFoo :: Name -> [Char] -> DecsQ
templateFoo _ _ = return []
