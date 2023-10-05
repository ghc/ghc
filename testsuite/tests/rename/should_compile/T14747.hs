{-# LANGUAGE PatternSynonyms, DisambiguateRecordFields #-}

module T14747 where

import T14747A

pattern T{x} = [x]

e = S { x = 42 }
