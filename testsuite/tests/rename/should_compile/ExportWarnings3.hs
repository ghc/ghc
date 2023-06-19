-- Case for explicit usage of imports + usage when not all import paths are deprecated
module ExportWarnings3 () where

import ExportWarnings_base
import ExportWarnings_aux
foo = x

bar :: S -> Int -> T
bar S1 v = T1 v
bar _ v  = T2 v


baz :: V -> V
baz = id