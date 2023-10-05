-- !!! Infix decls w/ infix data constructors

-- GHC used to barf on this...

module ShouldCompile where

infix 2 |-, |+

ps  |-  q:qs   = undefined
ps  |+  p:q:qs = undefined
