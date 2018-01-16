-- !!! Kind checking in a recursive situation
-- Exposes a bug in proto-4.09 (black hole)

module ShouldCompile where

data ChItem = ChItemX Stream
type Stream = ChItem 

