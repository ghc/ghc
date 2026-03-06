-- Should only report an error of kind:
--
--     Type constructor ‘T’ has conflicting definitions ...
--
-- Previously, GHC also used to emit an error regarding f:
--
--     Identifier ‘f’ has conflicting definitions ...
module T13180 (T, f) where

import qualified T13180A

type T = Int

f :: T -> T
f x = T13180A.f x
