{-# LANGUAGE RankNTypes, GADTs, DataKinds, PolyKinds, TypeOperators, TypeFamilies #-}
module T9222 where

import Data.Proxy

-- Nov 2014: actually the type of Want is ambiguous if we
--           do the full co/contra thing for subtyping,
--           which we now do
-- So this program is erroneous.  (But the original ticket was
-- a crash, and that's still fixed!)

data Want :: (i,j) -> * where
  Want :: (a ~ '(b,c) => Proxy b) -> Want a
