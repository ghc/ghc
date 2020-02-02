{-# LANGUAGE RankNTypes, GADTs, DataKinds, PolyKinds, TypeOperators, TypeFamilies #-}
module T9222 where

import Data.Kind
import Data.Proxy

-- Nov 2014: actually the type of Want is ambiguous if we
--           do the full co/contra thing for subtyping,
--           which we now do
-- So this program is erroneous.  (But the original ticket was
-- a crash, and that's still fixed!)

-- Apr 2020: with simple subsumption (#17775), the type isn't
-- ambiguous any more

data Want :: (i,j) -> Type where
  Want :: (a ~ '(b,c) => Proxy b) -> Want a
