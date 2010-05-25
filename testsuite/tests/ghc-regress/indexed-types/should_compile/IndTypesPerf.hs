
-- This used lots of memory, and took a long time to compile, with GHC 6.12:
-- http://www.haskell.org/pipermail/glasgow-haskell-users/2010-May/018835.html

module IndTypesPerf where

import IndTypesPerfMerge

data Rec1 = Rec1 !Int

mkRec1 v = mk $ merge v () where mk (Tagged i :* ()) = Rec1 i
