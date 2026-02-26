{-# LANGUAGE NoBlockArguments #-}
{-# LANGUAGE RecursiveDo #-}

module NoBlockArgumentsFail4 where

import Data.Tuple (swap)

fCaseOf x = (*2) case x of
  False -> 0
  True  -> 1

fIfThenElse x = (*2) if x then 1 else 0

fLetIn x = swap let y = x+1 in (x,y)

fDo x = id do x

fMDo x = id mdo x
