{-# LANGUAGE NoImplicitStagePersistence #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module LiftErrMsg where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

data B = B

local_b :: [B]
local_b = [B]

test :: Code Q (a -> a)
test = [|| id ||]

test2 :: Code Q (a -> a, a -> a)
test2 = [|| (id, id) ||]

test3 :: Code Q [B]
test3 = [|| local_b ||]

test4 :: a -> Code Q a
test4 x = [|| x ||]

test5 :: Lift a => a -> Code Q a
test5 x = [|| x ||]

