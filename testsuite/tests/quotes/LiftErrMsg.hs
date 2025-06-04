{-# LANGUAGE NoImplicitStagePersistence #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module LiftErrMsg where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

data B = B

local_b :: [B]
local_b = [B]

test :: Q Exp
test = [| id |]

test2 :: Q Exp
test2 = [| (id, id) |]

test3 :: Q Exp
test3 = [| local_b |]

test4 :: a -> Q Exp
test4 x = [| x |]

test5 :: Lift a => a -> Q Exp
test5 x = [| x |]

