{-# LANGUAGE NoImplicitStagePersistence #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module Main where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

data B = B

local_b :: [B]
local_b = [B]

test1 :: Q Exp
test1 = [| id |]

test2 :: Q Exp
test2 = [| (id, id) |]

test3 :: Q Exp
test3 = [| local_b |]

main = do
  runQ test1
  runQ test2
  runQ test3
  return ()
