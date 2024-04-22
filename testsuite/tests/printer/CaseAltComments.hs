{-# LANGUAGE PatternGuards #-}
module CaseAltComments where

nfCom = case expr of
      x :*: y  -- comment
         | x' <= y'  -> x' :*: y'
      _ -> blah
