{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Test10276 where

f1 = [| bar |]
f2 = [e| bar |]

class QQExp a b where
  qqExp x = [||fst $ runState $$(qqExpM x)
                        ((0,M.empty) :: (Int,M.Map L.Name [L.Operand]))||]

class QQExp2 a b where
  qqExp x = [e||fst $ runState $$(qqExpM x)
                        ((0,M.empty) :: (Int,M.Map L.Name [L.Operand]))||]
