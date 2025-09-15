{-# LANGUAGE TemplateHaskell,TypeOperators,DataKinds #-}

module Test10268 where

th = $footemplate

give :: b -> Pattern '[b] a
give = undefined

pfail :: Pattern '[] a
pfail = undefined
