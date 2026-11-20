{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, LambdaCase #-}
module LocalAliasField (AliasField(..), aliasField) where

type WorkerArgument = Bool

-- | A local worker selects the retained field's result type.
data AliasField where
  AliasField :: (foreach (b :: Bool) ->
    (case worker b of { True -> Int; False -> Char })) -> AliasField

aliasField :: AliasField
aliasField = AliasField (\case True -> 41; False -> 'f')

worker :: WorkerArgument -> Bool
worker False = False
worker True = True
