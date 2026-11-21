{-# LANGUAGE LiberalTypeSynonyms, NoExperimentalDependentTypes, RankNTypes #-}
module LiberalCallback (collectValues) where

type Callback input output = forall a. input a -> output a
type Identity a = a

collect :: Callback Identity []
collect value = [value]

-- | Use one higher-order synonym with a partial synonym and a constructor.
collectValues :: ([Bool], String)
collectValues = (collect True, collect 'q')
