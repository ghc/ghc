{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module ColInference6  where 

type family Elem c

type instance Elem [e] = e

class Col c where
  toList :: c -> [Elem c]


sumCol c = sum . toList $ c
