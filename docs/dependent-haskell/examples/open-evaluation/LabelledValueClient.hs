{-# LANGUAGE GADTs, Haskell2010, NoExperimentalDependentTypes #-}
{-# LANGUAGE ScopedTypeVariables, TypeAbstractions, TypeApplications #-}
module LabelledValueClient (labelledValues) where

import LabelledValue (LabelledValue(..))

readLabelledValue :: LabelledValue a -> (Int, a)
readLabelledValue (LabelledValue n @field (value :: field)) = (n, value)

-- | Choose the second field's type after supplying the number.
labelledValues :: ((Int, Bool), (Int, Char))
labelledValues =
    (readLabelledValue (LabelledValue 53 @Bool True),
     readLabelledValue (LabelledValue 59 @Char 'q'))
