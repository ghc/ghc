{-# LANGUAGE GADTs, Haskell2010, NoExperimentalDependentTypes #-}
{-# LANGUAGE ScopedTypeVariables, TypeAbstractions, TypeApplications #-}
module LabelledValueClient (labelledValues, labelledPair) where

import LabelledValue (LabelledValue(..), LabelledPair(..))

readLabelledValue :: LabelledValue a -> (Int, a)
readLabelledValue (LabelledValue n @field (value :: field)) = (n, value)

-- | Choose the second field's type after supplying the number.
labelledValues :: ((Int, Bool), (Int, Char))
labelledValues =
    (readLabelledValue (LabelledValue 53 @Bool True),
     readLabelledValue (LabelledValue 59 @Char 'q'))

-- | Recover both field types in the constructor's written order.
labelledPair :: (Int, Bool, Char)
labelledPair =
    case LabelledPair 61 @Bool False @Char 'z' of
        LabelledPair n @l (left :: l) @r (right :: r) -> (n, left, right)
