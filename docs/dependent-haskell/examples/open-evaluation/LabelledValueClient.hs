{-# LANGUAGE ExplicitNamespaces, GADTs, Haskell2010, NoExperimentalDependentTypes, RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeAbstractions, TypeApplications, ViewPatterns #-}
module LabelledValueClient (labelledValues, labelledPair, storedChecks, rankedPairs, typedNotes) where

import LabelledValue (LabelledValue(..), LabelledPair(..), StoredCheck(..), RankedPair(..), TypedNote(..))

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

comparePayload :: StoredCheck a -> a -> (Int, Bool)
comparePayload (StoredCheck n @payload (value :: payload)) other =
    (n, value == other)

-- | Use the dictionary stored with each payload.
storedChecks :: ((Int, Bool), (Int, Bool))
storedChecks =
    (comparePayload (StoredCheck 67 @Bool True) True,
     comparePayload (StoredCheck 71 @Char 'a') 'b')

ascending :: Ord a => (a, a) -> Bool
ascending (x, y) = x < y

readRankedPair :: RankedPair a -> Int
readRankedPair (RankedPair (ascending -> True)) = 1
readRankedPair (RankedPair (x, _)) = x

-- | Use the stored ordering in the field pattern and the equality in the branch.
rankedPairs :: (Int, Int)
rankedPairs = (readRankedPair (RankedPair (2, 3)),
               readRankedPair (RankedPair (73, 5)))

readTypedNote :: TypedNote a -> (Int, a)
readTypedNote (TypedNote n (type a) (value :: a)) = (n, value)

-- | Apply and match the required type between the two fields.
typedNotes :: ((Int, Bool), (Int, Char))
typedNotes =
    (readTypedNote (TypedNote 79 (type Bool) True),
     readTypedNote (TypedNote 83 (type Char) 'r'))
