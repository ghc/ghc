{-# LANGUAGE DuplicateRecordFields, ScopedTypeVariables #-}
module RecordSelectorPunUses where

data Row = MakeRow { label :: Bool }

useImplicit (value :: label) = (value :: label)

useExplicit :: forall label. label -> Row -> Bool
useExplicit _ row = label row

data First = First { duplicate :: Bool }
data Second = Second { duplicate :: Bool }

useDuplicate (value :: duplicate) = (value :: duplicate)
