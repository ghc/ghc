{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_HADDOCK redact-type-synonyms #-}
module RedactTypeSynonyms (
  ExportedType1,
  ExportedType2,
  ExportedType3,
  ExportedType4,
  ExportedType5,
  exportedFn,
) where

import Data.Kind (Type)

-- We don't want to see a warning about this being undocumented, because it is
-- only used in 'ExportedType', which is redacted.
--
-- (Warnings aren't currently tested, but this module would be a good case for
-- it.)
data Hidden

-- We do want to see a warning here, though.
data AlsoHidden

class ThisIsHiddenToo where

-- | A type that should be redacted.
type ExportedType1 = (Bool, Hidden)

-- | Another type that should be redacted, but that shouldn't suppress the
-- warning about AlsoHidden, because it's also visible in 'exportedFn'.
type ExportedType2 a = (a, AlsoHidden)

-- | For a little kind variety.
type ExportedType3 = ThisIsHiddenToo

-- | This type does not need to be redacted.
type ExportedType4 a = (a, ExportedType2 a)

-- | This type has parameters in the result kind.
type ExportedType5 :: forall k1 k2. (k1 -> k2) -> k1 -> (k1, k2, Type)
type ExportedType5 f a = '(a, f a, Hidden)

-- | A function.
exportedFn :: Bool -> AlsoHidden
exportedFn = undefined
