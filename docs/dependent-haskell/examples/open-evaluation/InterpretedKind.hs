{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, PolyKinds #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeOperators, UndecidableInstances #-}

module InterpretedKind (optionalFlag, nestedOptional) where

import Data.Kind (Type)

data Code = Value | Code :-> Code

-- | Interpret an arrow code as the kind of a type function.
type family InterpretKind (k :: Code) :: Type
type instance InterpretKind 'Value = Type
type instance InterpretKind (a ':-> b) = InterpretKind a -> InterpretKind b

data Tag :: Code -> Type where
    FlagTag :: Tag 'Value
    OptionalTag :: Tag ('Value ':-> 'Value)
    ApplyTag :: Tag (a ':-> b) -> Tag a -> Tag b

-- Check the kind interpreter before checking its uses in the next group.
$(pure [])

-- | Apply the interpreted function at the interpreted argument.
type family Interpret (t :: Tag k) :: InterpretKind k
type instance Interpret 'FlagTag = Bool
type instance Interpret 'OptionalTag = Maybe
type instance Interpret ('ApplyTag f x) = Interpret f (Interpret x)

-- | Interpret the function code as an ordinary type constructor.
optionalFlag :: Interpret 'OptionalTag (Interpret 'FlagTag)
optionalFlag = Just True

-- | Interpret two source applications as Maybe (Maybe Bool).
nestedOptional :: Interpret ('ApplyTag 'OptionalTag ('ApplyTag 'OptionalTag 'FlagTag))
nestedOptional = Just optionalFlag
