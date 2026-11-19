{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, TypeFamilies, TypeOperators #-}

module InterpretedKind (optionalFlag) where

import Data.Kind (Type)

data Code = Value | Code :-> Code

type family InterpretKind (k :: Code) :: Type where
    InterpretKind 'Value = Type
    InterpretKind (a ':-> b) = InterpretKind a -> InterpretKind b

data Tag :: Code -> Type where
    FlagTag :: Tag 'Value
    OptionalTag :: Tag ('Value ':-> 'Value)

type Interpret :: Tag k -> InterpretKind k
type family Interpret t where
    Interpret 'FlagTag = Bool
    Interpret 'OptionalTag = Maybe

-- | Interpret the function code as an ordinary type constructor.
optionalFlag :: Interpret 'OptionalTag (Interpret 'FlagTag)
optionalFlag = Just True
