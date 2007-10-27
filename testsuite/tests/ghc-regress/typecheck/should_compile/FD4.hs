{-# LANGUAGE
    MultiParamTypeClasses,
    FunctionalDependencies,
    UndecidableInstances,
    OverlappingInstances,
    FlexibleInstances,
    EmptyDataDecls #-}

-- Trac #1797

module ShouldCompile where

data True

data False

class TypeEq type1 type2 result | type1 type2 -> result where
    typeEq :: type1 -> type2 -> result

instance TypeEq soleType soleType True where
    typeEq _ _ = undefined

instance (TypeCast False result) => TypeEq type1 type2 result where
    typeEq _ _ = undefined

class TypeCast type1 type2 | type1 -> type2, type2 -> type1

instance TypeCast soleType soleType
