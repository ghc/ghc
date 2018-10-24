{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TypeFamilies #-}
module KindSigs where

import Data.Kind

-- Kind annotation on type family instance equation
type family Foo a where
  Foo a = Int :: Type

-- Kind annotation on component of tuple type
type Bar a = ( Int :: Type, Bool, Maybe a :: Type )
type Bar' a = (# Int :: Type, Bool, Maybe a :: Type #)

-- Kind annotation on type of list
type Baz = [ Int :: Type ]

-- Kind annotation inside paren type
qux :: (Int :: Type) -> Bool -> (() :: Type)
qux _ _ = ()

-- Kind annotation on promoted lists and tuples
type Quux = '[ True :: Bool ]
type Quux' = [ True :: Bool, False :: Bool  ]
type Quuux b = '( [Int, Bool] :: [Type], b )

-- Kind annotation on the RHS of a type synonym
type Sarsaparilla = Int :: Type

-- Note that 'true :: Bool :: Type' won't parse - you need some parens
true :: (Bool :: Type)
true = True
