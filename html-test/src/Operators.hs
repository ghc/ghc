{-# LANGUAGE PatternSynonyms, TypeOperators, TypeFamilies, MultiParamTypeClasses, GADTs #-}
-- | Test operators with or without fixity declarations
module Operators where

-- | Operator with no fixity
(+-) :: a -> a -> a
a +- _ = a

-- | Operator with infixr 7
(*/) :: a -> a -> a
_ */ b = b
infixr 7 */

-- | Named function with infixl 3
foo :: a -> a -> a
foo a _ = a
infixl 3 `foo`

-- | Data type with operator constructors
data Foo
  = Foo `Bar` Foo -- ^ Has infixl 3
  | Foo :- Foo  -- ^ Has infixr 5
infixr 5 :-
infixl 3 `Bar`

-- | Pattern synonym, infixr 3
pattern (:+) a b <- [a,b]
infixr 3 :+

-- | Type name, infixl 6 and GADT constructor
data (a <-> b) where
  (:<->) :: a -> b -> a <-> b
infixl 6 <->
infixr 6 :<->

-- | Type family with fixity
type family a ++ b
infix 3 ++

-- | Data family with fixity
data family a ** b
infix 9 **

-- | Class with fixity, including associated types
class a ><> b where
  type a <>< b :: *
  data a ><< b
  (>><), (<<>) :: a -> b -> ()

  -- | Multiple fixities
  (**>), (**<), (>**), (<**) :: a -> a -> ()

infixr 1 ><>
infixl 2 <><
infixl 3 ><<
infixr 4 >><
infixl 5 <<>

infixr 8 **>, >**
infixl 8 **<, <**

-- | Type synonym with fixity
type (a >-< b) = a <-> b
infixl 6 >-<
