{-# LANGUAGE PatternSynonyms, TypeOperators, TypeFamilies, MultiParamTypeClasses, GADTs #-}

module ClassOperator where

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
