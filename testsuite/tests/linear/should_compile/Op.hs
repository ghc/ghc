{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Op where
{-
See Control.Arrow and Data.Functor.Contravariant
-}

import GHC.Base

class Or p where
    or :: p a b -> p a b -> p a b

instance Or (->) where
    or x _ = x


foo = or Just (\x -> Just x)

{-
This caused an error in the earlier version of linear types:

linear-tests/Op.hs:18:16: error:
    • Couldn't match expected type ‘a ⊸ Maybe a’
                  with actual type ‘a0 -> Maybe a0’
    • The lambda expression ‘\ x -> Just x’ has one argument,
      its type is ‘p0 a b0’,
      it is specialized to ‘a ⊸ Maybe a’
      In the second argument of ‘or’, namely ‘(\ x -> Just x)’
      In the expression: or Just (\ x -> Just x)
    • Relevant bindings include
        foo :: a ⊸ Maybe a (bound at linear-tests/Op.hs:18:1)
   |
18 | foo = or Just (\x -> Just x)
   |                ^^^^^^^^^^^^
-}
