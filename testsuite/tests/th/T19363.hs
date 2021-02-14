{-# Language PatternSynonyms #-}
{-# Language TemplateHaskell #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}

module Main where

import Language.Haskell.TH

main =  runQ [d| data Operator = (:*) Int | (:**) { (^**) :: Int }

                 data (%*%) = (:%*%)
                 {-# COMPLETE (:%*%) :: (%*%) #-}
                 {-# ANN type (%*%) "yargh" #-}

                 f = (:**) { (^**) = 42 }
                 infix 5 `f`

                 (%%) :: [a] -> [a] -> [a]
                 (%%) = (++)
                 {-# INLINE (%%) #-}
                 {-# SPECIALISE (%%) :: String -> String -> String #-}
                 {-# ANN (%%) "blah" #-}

                 g (:**) { (^**) = x } = x

                 pattern a `H` b = (a, b)
                 pattern (:***) { (^***) } <- (:**) (^***) where
                   (:***) (^***) = (:**) (^***)

                 foreign import ccall unsafe "blah" (<^>) :: Int

                 type family (<%>) a
                 type (<%%>) a = a
               |] >>= putStrLn . pprint
