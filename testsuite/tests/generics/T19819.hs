{-# Language DeriveGeneric #-}
{-# Language DerivingStrategies #-}
{-# Language DerivingVia #-}

import GHC.Generics (Generic, Generically(..))

data T = T [Int] [Int] [Int] [Int] [Int] [Int]
 deriving
 stock (Show, Generic)

 deriving (Semigroup, Monoid)
 via Generically T

main :: IO ()
main = do
  print (mempty :: T)
  print (T [1,2] [3] [] [4,5,6] [7] [8] <> T [10,20] [30] [] [40,50,60] [70] [80])
