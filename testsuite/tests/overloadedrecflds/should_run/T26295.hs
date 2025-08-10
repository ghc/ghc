{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
import GHC.Records

-- large-records mangles record definitions to look like below to 
-- prevent selector function generation (even implicit ones)
data R = forall a b. (a ~ Int, b ~ Char) => MkR
   { field_a :: a
   , field_b :: b
   }

-- fields in R are naughty, so we can define custom HasField instancs for them
instance a ~ Int => HasField "field_a" R a where
    getField (MkR a _) = a

ex :: Int
ex = r.field_a
  where
    r :: R
    r = MkR 42 'x'

main :: IO ()
main = print ex
