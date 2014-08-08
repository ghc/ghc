--Testing flexible and Overlapping instances
class C a where { f :: a -> String; f _ = 3 }
instance C Int where { f = id }
:set -XFlexibleInstances
instance C [Int] where f _ = "First"
f [3::Int]
-- Should override the identical one preceding
instance C [Int] where f _ = "Second"
f [3::Int]
