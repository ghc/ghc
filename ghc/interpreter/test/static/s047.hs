--!!! Overlapping instances
module M where
instance Eq a => Eq (Either a a)
