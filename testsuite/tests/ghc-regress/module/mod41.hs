-- !!! Repeated variable in instance predicate
module M where
instance Eq a => Eq (Either a a)
