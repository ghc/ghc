--!!! instance decls with no binds
--
module M where

data Bar a = MkBar Int a

instance Eq a => Eq (Bar a)
instance Ord a => Ord (Bar a)
