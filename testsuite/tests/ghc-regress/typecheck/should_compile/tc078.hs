-- !!! instance decls with no binds
--
module ShouldFail where

data Bar a = MkBar Int a

instance Eq a => Eq (Bar a)
instance Ord a => Ord (Bar a)
