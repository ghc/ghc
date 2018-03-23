{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
module T13324_fail1 where

data Option a = None | Some a

deriving instance (Eq a, _) => Eq (Option a)
deriving instance (Show _) => Show (Option a)
