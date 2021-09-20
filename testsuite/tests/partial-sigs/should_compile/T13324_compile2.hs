{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
module T13324_compile where

data Option a = None | Some a

deriving instance _ => Show (Option a)
