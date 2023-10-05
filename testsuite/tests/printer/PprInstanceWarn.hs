module PprInstanceWarning where

data D a = D a

instance {-# DEPRECATED "do not use" #-}       {-# OVERLAPPING #-}  Show (D a) where
    show (D _) = "D"

instance {-# WARNING "do not use either" #-}  {-# OVERLAPPABLE #-}  Show (D a) where
    show (D _) = "Not D"

deriving instance {-# WARNING "definitely bad" #-} Show a => Show (D a)
