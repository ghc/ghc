module T2182ghci_A where
data T = T deriving (Show)
instance Show (a -> b) where
    show _ = "MyFunction"
