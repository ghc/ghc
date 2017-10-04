module A where
import GHC.Generics
data A = A deriving (Show, Generic)
data B = B A deriving (Show)
