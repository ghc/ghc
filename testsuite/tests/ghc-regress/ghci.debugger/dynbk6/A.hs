import Data.Typeable
import Data.Generics

data A = A deriving (Eq,Enum,Show,Ord,Typeable,Data)

f x = id x