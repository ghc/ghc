
import Data.Typeable

data K = K

instance Typeable K where typeRep _ = undefined
