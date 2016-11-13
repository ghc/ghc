{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Data.Data

data Foo = FooA | FooB

deriving instance Show Foo

deriving instance {-# Overlappable #-} Eq Foo
deriving instance {-# Overlapping  #-} Ord Foo
deriving instance {-# Overlaps     #-} Typeable Foo
deriving instance {-# Incoherent   #-} Data Foo

