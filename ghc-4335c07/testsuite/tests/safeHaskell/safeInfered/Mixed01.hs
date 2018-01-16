-- | Should fail compilation because safe imports aren't enabled
-- not because of trying to import an unsafe module
module Mixed01 where

import safe System.IO.Unsafe

f :: Int
f = 1

