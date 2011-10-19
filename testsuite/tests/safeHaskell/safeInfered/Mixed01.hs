-- | Should fail compilation because safe imports aren't enabled
module Mixed01 where

import safe Data.Word

f :: Int
f = 1

