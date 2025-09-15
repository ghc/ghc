module TH_abstractFamily where

import Language.Haskell.TH

-- Empty closed type families are okay...
ds1 :: Q [Dec]
ds1 = [d| type family F a where |]

-- ...but abstract ones should result in a type error
ds2 :: Q [Dec]
ds2 = [d| type family G a where .. |]
