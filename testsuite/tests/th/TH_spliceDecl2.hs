-- test splicing of quoted data and newtype declarations

module TH_spliceDecl2
where

import Language.Haskell.TH

-- splice a simple quoted declaration (x 2)
$([d| data T1 = C1 |])

$([d| newtype T2 = C2 String |])
