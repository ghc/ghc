-- test splicing of a quoted data declarations

module TH_spliceDecl2
where

import Language.Haskell.THSyntax

-- splice a simple quoted declaration
$([d| data T = C |])
