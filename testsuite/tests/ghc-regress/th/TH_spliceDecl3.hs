-- test splicing of reified and renamed data declarations

module TH_spliceDecl3
where

import Language.Haskell.THSyntax
import TH_spliceDecl3_Lib

data T = C

$(do {d <- reifyDecl T; rename' d})
