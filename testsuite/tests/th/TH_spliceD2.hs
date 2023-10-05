module TH_spliceD2 where

import qualified TH_spliceD2_Lib

$( [d| data T = T TH_spliceD2_Lib.T |] )
