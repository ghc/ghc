
module TH_reifyDecl2 where

import Language.Haskell.TH
import System.IO

$(
    do x <- reify ''Maybe
       runIO $ hPutStrLn stderr $ pprint x
       return []
 )

