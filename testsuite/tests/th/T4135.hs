{-# LANGUAGE TypeFamilies,TemplateHaskell #-}
module Bug where

import Language.Haskell.TH
import System.IO

class C a where
    type T a

$(do { ds <- [d|  
           	 instance C (Maybe a) where
           	    type T (Maybe a) = Char
             |]
     ; runIO $ do { putStrLn (pprint ds); hFlush stdout }
     ; return ds })

