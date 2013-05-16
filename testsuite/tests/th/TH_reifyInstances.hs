-- test reifyInstances

{-# LANGUAGE TypeFamilies #-}
module TH_reifyInstances where

import System.IO
import Language.Haskell.TH
import Text.PrettyPrint.HughesPJ

-- classes
class C1 a where f1 :: a

class C2 a where f2 :: a
instance C2 Int where f2 = 0
instance C2 Bool where f2 = True

-- type families
type family T1 a

type family T2 a
type instance T2 Int = Char
type instance T2 Bool = Int

-- data families
data family D1 a

data family D2 a
data instance D2 Int = DInt | DInt2
data instance D2 Bool = DBool

$(return [])

test :: ()
test = $(let
          display :: Name -> Q ()
          display n = do 
               { intTy <- [t| Int |]
               ; is1 <- reifyInstances n [intTy]
               ; runIO $ hPutStrLn stderr (nameBase n)
               ; runIO $ hPutStrLn stderr (pprint is1)
               }
        in do { display ''C1
              ; display ''C2
              ; display ''T1
              ; display ''T2
              ; display ''D1
              ; display ''D2
              ; [| () |]
              })
