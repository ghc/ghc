{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module T17688b where

import Data.Kind
import Language.Haskell.TH hiding (Type)
import System.IO

$(do decs <- [d| type T :: forall (a :: Type) -> (a ~ a) => Type
                 data T x |]
     runIO $ hPutStrLn stderr $ pprint decs
     return [] )
