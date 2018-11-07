{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module T15792 where

import Data.Kind
import Language.Haskell.TH hiding (Type)
import System.IO

newtype T (f :: forall a. a -> Type) = MkT (f Bool)

$(pure [])

$(do info <- reify ''T
     runIO $ hPutStrLn stderr $ pprint info
     pure [])
