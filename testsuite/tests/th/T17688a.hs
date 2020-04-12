{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module T17688a where

import Language.Haskell.TH
import System.IO

$( do ty <- [d| {-# SPECIALISE id :: forall a -> a -> a #-} |]
      runIO $ hPutStrLn stderr $ pprint ty
      return [] )
