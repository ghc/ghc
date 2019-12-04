{-# LANGUAGE TemplateHaskell, TypeApplications, ExplicitForAll, StarIsType #-}
{-# OPTIONS -Wno-star-is-type #-}

module TH_PprStar where

import Data.Proxy
import Language.Haskell.TH
import System.IO

do t <- [t| (Proxy @(*) String -> *) -> Either * ((* -> *) -> *) |]
   runIO $ do hPutStrLn stderr (pprint t)
   return []
