module T9233 where

import T9233a
import Data.Functor.Identity

upds :: (Monad m) => [String -> Options -> m Options]
upds = [
  \a o -> return o { flags = (flags o) { f1 = splitComma a ++ " " ++ f1 (flags o) } }
  ]

setAll :: Options -> Options
setAll _ = (getOpt upds :: Identity ()) `seq` undefined
