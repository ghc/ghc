{-# LANGUAGE TemplateHaskell #-}
module Bug where

import Language.Haskell.TH

sapply :: Quote m => Code m (a -> b) -> Code m a -> Code m b
sapply cf cx = [|| $$cf $$cx ||]
