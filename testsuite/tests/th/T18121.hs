{-# LANGUAGE TemplateHaskell #-}
module Bug where

import Language.Haskell.TH

sapply :: Q (TExp (a -> b)) -> Q (TExp a) -> Q (TExp b)
sapply cf cx = [|| $$cf $$cx ||]
