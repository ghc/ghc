{-# LANGUAGE TemplateHaskell #-}
module T23525 where

import Language.Haskell.TH

$(sequence [withDecDoc "doc string" $ tySynD (mkName ">-:") [] [t| () |]])
