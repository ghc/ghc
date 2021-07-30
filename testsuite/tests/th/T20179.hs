{-# LANGUAGE TemplateHaskell #-}
module Bug where

$(let slurmp :: [Maybe]
      slurmp = []

   in pure [])
