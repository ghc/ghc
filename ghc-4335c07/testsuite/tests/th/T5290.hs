{-# LANGUAGE TemplateHaskell #-}

module T5290 where

import Language.Haskell.TH

$( let n = mkName "T"
   in return [DataD [] n [] Nothing
                [NormalC n [(Bang SourceUnpack SourceStrict,ConT ''Int)]] []] )
