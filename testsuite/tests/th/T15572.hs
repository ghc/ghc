{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module T15572 where

import Language.Haskell.TH

$([d| type AbsoluteUnit1 = '() |])
$(pure [TySynD (mkName "AbsoluteUnit2") [] (ConT '())])
