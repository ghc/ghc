{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module T27826 where

import Language.Haskell.TH

foo :: Q Exp
foo = [| \cases x -> x |]
