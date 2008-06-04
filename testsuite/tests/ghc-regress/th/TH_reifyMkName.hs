-- Trac #2339

module Foo where

import Language.Haskell.TH

type C = Int

$(do
  a <- reify $ mkName "C"
  report False $ show a
  return []
  )
