module Settings.Flavours.Common where

import Expression

-- See https://ghc.haskell.org/trac/ghc/ticket/15286 and
-- https://phabricator.haskell.org/D4880
naturalInBaseFixArgs :: Args
naturalInBaseFixArgs = mconcat
  [ input "//Natural.hs" ? pure ["-fno-omit-interface-pragmas"]
  , input "//Num.hs" ? pure ["-fno-ignore-interface-pragmas"]
  ]
