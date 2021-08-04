module Examples.Formatting where

import           Data.Monoid
import           Options.Applicative
import           Prelude

opts :: Parser Int
opts = option auto $ mconcat
  [ long "test"
  , short 't'
  , value 0
  , metavar "FOO_BAR_BAZ_LONG_METAVARIABLE"
  , help "This is an options with a very very long description.  Hopefully, this will be nicely formatted by the help text generator." ]
