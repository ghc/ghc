module Rules.Lint
  ( lintRules
  ) where

import           Base

lintRules :: Rules ()
lintRules = "lint" ~> lint

lint :: Action ()
lint = do
    putBuild "| Running the linter…"
    lintBase
    putSuccess "| Done."

lintBase :: Action ()
lintBase =
  cmd_ "hlint -j --cpp-define x86_64_HOST_ARCH --cpp-include=./libraries/base/include/ --cpp-include=includes --cpp-include=./_build/stage1/lib/ -h libraries/base/.hlint.yaml libraries/base"
