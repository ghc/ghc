-- !!! Infix decls w/ infix data constructors

module ShouldFail where

infix 6 |-

ps  |-  q:qs   = undefined
