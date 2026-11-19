module NativeClosedEnumB (Token(..), choose, score) where

import Numeric.Natural (Natural)

data Token = Done | Ready

choose :: Natural -> Token
choose 0 = Ready
choose _ = Done

score :: Token -> Natural
score Ready = 17
score Done = 12
