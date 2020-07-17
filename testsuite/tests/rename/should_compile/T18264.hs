module T18264 where

import Data.Char (isDigit)
import Data.Char (isPrint)
import Data.Char (isLetter)
import Data.Maybe (isJust)
import Data.Maybe hiding (isNothing)

import qualified Data.Char as C

test1 x = isDigit x || isLetter x
test2a = isJust
test2b = fromJust
test3 x = C.isDigit x || C.isLetter x