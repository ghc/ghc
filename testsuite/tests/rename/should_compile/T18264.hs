module T18264 where

import Data.Char (isDigit)
import Data.Maybe (isJust)
import Data.Char (isPrint)
import Data.List (sortOn)
import Data.Char (isLetter)
import Data.Maybe hiding (isNothing)

import qualified Data.List as S (sort)
import qualified Data.Char as C --only isDigit & isLetter used later
import qualified Data.List as T (nub)

test1 x = isDigit x || isLetter x
test2a = isJust
test2b = fromJust
test3 x = C.isDigit x || C.isLetter x
test4 xs = S.sort xs
test5 xs = T.nub xs
test6 f xs = sortOn f xs
