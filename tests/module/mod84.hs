-- !!! Correct tycon import (explicit constructor list)
module M where
import Prelude(Either(Left,Right))
x = (Left 'a', Right 'a')
