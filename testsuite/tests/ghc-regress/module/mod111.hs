-- !!! Hiding an abstract (Prelude) type
module M where

import Prelude hiding ( Char )
import Char    hiding ( ord, Char )
import qualified Char ( ord )

type Char = Int

ord :: Char -> Int
ord x = Char.ord (chr x) + 1

