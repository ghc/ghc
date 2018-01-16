-- !!! Hiding an abstract (Prelude) type
module M where

import Prelude   hiding ( Char )
import Data.Char hiding ( ord, Char )
import qualified Data.Char ( ord )

type Char = Int

ord :: Char -> Int
ord x = Data.Char.ord (chr x) + 1

