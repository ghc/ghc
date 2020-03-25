module T11068 where

import Control.DeepSeq
import GHC.Generics
import T11068a

-- X1

instance NFData X1

x1_id :: X1 -> X1
x1_id = to . from

-- X1'

instance NFData X1'

x1'_id :: X1' -> X1'
x1'_id = to . from

-- X4

instance NFData X4

x4_id :: X4 -> X4
x4_id = to . from

-- X4'

instance NFData X4'

x4'_id :: X4' -> X4'
x4'_id = to . from

-- X8

instance NFData X8

x8_id :: X8 -> X8
x8_id = to . from

-- X8'

instance NFData X8'

x8'_id :: X8' -> X8'
x8'_id = to . from

-- X12'

instance NFData X12'

-- id for data types with strict fields fully optimizes up to 12x1
x12'_id :: X12' -> X12'
x12'_id = to . from

-- X16

instance NFData X16

x16_id :: X16 -> X16
x16_id = to . from

-- X16'

instance NFData X16'

-- X24

instance NFData X24

x24_id :: X24 -> X24
x24_id = to . from
