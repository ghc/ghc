{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module T11068 where

import Control.DeepSeq
import GHC.Generics

import T11068a
import T11068b

-- X1

instance NFData X1

x1_id :: X1 -> X1
x1_id = to . from

x1_lens :: Lens' X1 Integer
x1_lens = gfield @"x1_f1"

-- X1'

instance NFData X1'

x1'_id :: X1' -> X1'
x1'_id = to . from

x1'_lens :: Lens' X1' Integer
x1'_lens = gfield @"x1'_f1"

-- X4

instance NFData X4

x4_id :: X4 -> X4
x4_id = to . from

x4_lens :: Lens' X4 Integer
x4_lens = gfield @"x4_f1"

-- X4'

instance NFData X4'

x4'_id :: X4' -> X4'
x4'_id = to . from

x4'_lens :: Lens' X4' Integer
x4'_lens = gfield @"x4'_f1"

-- X8

instance NFData X8

x8_id :: X8 -> X8
x8_id = to . from

x8_lens :: Lens' X8 Integer
x8_lens = gfield @"x8_f1"

-- X8'

instance NFData X8'

x8'_id :: X8' -> X8'
x8'_id = to . from

x8'_lens :: Lens' X8' Integer
x8'_lens = gfield @"x8'_f1"

-- X12'

instance NFData X12'

-- id for data types with strict fields fully optimizes up to 12x1
x12'_id :: X12' -> X12'
x12'_id = to . from

x12'_lens :: Lens' X12' Integer
x12'_lens = gfield @"x12'_f1"

-- X16

instance NFData X16

x16_id :: X16 -> X16
x16_id = to . from

x16_lens :: Lens' X16 Integer
x16_lens = gfield @"x16_f1"

-- X16'

instance NFData X16'

x16'_lens :: Lens' X16' Integer
x16'_lens = gfield @"x16'_f1"

-- X24

instance NFData X24

x24_id :: X24 -> X24
x24_id = to . from
