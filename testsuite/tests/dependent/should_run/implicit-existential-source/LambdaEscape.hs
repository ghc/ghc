module LambdaEscape where

import Builder (Builder)
import Vector

use :: (Bool -> Vec n Bool) -> Bool -> Vec n Bool
use run b = run b

bad :: Builder -> Bool -> Vec n Bool
bad builder = \b -> use builder b
