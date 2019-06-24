{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module T15843a where

import Language.Haskell.TH

_1_2 = [| (909,) |]

_2_2 = [| (,909) |]

_empty_2 = [| (,) |]

_full_2 = [| (909,606) |]

_3_3 = [| (,,909) |]

_1_2_T = [|| (909,) ||]

_2_2_T = [|| (,909) ||]

_empty_2_T = [|| (,) ||]

_full_2_T = [|| (909,606) ||]

_3_3_T = [|| (,,909) ||]
