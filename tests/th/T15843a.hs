{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module T15843a where

import Language.Haskell.TH

first_of_2 = [| (909,) |]

second_of_2 = [| (,909) |]

empty_2 = [| (,) |]

full_2 = [| (909,606) |]

third_of_3 = [| (,,909) |]

first_of_2_T = [|| (909,) ||]

second_of_2_T = [|| (,909) ||]

empty_2_T = [|| (,) ||]

full_2_T = [|| (909,606) ||]

third_of_3_T = [|| (,,909) ||]

unb0 = [| (# , #) |]

unb1 = [| (# 'c', False #) |]

unb2  = [| (# 'c', #) |]

unb3 = [| (# ,False #) |]

unb4 = [| (# ,False #) 'c' |]
