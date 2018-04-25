import Testsuite

import Data.Array.Parallel.Unlifted

$(testcases [ ""        <@ [t| ( (), Char, Bool, Int ) |]
            , "acc"     <@ [t| ( (), Int             ) |]
            , "num"     <@ [t| ( Int                 ) |]
            , "ord"     <@ [t| ( (), Char, Bool, Int ) |]
            , "enum"    <@ [t| ( (), Char, Bool, Int ) |]
            ]
  [d|
  prop_loopU_replicateU :: (UA e, Eq acc, Eq e', UA e')
               => EFL acc e e' -> acc -> Len -> e -> Bool
  prop_loopU_replicateU em start (Len n) v =
      loopU em start (replicateU n v) ==
      loopU (\a _ -> em a v) start (unitsU n)
  
  {- FIXME: disabled - too many type variables
  prop_fusion2 :: (Eq acc1, Eq acc2, Eq e1, Eq e2, Eq e3,
                   UA e1, UA e2, UA e3)
               => LoopFn acc1 e1 e2 -> LoopFn acc2 e2 e3
               -> acc1 -> acc2 -> UArr e1 -> Bool
  prop_fusion2 em1 em2 start1 start2 arr =
    loopU em2 start2 (loopArr (loopU em1 start1 arr)) ==
      let
        em (acc1 :*: acc2) e = 
          case em1 acc1 e of
  	  (acc1' :*: Nothing) -> ((acc1' :*: acc2) :*: Nothing)
  	  (acc1' :*: Just e') ->
  	    case em2 acc2 e' of
  	      (acc2' :*: res) -> ((acc1' :*: acc2') :*: res)
      in
      loopSndAcc (loopU em (start1 :*: start2) arr)
  -}

  -- missing: segmented operations
  |])

