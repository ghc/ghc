module CaseBinderCPR where

-- This example, taken from nofib's transform (and heavily reduced) ensures that
-- CPR information is added to a case binder

f_list_cmp::(t1 -> t1 -> Int) -> [t1] -> [t1] -> Int;
f_list_cmp a_cmp [] []= 0
f_list_cmp a_cmp [] a_ys= -1
f_list_cmp a_cmp a_xs []= 1
f_list_cmp a_cmp (a_x:a_xs) (a_y:a_ys)=
    if r_order == 0
    then f_list_cmp a_cmp a_xs a_ys
    else r_order
  where
    r_order = a_cmp a_x a_y


-- But not every case binder has the CPR property.
-- x below does not and we should not CPR nestedly for it:
g :: [Int] -> (Int, Int)
g xs = let x = xs !! 0 in x `seq` (x, x)
