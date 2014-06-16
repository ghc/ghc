-- test for #949: the -hy heap profile should not include
-- "stg_ap_2_upd" or similar

main = length xs `seq` return xs
  where xs = map (+1) [1..1000000]
