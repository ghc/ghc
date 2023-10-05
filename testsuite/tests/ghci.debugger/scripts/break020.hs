import Break020b

line1 _ = return ()
line2 _ = return ()

in_another_decl _ = do line1 0
                       line2 0

main = do
  line1 0
  line2 0
  in_another_decl 0
  in_another_module 0
  line2 1
  return ()