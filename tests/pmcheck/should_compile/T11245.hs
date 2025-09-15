module T11245 where


foo x = do
  let a | Just i <- x
        , odd i
        = True

        | Nothing <- x
        = False

  print x
  print a
