import System.Environment

-- This covers #11303, wherein the pattern checker would explode
-- when it encountered patterns sharing a prefix.

main :: IO ()
main = do
  args <- getArgs
  print $ case head args of
                      "--primop-primop-info"  -> "turtle"
                      "--primop-tag" -> "asdf"
                      "--primop-list"  -> "casdhf"
                      "--primop-vector-uniques"  -> "this"
                      "--primop-vector-tys"  -> "is"
                      "--primop-vector-tys-exports"  -> "silly"
                      "--primop-vector-tycons"  -> "hmmm"
                      "--primop-vector-turtles"  -> "hmmm"
                      "--primop-vector-plugs"  -> "hmmm"
                      "--primop-vector-caps"  -> "hmmm"
                      "--primop-vector-wires"  -> "hmmm"
                      "--primop-vector-tornado"  -> "hmmm"
                      "--primop-vector-tomato"  -> "hmmm"
                      "--primop-vector-maps"  -> "hmmm"
                      "--primop-vector-paper"  -> "hmmm"
                      "--make-haskell-wrappers" -> "123512"
                      "--make-haskell-source"  -> "as;dg"
                      "--make-latex-doc" -> "adghiw"
                      _ -> error "Should not happen, known_args out of sync?"
