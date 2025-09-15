import System.Environment
doSomething = print "hi"
doBye = return "bye"

main = do
  args <- getArgs
  y <- doBye
  let x = y:args
  doSomething
