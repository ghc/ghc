import Control.Exception

main :: IO ()
main = do
  let msg = "no trailing whitespace"
  fail msg `catch` \(e :: SomeException) -> do
    putStrLn (displayException e)

