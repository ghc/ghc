import Control.Exception

-- Ensure that catch catches exceptions thrown during the evaluation of the
-- action-to-be-executed. This should output "it failed".
main :: IO ()
main = catch (error "uh oh") handler

handler :: SomeException -> IO ()
handler _ = putStrLn "it failed"
