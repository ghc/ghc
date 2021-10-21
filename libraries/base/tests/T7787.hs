import Control.Concurrent.MVar
import Control.Exception

main = do
  mv <- newMVar 'x'
  e <- try (modifyMVar mv $ \_ -> return undefined)
  let _ = e :: Either SomeExceptionWithLocation ()
  withMVar mv print -- should not hang
