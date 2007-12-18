import Control.Exception
import Control.Concurrent

main = do
  thr <- myThreadId
  evaluate $ increase_stack 1000
  throwTo thr (AsyncException ThreadKilled)
        `Control.Exception.catch` (\e -> case e of
                         AsyncException ThreadKilled -> return ()
                         _ -> throw e)
 where
  increase_stack 0 = 1
  increase_stack n = increase_stack (n-1) + n
