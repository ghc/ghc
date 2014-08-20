
import System.IO.Error

-- test for a bug in GHC <= 4.08.2: handles were being left locked after
-- being shown in an error message.
main = do
  getContents
  catchIOError getChar (\e -> print e >> return 'x')
  catchIOError getChar (\e -> print e >> return 'x')
