
-- test for a bug in GHC <= 4.08.2: handles were being left locked after
-- being shown in an error message.
main = do
  getContents
  catch getChar (\e -> print e >> return 'x')
  catch getChar (\e -> print e >> return 'x')
