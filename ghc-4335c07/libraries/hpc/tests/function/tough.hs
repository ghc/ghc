import Control.Exception as E

-- This test shows what hpc can really do.

main = do
  print ("Hello")
  foo "Hello"
  E.catch (print (badCase 22 44))
          (\ e -> print (e :: E.ErrorCall))
  E.catch (print (badCase 22 (error "Foo")))
          (\ e -> print (e :: E.ErrorCall))
  E.catch (print "Bark")
          (\ e -> print (e :: E.ErrorCall))
  (_,_) <- return $ ("Hello","World")
  return ()
  () <- return ()
  t <- case () of
    _ | otherwoz -> return "Hello"
    _             -> error "Bad Thing Happened"
  t <- case () of
    _ | otherwise -> return "Hello"
    _              -> error "Bad Thing Happened"
  t <- case () of
    _ | otherwise 
      , False      -> error "Bad Thing Happened"
    _              -> return "Hello"
  print t
  print foo2

foo x = do
  print x
  return ()

unused_ a = a

badCase :: Int -> Int -> Int
badCase a b = 
        if a > 100 
        then error "badCase" 
        else if a > 1000 
             then 1 
             else badCase (a + 1) (b - 1)


foo2 = (1,2, if True then 3 else 4)

otherwoz = True
