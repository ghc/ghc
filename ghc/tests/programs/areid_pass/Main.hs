module Main ( main ) where

main = do_actions [
	 sleep 5,
	 wrapup (_ccall_ printf ``"%d\n"'' (pass (14::Int)))
	]

do_actions :: [IO ()] -> IO ()
do_actions = foldr thenIO_ (returnIO ())

class Wrapper a where
 wrapup :: IO_Int# -> IO a

instance Wrapper () where
  wrapup a = a `thenIO_Int#` \ _ -> returnIO ()

instance Wrapper Int where
  wrapup a = a `thenIO_Int#` \ x# -> returnIO (I# x#)

instance Wrapper Char where
  wrapup a = a `thenIO_Int#` \ x# -> returnIO (toEnum (I# x#))

instance Wrapper Bool where
  wrapup a = a `thenIO_Int#` \ x# -> returnIO (x# /=# 0#)

class Pass a where
  pass :: a -> Int#

instance Pass Int where
  pass (I# i#) = i#

instance Pass Char where
  pass c = pass (fromEnum c)

instance Pass Bool where
  pass True = 0#
  pass False = 1#

sleep :: Int -> IO ()
usleep :: Int -> IO ()

sleep t = wrapup (ccall sleep (pass t))
usleep t = wrapup (ccall usleep (pass t))

