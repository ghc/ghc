import Control.Monad.X.Transformers

test2' _  = tell "1" >> throwError '2' >> return ()
test3' _  = put 1 >> throwError 2 >> return ()

test2     = do print =<< (runError $ runWriter $ test2' ())
               print =<< (runWriter $ runError $ test2' ())

test3     = do print =<< (runError $ runStateS 7 $ test3' ())
               print =<< (runStateS 7 $ runError $ test3' ())

