import Control.Monad.X.Error


t1    = test (throwError "x") (Left "x" :: Either String Int)
t2    = test (throwError "x" >>= undefined) (Left "x" :: Either String Int)
t3    = test (throwError "x" `catchError` return) (Right "x")
t4    = test (throwError "x" `catchError` throwError) (Left "x" :: Either String Int)
t5    = test (return 3 `catchError` undefined) (Right 3:: Either String Int)


test m e  = runError m == e

main  = print $ and [t1,t2,t3,t4,t5]

