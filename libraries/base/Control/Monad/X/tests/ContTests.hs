import Control.Monad.X.Transformers

test00' _ = do a <- local (+1) ask
               b <- ask
               return (a,b)


test0' _  = do a <- callCC $ \jmp -> local (+1) ask 
               b <- ask
               return (a,b)

-- this illustrates an interesting phenomenon.
-- if the reader is there before continuations,
-- jumping will not undo "local" changes to the environment,
-- and they will be seen in the continuation.
-- this happens because the jump is within the scope 
-- of the local. 
test1' _  = do a <- callCC $ \jmp -> local (+1) (ask >>= jmp)
               b <- ask
               return (a,b)

              
test2' _  = callCC $ \jmp -> tell [1] >> jmp 2

-- what should this do?
test22' _ = do (a,w) <- callCC $ \jmp -> tell [1] >> listen (jmp (3,[])) 
               tell [2]
               return (a,w)


output w  = do x <- get
               put (mappend x w)

list m    = do w <- get
               put mempty
               a <- m        -- this is wrong if m jumps as it will delete all output
               w' <- get
               put w
               return (a,w') 


test32' _ = do (a,w) <- callCC $ \jmp -> output "1" >> {-list-} (jmp (3,"")) 
               output "2"
               return (a,w)

test33' _ = do (a,w) <- callCC $ \jmp -> output "1" >> list (output "7")
               output "2"
               return (a,w)



test3' _  = callCC $ \jmp -> put 1 >> jmp 2


test00    = do print =<< (runCont $ runReader 7 $ test00' ())
               print =<< (runReader 7 $ runCont $ test00' ())

test0     = do print =<< (runCont $ runReader 7 $ test0' ())
               print =<< (runReader 7 $ runCont $ test0' ())

test1     = do print =<< (runCont $ runReader 7 $ test1' ())
               print =<< (runReader 7 $ runCont $ test1' ())

test2     = do print =<< (runCont $ runWriter $ test2' ())
               print =<< (runWriter $ runCont $ test2' ())

test3     = do print =<< (runCont $ runStateS 7 $ test3' ())
               print =<< (runStateS 7 $ runCont $ test3' ())

test32    = do print =<< (runCont $ runStateS [] $ test32' ())
               print =<< (runStateS [] $ runCont $ test32' ())

test33    = do print =<< (runCont $ runStateS [] $ test33' ())
               print =<< (runStateS [] $ runCont $ test33' ())
