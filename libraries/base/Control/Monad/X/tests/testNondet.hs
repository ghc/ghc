import Control.Monad.X.Transformers


pr x      = inBase (putStr $ show x ++ " ")




test3' _  = pr "1" `mplus` pr "2"

-- writer & nonedt
test4' _  = tell "1" `mplus` tell "2"
test5' _  = listen (tell "1") `mplus` (tell "2" >> return ((),"77"))
test6' _  = listen mzero `mplus` (tell "2" >> return ((),"77"))
test7' _  = do (x,w) <- listen (tell "b")
               if w == "a" then mzero else return 7

law3' _     = (m >>= f >>= g, m >>= \x -> f x >>= g)
  where m   = pr "m" >> mplus (pr "1") (pr "2")
        f _ = pr "f" >> mplus (pr "3") (pr "4")
        g _ = pr "g" >> mplus (pr "5") (pr "6")

law3        = do let (lhs,rhs) = law3' ()
                 print =<< runNondets lhs
                 print =<< runNondets rhs


test8' _  = (tell "1" >> mzero) `mplus` tell "2"

main = do -- x <- runWriter $ runNondets (test8' ())
          x <- runNondet $ runWriter $ test8' ()
          print x



