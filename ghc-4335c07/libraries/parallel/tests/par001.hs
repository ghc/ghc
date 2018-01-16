import Control.Parallel

parfib 0 = return 1
parfib 1 = return 1
parfib n = do
              n1 <- parfib (n - 1)
              n2 <- parfib (n - 2)
              n3 <- (n1 `par` (n2 `seq` (return (n1 + n2 + 1))))
              return n3

main = do x <- parfib 30; print x
