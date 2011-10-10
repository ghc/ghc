fib1 n = fib1' n
  where
    fib1' n = {-# SCC "nfib'" #-} nfib n -- %time: 0 (both individual and inherited)
      where
        nfib n = if n < 2 then 1 else nfib (n-1) + nfib (n-2)

fib2 n = fib2' n
fib2' n = nfib n -- %time: 100 (both individual and inherited)
  where
    nfib n = if n < 2 then 1 else nfib (n-1) + nfib (n-2)

fib3 = fib3'
fib3' n = nfib n -- %time: 100 (both individual and inherited)
  where
    nfib n = if n < 2 then 1 else nfib (n-1) + nfib (n-2)

main = do
  print (fib1 28)
  print (fib2 28)
  print (fib3 28)
