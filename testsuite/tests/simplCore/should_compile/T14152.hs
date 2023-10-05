module T14152 where

{-
This test case tests exitification. With exitification, the "case thunk" should
be lifted out of "innerGo", then "thunk" gets inlined, we have case-of-case and
case-of-known-constructor and the string "dead code" should disappear

The test case T14152b uses the same file, but with -fno-exitification, and checks
that the strings "dead code" is still present. If not, then some other optimization
does the job now (great!), but then we need to come up with a better test case
for exitification (or get rid of it).
-}

go 0 y = y
go n y = innerGo n y
  where
    innerGo 10 y = 0 -- we want to be lazy in thunk
    innerGo 0 y = case thunk of
        Nothing -> error "dead code"
        Just y' -> go (n-1) (y + y')
    innerGo n y = innerGo (n-1) (y*y)

    thunk = if y==0 then Just (y + y) else Just y
