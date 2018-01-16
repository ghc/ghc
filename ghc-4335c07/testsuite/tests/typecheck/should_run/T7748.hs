-- Correct type, which is inferred by GHC 7.6.1 and
-- works fine with GHC 7.6.2:
-- test :: Maybe (Maybe (r -> ())) -> r -> ()

-- Incorrect type, resulting in a runtime <<loop>>,
-- inferred by GHC 7.6.2 and also accepted by GHC 7.6.1:
-- test :: a -> r -> ()

module Main where

test zd
 = let f = let g = case zd of
                       Nothing       -> const ()
                       Just Nothing  -> const ()
                       Just (Just p) -> p
           in  g
   in f

main :: IO ()
main = do
  let x = test (Just Nothing) ()
  print x



{-
There is a bug in the type checker of 7.6.1 and 7.6.2, which accepts
an incorrect type resulting in <<loop>> at runtime. Furthermore, 7.6.2
even automatically infers this incorrect type. Hence, the attached
code has the following behavior:

- No explicit type: Code works in 7.6.1, loops in 7.6.2 
- Correct explicit type: Code works fine in 7.6.1 and 7.6.2 
- Incorrect explicit type: Code loops in 7.6.1 and 7.6.2

The incorrect type has a parameter that is too polymorphic, i.e. an
"a" type is accepted while the function does a pattern match on the
parameter from which it is clear that it should be a Maybe type. In
7.4.* versions this incorrect type is not accepted.
-}