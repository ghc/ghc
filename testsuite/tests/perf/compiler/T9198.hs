-- Type checking time for this test used to be quadratic in the number of `a`s
-- in `main` from GHC 7.8 to 8.6. It appears to have been fixed in 8.8.
begin :: Monad m => (m () -> t) -> t
begin cont = cont (return ())

a :: IO a -> (IO () -> t) -> t
a m cont = cont (m >> putStrLn "a")

end :: t -> t
end m = m

main = begin a a a a a a a a a a a a a a a a a a end
