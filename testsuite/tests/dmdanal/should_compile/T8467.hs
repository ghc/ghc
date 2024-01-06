module T8467 where

newtype Void = Void Void

data Decision a = Disproved (a -> Void)

bar :: Int -> Void
bar = \_ -> undefined

foo :: () -> Decision Int
foo _ = Disproved (\_ -> undefined)
