import Control.Monad
import Data.Binary
import Data.List

newtype A a = A [a]

instance Binary a => Binary (A a) where
    put (A xs) = case splitAt 254 xs of
        (_, []) -> mapM_ put xs
        (a, b)  -> put (A b)

    get = do xs <- replicateM 254 get
             A ys <- get
             return $ A $ xs ++ ys

main :: IO ()
main = undefined

{-
This intermittently failed with although I was never able to reliably reproduce,

$ ./inplace/bin/ghc-stage2 -O2 Test.hs -fforce-recomp
[1 of 1] Compiling Main             ( Test.hs, Test.o )
ghc-stage2: panic! (the 'impossible' happened)
  (GHC version 7.10.1.20150708 for x86_64-unknown-linux):
        Template variable unbound in rewrite rule
  sg_s5zh
  [sc_s5zf, sc_s5zg, sg_s5zh, sg_s5zi]
  [sc_s5zf, sc_s5zg, sg_s5zh, sg_s5zi]
  [: @ a_a3fv sc_s5zf sc_s5zg]
  [: @ a_a3fv sc_s5zb sc_s5zc]

Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug
-}
