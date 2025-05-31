{-# LANGUAGE RecordWildCards, NamedFieldPuns, Arrows #-}

import Control.Monad.Identity
import Control.Arrow (runKleisli, arr, returnA)
import Data.Maybe
import Data.List
import Data.Bifunctor
import Trace.Hpc.Mix
import Trace.Hpc.Tix
import Trace.Hpc.Reflect

data Foo = Foo { fooA, fooB, fooC, fooD, fooE, fooF, fooG, fooH, fooI
               , fooJ, fooK, fooL, fooM, fooN, fooO :: Int }
data Bar = Bar { barFoo :: Foo }

fAB Foo{..} = fooA + fooB
fC Foo{fooC} = fooC
fD x Foo{..} = fromMaybe 0 $ if x then Just fooD else Nothing
fE Bar{barFoo = Foo{..}} = fooE
fF Foo{fooF = f} = f
fG f = let Foo{..} = f in fooG
fH f = runIdentity $ do
  Foo{..} <- pure f
  return fooH
fI f = runIdentity $ do
  let Foo{..} = f
  return fooI
fJ f = [ fooJ | let Foo{..} = f ] !! 0
fK = runIdentity . runKleisli (proc f -> do
       Foo{..} <- arr id -< f
       returnA -< fooK)
fL = runIdentity . runKleisli (proc f -> do
       let Foo{..} = f;
       returnA -< fooL)
fM f | Foo{..} <- f = fooM
fN f = fooN f
fO = runIdentity . runKleisli (proc Foo{..} -> returnA -< fooO)

recSel (n, TopLevelBox [s]) | any (`isPrefixOf` s) ["foo", "bar"] = Just (n, s)
recSel _ = Nothing

main = do
  let foo = Foo 42 23 0 1 2 3 4 5 6 7 0xaffe 9 10 11 12
  mapM_ (print . ($ foo))
        [fAB, fC, fD False, fE . Bar, fF, fG, fH, fI, fJ, fK, fL, fM, fN, fO]
  (Mix _ _ _ _ mixs) <- readMix [".hpc"] (Left "Main")
  let sels = mapMaybe recSel . zip [0..] $ map snd mixs
  (Tix [TixModule "Main" _ _ tix]) <- examineTix
  mapM_ print . sortOn snd $ map (first (tix !!)) sels
