{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs, NamedWildCards, PartialTypeSignatures #-}
module Test25885 where

{-# SPECIALIZE bug :: Int -> Int #-}

{-# SPECIALISE
    showsIArray :: (Show i) => UA i -> String
  #-}


{-# SPECIALISE f1 :: Eq [e] => Word -> [e] -> Int #-}
{-# SPECIALISE f1_qc :: (   forall y . Eq  y => Eq ( g y ),
                        Eq ( g e ) ) => Proxy g -> g e -> Word -> Char #-}

{-# SPECIALISE f2 :: Eq c => c -> c -> Word -> Int #-}

{-# SPECIALISE f3 :: Eq c => [ c ] -> Bool #-}

{-# SPECIALISE f3 :: ( forall y. Eq y => Eq ( g y ) ) => g Int -> Bool #-}
{-# SPECIALISE f4 :: forall s b. Eq b => b -> ST s b #-}

{-# SPECIALISE f4_qc :: forall r n b. (forall m. Monad m => Monad (r m)) => r n Int -> () #-}

{-# SPECIALISE f5 :: D Int -> Bool #-}

{-# SPECIALISE f5_qc :: D Int -> Bool #-}

{-# SPECIALISE f5_qc :: ( forall y. ( Eq y, Eq ( T y ) ) => Eq ( g y ) ) => g Int -> Bool #-}
{-# SPECIALISE f6 :: Ord c => c -> c -> Word -> Char #-}

{-# SPECIALISE f6_qc :: ( forall z. Eq z => Ord ( h z ) ) => Proxy h -> Proxy h -> Word -> Char #-}

{-# SPECIALISE f7 :: Cls Bool => Int -> Int #-}

{-# SPECIALISE qcfd :: G () #-}

------------------------------------------------------------------------

{-# SPECIALISE INLINE op :: Example False -> Int #-}

$( [d| baz :: Num a => a -> a
       {-# SPECIALISE INLINE [~1] baz @Double #-}
       baz x = x * 10        |] )

{-# SPECIALISE foo @Int #-}

{-# SPECIALISE foo @Float :: Float -> Float #-}

{-# SPECIALISE foo (3 :: Int) #-}
{-# SPECIALISE foo @Int 4 #-}


{-# SPECIALISE INLINE foo @Double #-}

{-# SPECIALISE bar @Float :: Float -> Int -> Float #-}

{-# SPECIALISE bar @Double 3 :: Integer -> Double #-}

{-# SPECIALISE [1] bar @_ @Int #-}

{-# SPECIALISE bar @_a @_a #-}

{-# SPECIALISE [~1] forall a. forall. baz @a @_ @a #-}


{-# SPECIALISE tyEq :: Typeable c => Proxy c -> Proxy c -> Float #-}

{-# SPECIALISE foo :: Float -> Float,
                      Double -> Double #-}
