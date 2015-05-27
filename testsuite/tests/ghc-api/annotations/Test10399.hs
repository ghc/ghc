{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test10399 where

type MPI = ?mpi_secret :: MPISecret

mkPoli = mkBila . map ((,,(),,()) <$> P.base <*> P.pos <*> P.form)

data MaybeDefault v where
    SetTo :: forall v . ( Eq v, Show v ) => !v -> MaybeDefault v
    SetTo4 :: forall v a. (( Eq v, Show v ) => v -> MaybeDefault v
                                            -> a -> MaybeDefault [a])

[t| Map.Map T.Text $tc |]

bar $( [p| x |] ) = x
