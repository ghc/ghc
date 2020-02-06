{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE RankNTypes, ImplicitParams, UnboxedTuples #-}

-- Test two slightly exotic things about type signatures

module ShouldCompile where

        -- The for-all hoisting should hoist the
        -- implicit parameter to give
        --      r :: (?param::a) => a
    r :: Int -> ((?param :: a) => a)
    r _ = error "urk"

        -- The unboxed tuple is OK because it is
        -- used on the right hand end of an arrow
    type T = (# Int, Int #)

    f :: Int -> T
    f = error "urk"
