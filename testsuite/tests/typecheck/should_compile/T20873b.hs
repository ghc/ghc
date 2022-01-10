
{-# LANGUAGE NoDataKinds #-}

module T20873b where

import T20873b_aux (P)

type Q = P
  -- P = 'MkA is a promoted data constructor,
  -- but we should still allow users to use P with -XNoDataKinds,
  -- to avoid implementation details of M1 leaking.
