{-# LANGUAGE RecordWildCards #-}

module T12229 where

data T = MkT { x, pi :: Float }

f x = MkT { .. }  -- 'pi' is not initialised, because
                  -- there is no local binding

g x pi = MkT { .. }  -- 'pi' is initialised

