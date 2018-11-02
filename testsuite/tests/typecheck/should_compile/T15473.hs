{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE UndecidableInstances #-}
module Bug where

type family Undefined :: k where {}

type family LetInterleave xs t ts is (a_ahkO :: [a]) (a_ahkP :: [[a]]) :: [[a]] where
  LetInterleave xs t ts is y z = Undefined y z

