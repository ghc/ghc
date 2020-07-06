{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE TypeOperators          #-}

(~#) :: Comonad w => CascadeW w (t ': ts) -> w t -> Last (t ': ts)
(~#) = cascadeW
infixr 0 ~#
