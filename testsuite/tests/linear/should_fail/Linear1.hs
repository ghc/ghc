{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LambdaCase, GADTs #-}
{-# LANGUAGE RebindableSyntax #-}
module Linear1 where


-- Must fail:
incorrectDup :: a ⊸ (a,a)
incorrectDup x = (x,x)

-- Must fail:
incorrectDrop :: a ⊸ ()
incorrectDrop x = ()
