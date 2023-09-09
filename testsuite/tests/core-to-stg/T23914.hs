{-# LANGUAGE UnboxedTuples #-}
module T23914 where

type Registers = (# (), () #)

p :: Registers -> ()
p x = control0 () x

control0 :: () -> Registers -> ()
control0 x = controlWithMode x
{-# SCC control0 #-}

controlWithMode :: () -> Registers -> ()
controlWithMode x = thro x
{-# SCC controlWithMode #-}

thro :: () -> Registers -> ()
thro x y = thro x y
