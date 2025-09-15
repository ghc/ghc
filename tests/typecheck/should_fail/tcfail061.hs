-- !! signature bugs exposed by Sigbjorn Finne
--
module ShouldFail where

type Flarp a = (b,b)

--More fun can be had if we change the signature slightly

type Bob a = a

type Flarp2 a = Bob (b,b)
