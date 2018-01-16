import qualified Data.Vector as U
import Data.Bits
main = print . U.sum $ unfoldU 10000 k (0::Int)
    where
        k b = JustS (b :*: b+1) -- enumFromTo
