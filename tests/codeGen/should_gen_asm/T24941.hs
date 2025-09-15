module T24941 where

data F = F
            !Float !Float !Float !Float !Float !Float !Float !Float !Float !Float
            !Float !Float !Float !Float !Float !Float !Float !Float !Float !Float
            !Float !Float !Float !Float !Float !Float !Float !Float !Float !Float
            !Float  !Float


foo     (   F
            x00 x01 x02 x03 x04 x05 x06 x07 x08 x09
            x10 x11 x12 x13 x14 x15 x16 x17 x18 x19
            x20 x21 x22 x23 x24 x25 x26 x27 x28 x29
            x30 x31
        )
    =

    F
    x00 x01 x02 x03 x04 x05 x06 x07 x08 x09
    x10 x11 x12 x13 x14 x15 x16 x17 x18 x19
    x20 x21 x22 x23 x24 x25 x26 x27 x28 x29

    x30 (x31+1)