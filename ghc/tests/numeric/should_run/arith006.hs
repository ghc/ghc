-- !!! printing Floats; was a bug in hbc (reported by andy)
--

import Int( Num(fromInt) )

main = print ((fromInt 42) :: Float)
