-- !!! printing Floats; was a bug in hbc (reported by andy)
--

main = print ((fromIntegral (42 :: Int)) :: Float)
