-- !!! ds022 -- literal patterns (wimp version)
--
module ShouldCompile where

f 1 1.1 = []
f 2 2.2 = []
f 3 3.3 = []
f 4 4.4 = []

g 11111111111111111111111 1.11111111111111111 = []
g 22222222222222222222222 2.22222222222222222 = []
g 33333333333333333333333 3.33333333333333333 = []
g 44444444444444444444444 4.44444444444444444 = []

h 'a'	    ""			= []
h '\''	    "foo"		= []
h '"'	    ('b':'a':'r':[])	= []
h '\o250'   blob		= []

i 1 1.1     = []
i 2 2.2     = []
i 1 0.011e2 = []
i 2 2.20000 = []

{-
j one@1 oneone@1.1
  | ((fromFloat oneone) - (fromIntegral (fromInt one)))
	/= (fromIntegral (fromInt 0)) = []
j two@2 twotwo@2.2
  | ((fromFloat twotwo) * (fromIntegral (fromInt 2)))
	== (fromIntegral (fromInt 4.4)) = []
-}
