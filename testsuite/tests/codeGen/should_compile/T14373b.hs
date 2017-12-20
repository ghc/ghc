import T14373

{-# NOINLINE earlySwitch #-}
earlySwitch A = True
earlySwitch B = False
earlySwitch C = False

main = print $ earlySwitch B
