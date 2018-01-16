module Delay (delay,delaya) where

delay_time :: Int

#ifdef mc68020
delay_time = 18
#else
#ifdef sparc
delay_time = 48
#else /* Alpha */
--delay_time = 200
delay_time = 1
#endif
#endif

ddelay :: Int -> Int -> Int
ddelay 0 0 = 0
ddelay m 0 = ddelay (m-1) delay_time
ddelay m n = ddelay m (n-1)

delay d = ddelay d 0

delaya :: Int -> Int -> Int
delaya a d = delay d
