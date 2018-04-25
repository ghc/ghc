-- Trac #3263.  New kind of warning on ignored monadic bindings

module T3263 where

nullM :: IO ()
nullM = return ()

nonNullM :: IO Int
nonNullM = return 10

-- No warning
t1 = do
  nonNullM

-- No warning
t2 = nonNullM

-- No warning
t3 = do
  nullM
  nonNullM

-- Warning
t4 = do
  nonNullM
  nullM

-- No warning
t5 = do
  _ <- nonNullM
  nullM

-- Warning
t6 = mdo
  nonNullM
  nullM