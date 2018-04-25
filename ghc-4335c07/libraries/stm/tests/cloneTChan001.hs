import Control.Concurrent.STM

main = do
  c <- newTChanIO
  atomically $ writeTChan c 'a'
  c1 <- atomically $ cloneTChan c
  atomically (readTChan c) >>= print
  atomically (readTChan c1) >>= print
  atomically (writeTChan c 'b')
  atomically (readTChan c) >>= print
  atomically (readTChan c1) >>= print
