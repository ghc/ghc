main = do
  print 1e646457008
  print 1e646457009 -- T15271: This incorrectly printed 0.0
  print 1e1555550000 -- This is still infinity
  print 1e1000000000 -- T15271: This incorrectly printed 0.0
