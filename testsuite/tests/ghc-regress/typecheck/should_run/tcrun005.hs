-- !!! Dfun naming bug

module Main where


  data TT  = TT
  data TTT = TTT

  class CC  a where
	op_cc :: a -> a
	
  class CCT a where
	op_cct :: a -> a

  -- These two instances should get different dfun names!
  -- In GHC 4.04 they both got $fCCTTT

  instance CC TTT where
	op_cc = id

  instance CCT TT where
	op_cct = id

  main = case op_cc TTT of
	   TTT -> print "ok"
