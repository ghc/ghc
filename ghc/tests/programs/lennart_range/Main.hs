{-
Date: Thu, 22 Sep 1994 01:59:49 +0200
From: Lennart Augustsson <augustss@cs.chalmers.se>
Message-Id: <199409212359.BAA01719@statler.cs.chalmers.se>
To: glasgow-haskell-bugs@dcs.glasgow.ac.uk
Subject: ghc bug


Some floating constants that are within the floating range
become wrong, e.g. 

	1.82173691287639817263897126389712638972163e-300::Double

	-- Lennart

PS.  Maybe you use fromRational as defined in the Prelude?
That won't do.  It is badly broken, tell me if you want
one that works.
-}

-- I have turned this into a general test of extreme constants.
-- WDP 94/12

main = putStr (shows (1.82173691287639817263897126389712638972163e-300::Double) "\n")
