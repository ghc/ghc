{-
Date: Thu, 22 Sep 1994 01:45:39 +0200
From: Lennart Augustsson <augustss@cs.chalmers.se>
Message-Id: <199409212345.BAA01703@statler.cs.chalmers.se>
To: glasgow-haskell-bugs@dcs.glasgow.ac.uk
Subject: ghc bug


Ghc has the wrong semantics for arrays as exemplified by this simple
program:
-}

import Array -- 1.3

main = print (array (1,1) [ (1,2), (1,3) ])

{-
As can be seen from the reference implementation in the report this
should give an error, but there is no complaint when the program
is run.

	-- Lennart
-}
