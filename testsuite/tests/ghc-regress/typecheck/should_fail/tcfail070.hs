{- 
From: Wolfgang Drotschmann <drotschm@athene.informatik.uni-bonn.de>
Resent-Date:  Thu, 15 May 1997 17:23:09 +0100

I'm still using the old ghc-2.01. In one program I ran into a problem
I couldn't fix. But I played around with it, I found a small little
script which reproduces it very well:

panic! (the `impossible' happened):
	tlist
-}

module TcFail where

type State = ([Int] Bool)

