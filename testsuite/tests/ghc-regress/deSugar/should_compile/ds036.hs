{-
From dmc@minster.york.ac.uk Tue Mar 10 17:15:20 1992
Via: uk.ac.york.minster; Tue, 10 Mar 92 17:15:14 GMT
Message-Id: <swordfish.700247842@minster.york.ac.uk>
From: dmc@minster.york.ac.uk
To: partain
Date:       10 Mar 1992 17:17:21 GMT

Will,

I have just started using Haskell at York and have found a compilation 
error in the code below which disappears when the last line is 
commented out
-}

{-# LANGUAGE NPlusKPatterns #-}

module ShouldCompile where

--brack :: (Eq a) => a -> a -> [a] -> ([a],[a])
--brack open close = brack' open close (1 :: Int)

brack' :: (Eq a) => a -> a -> Int -> [a] -> ([a],[a])
brack' open close 0 xs = ([],xs)
brack' open close (n+1) [] = ([],[])
brack' open close (n+1) (h:t) | h == open = ([],[])

{-
Is this something I have done wrong or a fault with the compiler?

Cheers
Dave


-----------------------------------------------------------------------
David Cattrall			Telephone +44 904 432777
Department of Computer Science	
University of York		JANET:	dmc@uk.ac.york.minster
YORK Y01 5DD
United Kingdom			UUNET:	uucp!ukc!minster!dmc
-----------------------------------------------------------------------
-}

-- and this was Kevin's idea, subsequently...

kh (n+2) x | x > n = x * 2
kh (x+1) (m+1) = m
