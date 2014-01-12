module ShouldSucceed where

-- !!! monomorphism restriction and defaulting

x = 3

main = print $ 6 / x

{-
Hugs 1.4 complains: ERROR "Strange.hs" (line 3): Int is not an
instance of class "Fractional".  GHC however compiles the program.
Substitute for x and Hugs is happy.  What's going on?

I haven't studied the numeric classes much so perhaps I'm missing
something obvious here.  (I see that the bugs page alludes to some 1.4
features not in Hugs leading to type errors.  If this is it, maybe you
should give it as an example?)

  Bjarte

------- Message 2

Date:    Wed, 25 Feb 98 14:01:35 -0500
From:    "John C. Peterson" <peterson-john@CS.YALE.EDU>
To:      bjartem@idi.ntnu.no
cc:      hugs-bugs@CS.YALE.EDU
Subject: Re: Fractional and Int?

This is a known hugs bug.  x should be monomorphic, allowing the usage
in main to constrain it to Fractional.  Instead, it is generalized and
then defaulted to Int without being influenced by main.  So ghc is
right and hugs is wrong on this one.  I expect this will be fixed
eventually. 

   John
-}
