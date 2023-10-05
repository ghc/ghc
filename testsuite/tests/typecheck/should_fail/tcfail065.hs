{-

------- Forwarded Message

Date:    Wed, 30 Nov 1994 16:34:18 +0100
From:    John Hughes <rjmh@cs.chalmers.se>
To:      augustss@cs.chalmers.se, simonpj@dcs.gla.ac.uk
Subject: Nice little program


Lennart, Simon,

You might like to look at the fun little program below.

THUMBS DOWN to hbc for compiling it (it prints [72, 101, 108, 108, 111])
THUMBS UP to ghc for rejecting it --- but what an error message!
nhc and gofer both reject it with the right error message.
I haven't tried Yale Haskell.

Enjoy!
- ----------------------------
-}

class HasX a where
  setX :: x->a->a

data X x = X x
instance HasX (X x) where
  setX x (X _) = X x

changetype x = case setX x (X (error "change type!")) of X y->y

main = print (changetype "Hello" :: [Int])

{-
------- End of Forwarded Message
-}
