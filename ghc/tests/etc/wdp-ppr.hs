{-
From: Kubiak Ryszard <fozzie>
To: partain
Subject: You may test the new pretty-printer on the following text:
Date: Wed, 2 Oct 91 18:06:05 BST
-}

data LList alpha = Nill | Conss alpha (LList alpha) 

append :: LList a -> LList a -> LList a
append xs ys  = case xs of
                  Nill -> ys
                  (Conss z zs)  -> Conss z (append zs ys)
