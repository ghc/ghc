{-
From: Jon Hill <hilly@dcs.qmw.ac.uk@jess.gla.ac.uk@pp.dcs.glasgow.ac.uk>
To: glasgow-haskell-bugs
Subject: Unfriendly error message
Date: Thu, 25 Jun 1992 09:22:55 +0100

Hello again,

I came across a rather nasty error message when I gave a function an
incorrect type signature (the context is wrong). I can remember reading 
in the source about this problem - I just thought I'd let you know anyway :-)
-}
module ShouldFail where


test::(Num a, Eq a) => a -> Bool
test x = (x `mod` 3) == 0

{-
granite> ndph bug002.ldh
Data Parallel Haskell Compiler, version 0.01 (Glasgow 0.07)

 
"<unknown>", line <unknown>: Cannot express dicts in terms of dictionaries available:
dicts_encl:
   "<built-in>", line : dict.87 :: <Num a>
   "<built-in>", line : dict.88 :: <Eq a>
dicts_encl':
   "<built-in>", line : dict.87 :: <Num a>
   "<built-in>", line : dict.88 :: <Eq a>
dicts:
   "<built-in>", line : dict.87 :: <Num a>
   "<built-in>", line : dict.88 :: <Eq a>
super_class_dict: "<built-in>", line : dict.80 :: <Integral a>
Fail: Compilation errors found

dph: execution of the Haskell compiler had trouble

-}
