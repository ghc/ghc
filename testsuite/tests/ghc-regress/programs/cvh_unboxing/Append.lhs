\section{Versions of {\tt append}}

\begin{code}
{-# LANGUAGE MagicHash #-}

module Append where

import GHC.Exts
import Types
\end{code}

\begin{code}
append_ :: [a] -> [a] -> [a]
append_ (x:xs) ys = x : (append_ xs ys)
append_ [] ys = ys

append_L_S_S :: String -> S Char -> S Char
append_L_S_S (a: b: c: d: e: a1: b1: c1: d1: e1: xs) ys  
 = S5 a b c d e (S5 a1 b1 c1 d1 e1 (append_L_S_S xs ys))
append_L_S_S (a: b: c: d: e: xs) ys  
 = S5 a b c d e (append_L_S_S xs ys)
append_L_S_S (a: b: c: d: _) ys
 = S4 a b c d ys
append_L_S_S (a: b: c: _) ys
 = S3 a b c ys
append_L_S_S (a: b: _) ys
 = S2 a b ys
append_L_S_S [a] ys
 = S1 a ys
append_L_S_S [] ys = ys

append_F_S_S :: F Char -> S Char -> S Char
append_F_S_S (F5 a b c d e (F5 a1 b1 c1 d1 e1 xs)) ys  
 = S5 a b c d e (S5 a1 b1 c1 d1 e1 (append_F_S_S xs ys))
append_F_S_S (F5 a b c d e xs) ys  
 = S5 a b c d e (append_F_S_S xs ys)
append_F_S_S (F4 a b c d) ys
 = S4 a b c d ys
append_F_S_S (F3 a b c) ys
 = S3 a b c ys
append_F_S_S (F2 a b) ys
 = S2 a b ys
append_F_S_S (F1 a) ys
 = S1 a ys
append_F_S_S FN ys = ys


append_L_SC_SC :: String -> SC -> SC
append_L_SC_SC (C# a: C# b: C# c: C# d: 
                C# e: C# a1: C# b1: C# c1: 
                C# d1: C# e1: xs) ys  
 = SC5 a b c d e (SC5 a1 b1 c1 d1 e1 (append_L_SC_SC xs ys))
append_L_SC_SC (C# a: C# b: C# c: C# d: 
                C# e: xs) ys  
 = SC5 a b c d e (append_L_SC_SC xs ys)
append_L_SC_SC (C# a: C# b: C# c: C# d: _) ys
 = SC4 a b c d ys
append_L_SC_SC (C# a: C# b: C# c: _) ys
 = SC3 a b c ys
append_L_SC_SC (C# a: C# b: _) ys
 = SC2 a b ys
append_L_SC_SC [C# a] ys
 = SC1 a ys
append_L_SC_SC [] ys = ys


append_FC_SC_SC :: FC -> SC -> SC
append_FC_SC_SC (FC5 a b c d e (FC5 a1 b1 c1 d1 e1 xs)) ys  
 = SC5 a b c d e (SC5 a1 b1 c1 d1 e1 (append_FC_SC_SC xs ys))
append_FC_SC_SC (FC5 a b c d e xs) ys  
 = SC5 a b c d e (append_FC_SC_SC xs ys)
append_FC_SC_SC (FC4 a b c d) ys
 = SC4 a b c d ys
append_FC_SC_SC (FC3 a b c) ys
 = SC3 a b c ys
append_FC_SC_SC (FC2 a b) ys
 = SC2 a b ys
append_FC_SC_SC (FC1 a) ys
 = SC1 a ys
append_FC_SC_SC FCN ys = ys

append_F_L_L :: F a -> [a] -> [a]
append_F_L_L (F5 a b c d e (F5 a1 b1 c1 d1 e1 xs)) ys  
 = a: b: c: d: e: a1: b1: c1: d1: e1: (append_F_L_L xs ys)
append_F_L_L (F5 a b c d e xs) ys  
 = a: b: c: d: e: (append_F_L_L xs ys)
append_F_L_L (F4 a b c d) ys  = a: b: c: d: ys
append_F_L_L (F3 a b c) ys  = a: b: c: ys
append_F_L_L (F2 a b) ys  = a: b: ys
append_F_L_L (F1 a) ys  = a: ys
append_F_L_L FN ys = ys

append_S_L_L :: S Char -> String -> String
append_S_L_L (S5 a b c d e (S5 a1 b1 c1 d1 e1 xs)) ys  
 =  a:  b:  c:  d:  e:  a1: b1:  c1:  d1:  e1: (append_S_L_L xs ys)
append_S_L_L (S5 a b c d e xs) ys  
 =  a:  b:  c:  d: e: (append_S_L_L xs ys)
append_S_L_L (S4 a b c d xs) ys  
 =  a:  b:  c:  d: (append_S_L_L xs ys)
append_S_L_L (S3 a b c xs) ys  
 =  a:  b:  c: (append_S_L_L xs ys)
append_S_L_L (S2 a b xs) ys  
 =  a:  b: (append_S_L_L xs ys)
append_S_L_L (S1 a xs) ys  
 =  a: (append_S_L_L xs ys)
append_S_L_L SN ys = ys


append_FC_L_L :: FC -> String -> String
append_FC_L_L (FC5 a b c d e (FC5 a1 b1 c1 d1 e1 xs)) ys  
 = C# a: C# b: C# c: C# d: C# e: C# a1: 
      C# b1: C# c1: C# d1: C# e1: 
         (append_FC_L_L xs ys)
append_FC_L_L (FC5 a b c d e xs) ys  
 = C# a: C# b: C# c: C# d: 
      C# e: (append_FC_L_L xs ys)
append_FC_L_L (FC4 a b c d) ys  
 = C# a: C# b: C# c: C# d: ys
append_FC_L_L (FC3 a b c) ys  
 = C# a: C# b: C# c: ys
append_FC_L_L (FC2 a b) ys  = C# a: C# b: ys
append_FC_L_L (FC1 a) ys  = C# a: ys
append_FC_L_L FCN ys = ys

append_SC_L_L :: SC -> String -> String
append_SC_L_L (SC5 a b c d e (SC5 a1 b1 c1 d1 e1 xs)) ys  
 = C# a: C# b: C# c: C# d: C# e: C# a1: 
      C# b1: C# c1: C# d1: C# e1: 
          (append_SC_L_L xs ys)
append_SC_L_L (SC5 a b c d e xs) ys  
 = C# a: C# b: C# c: C# d: 
      C# e: (append_SC_L_L xs ys)
append_SC_L_L (SC4 a b c d xs) ys  
 = C# a: C# b: C# c: C# d: (append_SC_L_L xs ys)
append_SC_L_L (SC3 a b c xs) ys  
 = C# a: C# b: C# c: (append_SC_L_L xs ys)
append_SC_L_L (SC2 a b xs) ys  
 = C# a: C# b: (append_SC_L_L xs ys)
append_SC_L_L (SC1 a xs) ys  
 = C# a: (append_SC_L_L xs ys)
append_SC_L_L SCN ys = ys
\end{code}
