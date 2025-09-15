\section{IDS types}

\begin{code}
{-# LANGUAGE MagicHash #-}

module Types where

import GHC.Exts

data F a = FN | F1 a | F2 a a | F3 a a a 
         | F4 a a a a 
         | F5 a a a a a (F a) 

data FI = FIN | FI1 Int# | FI2 Int# Int# | FI3 Int# Int# Int# 
        | FI4 Int# Int# Int# Int# 
        | FI5 Int# Int# Int# Int# Int# FI

data FC = FCN | FC1 Char# | FC2 Char# Char# 
        | FC3 Char# Char# Char# 
        | FC4 Char# Char# Char# Char# 
        | FC5 Char# Char# Char# Char# Char# FC
\end{code}

\begin{code}
data F2 a b = F2N | F21 a b | F22 a b a b | F23 a b a b a b 
            | F24 a b a b a b a b 
            | F25 a b a b a b a b a b (F2 a b) 

data F3 a b c = F3N | F31 a b c | F32 a b c a b c 
              | F33 a b c a b c a b c
              | F34 a b c a b c a b c a b c
              | F35 a b c a b c a b c a b c a b c (F3 a b c) 

data F3I = F3IN 
         | F3I1 Int# Int# Int# 
         | F3I2 Int# Int# Int# Int# Int# Int# 
         | F3I3 Int# Int# Int# Int# Int# Int# Int# Int# Int#
         | F3I4 Int# Int# Int# Int# Int# Int# Int# Int# Int# 
                Int# Int# Int#
         | F3I5 Int# Int# Int# Int# Int# Int# Int# Int# Int# 
                Int# Int# Int# Int# Int# Int# F3I
\end{code}

\begin{code}
data S a = SN | S1 a (S a) | S2 a a (S a) | S3 a a a (S a)
         | S4 a a a a (S a)
         | S5 a a a a a (S a) 

data SI = SIN | SI1 Int# SI | SI2 Int# Int# SI 
        | SI3 Int# Int# Int# SI
        | SI4 Int# Int# Int# Int# SI
        | SI5 Int# Int# Int# Int# Int# SI


data SC = SCN | SC1 Char# SC | SC2 Char# Char# SC 
        | SC3 Char# Char# Char# SC
        | SC4 Char# Char# Char# Char# SC
        | SC5 Char# Char# Char# Char# Char# SC
\end{code}




