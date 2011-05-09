{-# OPTIONS_GHC -XNoImplicitPrelude  #-}
{-# OPTIONS_GHC -XDeriveGeneric      #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Tuple
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/ghc-prim/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  unportable
--
-- The tuple data types
--
-----------------------------------------------------------------------------

module GHC.Tuple where

import GHC.Generics (Generic)


default () -- Double and Integer aren't available yet

data (,) a b = (,) a b
	deriving Generic
data (,,) a b c = (,,) a b c
	deriving Generic
data (,,,) a b c d = (,,,) a b c d
	deriving Generic
data (,,,,) a b c d e = (,,,,) a b c d e
	deriving Generic
data (,,,,,) a b c d e f = (,,,,,) a b c d e f
	deriving Generic
data (,,,,,,) a b c d e f g = (,,,,,,) a b c d e f g
	deriving Generic
data (,,,,,,,) a b c d e f g h = (,,,,,,,) a b c d e f g h
	deriving Generic
data (,,,,,,,,) a b c d e f g h i = (,,,,,,,,) a b c d e f g h i
	deriving Generic
data (,,,,,,,,,) a b c d e f g h i j = (,,,,,,,,,) a b c d e f g h i j
	deriving Generic
data (,,,,,,,,,,) a b c d e f g h i j k = (,,,,,,,,,,) a b c d e f g h i j k
	deriving Generic
data (,,,,,,,,,,,) a b c d e f g h i j k l = (,,,,,,,,,,,) a b c d e f g h i j k l
	deriving Generic
data (,,,,,,,,,,,,) a b c d e f g h i j k l m = (,,,,,,,,,,,,) a b c d e f g h i j k l m
	deriving Generic
data (,,,,,,,,,,,,,) a b c d e f g h i j k l m n = (,,,,,,,,,,,,,) a b c d e f g h i j k l m n
	deriving Generic
data (,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o = (,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o
	deriving Generic
data (,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p = (,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p
	deriving Generic
data (,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q
 = (,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q
	deriving Generic
data (,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r
 = (,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r
	deriving Generic
data (,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s
 = (,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t
 = (,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u
 = (,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v
 = (,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w
 = (,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x
 = (,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y
 = (,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z
 = (,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__
	deriving Generic
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__
{- Manuel says: Including one more declaration gives a segmentation fault.
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___ b___
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___ b___
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___ b___ c___
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___ b___ c___
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___ b___ c___ d___
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___ b___ c___ d___
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___ b___ c___ d___ e___
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___ b___ c___ d___ e___
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___ b___ c___ d___ e___ f___
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___ b___ c___ d___ e___ f___
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___ b___ c___ d___ e___ f___ g___
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___ b___ c___ d___ e___ f___ g___
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___ b___ c___ d___ e___ f___ g___ h___
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___ b___ c___ d___ e___ f___ g___ h___
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___ b___ c___ d___ e___ f___ g___ h___ i___
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___ b___ c___ d___ e___ f___ g___ h___ i___
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___ b___ c___ d___ e___ f___ g___ h___ i___ j___
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___ b___ c___ d___ e___ f___ g___ h___ i___ j___
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___ b___ c___ d___ e___ f___ g___ h___ i___ j___ k___
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___ b___ c___ d___ e___ f___ g___ h___ i___ j___ k___
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___ b___ c___ d___ e___ f___ g___ h___ i___ j___ k___ l___
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___ b___ c___ d___ e___ f___ g___ h___ i___ j___ k___ l___
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___ b___ c___ d___ e___ f___ g___ h___ i___ j___ k___ l___ m___
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___ b___ c___ d___ e___ f___ g___ h___ i___ j___ k___ l___ m___
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___ b___ c___ d___ e___ f___ g___ h___ i___ j___ k___ l___ m___ n___
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___ b___ c___ d___ e___ f___ g___ h___ i___ j___ k___ l___ m___ n___
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___ b___ c___ d___ e___ f___ g___ h___ i___ j___ k___ l___ m___ n___ o___
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___ b___ c___ d___ e___ f___ g___ h___ i___ j___ k___ l___ m___ n___ o___
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___ b___ c___ d___ e___ f___ g___ h___ i___ j___ k___ l___ m___ n___ o___ p___
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___ b___ c___ d___ e___ f___ g___ h___ i___ j___ k___ l___ m___ n___ o___ p___
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___ b___ c___ d___ e___ f___ g___ h___ i___ j___ k___ l___ m___ n___ o___ p___ q___
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___ b___ c___ d___ e___ f___ g___ h___ i___ j___ k___ l___ m___ n___ o___ p___ q___
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___ b___ c___ d___ e___ f___ g___ h___ i___ j___ k___ l___ m___ n___ o___ p___ q___ r___
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___ b___ c___ d___ e___ f___ g___ h___ i___ j___ k___ l___ m___ n___ o___ p___ q___ r___
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___ b___ c___ d___ e___ f___ g___ h___ i___ j___ k___ l___ m___ n___ o___ p___ q___ r___ s___
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___ b___ c___ d___ e___ f___ g___ h___ i___ j___ k___ l___ m___ n___ o___ p___ q___ r___ s___
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___ b___ c___ d___ e___ f___ g___ h___ i___ j___ k___ l___ m___ n___ o___ p___ q___ r___ s___ t___ 
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___ b___ c___ d___ e___ f___ g___ h___ i___ j___ k___ l___ m___ n___ o___ p___ q___ r___ s___ t___
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___ b___ c___ d___ e___ f___ g___ h___ i___ j___ k___ l___ m___ n___ o___ p___ q___ r___ s___ t___  u___
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___ b___ c___ d___ e___ f___ g___ h___ i___ j___ k___ l___ m___ n___ o___ p___ q___ r___ s___ t___ u___
data (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___ b___ c___ d___ e___ f___ g___ h___ i___ j___ k___ l___ m___ n___ o___ p___ q___ r___ s___ t___  u___ v___
 = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__ k__ l__ m__ n__ o__ p__ q__ r__ s__ t__ u__ v__ w__ x__ y__ z__ a___ b___ c___ d___ e___ f___ g___ h___ i___ j___ k___ l___ m___ n___ o___ p___ q___ r___ s___ t___ u___ v___
-}


--------------------------------------------------------------------------------
-- Generic representations
--------------------------------------------------------------------------------

--deriving instance Generic ((,) a b)
-- deriving instance Generic ((,,) a b c)
{-
deriving instance Generic ((,,,) a b c d)
deriving instance Generic ((,,,,) a b c d e)
deriving instance Generic ((,,,,,) a b c d e f)
deriving instance Generic ((,,,,,,) a b c d e f g)
deriving instance Generic ((,,,,,,,) a b c d e f g h)
deriving instance Generic ((,,,,,,,,) a b c d e f g h i)
deriving instance Generic ((,,,,,,,,,) a b c d e f g h i j)
deriving instance Generic ((,,,,,,,,,,) a b c d e f g h i j k)
deriving instance Generic ((,,,,,,,,,,,) a b c d e f g h i j k l)
deriving instance Generic ((,,,,,,,,,,,,) a b c d e f g h i j k l m)
deriving instance Generic ((,,,,,,,,,,,,,) a b c d e f g h i j k l m n)
deriving instance Generic ((,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o)
deriving instance Generic ((,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p)
deriving instance Generic ((,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q)
deriving instance Generic ((,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__)
deriving instance Generic ((,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_ p_ q_ r_ s_ t_ u_ v_ w_ x_ y_ z_ a__ b__ c__ d__ e__ f__ g__ h__ i__ j__)
-}
