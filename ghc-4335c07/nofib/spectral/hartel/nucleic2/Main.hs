#include "unboxery.h"

#ifdef USE_GLASGOW_HACKS
module Main(mainPrimIO) where
import PreludeGlaST
#else
module Main(main) where
#endif

import Types
import RA
import RC
import RG
import RU

#ifdef USE_UNBOXED_FLOATS
-- EASY WAY:
--atan2Float# :: Float# -> Float# -> Float#
--atan2Float# x y = case atan2 (F# x) (F# y) of { F# z -> z }

atan2Float# :: Float# -> Float# -> Float#
atan2Float# y x  =
	if x `geFloat#` 0.0# then
		if y `eqFloat#` 0.0# then
			0.0#
		else
			atanFloat# (y `divideFloat#` x)
	else if x `ltFloat#` 0.0# then
		if y `eqFloat#` 0.0# then
			pi#
		else
			atanFloat# (y `divideFloat#` x) `plusFloat#` pi#
	else if y `gtFloat#` 0.0# then
		pi# `divideFloat#` 2.0#

	else
		negpi# `divideFloat#` 2.0#

	where pi# =     3.1415926536#
	      negpi# = -3.1415926536#
#endif

--  File: "nucleic2.m"
--
--  Author: Marc Feeley (feeley@iro.umontreal.ca)
--
--  Last modified: June 6, 1994
--
--  This program is a modified version of the program described in the paper:
--
--    M. Feeley, M. Turcotte, G. Lapalme, "Using Multilisp for Solving
--    Constraint Satisfaction Problems: an Application to Nucleic Acid 3D
--    Structure Determination" published in the journal "Lisp and Symbolic
--    Computation".
--
--  The differences between this program and the original are described in
--  the paper:
--
--    "???" published in the "Journal of Functional Programming".

-- -- POINTS ------------------------------------------------------------------

pt_sub (Pt x1 y1 z1) (Pt x2 y2 z2) = Pt (x1 _SUB_ x2) (y1 _SUB_ y2) (z1 _SUB_ z2)

pt_dist (Pt x1 y1 z1) (Pt x2 y2 z2) = _SQRT_ ((dx _MUL_ dx) _ADD_ (dy _MUL_ dy) _ADD_ (dz _MUL_ dz))
    where dx = x1 _SUB_ x2
          dy = y1 _SUB_ y2
          dz = z1 _SUB_ z2

pt_phi (Pt x y z) = _ATAN2_ ((_COS_ b _MUL_ z) _ADD_ (_SIN_ b _MUL_ x)) y
    where b = _ATAN2_ x z

pt_theta (Pt x y z) = _ATAN2_ x z

-- -- COORDINATE TRANSFORMATIONS ----------------------------------------------

-- The notation for the transformations follows "Paul, R.P. (1981) Robot
-- Manipulators.  MIT Press." with the exception that our transformation
-- matrices don't have the perspective terms and are the transpose of
-- Paul's one.  See also "M\"antyl\"a, M. (1985) An Introduction to
-- Solid Modeling, Computer Science Press" Appendix A.
--
-- The components of a transformation matrix are named like this:
--
--  a  b  c
--  d  e  f
--  g  h  i
-- tx ty tz
--
-- The components tx, ty, and tz are the translation vector.

tfo_id = Tfo FL_LIT(1.0) FL_LIT(0.0) FL_LIT(0.0) FL_LIT(0.0) FL_LIT(1.0) FL_LIT(0.0) FL_LIT(0.0) FL_LIT(0.0) FL_LIT(1.0) FL_LIT(0.0) FL_LIT(0.0) FL_LIT(0.0)

-- The function "tfo-apply" multiplies a transformation matrix, tfo, by a
-- point vector, p.  The result is a new point.

tfo_apply (Tfo a b c d e f g h i tx ty tz) (Pt x y z)
  = Pt ((x _MUL_ a) _ADD_ (y _MUL_ d) _ADD_ (z _MUL_ g) _ADD_ tx)
       ((x _MUL_ b) _ADD_ (y _MUL_ e) _ADD_ (z _MUL_ h) _ADD_ ty)
       ((x _MUL_ c) _ADD_ (y _MUL_ f) _ADD_ (z _MUL_ i) _ADD_ tz)

-- The function "tfo-combine" multiplies two transformation matrices A and B.
-- The result is a new matrix which cumulates the transformations described
-- by A and B.

tfo_combine
  (Tfo a_a a_b a_c a_d a_e a_f a_g a_h a_i a_tx a_ty a_tz)
  (Tfo b_a b_b b_c b_d b_e b_f b_g b_h b_i b_tx b_ty b_tz)
  = Tfo
      ((a_a _MUL_ b_a) _ADD_ (a_b _MUL_ b_d) _ADD_ (a_c _MUL_ b_g))
      ((a_a _MUL_ b_b) _ADD_ (a_b _MUL_ b_e) _ADD_ (a_c _MUL_ b_h))
      ((a_a _MUL_ b_c) _ADD_ (a_b _MUL_ b_f) _ADD_ (a_c _MUL_ b_i))
      ((a_d _MUL_ b_a) _ADD_ (a_e _MUL_ b_d) _ADD_ (a_f _MUL_ b_g))
      ((a_d _MUL_ b_b) _ADD_ (a_e _MUL_ b_e) _ADD_ (a_f _MUL_ b_h))
      ((a_d _MUL_ b_c) _ADD_ (a_e _MUL_ b_f) _ADD_ (a_f _MUL_ b_i))
      ((a_g _MUL_ b_a) _ADD_ (a_h _MUL_ b_d) _ADD_ (a_i _MUL_ b_g))
      ((a_g _MUL_ b_b) _ADD_ (a_h _MUL_ b_e) _ADD_ (a_i _MUL_ b_h))
      ((a_g _MUL_ b_c) _ADD_ (a_h _MUL_ b_f) _ADD_ (a_i _MUL_ b_i))
      ((a_tx _MUL_ b_a) _ADD_ (a_ty _MUL_ b_d) _ADD_ (a_tz _MUL_ b_g) _ADD_ b_tx)
      ((a_tx _MUL_ b_b) _ADD_ (a_ty _MUL_ b_e) _ADD_ (a_tz _MUL_ b_h) _ADD_ b_ty)
      ((a_tx _MUL_ b_c) _ADD_ (a_ty _MUL_ b_f) _ADD_ (a_tz _MUL_ b_i) _ADD_ b_tz)

-- The function "tfo-inv-ortho" computes the inverse of a homogeneous
-- transformation matrix.

tfo_inv_ortho (Tfo a b c d e f g h i tx ty tz)
  = Tfo
     a d g
     b e h
     c f i
     (_NEG_ ((a _MUL_ tx) _ADD_ (b _MUL_ ty) _ADD_ (c _MUL_ tz)))
     (_NEG_ ((d _MUL_ tx) _ADD_ (e _MUL_ ty) _ADD_ (f _MUL_ tz)))
     (_NEG_ ((g _MUL_ tx) _ADD_ (h _MUL_ ty) _ADD_ (i _MUL_ tz)))

-- Given three points p1, p2, and p3, the function "tfo-align" computes
-- a transformation matrix such that point p1 gets mapped to (0,0,0), p2 gets
-- mapped to the Y axis and p3 gets mapped to the YZ plane.

tfo_align (Pt x1 y1 z1) (Pt x2 y2 z2) (Pt x3 y3 z3)
  = Tfo
      ((cost _MUL_ cosr) _SUB_ (cospsint _MUL_ sinr))
      sinpsint
      ((cost _MUL_ sinr) _ADD_ (cospsint _MUL_ cosr))
      (sinp _MUL_ sinr)
      cosp
      (_NEG_ (sinp _MUL_ cosr))
      ((_NEG_ (sint _MUL_ cosr)) _SUB_ (cospcost _MUL_ sinr))
      sinpcost
      ((_NEG_ (sint _MUL_ sinr)) _ADD_ (cospcost _MUL_ cosr))
      ((x _MUL_ cosr) _SUB_ (z _MUL_ sinr))
      y
      ((x _MUL_ sinr) _ADD_ (z _MUL_ cosr))
    where
      x31 = x3 _SUB_ x1
      y31 = y3 _SUB_ y1
      z31 = z3 _SUB_ z1
      rotpy = pt_sub (Pt x2 y2 z2) (Pt x1 y1 z1)
      phi = pt_phi rotpy
      theta = pt_theta rotpy
      sinp = _SIN_ phi
      sint = _SIN_ theta
      cosp = _COS_ phi
      cost = _COS_ theta
      sinpsint = sinp _MUL_ sint
      sinpcost = sinp _MUL_ cost
      cospsint = cosp _MUL_ sint
      cospcost = cosp _MUL_ cost
      rotpz = Pt
                ((cost _MUL_ x31) _SUB_ (sint _MUL_ z31))
                ((sinpsint _MUL_ x31) _ADD_
                 (cosp _MUL_ y31) _ADD_
                 (sinpcost _MUL_ z31))
                ((cospsint _MUL_ x31) _ADD_
                 (_NEG_ (sinp _MUL_ y31)) _ADD_
                 (cospcost _MUL_ z31))
      rho = pt_theta rotpz
      cosr = _COS_ rho
      sinr = _SIN_ rho
      x = ((_NEG_ (x1 _MUL_ cost)) _ADD_ (z1 _MUL_ sint))
      y = (((_NEG_ (x1 _MUL_ sinpsint)) _SUB_ (y1 _MUL_ cosp)) _SUB_
           (z1 _MUL_ sinpcost))
      z = (((_NEG_ (x1 _MUL_ cospsint)) _ADD_ (y1 _MUL_ sinp)) _SUB_
           (z1 _MUL_ cospcost))

-- -- NUCLEIC ACID CONFORMATIONS DATA BASE ------------------------------------

-- Numbering of atoms follows the paper:
--
-- IUPAC-IUB Joint Commission on Biochemical Nomenclature (JCBN)
-- (1983) Abbreviations and Symbols for the Description of
-- Conformations of Polynucleotide Chains.  Eur. J. Biochem 131,
-- 9-15.

-- Define part common to all 4 nucleotide types.

--  dgf_base_tfo  ; defines the standard position for wc and wc_dumas
--  p_o3XXX_275_tfo ; defines the standard position for the connect function
--  p_o3XXX_180_tfo
--  p_o3XXX_60_tfo
--  p o1p o2p o5XXX c5XXX h5XXX h5XXXXXX c4XXX h4XXX o4XXX c1XXX h1XXX c2XXX h2XXXXXX o2XXX h2XXX c3XXX
--  h3XXX o3XXX n1 n3 c2 c4 c5 c6

is_A (Nuc
        dgf_base_tfo p_o3XXX_275_tfo p_o3XXX_180_tfo p_o3XXX_60_tfo
        p o1p o2p o5XXX c5XXX h5XXX h5XXXXXX c4XXX h4XXX o4XXX c1XXX h1XXX c2XXX h2XXXXXX o2XXX h2XXX
        c3XXX h3XXX o3XXX n1 n3 c2 c4 c5 c6
        (A n6 n7 n9 c8 h2 h61 h62 h8))
  = True
is_A x
  = False

is_C (Nuc
        dgf_base_tfo p_o3XXX_275_tfo p_o3XXX_180_tfo p_o3XXX_60_tfo
        p o1p o2p o5XXX c5XXX h5XXX h5XXXXXX c4XXX h4XXX o4XXX c1XXX h1XXX c2XXX h2XXXXXX o2XXX h2XXX
        c3XXX h3XXX o3XXX n1 n3 c2 c4 c5 c6
        (C n4 o2 h41 h42 h5 h6))
  = True
is_C x
  = False

is_G (Nuc
        dgf_base_tfo p_o3XXX_275_tfo p_o3XXX_180_tfo p_o3XXX_60_tfo
        p o1p o2p o5XXX c5XXX h5XXX h5XXXXXX c4XXX h4XXX o4XXX c1XXX h1XXX c2XXX h2XXXXXX o2XXX h2XXX
        c3XXX h3XXX o3XXX n1 n3 c2 c4 c5 c6
        (G n2 n7 n9 c8 o6 h1 h21 h22 h8))
  = True
is_G x
  = False

nuc_C1XXX
  (Nuc
    dgf_base_tfo p_o3XXX_275_tfo p_o3XXX_180_tfo p_o3XXX_60_tfo
    p o1p o2p o5XXX c5XXX h5XXX h5XXXXXX c4XXX h4XXX o4XXX c1XXX h1XXX c2XXX h2XXXXXX o2XXX h2XXX
    c3XXX h3XXX o3XXX n1 n3 c2 c4 c5 c6
    x)
  = c1XXX

nuc_C2 
  (Nuc
    dgf_base_tfo p_o3XXX_275_tfo p_o3XXX_180_tfo p_o3XXX_60_tfo
    p o1p o2p o5XXX c5XXX h5XXX h5XXXXXX c4XXX h4XXX o4XXX c1XXX h1XXX c2XXX h2XXXXXX o2XXX h2XXX
    c3XXX h3XXX o3XXX n1 n3 c2 c4 c5 c6
    x)
  = c2

nuc_C3XXX
  (Nuc
    dgf_base_tfo p_o3XXX_275_tfo p_o3XXX_180_tfo p_o3XXX_60_tfo
    p o1p o2p o5XXX c5XXX h5XXX h5XXXXXX c4XXX h4XXX o4XXX c1XXX h1XXX c2XXX h2XXXXXX o2XXX h2XXX
    c3XXX h3XXX o3XXX n1 n3 c2 c4 c5 c6
    x)
  = c3XXX

nuc_C4 
  (Nuc
    dgf_base_tfo p_o3XXX_275_tfo p_o3XXX_180_tfo p_o3XXX_60_tfo
    p o1p o2p o5XXX c5XXX h5XXX h5XXXXXX c4XXX h4XXX o4XXX c1XXX h1XXX c2XXX h2XXXXXX o2XXX h2XXX
    c3XXX h3XXX o3XXX n1 n3 c2 c4 c5 c6
    x)
  = c4

nuc_C4XXX
  (Nuc
    dgf_base_tfo p_o3XXX_275_tfo p_o3XXX_180_tfo p_o3XXX_60_tfo
    p o1p o2p o5XXX c5XXX h5XXX h5XXXXXX c4XXX h4XXX o4XXX c1XXX h1XXX c2XXX h2XXXXXX o2XXX h2XXX
    c3XXX h3XXX o3XXX n1 n3 c2 c4 c5 c6
    x)
  = c4XXX

nuc_N1 
  (Nuc
    dgf_base_tfo p_o3XXX_275_tfo p_o3XXX_180_tfo p_o3XXX_60_tfo
    p o1p o2p o5XXX c5XXX h5XXX h5XXXXXX c4XXX h4XXX o4XXX c1XXX h1XXX c2XXX h2XXXXXX o2XXX h2XXX
    c3XXX h3XXX o3XXX n1 n3 c2 c4 c5 c6
    x)
  = n1

nuc_O3XXX
  (Nuc
    dgf_base_tfo p_o3XXX_275_tfo p_o3XXX_180_tfo p_o3XXX_60_tfo
    p o1p o2p o5XXX c5XXX h5XXX h5XXXXXX c4XXX h4XXX o4XXX c1XXX h1XXX c2XXX h2XXXXXX o2XXX h2XXX
    c3XXX h3XXX o3XXX n1 n3 c2 c4 c5 c6
    x)
  = o3XXX

nuc_P
  (Nuc
    dgf_base_tfo p_o3XXX_275_tfo p_o3XXX_180_tfo p_o3XXX_60_tfo
    p o1p o2p o5XXX c5XXX h5XXX h5XXXXXX c4XXX h4XXX o4XXX c1XXX h1XXX c2XXX h2XXXXXX o2XXX h2XXX
    c3XXX h3XXX o3XXX n1 n3 c2 c4 c5 c6
    x)
  = p

nuc_dgf_base_tfo
  (Nuc
    dgf_base_tfo p_o3XXX_275_tfo p_o3XXX_180_tfo p_o3XXX_60_tfo
    p o1p o2p o5XXX c5XXX h5XXX h5XXXXXX c4XXX h4XXX o4XXX c1XXX h1XXX c2XXX h2XXXXXX o2XXX h2XXX
    c3XXX h3XXX o3XXX n1 n3 c2 c4 c5 c6
    x)
  = dgf_base_tfo

nuc_p_o3XXX_180_tfo
  (Nuc
    dgf_base_tfo p_o3XXX_275_tfo p_o3XXX_180_tfo p_o3XXX_60_tfo
    p o1p o2p o5XXX c5XXX h5XXX h5XXXXXX c4XXX h4XXX o4XXX c1XXX h1XXX c2XXX h2XXXXXX o2XXX h2XXX
    c3XXX h3XXX o3XXX n1 n3 c2 c4 c5 c6
    x)
  = p_o3XXX_180_tfo

nuc_p_o3XXX_275_tfo
  (Nuc
    dgf_base_tfo p_o3XXX_275_tfo p_o3XXX_180_tfo p_o3XXX_60_tfo
    p o1p o2p o5XXX c5XXX h5XXX h5XXXXXX c4XXX h4XXX o4XXX c1XXX h1XXX c2XXX h2XXXXXX o2XXX h2XXX
    c3XXX h3XXX o3XXX n1 n3 c2 c4 c5 c6
    x)
  = p_o3XXX_275_tfo

nuc_p_o3XXX_60_tfo
  (Nuc
    dgf_base_tfo p_o3XXX_275_tfo p_o3XXX_180_tfo p_o3XXX_60_tfo
    p o1p o2p o5XXX c5XXX h5XXX h5XXXXXX c4XXX h4XXX o4XXX c1XXX h1XXX c2XXX h2XXXXXX o2XXX h2XXX
    c3XXX h3XXX o3XXX n1 n3 c2 c4 c5 c6
    x)
  = p_o3XXX_60_tfo

rA_N9  
  (Nuc
    dgf_base_tfo p_o3XXX_275_tfo p_o3XXX_180_tfo p_o3XXX_60_tfo
    p o1p o2p o5XXX c5XXX h5XXX h5XXXXXX c4XXX h4XXX o4XXX c1XXX h1XXX c2XXX h2XXXXXX o2XXX h2XXX
    c3XXX h3XXX o3XXX n1 n3 c2 c4 c5 c6
    (A n6 n7 n9 c8 h2 h61 h62 h8))
  = n9

rG_N9  
  (Nuc
    dgf_base_tfo p_o3XXX_275_tfo p_o3XXX_180_tfo p_o3XXX_60_tfo
    p o1p o2p o5XXX c5XXX h5XXX h5XXXXXX c4XXX h4XXX o4XXX c1XXX h1XXX c2XXX h2XXXXXX o2XXX h2XXX
    c3XXX h3XXX o3XXX n1 n3 c2 c4 c5 c6
    (G n2 n7 n9 c8 o6 h1 h21 h22 h8))
  = n9

-- Define remaining atoms for each nucleotide type.

-- Database of nucleotide conformations:


-- -- PARTIAL INSTANTIATIONS --------------------------------------------------

data Var = Var INT_TY Tfo Nuc

-- If you want lazy computation, comment the next two definitions and
-- uncomment the alternative definitions that follow.

#ifdef USE_HARTEL_LAZINESS
mk_var i t n = Var i t (make_relative_nuc t n)
absolute_pos (Var i t n) p = p
#else
mk_var i t n = Var i t n
absolute_pos (Var i t n) p = tfo_apply t p
#endif

atom_pos atom v@(Var i t n) = absolute_pos v (atom n)

get_var id (v@(Var i t n):lst) = if i _EQ_INT_ id then v else get_var id lst

make_relative_nuc
  t
  (Nuc
    dgf_base_tfo p_o3XXX_275_tfo p_o3XXX_180_tfo p_o3XXX_60_tfo
    p o1p o2p o5XXX c5XXX h5XXX h5XXXXXX c4XXX h4XXX o4XXX c1XXX h1XXX c2XXX h2XXXXXX o2XXX h2XXX
    c3XXX h3XXX o3XXX n1 n3 c2 c4 c5 c6
    (A n6 n7 n9 c8 h2 h61 h62 h8))
  = Nuc
      dgf_base_tfo p_o3XXX_275_tfo p_o3XXX_180_tfo p_o3XXX_60_tfo
      (tfo_apply t p)
      (tfo_apply t o1p)
      (tfo_apply t o2p)
      (tfo_apply t o5XXX)
      (tfo_apply t c5XXX)
      (tfo_apply t h5XXX)
      (tfo_apply t h5XXXXXX)
      (tfo_apply t c4XXX)
      (tfo_apply t h4XXX)
      (tfo_apply t o4XXX)
      (tfo_apply t c1XXX)
      (tfo_apply t h1XXX)
      (tfo_apply t c2XXX)
      (tfo_apply t h2XXXXXX)
      (tfo_apply t o2XXX)
      (tfo_apply t h2XXX)
      (tfo_apply t c3XXX)
      (tfo_apply t h3XXX)
      (tfo_apply t o3XXX)
      (tfo_apply t n1)
      (tfo_apply t n3)
      (tfo_apply t c2)
      (tfo_apply t c4)
      (tfo_apply t c5)
      (tfo_apply t c6)
      (A
      (tfo_apply t n6)
      (tfo_apply t n7)
      (tfo_apply t n9)
      (tfo_apply t c8)
      (tfo_apply t h2)
      (tfo_apply t h61)
      (tfo_apply t h62)
      (tfo_apply t h8)
      )

make_relative_nuc
  t
  (Nuc
    dgf_base_tfo p_o3XXX_275_tfo p_o3XXX_180_tfo p_o3XXX_60_tfo
    p o1p o2p o5XXX c5XXX h5XXX h5XXXXXX c4XXX h4XXX o4XXX c1XXX h1XXX c2XXX h2XXXXXX o2XXX h2XXX
    c3XXX h3XXX o3XXX n1 n3 c2 c4 c5 c6
    (C n4 o2 h41 h42 h5 h6))
  = Nuc
      dgf_base_tfo p_o3XXX_275_tfo p_o3XXX_180_tfo p_o3XXX_60_tfo
      (tfo_apply t p)
      (tfo_apply t o1p)
      (tfo_apply t o2p)
      (tfo_apply t o5XXX)
      (tfo_apply t c5XXX)
      (tfo_apply t h5XXX)
      (tfo_apply t h5XXXXXX)
      (tfo_apply t c4XXX)
      (tfo_apply t h4XXX)
      (tfo_apply t o4XXX)
      (tfo_apply t c1XXX)
      (tfo_apply t h1XXX)
      (tfo_apply t c2XXX)
      (tfo_apply t h2XXXXXX)
      (tfo_apply t o2XXX)
      (tfo_apply t h2XXX)
      (tfo_apply t c3XXX)
      (tfo_apply t h3XXX)
      (tfo_apply t o3XXX)
      (tfo_apply t n1)
      (tfo_apply t n3)
      (tfo_apply t c2)
      (tfo_apply t c4)
      (tfo_apply t c5)
      (tfo_apply t c6)
      (C
      (tfo_apply t n4)
      (tfo_apply t o2)
      (tfo_apply t h41)
      (tfo_apply t h42)
      (tfo_apply t h5)
      (tfo_apply t h6)
      )

make_relative_nuc
  t
  (Nuc
    dgf_base_tfo p_o3XXX_275_tfo p_o3XXX_180_tfo p_o3XXX_60_tfo
    p o1p o2p o5XXX c5XXX h5XXX h5XXXXXX c4XXX h4XXX o4XXX c1XXX h1XXX c2XXX h2XXXXXX o2XXX h2XXX
    c3XXX h3XXX o3XXX n1 n3 c2 c4 c5 c6
    (G n2 n7 n9 c8 o6 h1 h21 h22 h8))
  = Nuc
      dgf_base_tfo p_o3XXX_275_tfo p_o3XXX_180_tfo p_o3XXX_60_tfo
      (tfo_apply t p)
      (tfo_apply t o1p)
      (tfo_apply t o2p)
      (tfo_apply t o5XXX)
      (tfo_apply t c5XXX)
      (tfo_apply t h5XXX)
      (tfo_apply t h5XXXXXX)
      (tfo_apply t c4XXX)
      (tfo_apply t h4XXX)
      (tfo_apply t o4XXX)
      (tfo_apply t c1XXX)
      (tfo_apply t h1XXX)
      (tfo_apply t c2XXX)
      (tfo_apply t h2XXXXXX)
      (tfo_apply t o2XXX)
      (tfo_apply t h2XXX)
      (tfo_apply t c3XXX)
      (tfo_apply t h3XXX)
      (tfo_apply t o3XXX)
      (tfo_apply t n1)
      (tfo_apply t n3)
      (tfo_apply t c2)
      (tfo_apply t c4)
      (tfo_apply t c5)
      (tfo_apply t c6)
      (G
      (tfo_apply t n2)
      (tfo_apply t n7)
      (tfo_apply t n9)
      (tfo_apply t c8)
      (tfo_apply t o6)
      (tfo_apply t h1)
      (tfo_apply t h21)
      (tfo_apply t h22)
      (tfo_apply t h8)
      )

make_relative_nuc
  t
  (Nuc
    dgf_base_tfo p_o3XXX_275_tfo p_o3XXX_180_tfo p_o3XXX_60_tfo
    p o1p o2p o5XXX c5XXX h5XXX h5XXXXXX c4XXX h4XXX o4XXX c1XXX h1XXX c2XXX h2XXXXXX o2XXX h2XXX
    c3XXX h3XXX o3XXX n1 n3 c2 c4 c5 c6
    (U o2 o4 h3 h5 h6))
  = Nuc
      dgf_base_tfo p_o3XXX_275_tfo p_o3XXX_180_tfo p_o3XXX_60_tfo
      (tfo_apply t p)
      (tfo_apply t o1p)
      (tfo_apply t o2p)
      (tfo_apply t o5XXX)
      (tfo_apply t c5XXX)
      (tfo_apply t h5XXX)
      (tfo_apply t h5XXXXXX)
      (tfo_apply t c4XXX)
      (tfo_apply t h4XXX)
      (tfo_apply t o4XXX)
      (tfo_apply t c1XXX)
      (tfo_apply t h1XXX)
      (tfo_apply t c2XXX)
      (tfo_apply t h2XXXXXX)
      (tfo_apply t o2XXX)
      (tfo_apply t h2XXX)
      (tfo_apply t c3XXX)
      (tfo_apply t h3XXX)
      (tfo_apply t o3XXX)
      (tfo_apply t n1)
      (tfo_apply t n3)
      (tfo_apply t c2)
      (tfo_apply t c4)
      (tfo_apply t c5)
      (tfo_apply t c6)
      (U
      (tfo_apply t o2)
      (tfo_apply t o4)
      (tfo_apply t h3)
      (tfo_apply t h5)
      (tfo_apply t h6)
      )

-- -- SEARCH ------------------------------------------------------------------

-- Sequential backtracking algorithm

search partial_inst [] constraint = [partial_inst]
search partial_inst (h:t) constraint = try_assignments (h partial_inst)
    where try_assignments [] = []
          try_assignments (v:vs) | constraint v partial_inst = (search (v:partial_inst) t constraint) ++ (try_assignments vs)
				 | otherwise                 = try_assignments vs

-- -- DOMAINS -----------------------------------------------------------------

-- Primary structure:   strand A CUGCCACGUCUG, strand B CAGACGUGGCAG
--
-- Secondary structure: strand A CUGCCACGUCUG
--                               ------------
--                               GACGGUGCAGAC strand B
--
-- Tertiary structure:
--
--    5XXX end of strand A C1----G12 3XXX end of strand B
--                     U2-------A11
--                    G3-------C10
--                    C4-----G9
--                     C5---G8
--                        A6
--                      G6-C7
--                     C5----G8
--                    A4-------U9
--                    G3--------C10
--                     A2-------U11
--   5' end of strand B C1----G12 3' end of strand A
--
-- "helix", "stacked" and "connected" describe the spatial relationship
-- between two consecutive nucleotides. E.g. the nucleotides C1 and U2
-- from the strand A.
--
-- "wc" (stands for Watson-Crick and is a type of base-pairing),
-- and "wc-dumas" describe the spatial relationship between 
-- nucleotides from two chains that are growing in opposite directions.
-- E.g. the nucleotides C1 from strand A and G12 from strand B.

-- Dynamic Domains

-- Given,
--   "ref" a nucleotide which is already positioned,
--   "nuc" the nucleotide to be placed,
--   and "tfo" a transformation matrix which expresses the desired
--   relationship between "ref" and "nuc",
-- the function "dgf-base" computes the transformation matrix that
-- places the nucleotide "nuc" in the given relationship to "ref".

dgf_base tfo v@(Var i t n) nuc
  = tfo_combine (nuc_dgf_base_tfo nuc)
                (tfo_combine tfo (tfo_inv_ortho x))
    where
       x | is_A n
	 = tfo_align (atom_pos nuc_C1XXX v)
                     (atom_pos rA_N9   v)
                     (atom_pos nuc_C4  v)
	 | is_C n
         = tfo_align (atom_pos nuc_C1XXX v)
                     (atom_pos nuc_N1  v)
                     (atom_pos nuc_C2  v)
	 | is_G n
         = tfo_align (atom_pos nuc_C1XXX v)
                     (atom_pos rG_N9   v)
                     (atom_pos nuc_C4  v)
	 | otherwise
         = tfo_align (atom_pos nuc_C1XXX v)
                     (atom_pos nuc_N1  v)
                     (atom_pos nuc_C2  v)

-- Placement of first nucleotide.

reference nuc i partial_inst = [ mk_var i tfo_id nuc ]

-- The transformation matrix for wc is from:
--
-- Chandrasekaran R. et al (1989) A Re-Examination of the Crystal
-- Structure of A-DNA Using Fiber Diffraction Data. J. Biomol.
-- Struct. & Dynamics 6(6):1189-1202.

wc_tfo
  = Tfo
      FL_LIT(-1.0000)  FL_LIT(0.0028) FL_LIT(-0.0019)
       FL_LIT(0.0028)  FL_LIT(0.3468) FL_LIT(-0.9379)
      FL_LIT(-0.0019) FL_LIT(-0.9379) FL_LIT(-0.3468)
      FL_LIT(-0.0080)  FL_LIT(6.0730)  FL_LIT(8.7208)

wc nuc i j partial_inst
  = [ mk_var i (dgf_base wc_tfo (get_var j partial_inst) nuc) nuc ]

wc_dumas_tfo
  = Tfo
      FL_LIT(-0.9737) FL_LIT(-0.1834)  FL_LIT(0.1352)
      FL_LIT(-0.1779)  FL_LIT(0.2417) FL_LIT(-0.9539)
       FL_LIT(0.1422) FL_LIT(-0.9529) FL_LIT(-0.2679)
       FL_LIT(0.4837)  FL_LIT(6.2649)  FL_LIT(8.0285)
         
wc_dumas nuc i j partial_inst
  = [ mk_var i (dgf_base wc_dumas_tfo (get_var j partial_inst) nuc) nuc ]

helix5XXX_tfo
  = Tfo
       FL_LIT(0.9886) FL_LIT(-0.0961)  FL_LIT(0.1156)
       FL_LIT(0.1424)  FL_LIT(0.8452) FL_LIT(-0.5152)
      FL_LIT(-0.0482)  FL_LIT(0.5258)  FL_LIT(0.8492)
      FL_LIT(-3.8737)  FL_LIT(0.5480)  FL_LIT(3.8024)

helix5XXX nuc i j partial_inst
  = [ mk_var i (dgf_base helix5XXX_tfo (get_var j partial_inst) nuc) nuc ]

helix3XXX_tfo
  = Tfo
       FL_LIT(0.9886)  FL_LIT(0.1424) FL_LIT(-0.0482)
      FL_LIT(-0.0961)  FL_LIT(0.8452)  FL_LIT(0.5258)
       FL_LIT(0.1156) FL_LIT(-0.5152)  FL_LIT(0.8492)
       FL_LIT(3.4426)  FL_LIT(2.0474) FL_LIT(-3.7042)

helix3XXX nuc i j partial_inst
  = [ mk_var i (dgf_base helix3XXX_tfo (get_var j partial_inst) nuc) nuc ]

g37_a38_tfo
  = Tfo
       FL_LIT(0.9991)  FL_LIT(0.0164) FL_LIT(-0.0387)
      FL_LIT(-0.0375)  FL_LIT(0.7616) FL_LIT(-0.6470)
       FL_LIT(0.0189)  FL_LIT(0.6478)  FL_LIT(0.7615)
      FL_LIT(-3.3018)  FL_LIT(0.9975)  FL_LIT(2.5585)

g37_a38 nuc i j partial_inst
  = mk_var i (dgf_base g37_a38_tfo (get_var j partial_inst) nuc) nuc

stacked5XXX nuc i j partial_inst
  = (g37_a38 nuc i j partial_inst) : (helix5XXX nuc i j partial_inst)

a38_g37_tfo
  = Tfo
       FL_LIT(0.9991) FL_LIT(-0.0375)  FL_LIT(0.0189)
       FL_LIT(0.0164)  FL_LIT(0.7616)  FL_LIT(0.6478) 
      FL_LIT(-0.0387) FL_LIT(-0.6470)  FL_LIT(0.7615)
       FL_LIT(3.3819)  FL_LIT(0.7718) FL_LIT(-2.5321)

a38_g37 nuc i j partial_inst
  = mk_var i (dgf_base a38_g37_tfo (get_var j partial_inst) nuc) nuc
   
stacked3XXX nuc i j partial_inst
  = a38_g37 nuc i j partial_inst : helix3XXX nuc i j partial_inst

p_o3XXX nucs i j partial_inst
  = generate [] nucs
    where
      ref = get_var j partial_inst
      align = tfo_inv_ortho
                (tfo_align (atom_pos nuc_O3XXX ref)
                           (atom_pos nuc_C3XXX ref)
                           (atom_pos nuc_C4XXX ref))
      generate domains []
        = domains
      generate domains (n:ns)
        = generate
           ((mk_var i (tfo_combine (nuc_p_o3XXX_60_tfo n) align) n):
            (mk_var i (tfo_combine (nuc_p_o3XXX_180_tfo n) align) n):
            (mk_var i (tfo_combine (nuc_p_o3XXX_275_tfo n) align) n):domains)
           ns

-- -- PROBLEM STATEMENT -------------------------------------------------------

-- Define anticodon problem -- Science 253:1255 Figure 3a, 3b and 3c

anticodon_domains
  = [
     reference rC  INT_LIT(27),
     helix5XXX   rC  INT_LIT(28) INT_LIT(27),
     helix5XXX   rA  INT_LIT(29) INT_LIT(28),
     helix5XXX   rG  INT_LIT(30) INT_LIT(29),
     helix5XXX   rA  INT_LIT(31) INT_LIT(30),
     wc        rU  INT_LIT(39) INT_LIT(31),
     helix5XXX   rC  INT_LIT(40) INT_LIT(39),
     helix5XXX   rU  INT_LIT(41) INT_LIT(40),
     helix5XXX   rG  INT_LIT(42) INT_LIT(41),
     helix5XXX   rG  INT_LIT(43) INT_LIT(42),
     stacked3XXX rA  INT_LIT(38) INT_LIT(39),
     stacked3XXX rG  INT_LIT(37) INT_LIT(38),
     stacked3XXX rA  INT_LIT(36) INT_LIT(37),
     stacked3XXX rA  INT_LIT(35) INT_LIT(36),
     stacked3XXX rG  INT_LIT(34) INT_LIT(35), -- <-. Distance
     p_o3XXX     rCs INT_LIT(32) INT_LIT(31), --  | Constraint
     p_o3XXX     rUs INT_LIT(33) INT_LIT(32)  -- <-XXX 3.0 Angstroms
    ]

-- Anticodon constraint

anticodon_constraint v@(Var INT_LIT(33) t n) partial_inst = dist INT_LIT(34) _LE_FLT_ FL_LIT(3.0)
    where dist j = pt_dist p o3XXX
                   where p = atom_pos nuc_P (get_var j partial_inst)
                         o3XXX = atom_pos nuc_O3XXX v
anticodon_constraint _ _ = True

anticodon = search [] anticodon_domains anticodon_constraint

-- Define pseudoknot problem -- Science 253:1255 Figure 4a and 4b

pseudoknot_domains
  = [
     reference rA  INT_LIT(23),
     wc_dumas  rU   INT_LIT(8) INT_LIT(23),
     helix3XXX   rG  INT_LIT(22) INT_LIT(23),
     wc_dumas  rC   INT_LIT(9) INT_LIT(22),
     helix3XXX   rG  INT_LIT(21) INT_LIT(22),
     wc_dumas  rC  INT_LIT(10) INT_LIT(21),
     helix3XXX   rC  INT_LIT(20) INT_LIT(21),
     wc_dumas  rG  INT_LIT(11) INT_LIT(20),
     helix3XXX   rU'{-'-} INT_LIT(19) INT_LIT(20), -- <-.
     wc_dumas  rA  INT_LIT(12) INT_LIT(19), --  | Distance
--                        --  | Constraint
-- Helix 1                --  | 4.0 Angstroms
     helix3XXX   rC   INT_LIT(3) INT_LIT(19), --  |
     wc_dumas  rG  INT_LIT(13)  INT_LIT(3), --  |
     helix3XXX   rC   INT_LIT(2)  INT_LIT(3), --  |
     wc_dumas  rG  INT_LIT(14)  INT_LIT(2), --  |
     helix3XXX   rC   INT_LIT(1)  INT_LIT(2), --  |
     wc_dumas  rG'{-'-} INT_LIT(15)  INT_LIT(1), --  |
--                        --  |
-- L2 LOOP                --  |
     p_o3XXX     rUs INT_LIT(16) INT_LIT(15), --  |
     p_o3XXX     rCs INT_LIT(17) INT_LIT(16), --  |
     p_o3XXX     rAs INT_LIT(18) INT_LIT(17), -- <-XXX
--
-- L1 LOOP
     helix3XXX   rU   INT_LIT(7)  INT_LIT(8), -- <-.
     p_o3XXX     rCs  INT_LIT(4)  INT_LIT(3), --  | Constraint
     stacked5XXX rU   INT_LIT(5)  INT_LIT(4), --  | 4.5 Angstroms
     stacked5XXX rC   INT_LIT(6)  INT_LIT(5)  -- <-XXX
    ]
  
-- Pseudoknot constraint

pseudoknot_constraint v@(Var i t n) partial_inst
  | i _EQ_INT_ INT_LIT(18)
  = dist INT_LIT(19) _LE_FLT_ FL_LIT(4.0)
  | i _EQ_INT_ INT_LIT(6)
  = dist INT_LIT(7) _LE_FLT_ FL_LIT(4.5)
  | otherwise
  = True
    where dist j = pt_dist p o3XXX
                   where p = atom_pos nuc_P (get_var j partial_inst)
                         o3XXX = atom_pos nuc_O3XXX v

pseudoknot = search [] pseudoknot_domains pseudoknot_constraint

-- -- TESTING -----------------------------------------------------------------

list_of_atoms n = list_of_common_atoms n ++ list_of_specific_atoms n

list_of_common_atoms
  (Nuc
    dgf_base_tfo p_o3XXX_275_tfo p_o3XXX_180_tfo p_o3XXX_60_tfo
    p o1p o2p o5XXX c5XXX h5XXX h5XXXXXX c4XXX h4XXX o4XXX c1XXX h1XXX c2XXX h2XXXXXX o2XXX h2XXX
    c3XXX h3XXX o3XXX n1 n3 c2 c4 c5 c6
    x)
  = [p,o1p,o2p,o5XXX,c5XXX,h5XXX,h5XXXXXX,c4XXX,h4XXX,o4XXX,c1XXX,h1XXX,c2XXX,h2XXXXXX,o2XXX,h2XXX,c3XXX,
     h3XXX,o3XXX,n1,n3,c2,c4,c5,c6]

list_of_specific_atoms
  (Nuc
    dgf_base_tfo p_o3XXX_275_tfo p_o3XXX_180_tfo p_o3XXX_60_tfo
    p o1p o2p o5XXX c5XXX h5XXX h5XXXXXX c4XXX h4XXX o4XXX c1XXX h1XXX c2XXX h2XXXXXX o2XXX h2XXX
    c3XXX h3XXX o3XXX n1 n3 c2 c4 c5 c6
    (A n6 n7 n9 c8 h2 h61 h62 h8))
  = [n6,n7,n9,c8,h2,h61,h62,h8]

list_of_specific_atoms
  (Nuc
    dgf_base_tfo p_o3XXX_275_tfo p_o3XXX_180_tfo p_o3XXX_60_tfo
    p o1p o2p o5XXX c5XXX h5XXX h5XXXXXX c4XXX h4XXX o4XXX c1XXX h1XXX c2XXX h2XXXXXX o2XXX h2XXX
    c3XXX h3XXX o3XXX n1 n3 c2 c4 c5 c6
    (C n4 o2 h41 h42 h5 h6))
  = [n4,o2,h41,h42,h5,h6]

list_of_specific_atoms
  (Nuc
    dgf_base_tfo p_o3XXX_275_tfo p_o3XXX_180_tfo p_o3XXX_60_tfo
    p o1p o2p o5XXX c5XXX h5XXX h5XXXXXX c4XXX h4XXX o4XXX c1XXX h1XXX c2XXX h2XXXXXX o2XXX h2XXX
    c3XXX h3XXX o3XXX n1 n3 c2 c4 c5 c6
    (G n2 n7 n9 c8 o6 h1 h21 h22 h8))
  = [n2,n7,n9,c8,o6,h1,h21,h22,h8]

list_of_specific_atoms
  (Nuc
    dgf_base_tfo p_o3XXX_275_tfo p_o3XXX_180_tfo p_o3XXX_60_tfo
    p o1p o2p o5XXX c5XXX h5XXX h5XXXXXX c4XXX h4XXX o4XXX c1XXX h1XXX c2XXX h2XXXXXX o2XXX h2XXX
    c3XXX h3XXX o3XXX  n1 n3 c2 c4 c5 c6
    (U o2 o4 h3 h5 h6))
  = [o2,o4,h3,h5,h6]

var_most_distant_atom v@(Var i t n)
#ifdef USE_GLASGOW_HACKS
  = maximum_map distance (list_of_atoms n)
#else
  = maximum (map distance (list_of_atoms n))
#endif
#ifdef USE_UNBOXED_FLOATS
    -- partain: can't do lazy pattern-match if x y z are unboxed...
    where distance p = case (absolute_pos v p) of { Pt x y z ->
		       _SQRT_ ((x _MUL_ x) _ADD_ (y _MUL_ y) _ADD_ (z _MUL_ z))
		       }
#else
    -- original (partain)
    where distance p = BOX_FLOAT(_SQRT_ ((x _MUL_ x) _ADD_ (y _MUL_ y) _ADD_ (z _MUL_ z)))
                       where Pt x y z = absolute_pos v p
#endif

#ifdef USE_GLASGOW_HACKS
sol_most_distant_atom s = maximum_map var_most_distant_atom s

most_distant_atom sols = maximum_map sol_most_distant_atom sols
#else
sol_most_distant_atom s = maximum (map var_most_distant_atom s)

most_distant_atom sols = maximum (map sol_most_distant_atom sols)
#endif

#ifdef USE_GLASGOW_HACKS
maximum_map :: (a->Float#) -> [a]->Float#
maximum_map f (h:t) =
	max f t (f h)
	where max f (x:xs) m = max f xs (let fx = f x in if fx `gtFloat#` m then fx else m)
	      max f [] m = m
	      max :: (a->Float#) -> [a] -> Float# -> Float#
#endif

check = length pseudoknot

-- To run program, evaluate: run

-- Printing is slow because of the way the Prelude's structured.
-- We use direct C Calls and monadic IO instead

#ifdef USE_GLASGOW_HACKS
mainPrimIO = 
	let most_distant = most_distant_atom pseudoknot in
	_ccall_ printf ``"%f\n"'' (F# most_distant) `seqPrimIO`
	returnPrimIO ()
#else
main = print ({-run=-} most_distant_atom pseudoknot)
#endif
