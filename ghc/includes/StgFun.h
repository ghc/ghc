/* -----------------------------------------------------------------------------
 * (c) The GHC Team, 2002
 *
 * Things for functions.
 * ---------------------------------------------------------------------------*/

#ifndef STGFUN_H
#define STGFUN_H

/* generic - function comes with a small bitmap */
#define ARG_GEN      0   

/* generic - function comes with a large bitmap */
#define ARG_GEN_BIG  1

/* specialised function types: bitmaps and calling sequences
 * for these functions are pre-generated (see ghc/utils/genapply), and
 * the generated code in ghc/rts/AutoApply.hc.
 */
#define ARG_N        2
#define ARG_P        3
#define ARG_F        4
#define ARG_D        5
#define ARG_L        6
#define ARG_NN       7
#define ARG_NP       8
#define ARG_PN       9
#define ARG_PP       10
#define ARG_FF       11
#define ARG_DD       12
#define ARG_LL       13
#define ARG_NNN      14
#define ARG_NNP      15
#define ARG_NPN      16
#define ARG_NPP      17
#define ARG_PNN      18
#define ARG_PNP      19
#define ARG_PPN      20
#define ARG_PPP      21
#define ARG_PPPP     22
#define ARG_PPPPP    23
#define ARG_PPPPPP   24
#define ARG_PPPPPPP  25
#define ARG_PPPPPPPP 26

#endif // STGFUN_H
