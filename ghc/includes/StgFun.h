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

/* BCO - function is really a BCO */
#define ARG_BCO      2

/* specialised function types: bitmaps and calling sequences
 * for these functions are pre-generated (see ghc/utils/genapply), and
 * the generated code in ghc/rts/AutoApply.hc.
 */
#define ARG_N        3 
#define ARG_P        4 
#define ARG_F        5 
#define ARG_D        6 
#define ARG_L        7 
#define ARG_NN       8 
#define ARG_NP       9 
#define ARG_PN       10
#define ARG_PP       11
#define ARG_FF       12
#define ARG_DD       13
#define ARG_LL       14
#define ARG_NNN      15
#define ARG_NNP      16
#define ARG_NPN      17
#define ARG_NPP      18
#define ARG_PNN      19
#define ARG_PNP      20
#define ARG_PPN      21
#define ARG_PPP      22
#define ARG_PPPP     23
#define ARG_PPPPP    24
#define ARG_PPPPPP   25
#define ARG_PPPPPPP  26
#define ARG_PPPPPPPP 27

#endif // STGFUN_H
