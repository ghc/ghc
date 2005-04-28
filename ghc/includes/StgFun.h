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

/*
 * Specialised function types: bitmaps and calling sequences
 * for these functions are pre-generated: see ghc/utils/genapply and
 * generated code in ghc/rts/AutoApply.cmm.
 *
 *  NOTE: other places to change if you change this table:
 *       - utils/genapply/GenApply.hs: stackApplyTypes
 *       - compiler/codeGen/CgCallConv.lhs: stdPattern
 */
#define ARG_NONE     3 
#define ARG_N        4  
#define ARG_P        5 
#define ARG_F        6 
#define ARG_D        7 
#define ARG_L        8 
#define ARG_NN       9 
#define ARG_NP       10
#define ARG_PN       11
#define ARG_PP       12
#define ARG_NNN      13
#define ARG_NNP      14
#define ARG_NPN      15
#define ARG_NPP      16
#define ARG_PNN      17
#define ARG_PNP      18
#define ARG_PPN      19
#define ARG_PPP      20
#define ARG_PPPP     21
#define ARG_PPPPP    22
#define ARG_PPPPPP   23
#define ARG_PPPPPPP  24
#define ARG_PPPPPPPP 25

#endif /* STGFUN_H */
