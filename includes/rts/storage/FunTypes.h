/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2002
 *
 * Things for functions.
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTS_STORAGE_FUNTYPES_H
#define RTS_STORAGE_FUNTYPES_H

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
#define ARG_V16      9 
#define ARG_V32      10
#define ARG_NN       11 
#define ARG_NP       12
#define ARG_PN       13
#define ARG_PP       14
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

#endif /* RTS_STORAGE_FUNTYPES_H */
