/* -*- mode: hugs-c; -*- */
/* --------------------------------------------------------------------------
 * Pretty printer for stg code:
 * ------------------------------------------------------------------------*/

Void printStg( FILE *fp, StgVar b);
            
#if DEBUG_PRINTER
extern Void ppStg        ( StgVar v );
extern Void ppStgExpr    ( StgExpr e );
extern Void ppStgRhs     ( StgRhs rhs );
extern Void ppStgAlts    ( List alts );
extern Void ppStgPrimAlts( List alts );
extern Void ppStgVars    ( List vs );
#endif

