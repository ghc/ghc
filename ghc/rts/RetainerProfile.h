/* -----------------------------------------------------------------------------
 * $Id: RetainerProfile.h,v 1.1 2001/11/22 14:25:12 simonmar Exp $
 *
 * (c) The GHC Team, 2001
 * Author: Sungwoo Park
 *
 * Retainer profiling interface.
 *
 * ---------------------------------------------------------------------------*/

#ifndef RETAINERPROFILE_H
#define RETAINERPROFILE_H

#ifdef PROFILING

extern void  initRetainerProfiling ( void );
extern void  endRetainerProfiling  ( void );
extern void  printRetainer         ( FILE *, retainer );
extern void  retainerProfile       ( void );
extern void  resetStaticObjectForRetainerProfiling ( void );

// firstStack is exported because memInventory() in Schedule.c uses it.
#ifdef DEBUG
extern bdescr *firstStack;
#endif

#endif /* PROFILING */

#endif /* RETAINERPROFILE_H */
