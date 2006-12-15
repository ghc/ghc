/* -----------------------------------------------------------------------------
 * ThreadLabels.h
 *
 * (c) The GHC Team 2002-2006
 *
 * Table of thread labels.
 *
 * ---------------------------------------------------------------------------*/

#ifndef __THREADLABELS_H__
#define __THREADLABELS_H__

#if defined(DEBUG)
void    initThreadLabelTable (void);
void    freeThreadLabelTable (void);
void    updateThreadLabel    (StgWord key, void *data);
void *  lookupThreadLabel    (StgWord key);
void    removeThreadLabel    (StgWord key);
void    labelThread          (StgPtr tso, char *label);
#endif

#endif /* __THREADLABELS_H__ */
