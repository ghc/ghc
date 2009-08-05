/* -----------------------------------------------------------------------------
 * ThreadLabels.h
 *
 * (c) The GHC Team 2002-2006
 *
 * Table of thread labels.
 *
 * ---------------------------------------------------------------------------*/

#ifndef THREADLABELS_H
#define THREADLABELS_H

#pragma GCC visibility push(hidden)

#if defined(DEBUG)
void    initThreadLabelTable (void);
void    freeThreadLabelTable (void);
void    updateThreadLabel    (StgWord key, void *data);
void *  lookupThreadLabel    (StgWord key);
void    removeThreadLabel    (StgWord key);
void    labelThread          (StgPtr tso, char *label);
#endif

#pragma GCC visibility pop

#endif /* THREADLABELS_H */
