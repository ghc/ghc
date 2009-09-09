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

BEGIN_RTS_PRIVATE

#if defined(DEBUG)
void    initThreadLabelTable (void);
void    freeThreadLabelTable (void);
void    updateThreadLabel    (StgWord key, void *data);
void *  lookupThreadLabel    (StgWord key);
void    removeThreadLabel    (StgWord key);
void    labelThread          (StgPtr tso, char *label);
#endif

END_RTS_PRIVATE

#endif /* THREADLABELS_H */
