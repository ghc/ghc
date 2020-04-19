/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * Task API
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * -------------------------------------------------------------------------- */

#pragma once

typedef struct Task_ Task;

// Create a new Task for a bound thread.  This Task must be released
// by calling boundTaskExiting.  The Task is cached in
// thread-local storage and will remain even after boundTaskExiting()
// has been called; to free the memory, see freeMyTask().
//
Task* newBoundTask (void);

// Return the current OS thread's Task, which is created if it doesn't already
// exist.  After you have finished using RTS APIs, you should call freeMyTask()
// to release this thread's Task.
Task* getTask (void);

// The current task is a bound task that is exiting.
//
void boundTaskExiting (Task *task);

// Free a Task if one was previously allocated by newBoundTask().
// This is not necessary unless the thread that called newBoundTask()
// will be exiting, or if this thread has finished calling Haskell
// functions.
//
void freeMyTask(void);

