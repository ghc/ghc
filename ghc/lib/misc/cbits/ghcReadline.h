#ifndef GHC_READLINE_H
#define GHC_READLINE_H

/* Included to see the defn. the HAVE_* below */
#include "config.h"

#if HAVE_READLINE_READLINE_H
#include <readline/readline.h>
#include <readline/history.h>
#endif

/* For some reason the following 3 aren't defined in readline.h */
extern int rl_mark;
extern int rl_done;
extern int rl_pending_input;


/* Our C Hackery stuff for Callbacks */
typedef I_ KeyCode;
extern StgStablePtr cbackList;
I_ genericRlCback (I_, I_);
extern StgStablePtr haskellRlEntry;
extern I_ current_narg, rl_return;
extern KeyCode current_kc;
extern char* rl_prompt_hack;

#endif /* !GHC_READLINE_H */
