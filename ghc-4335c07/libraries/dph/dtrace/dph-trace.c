#include "dph-trace.h"

void dph_loop_entry(char *s)
{
  if( HASKELL_DPH_LOOP_ENTRY_ENABLED() )
    HASKELL_DPH_LOOP_ENTRY(s);
}

void dph_loop_exit(char *s)
{
  if( HASKELL_DPH_LOOP_EXIT_ENABLED() )
    HASKELL_DPH_LOOP_EXIT(s);
}

