/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: timezone.c,v 1.2 1998/12/02 13:28:00 simonm Exp $
 *
 * Timezone Runtime Support
 */

#include "Rts.h"
#include "stgio.h"
#include "timezone.h"

StgInt get_tm_sec   ( StgAddr x ) { return ((struct tm*)x)->tm_sec;   }
StgInt get_tm_min   ( StgAddr x ) { return ((struct tm*)x)->tm_min;   }
StgInt get_tm_hour  ( StgAddr x ) { return ((struct tm*)x)->tm_hour;  }
StgInt get_tm_mday  ( StgAddr x ) { return ((struct tm*)x)->tm_mday;  }
StgInt get_tm_mon   ( StgAddr x ) { return ((struct tm*)x)->tm_mon;   }
StgInt get_tm_year  ( StgAddr x ) { return ((struct tm*)x)->tm_year;  }
StgInt get_tm_wday  ( StgAddr x ) { return ((struct tm*)x)->tm_wday;  }
StgInt get_tm_yday  ( StgAddr x ) { return ((struct tm*)x)->tm_yday;  }
StgInt get_tm_isdst ( StgAddr x ) { return ((struct tm*)x)->tm_isdst; }
StgAddr prim_ZONE    ( StgAddr x ) { return ZONE(x);   }
StgInt prim_GMTOFF  ( StgAddr x ) { return GMTOFF(x); }

StgInt prim_SETZONE ( StgAddr x, StgAddr y )
{
  SETZONE(x,y);
}

StgInt sizeof_word      ( void ) { return (sizeof(unsigned int)); }
StgInt sizeof_struct_tm	( void ) { return (sizeof(struct tm)); }
StgInt sizeof_time_t    ( void ) { return (sizeof(time_t)); }

