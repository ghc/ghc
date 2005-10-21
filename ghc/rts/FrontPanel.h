/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2000-2005
 *
 * RTS GTK Front Panel
 *
 * ---------------------------------------------------------------------------*/

#ifndef FRONTPANEL_H
#define FRONTPANEL_H

#ifdef RTS_GTK_FRONTPANEL

#include "Rts.h"  /* needed because this file gets included by
		   * auto-generated code */

void initFrontPanel( void );
void stopFrontPanel( void );
void updateFrontPanelBeforeGC( nat N );
void updateFrontPanelAfterGC( nat N, lnat live );
void updateFrontPanel( void );


/* --------- PRIVATE ----------------------------------------- */

#include <gdk/gdktypes.h>

typedef enum { BeforeGC, AfterGC, BeforeAfterGC, Continuous } UpdateMode;
extern UpdateMode update_mode;
extern gboolean continue_now, stop_now, quit;

#endif /* RTS_GTK_FRONTPANEL */

#endif /* FRONTPANEL_H */

