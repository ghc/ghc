/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2000
 *
 * RTS GTK Front Panel (callbacks)
 *
 * ---------------------------------------------------------------------------*/

#ifdef RTS_GTK_FRONTPANEL

#include "Rts.h"

#include <gtk/gtk.h>

#include "VisCallbacks.h"
#include "VisWindow.h"
#include "VisSupport.h"
#include "FrontPanel.h"

void
on_cont_radio_clicked                  (GtkButton       *button,
                                        gpointer         user_data)
{
    update_mode = Continuous;
}


void
on_stop_before_radio_clicked           (GtkButton       *button,
                                        gpointer         user_data)
{
    update_mode = BeforeGC;
}


void
on_stop_after_radio_clicked            (GtkButton       *button,
                                        gpointer         user_data)
{
    update_mode = AfterGC;
}


void
on_stop_both_radio_clicked             (GtkButton       *button,
                                        gpointer         user_data)
{
    update_mode = BeforeAfterGC;
}


void
on_stop_but_clicked                    (GtkButton       *button,
                                        gpointer         user_data)
{
    stop_now = TRUE;
}


void
on_continue_but_clicked                (GtkButton       *button,
                                        gpointer         user_data)
{
    continue_now = TRUE;
}


void
on_quit_but_clicked                    (GtkButton       *button,
                                        gpointer         user_data)
{
    quit = TRUE;
}

#endif /* RTS_GTK_FRONTPANEL */
