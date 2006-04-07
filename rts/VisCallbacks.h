#include <gtk/gtk.h>


void
on_cont_radio_clicked                  (GtkButton       *button,
                                        gpointer         user_data);

void
on_stop_before_radio_clicked           (GtkButton       *button,
                                        gpointer         user_data);

void
on_stop_after_radio_clicked            (GtkButton       *button,
                                        gpointer         user_data);

void
on_stop_both_radio_clicked             (GtkButton       *button,
                                        gpointer         user_data);

void
on_stop_but_clicked                    (GtkButton       *button,
                                        gpointer         user_data);

void
on_continue_but_clicked                (GtkButton       *button,
                                        gpointer         user_data);

void
on_quit_but_clicked                    (GtkButton       *button,
                                        gpointer         user_data);
