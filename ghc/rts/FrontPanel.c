/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2000
 *
 * RTS GTK Front Panel
 *
 * ---------------------------------------------------------------------------*/

#ifdef RTS_GTK_FRONTPANEL

/* Alas, not Posix. */
/* #include "PosixSource.h" */

#include "Rts.h"
#include "RtsUtils.h"
#include "MBlock.h"
#include "FrontPanel.h"
#include "Storage.h"
#include "Stats.h"
#include "RtsFlags.h"
#include "Schedule.h"

#include <unistd.h>
#include <gdk/gdktypes.h>
#include <gtk/gtk.h>

#include "VisSupport.h"
#include "VisWindow.h"

static GtkWidget *window, *map_drawing_area, *gen_drawing_area;
static GtkWidget *res_drawing_area;
static GtkWidget *continue_but, *stop_but, *quit_but;
static GtkWidget *statusbar;
static GtkWidget *live_label, *allocated_label;
static GtkWidget *footprint_label, *alloc_rate_label;
static GtkWidget *map_ruler, *gen_ruler;
static GtkWidget *res_vruler, *res_hruler;
static GtkWidget *running_label, *b_read_label, *b_write_label, *total_label;
static GtkWidget *b_mvar_label, *b_bh_label, *b_throwto_label, *sleeping_label;

static guint status_context_id;

gboolean continue_now = FALSE, stop_now = FALSE, quit = FALSE;
UpdateMode update_mode = Continuous;

static GdkPixmap *map_pixmap = NULL;
static GdkPixmap *gen_pixmap = NULL;
static GdkPixmap *res_pixmap = NULL;

#define N_GENS 10

static GdkColor 
    bdescr_color = { 0, 0xffff, 0, 0 },	/* red */
    free_color   = { 0, 0, 0, 0xffff },	/* blue */
    gen_colors[N_GENS] = {
	{ 0, 0, 0xffff, 0 },
	{ 0, 0, 0xf000, 0 },
	{ 0, 0, 0xe000, 0 },
	{ 0, 0, 0xd000, 0 },
	{ 0, 0, 0xc000, 0 },
	{ 0, 0, 0xb000, 0 },
	{ 0, 0, 0xa000, 0 },
	{ 0, 0, 0x9000, 0 },
	{ 0, 0, 0x8000, 0 },
	{ 0, 0, 0x7000, 0 }
    };

GdkGC *my_gc = NULL;

static void *mem_start = (void *) 0x50000000;

static void colorBlock( void *addr, GdkColor *color, 
			nat block_width, nat block_height, 
			nat blocks_per_line );

static void residencyCensus( void );
static void updateResidencyGraph( void );
static void updateThreadsPanel( void );

/* Some code pinched from examples/scribble-simple in the GTK+
 * distribution.
 */

/* Create a new backing pixmap of the appropriate size */
static gint 
configure_event( GtkWidget *widget, GdkEventConfigure *event STG_UNUSED,
		 GdkPixmap **pixmap )
{
  if (*pixmap)
    gdk_pixmap_unref(*pixmap);

  *pixmap = gdk_pixmap_new(widget->window,
			   widget->allocation.width,
			   widget->allocation.height,
			   -1);

  gdk_draw_rectangle (*pixmap,
		      widget->style->white_gc,
		      TRUE,
		      0, 0,
		      widget->allocation.width,
		      widget->allocation.height);

  debugBelch("configure!\n");
  updateFrontPanel();
  return TRUE;
}

/* Redraw the screen from the backing pixmap */
static gint 
expose_event( GtkWidget *widget, GdkEventExpose *event, GdkPixmap **pixmap )
{
  gdk_draw_pixmap(widget->window,
		  widget->style->fg_gc[GTK_WIDGET_STATE (widget)],
		  *pixmap,
		  event->area.x, event->area.y,
		  event->area.x, event->area.y,
		  event->area.width, event->area.height);

  return FALSE;
}

void
initFrontPanel( void )
{
    GdkColormap *colormap;
    GtkWidget *gen_hbox;

    gtk_init( &prog_argc, &prog_argv );

    window = create_GHC_Front_Panel();
    map_drawing_area  = lookup_widget(window, "memmap");
    gen_drawing_area  = lookup_widget(window, "generations");
    res_drawing_area  = lookup_widget(window, "res_drawingarea");
    stop_but          = lookup_widget(window, "stop_but");
    continue_but      = lookup_widget(window, "continue_but");
    quit_but          = lookup_widget(window, "quit_but");
    statusbar         = lookup_widget(window, "statusbar");
    live_label        = lookup_widget(window, "live_label");
    footprint_label   = lookup_widget(window, "footprint_label");
    allocated_label   = lookup_widget(window, "allocated_label");
    alloc_rate_label  = lookup_widget(window, "alloc_rate_label");
    gen_hbox          = lookup_widget(window, "gen_hbox");
    gen_ruler         = lookup_widget(window, "gen_ruler");
    map_ruler         = lookup_widget(window, "map_ruler");
    res_vruler        = lookup_widget(window, "res_vruler");
    res_hruler        = lookup_widget(window, "res_hruler");
    running_label     = lookup_widget(window, "running_label");
    b_read_label      = lookup_widget(window, "blockread_label");
    b_write_label     = lookup_widget(window, "blockwrite_label");
    b_mvar_label      = lookup_widget(window, "blockmvar_label");
    b_bh_label        = lookup_widget(window, "blockbh_label");
    b_throwto_label   = lookup_widget(window, "blockthrowto_label");
    sleeping_label    = lookup_widget(window, "sleeping_label");
    total_label       = lookup_widget(window, "total_label");
    
    status_context_id = 
	gtk_statusbar_get_context_id( GTK_STATUSBAR(statusbar), "context" );

    /* hook up some signals for the mem map drawing area */
    gtk_signal_connect (GTK_OBJECT(map_drawing_area), "expose_event",
			(GtkSignalFunc)expose_event, &map_pixmap);
    gtk_signal_connect (GTK_OBJECT(map_drawing_area), "configure_event",
			(GtkSignalFunc)configure_event, &map_pixmap);

    gtk_widget_set_events(map_drawing_area, GDK_EXPOSURE_MASK);

    /* hook up some signals for the gen drawing area */
    gtk_signal_connect (GTK_OBJECT(gen_drawing_area), "expose_event",
			(GtkSignalFunc)expose_event, &gen_pixmap);
    gtk_signal_connect (GTK_OBJECT(gen_drawing_area), "configure_event",
			(GtkSignalFunc)configure_event, &gen_pixmap);

    gtk_widget_set_events(gen_drawing_area, GDK_EXPOSURE_MASK);
    
    /* hook up some signals for the res drawing area */
    gtk_signal_connect (GTK_OBJECT(res_drawing_area), "expose_event",
			(GtkSignalFunc)expose_event, &res_pixmap);
    gtk_signal_connect (GTK_OBJECT(res_drawing_area), "configure_event",
			(GtkSignalFunc)configure_event, &res_pixmap);

    gtk_widget_set_events(res_drawing_area, GDK_EXPOSURE_MASK);
    
    /* allocate our colors */
    colormap = gdk_colormap_get_system();
    gdk_colormap_alloc_color(colormap, &bdescr_color, TRUE, TRUE);
    gdk_colormap_alloc_color(colormap, &free_color, TRUE, TRUE);

    {
	gboolean success[N_GENS];
	gdk_colormap_alloc_colors(colormap, gen_colors, N_GENS, TRUE,
				  TRUE, success);
	if (!success) { barf("can't allocate colors"); }
    }

    /* set the labels on the generation histogram */
    {
	char buf[64];
	nat g, s;
	GtkWidget *label;

	for(g = 0; g < RtsFlags.GcFlags.generations; g++) {
	    for(s = 0; s < generations[g].n_steps; s++) {
		g_snprintf( buf, 64, "%d.%d", g, s );
		label = gtk_label_new( buf );
		gtk_box_pack_start( GTK_BOX(gen_hbox), label,
				    TRUE, TRUE, 5 );
		gtk_widget_show(label);
	    }
	}
    }

    gtk_widget_show(window);

    /* wait for the user to press "Continue" before getting going... */
    gtk_statusbar_push( GTK_STATUSBAR(statusbar), status_context_id, 
			"Program start");
    gtk_widget_set_sensitive( stop_but, FALSE );
    continue_now = FALSE;
    while (continue_now == FALSE) {
	gtk_main_iteration();
    }
    gtk_statusbar_pop( GTK_STATUSBAR(statusbar), status_context_id );
    gtk_statusbar_push( GTK_STATUSBAR(statusbar), status_context_id, 
			"Running");

    gtk_widget_set_sensitive( continue_but, FALSE );
    gtk_widget_set_sensitive( stop_but, TRUE );
    gtk_widget_set_sensitive( quit_but, FALSE );

    while (gtk_events_pending()) {
	gtk_main_iteration();
    }
}

void
stopFrontPanel( void )
{
    gtk_widget_set_sensitive( quit_but, TRUE );
    gtk_widget_set_sensitive( continue_but, FALSE );
    gtk_widget_set_sensitive( stop_but, FALSE );

    updateFrontPanel();

    gtk_statusbar_push( GTK_STATUSBAR(statusbar), status_context_id, 
			"Program finished");

    quit = FALSE;
    while (quit == FALSE) {
	gtk_main_iteration();
    }
}

static void
waitForContinue( void )
{
    gtk_widget_set_sensitive( continue_but, TRUE );
    gtk_widget_set_sensitive( stop_but, FALSE );
    stop_now = FALSE;
    continue_now = FALSE;
    while (continue_now == FALSE) {
	gtk_main_iteration();
    }
    gtk_widget_set_sensitive( continue_but, FALSE );
    gtk_widget_set_sensitive( stop_but, TRUE );
}

void
updateFrontPanelBeforeGC( nat N )
{
    char buf[1000];

    updateFrontPanel();

    if (update_mode == BeforeGC 
	|| update_mode == BeforeAfterGC
	|| stop_now == TRUE) {
	g_snprintf( buf, 1000, "Stopped (before GC, generation %d)", N );
	gtk_statusbar_push( GTK_STATUSBAR(statusbar), status_context_id, buf );
	waitForContinue();
	gtk_statusbar_pop( GTK_STATUSBAR(statusbar), status_context_id );
    }

    g_snprintf( buf, 1000, "Garbage collecting (generation %d)", N );
    gtk_statusbar_push( GTK_STATUSBAR(statusbar), status_context_id, buf);

    while (gtk_events_pending()) {
	gtk_main_iteration();
    }
}

static void
numLabel( GtkWidget *lbl, nat n )
{
    char buf[64];
    g_snprintf(buf, 64, "%d", n);
    gtk_label_set_text( GTK_LABEL(lbl), buf );
}

void
updateFrontPanelAfterGC( nat N, lnat live )
{
    char buf[1000];

    gtk_statusbar_pop( GTK_STATUSBAR(statusbar), status_context_id );

    /* is a major GC? */
    if (N == RtsFlags.GcFlags.generations-1) {
	residencyCensus();
    }

    updateFrontPanel();

    if (update_mode == AfterGC 
	|| update_mode == BeforeAfterGC
	|| stop_now == TRUE) {
	snprintf( buf, 1000, "Stopped (after GC, generation %d)", N );
	gtk_statusbar_push( GTK_STATUSBAR(statusbar), status_context_id, buf );
	waitForContinue();
	gtk_statusbar_pop( GTK_STATUSBAR(statusbar), status_context_id );
    }

    {
	double words_to_megs = (1024 * 1024) / sizeof(W_);
	double time = mut_user_time();

	snprintf( buf, 1000, "%.2f", (double)live / words_to_megs );
	gtk_label_set_text( GTK_LABEL(live_label), buf );

	snprintf( buf, 1000, "%.2f", (double)total_allocated / words_to_megs );
	gtk_label_set_text( GTK_LABEL(allocated_label), buf );

	snprintf( buf, 1000, "%.2f",
		  (double)(mblocks_allocated * MBLOCK_SIZE_W) / words_to_megs );
	gtk_label_set_text( GTK_LABEL(footprint_label), buf );

	if ( time == 0.0 )
	    snprintf( buf, 1000, "%.2f", time );
	else
	    snprintf( buf, 1000, "%.2f",
		      (double)(total_allocated / words_to_megs) / time );
	gtk_label_set_text( GTK_LABEL(alloc_rate_label), buf );
    }

    while (gtk_events_pending()) {
	gtk_main_iteration();
    }
}

void
updateFrontPanel( void )
{
    void *m, *a;
    bdescr *bd;

    updateThreadsPanel();

    if (my_gc == NULL) {
	my_gc = gdk_gc_new( window->window );
    }

    if (map_pixmap != NULL) {
	nat height, width, blocks_per_line, 
	    block_height, block_width, mblock_height;

	height = map_drawing_area->allocation.height;
	width  = map_drawing_area->allocation.width;

	mblock_height =  height / mblocks_allocated;
	blocks_per_line = 16;
	block_height  = mblock_height / 
	    ((MBLOCK_SIZE/BLOCK_SIZE) / blocks_per_line);
	while (block_height == 0) {
	    blocks_per_line *= 2;
	    block_height  = mblock_height / 
		((MBLOCK_SIZE/BLOCK_SIZE) / blocks_per_line);
	}
	block_width = width / blocks_per_line;

	gdk_draw_rectangle (map_pixmap,
			    map_drawing_area->style->bg_gc[GTK_STATE_NORMAL],
			    TRUE,
			    0, 0,
			    map_drawing_area->allocation.width,
			    map_drawing_area->allocation.height);
	
	for ( m = mem_start; 
	      (char *)m < (char *)mem_start + 
		  (mblocks_allocated * MBLOCK_SIZE); 
	      (char *)m += MBLOCK_SIZE ) {
	    
	    /* color the bdescr area first */
	    for (a = m; a < FIRST_BLOCK(m); (char *)a += BLOCK_SIZE) {
		colorBlock( a, &bdescr_color, 
			    block_width, block_height, blocks_per_line );
	    }
	    
#if 0 /* Segfaults because bd appears to be bogus but != NULL. stolz, 2003-06-24 */
	    /* color each block */
	    for (; a <= LAST_BLOCK(m); (char *)a += BLOCK_SIZE) {
		bd = Bdescr((P_)a);
		ASSERT(bd->start == a);
                if (bd->flags & BF_FREE) {
		    colorBlock( a, &free_color, 
				block_width, block_height, blocks_per_line );
		} else {
		    colorBlock( a, &gen_colors[bd->gen_no],
				block_width, block_height, blocks_per_line );
		}
	    }
#endif
	}

	
	{ 
	    nat height = map_drawing_area->allocation.height,
		block_height, mblock_height;

	    block_height = (height / mblocks_allocated) / 
		((MBLOCK_SIZE/BLOCK_SIZE) / blocks_per_line);
	    if (block_height < 1) block_height = 1;
	    mblock_height = block_height * 
		((MBLOCK_SIZE/BLOCK_SIZE) / blocks_per_line);

	    gtk_ruler_set_range( GTK_RULER(map_ruler), 0, 
				 (double)(height * mblocks_allocated) / 
				 (double)((mblock_height * mblocks_allocated)),
				 0,
				 (double)(height * mblocks_allocated) / 
				 (double)((mblock_height * mblocks_allocated))
		);
	}
				  
	gtk_widget_draw( map_drawing_area, NULL );
    }

    if (gen_pixmap != NULL) {

	GdkRectangle rect;
	nat g, s, columns, column, max_blocks, height_blocks,
	    width, height;
	
	gdk_draw_rectangle (gen_pixmap,
			    gen_drawing_area->style->white_gc,
			    TRUE,
			    0, 0,
			    gen_drawing_area->allocation.width,
			    gen_drawing_area->allocation.height);

	height = gen_drawing_area->allocation.height;
	width  = gen_drawing_area->allocation.width;

	columns = 0; max_blocks = 0;
	for(g = 0; g < RtsFlags.GcFlags.generations; g++) {
	    columns += generations[g].n_steps;
	    for(s = 0; s < generations[g].n_steps; s++) {
		if (generations[g].steps[s].n_blocks > max_blocks) {
		    max_blocks = generations[g].steps[s].n_blocks;
		}
	    }
	}

	/* find a reasonable height value larger than max_blocks */
	{ 
	    nat n = 0;
	    while (max_blocks != 0) {
		max_blocks >>= 1; n++;
	    }
	    height_blocks = 1 << n;
	}

	column = 0;
	for(g = 0; g < RtsFlags.GcFlags.generations; g++) {
	    for(s = 0; s < generations[g].n_steps; s++, column++) {
		gdk_gc_set_foreground(my_gc, &gen_colors[g]);

		rect.x = column * (width / columns);

		if (generations[g].steps[s].n_blocks == 0)
		    rect.y = height;
		else
		    rect.y = height - 
			(height * generations[g].steps[s].n_blocks
			 / height_blocks);

		rect.width = (width / columns);
		rect.height = height - rect.y;

		gdk_draw_rectangle( gen_pixmap, my_gc, TRUE/*filled*/, 
				    rect.x, rect.y, rect.width,
				    rect.height );
	    }
	}

	gtk_ruler_set_range( GTK_RULER(gen_ruler), 
			     height_blocks * BLOCK_SIZE / (1024 * 1024),
			     0, 0,
			     height_blocks * BLOCK_SIZE / (1024 * 1024)
	    );

	gtk_widget_draw( gen_drawing_area, NULL );
    }

    if (res_pixmap != NULL) {
	updateResidencyGraph();
    }

    while (gtk_events_pending()) {
	gtk_main_iteration_do(FALSE/*don't block*/);
    }
}

static void
colorBlock( void *addr, GdkColor *color, 
	    nat block_width, nat block_height, nat blocks_per_line )
{
    GdkRectangle rect;
    nat block_no;

    gdk_gc_set_foreground(my_gc, color);

    block_no = ((char *)addr - (char *)mem_start) / BLOCK_SIZE;

    rect.x = (block_no % blocks_per_line) * block_width;
    rect.y = block_no / blocks_per_line * block_height;
    rect.width = block_width;
    rect.height = block_height;
    gdk_draw_rectangle( map_pixmap, my_gc, TRUE/*filled*/, 
			rect.x, rect.y, rect.width, rect.height );
}

static void
updateThreadsPanel( void )
{
    nat running = 0,
	b_read = 0,
	b_write = 0,
	b_mvar = 0,
	b_throwto = 0,
	b_bh = 0,
	sleeping = 0,
	total = 0;

    StgTSO *t;

    for (t = all_threads; t != END_TSO_QUEUE; t = t->global_link) {
	switch (t->what_next) {
	case ThreadKilled:	    break;
	case ThreadComplete:	    break;
	default:
	    switch (t->why_blocked) {
	    case BlockedOnRead:       b_read++;    break;
	    case BlockedOnWrite:      b_write++;   break;
	    case BlockedOnDelay:      sleeping++;  break;
	    case BlockedOnMVar:       b_mvar++;    break;
	    case BlockedOnException:  b_throwto++; break;
	    case BlockedOnBlackHole:  b_bh++;      break;
	    case NotBlocked:          running++;   break;
	    }
	}
    }
    total = running + b_read + b_write + b_mvar + b_throwto + b_bh + sleeping;
    numLabel(running_label,   running);
    numLabel(b_read_label,    b_read);
    numLabel(b_write_label,   b_write);
    numLabel(b_mvar_label,    b_mvar);
    numLabel(b_bh_label,      b_bh);
    numLabel(b_throwto_label, b_throwto);
    numLabel(sleeping_label,  sleeping);
    numLabel(total_label,     total);
}

typedef enum { Thunk, Fun, Constr, BlackHole,
	       Array, Thread, Other, N_Cats } ClosureCategory;

#define N_SLICES 100

static nat *res_prof[N_SLICES];
static double res_time[N_SLICES];
static nat next_slice = 0;

static void
residencyCensus( void )
{
    nat slice = next_slice++, *prof;
    bdescr *bd;
    nat g, s, size, type;
    StgPtr p;
    StgInfoTable *info;

    if (slice >= N_SLICES) {
	barf("too many slices");
    }
    res_prof[slice] = stgMallocBytes(N_Cats * sizeof(nat), "residencyCensus");
    prof = res_prof[slice];
    memset(prof, 0, N_Cats * sizeof(nat));

    res_time[slice] = mut_user_time();
    
    for(g = 0; g < RtsFlags.GcFlags.generations; g++) {
	for(s = 0; s < generations[g].n_steps; s++) {

	    /* skip over g0s0 if multi-generational */
	    if (RtsFlags.GcFlags.generations > 1 &&
		g == 0 && s == 0) continue;

	    if (RtsFlags.GcFlags.generations == 1) {
		bd = generations[g].steps[s].to_blocks;
	    } else {
		bd = generations[g].steps[s].blocks;
	    }

	    for (; bd != NULL; bd = bd->link) {

		p = bd->start;

		while (p < bd->free) {
		    info = get_itbl((StgClosure *)p);
		    type = Other;
		    
		    switch (info->type) {

		    case CONSTR:
		    case BCO:
			if (((StgClosure *)p)->header.info == &stg_DEAD_WEAK_info) {
			    size = sizeofW(StgWeak);
			    type = Other;
			    break;
			}
			/* else, fall through... */
		    case CONSTR_1_0:
		    case CONSTR_0_1:
		    case CONSTR_1_1:
		    case CONSTR_0_2:
		    case CONSTR_2_0:
			size = sizeW_fromITBL(info);
			type = Constr;
			break;
			
		    case FUN_1_0:
		    case FUN_0_1:
			size = sizeofW(StgHeader) + 1;
			goto fun;
		    case FUN_1_1:
		    case FUN_0_2:
		    case FUN_2_0:
		    case FUN:
			size = sizeW_fromITBL(info);
		    fun:
			type = Fun;
			break;

		    case THUNK_1_0:
		    case THUNK_0_1:
		    case THUNK_SELECTOR:
			size = sizeofW(StgHeader) + 2;
			goto thunk;
		    case THUNK_1_1:
		    case THUNK_0_2:
		    case THUNK_2_0:
		    case THUNK:
			size = sizeW_fromITBL(info);
		    thunk:
			type = Thunk;
			break;

		    case CAF_BLACKHOLE:
		    case SE_CAF_BLACKHOLE:
		    case SE_BLACKHOLE:
		    case BLACKHOLE:
		    case BLACKHOLE_BQ:
			size = sizeW_fromITBL(info);
			type = BlackHole;
			break;

		    case AP:
			size = pap_sizeW((StgPAP *)p);
			type = Thunk;
			break;

		    case PAP:
			size = pap_sizeW((StgPAP *)p);
			type = Fun;
			break;
			
		    case ARR_WORDS:
			size = arr_words_sizeW(stgCast(StgArrWords*,p));
			type = Array;
			break;
			
		    case MUT_ARR_PTRS:
		    case MUT_ARR_PTRS_FROZEN:
			size = mut_arr_ptrs_sizeW((StgMutArrPtrs *)p);
			type = Array;
			break;
			
		    case TSO:
			size = tso_sizeW((StgTSO *)p);
			type = Thread;
			break;
			
		    case WEAK:
		    case FOREIGN:
		    case STABLE_NAME:
		    case MVAR:
		    case MUT_VAR:
		    case MUT_CONS:
		    case IND_PERM:
		    case IND_OLDGEN_PERM:
			size = sizeW_fromITBL(info);
			type = Other;
			break;

		    default:
			barf("updateResidencyGraph: strange closure "
                             "%d", info->type );
		    }

		    prof[type] += size;
		    p += size;
		}
	    }
	}
    }

}
	    
static void
updateResidencyGraph( void )
{
    nat total, prev_total, i, max_res;
    double time;
    double time_scale = 1;
    nat last_slice = next_slice-1;
    double res_scale  = 1; /* in megabytes, doubles */
    nat *prof;
    nat width, height;
    GdkPoint points[4];

    gdk_draw_rectangle (res_pixmap,
			res_drawing_area->style->bg_gc[GTK_STATE_NORMAL],
			TRUE,
			0, 0,
			res_drawing_area->allocation.width,
			res_drawing_area->allocation.height);
    
    if (next_slice == 0) return;

    time = res_time[last_slice];
    while (time > time_scale) {
	time_scale *= 2;
    }

    max_res = 0; 
    for (i = 0; i < next_slice; i++) {
	prof = res_prof[i];
	total = prof[Thunk] + prof[Fun] + prof[Constr] +
	    prof[BlackHole] + prof[Array] + prof[Other];
	if (total > max_res) {
	    max_res = total;
	}
    }
    while (max_res > res_scale) {
	res_scale *= 2;
    }

    height = res_drawing_area->allocation.height;
    width  = res_drawing_area->allocation.width;

    points[0].x = 0;
    points[0].y = height;
    points[1].y = height;
    points[3].x = 0;
    points[3].y = height;

    gdk_gc_set_foreground(my_gc, &free_color);

    prev_total = 0;
    for (i = 0; i < next_slice; i++) {
	prof = res_prof[i];
	total = prof[Thunk] + prof[Fun] + prof[Constr] +
	    prof[BlackHole] + prof[Array] + prof[Other];
	points[1].x = width * res_time[i] / time_scale;
	points[2].x = points[1].x;
	points[2].y = height - ((height * total) / res_scale);
	gdk_draw_polygon(res_pixmap, my_gc, TRUE/*filled*/, points, 4);
	points[3] = points[2];
	points[0] = points[1];
    }

    gtk_ruler_set_range( GTK_RULER(res_vruler), 
			 res_scale / ((1024*1024)/sizeof(W_)),
			 0, 0,
			 res_scale / ((1024*1024)/sizeof(W_)) );

    gtk_ruler_set_range( GTK_RULER(res_hruler), 
			 0, time_scale, 0, time_scale );


    gtk_widget_draw( res_drawing_area, NULL );
}

#endif /* RTS_GTK_FRONTPANEL */
