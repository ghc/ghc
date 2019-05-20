#include "Rts.h"
#include "TraceDump.h"
#include "Trace.h"
#include "Printer.h"

#if defined(TRACE_DUMP)
static int gc_n = 0;
static FILE *trace_dump = NULL;
static char current_src[255] = "unknown";

void 
trace_dump_start_gc(void)
{
    trace_dump_end_gc();

    char fname[255];
    snprintf(fname, 255, "trace-dumps/%05d.dot", gc_n);
    trace_dump = fopen(fname, "w");
    if (trace_dump == NULL) abort();
    fprintf(trace_dump, "digraph {\n");

    debugBelch("trace dump: Starting trace %d\n", gc_n);
    //trace(TRACE_gc, "trace dump: Starting trace %d\n", gc_n);
    gc_n++;
}

void
trace_dump_end_gc(void)
{
    if (trace_dump) {
        fprintf(trace_dump, "}\n");
        fclose(trace_dump);
    }
    trace_dump = NULL;
}

void
trace_dump_note(const char *s)
{
    if (!trace_dump)
        return;
    fprintf(trace_dump, "  # %s\n", s);
}

void
trace_dump_set_source(const char *c)
{
    strncpy(current_src, c, sizeof(current_src));
}

void
trace_dump_set_source_closure(StgClosure *c)
{
    c = UNTAG_CLOSURE(c);
    snprintf(current_src, sizeof(current_src), "%p", c);
    if (!trace_dump)
        return;

    const StgInfoTable *info = get_itbl(c);
    const char *type;
    switch ( info->type ) {
    case CONSTR:
    case CONSTR_1_0: case CONSTR_0_1:
    case CONSTR_1_1: case CONSTR_0_2: case CONSTR_2_0:
    case CONSTR_NOCAF:
    {
        const StgConInfoTable *con_info = get_con_itbl (c);
        type = GET_CON_DESC(con_info);
        break;
    }
    default:
        type = closure_type_names[info->type];
    }

    const char *where;
    if (HEAP_ALLOCED(c)) {
        if (Bdescr((StgPtr) c)->flags & BF_NONMOVING) {
            where = "nonmoving";
        } else {
            where = "moving";
        }
    } else {
        where = "static";
    }

    fprintf(trace_dump, "  \"%p\" [label=\"%p\\n%s\" info=\"%p\" type=\"%s\" where=\"%s\"];\n", 
            UNTAG_CLOSURE(c), UNTAG_CLOSURE(c), type, info, type, where);
}

void
trace_dump_edge(StgClosure *tgt)
{
    if (!trace_dump)
        return;
    fprintf(trace_dump, "  \"%s\" -> \"%p\";\n", current_src, UNTAG_CLOSURE(tgt));
}

#endif
