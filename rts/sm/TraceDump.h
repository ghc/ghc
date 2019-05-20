#if defined(DEBUG)
//#define TRACE_DUMP
#endif

#if defined(TRACE_DUMP)

void trace_dump_start_gc(void);
void trace_dump_end_gc(void);
void trace_dump_note(const char *s);
void trace_dump_set_source(const char *c);
void trace_dump_set_source_closure(StgClosure *c);
void trace_dump_edge(StgClosure *tgt);

#else

static inline void trace_dump_start_gc(void) {}
static inline void trace_dump_end_gc(void) {}
static inline void trace_dump_note(const char *s STG_UNUSED) {}
static inline void trace_dump_set_source(const char *c STG_UNUSED) {}
static inline void trace_dump_set_source_closure(StgClosure *c STG_UNUSED) {}
static inline void trace_dump_edge(StgClosure *tgt STG_UNUSED) {}

#endif
