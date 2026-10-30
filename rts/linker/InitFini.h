#pragma once

enum InitFiniKind {
    INITFINI_INIT,       // .init section
    INITFINI_FINI,       // .fini section
    INITFINI_CTORS,      // .ctors section
    INITFINI_DTORS,      // .dtors section
    INITFINI_INIT_ARRAY, // .init_array section
    INITFINI_FINI_ARRAY, // .fini_array section
};

// A linked-list of initializer or finalizer sections.
struct InitFiniList {
    Section *section;
    uint32_t priority;
    enum InitFiniKind kind;
    struct InitFiniList *next;
};

RTS_PUBLIC void addInitFini(struct InitFiniList **slist, Section *section, enum InitFiniKind kind, uint32_t priority);
RTS_PUBLIC void freeInitFiniList(struct InitFiniList *slist);
RTS_PUBLIC bool runInit(struct InitFiniList **slist);
RTS_PUBLIC bool runFini(struct InitFiniList **slist);
