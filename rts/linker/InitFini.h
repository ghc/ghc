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

void addInitFini(struct InitFiniList **slist, Section *section, enum InitFiniKind kind, uint32_t priority);
void freeInitFiniList(struct InitFiniList *slist);
bool runInit(struct InitFiniList **slist);
bool runFini(struct InitFiniList **slist);
