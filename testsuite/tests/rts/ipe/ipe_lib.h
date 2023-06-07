#pragma once

#include "Rts.h"

typedef struct {
    char *buffer;
    size_t n;
    size_t size;
} StringTable;

void init_string_table(StringTable *st);
uint32_t add_string(StringTable *st, const char *s);

IpeBufferListNode *makeAnyProvEntries(Capability *cap, int start, int end);
IpeBufferEntry makeAnyProvEntry(Capability *cap, StringTable *st, int i);
void dumpIPEToEventLog(void);

