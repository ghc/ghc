#include "BufferBuilder.h"

void *build_jit_object_coff(ObjectCode *oc)
{
    struct BufferBuilder bb = buffer_builder_new(4096);

    // COFF header
    buffer_builder_uint16(bb, IMAGE_FILE_MACHINE_AMD64);
    buffer_builder_uint16(bb, 1); // NumberOfSections
    buffer_builder_uint32(bb, 0); // TimeDateStamp
    uint32_t *ptr_to_symbol_table = buffer_builder_uint32(bb, 0); // PointerToSymbolTable
    buffer_builder_uint32(bb, n_syms); // NumberOfSymbols
    buffer_builder_uint16(bb, 0); // SizeOfOptionalHeader
    buffer_builder_uint16(bb, 0); // Characteristics

    // Sections
    buffer_builder_push(bb, ".text\0\0\0", 8); // Name
    buffer_builder_uint32(bb, sect_sz);        // VirtualSize
    buffer_builder_uint32(bb, sect_base);      // VirtualAddress
    buffer_builder_uint32(bb, 0);              // SizeOfRawData
    buffer_builder_uint32(bb, 0);              // PointerToRawData
    buffer_builder_uint32(bb, 0);              // PointerToRelocations
    buffer_builder_uint32(bb, 0);              // PointerToLinenumbers
    buffer_builder_uint16(bb, 0);              // NumberOfRelocations
    buffer_builder_uint16(bb, 0);              // NumberOfLinenumbers
    buffer_builder_uint32(bb, 0);              // Characteristics

    struct BufferBuilder strings = buffer_builder_new(4096);
    for (int i=0; i < oc->n_symbols; i++) {
        Symbol_t *sym = oc->symbols[i];
        size_t offset = buffer_builder_filled_size(strings);
        buffer_builder_push(strings, sym->name);
        buffer_builder_uint32(bb, 0);
        buffer_builder_uint32(bb, offset); // RawName index
        buffer_builder_uint32(bb, value);  // Value
        buffer_builder_uint16(bb, 1);      // SectionNumber
        buffer_builder_uint8(bb, 0x20);    // StorageClass
        buffer_builder_uint8(bb, 0);       // NumberOfAuxSymbols
    }

    buffer_builder_append(bb, strings);
    return bb->start;
}
