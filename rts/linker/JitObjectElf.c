#include "BufferBuilder.h"
#include "elf_compat.h"
#include "linker/Elf.h"
#include "JitObject.h"
#include "LinkerInternals.h"
#include "RtsUtils.h"

#include <string.h>

#define TEXT_SECTION_IDX 1

struct BufferBuilder build_jit_object(ObjectCode *oc)
{
    struct BufferBuilder bb = buffer_builder_new(4096);

    // ELF header
    Elf64_Ehdr ehdr = {
        .e_ident = {
            ELFMAG0, ELFMAG1, ELFMAG2, ELFMAG3,
            ELFCLASS64,
            ELFDATA2LSB,
            EV_CURRENT,
        },
        .e_type = ET_REL,
        .e_machine = EM_386,
        .e_version = EV_CURRENT,
        .e_entry = 0,
        .e_phoff = 0,
        .e_shoff = 0, // will be filled in later
        .e_flags = 0,
        .e_ehsize = sizeof(Elf64_Ehdr),
        .e_phentsize = sizeof(Elf64_Phdr),
        .e_phnum = 0,
        .e_shentsize = sizeof(Elf64_Shdr),
        .e_shnum = 2,
        .e_shstrndx =  0,
    };

    Elf64_Ehdr *ehdr_ptr = buffer_builder_push_struct(&bb, ehdr);

    // String table
    struct BufferBuilder strings = buffer_builder_new(4096);
    buffer_builder_uint8(&strings, 0);

    // Section table
    struct BufferBuilder shtab = buffer_builder_new(4096);
    Elf64_Shdr shtab_zero_ent = {
        .sh_name = 0,
        .sh_type = SHT_NULL,
        .sh_flags = 0,
        .sh_addr = 0,
        .sh_offset = 0,
        .sh_size = 0,
        .sh_link = SHN_UNDEF,
        .sh_info = 0,
        .sh_addralign = 0,
        .sh_entsize = 0,
    };
    buffer_builder_push_struct(&shtab, shtab_zero_ent);

    Elf64_Shdr shtab_text_ent = {
        .sh_name = 1,
        .sh_type = SHT_NULL,
        .sh_flags = SHF_ALLOC | SHF_EXECINSTR,
        .sh_addr = 0,
        .sh_offset = 0, // will be filled in later
        .sh_size = 0,
        .sh_link = SHN_UNDEF,
        .sh_info = 0,
        .sh_addralign = 0,
        .sh_entsize = 0,
    };
    buffer_builder_push_struct(&shtab, shtab_zero_ent);

    // Symbol table
    struct BufferBuilder symtab = buffer_builder_new(4096);
    Elf64_Sym symtab_zero_ent = {
        .st_name = 0,
        .st_value = 0,
        .st_size = 0,
        .st_info = 0,
        .st_other = 0,
        .st_shndx = SHN_UNDEF,
    };
    buffer_builder_push_struct(&symtab, symtab_zero_ent);

    for (int i=0; i < oc->n_symbols; i++) {
        Symbol_t *sym = &oc->symbols[i];
        size_t sym_name_offset = buffer_builder_filled_size(&strings);
        buffer_builder_push(&strings, sym->name, strlen(sym->name)+1);

        Elf64_Sym symtab_ent = {
            .st_name = sym_name_offset,
            .st_value = hi,
            .st_size = 0,
            .st_info = 0,
            .st_other = 0,
            .st_shndx = TEXT_SECTION_IDX,
        };
        buffer_builder_push_struct(&symtab, symtab_ent);
    }

    return bb;
}

