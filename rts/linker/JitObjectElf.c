/*
 * GDB JIT object interface
 *
 * This module is responsible for generating and registering dummy interface
 * files with gdb for reporting symbols loaded by GHC's runtime linker.
 */

#include "Rts.h"
#include "elf_compat.h"
#include "linker/Elf.h"
#include "JitObject.h"
#include "LinkerInternals.h"
#include "RtsUtils.h"

#include <string.h>

#define CC WSTR("clang")

struct temp_file {
    pathchar *path;
    FILE *handle;
};

static struct temp_file create_temp_file(pathchar *prefix, pathchar *suffix)
{
#if defined(mingw32_HOST_OS)
    // Windows doesn't have a sane temporary file interface that allows setting
    // of a file suffix. Doing this correctly is quite tiresome so we instead
    // use this hack since this is just a debugging facility.
    GUID guid;
    ZeroMemory(&guid, sizeof (guid));
    if (CoCreateGuid(&guid) != S_OK) {
        goto fail;
    }

    RPC_WSTR guid_str;
    if (UuidToStringW ((UUID*)&guid, &guid_str) != S_OK) {
        goto fail;
    }

    size_t path_len = pathlen(prefix) + pathlen(guid_str) + pathlen(suffix) + 1;
    CHECK(path_len < MAX_PATH);
    pathchar *path = stgMallocBytes(sizeof(pathchar) * path_len, "create_temp_file");
    pathcopy(path, prefix);
    pathcopy(path + pathlen(path), guid_str);
    pathcopy(path + pathlen(path), suffix);

    FILE *handle = pathopen(path, WSTR("w+"));
    if (handle == NULL) {
        debugBelch("build_asm: Failed to create temporary file handle: %s\n", strerror(errno));
        goto fail;
    }

    return (struct temp_file) {
        .path = path,
        .handle = handle,
    };
#else
    size_t path_len = strlen(prefix) + 6 + strlen(suffix) + 1;
    char *path = stgMallocBytes(path_len, "create_temp_file");
    snprintf(path, path_len, "%sXXXXXX%s", prefix, suffix);

    int fd = mkstemp(path);
    if (fd == -1) {
        debugBelch("build_asm: Failed to create temporary file: %s\n", strerror(errno));
        goto fail;
    }

    FILE *handle = fdopen(fd, "w+");
    if (handle == NULL) {
        debugBelch("build_asm: Failed to create temporary file handle: %s\n", strerror(errno));
        goto fail;
    }

    return (struct temp_file) {
        .path = path,
        .handle = handle,
    };
#endif

fail:
    return (struct temp_file) {
        .path = NULL,
        .handle = NULL,
    };
}

/* Returns path to assembler source file */
static pathchar* build_asm(ObjectCode *oc)
{
    struct temp_file f = create_temp_file(WSTR("jit-object"), WSTR(".s"));
    if (f.path == NULL) {
        debugBelch("Failed to open JIT object source file: %s\n", strerror(errno));
        return NULL;
    }

    //fprintf(f.handle, ".text\n");
    for (int i=0; i < oc->n_symbols; i++) {
        const Symbol_t *sym = &oc->symbols[i];
        if (sym->name) {
            fprintf(f.handle, ".global %s\n", sym->name);
#if defined(OBJFORMAT_PEi386)
            //fprintf(f.handle, ".def %s; .scl 2; .type 32; .endef\n", sym->name);
#else
            fprintf(f.handle, ".type %s, %%object\n", sym->name);
#endif
            fprintf(f.handle, ".set %s, 0x%" PRIxPTR "\n", sym->name, (uintptr_t) sym->addr);
        }
    }

    fclose(f.handle);
    return f.path;
}

static ssize_t get_file_size(FILE *f) {
    long orig_off = ftell(f);
    if (orig_off == -1) {
        return -1;
    }
    if (fseek(f, 0L, SEEK_END) == -1) {
        return -1;
    }
    long sz = ftell(f);
    if (sz == -1) {
        return -1;
    }

    if (fseek(f, 0L, SEEK_SET) == -1) {
        return -1;
    }
    return sz;
}

struct JitObject build_jit_object(ObjectCode *oc)
{
    pathchar *s_path = build_asm(oc);
    if (s_path == NULL) {
        goto fail;
    }

    struct temp_file f = create_temp_file(WSTR("jit-object"), WSTR(".o"));
    if (f.path == NULL) {
        debugBelch("build_jit_object: Failed to create temporary file: %s\n", strerror(errno));
        goto fail;
    }

    // Assemble it
    const int sz = snprintf(NULL, 0, "%" PATH_FMT" -c %" PATH_FMT" -o %" PATH_FMT, CC, s_path, f.path);
    char *cmdline = malloc(sz+1);
    snprintf(cmdline, sz+1, "%" PATH_FMT " -c %" PATH_FMT " -o %" PATH_FMT, CC, s_path, f.path);
    int ret = system(cmdline);
    free(cmdline);
    free(s_path);
    free(f.path);
    
    if (ret != 0) {
        debugBelch("Error assembling JIT object (exit code %d)\n", ret);
        fclose(f.handle);
        goto fail;
    }

    // Read the resulting object
    ssize_t o_size = get_file_size(f.handle);
    if (o_size == -1) {
        debugBelch("Error reading JIT object: %s\n", strerror(errno));
        goto fail;
    } else if (o_size == 0) {
        debugBelch("Assembler produced empty object\n");
        goto fail;
    }

    uint8_t *buffer = stgMallocBytes(o_size, "build_jit_object");
    if (fread(buffer, 1, o_size, f.handle) != (size_t) o_size) {
        debugBelch("Error reading JIT object: %s\n", strerror(errno));
        goto fail;
    }
    fclose(f.handle);

    return (struct JitObject) {
        .buffer = buffer,
        .size = o_size,
    };

fail:
    return (struct JitObject) {
        .buffer = NULL,
        .size = 0,
    };
}
