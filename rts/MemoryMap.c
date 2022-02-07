/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2004
 *
 * Memory-map dumping.
 *
 * This is intended to be used for reporting the process memory-map
 * in diagnostics when the RTS fails to map a block of memory.
 *
 * ---------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
#include "Rts.h"

#include <string.h>

#if defined(darwin_HOST_OS)
#include <mach/mach.h>
#include <mach/mach_vm.h>
#include <mach/vm_region.h>
#include <mach/vm_statistics.h>
#endif

#include "MemoryMap.h"

#if defined(mingw32_HOST_OS)

void reportMemoryMap() {
    debugBelch("\nMemory map:\n");
    uint8_t *addr = NULL;
    while (true) {
        MEMORY_BASIC_INFORMATION info;
        int res = VirtualQuery(addr, &info, sizeof(info));
        if (!res && GetLastError() == ERROR_INVALID_PARAMETER) {
            return;
        } else if (!res) {
            sysErrorBelch("VirtualQuery failed");
            return;
        }

        if (info.State & MEM_FREE) {
            // free range
        } else {
            const char *protection;
            switch (info.Protect) {
            case PAGE_EXECUTE:           protection = "--x"; break;
            case PAGE_EXECUTE_READ:      protection = "r-x"; break;
            case PAGE_EXECUTE_READWRITE: protection = "rwx"; break;
            case PAGE_EXECUTE_WRITECOPY: protection = "rcx"; break;
            case PAGE_NOACCESS:          protection = "---"; break;
            case PAGE_READONLY:          protection = "r--"; break;
            case PAGE_READWRITE:         protection = "rw-"; break;
            case PAGE_WRITECOPY:         protection = "rc-"; break;
            default:                     protection = "???"; break;
            }

            const char *type;
            switch (info.Type) {
            case MEM_IMAGE:   type = "image"; break;
            case MEM_MAPPED:  type = "mapped"; break;
            case MEM_PRIVATE: type = "private"; break;
            default:          type = "unknown"; break;
            }

            debugBelch("%08llx-%08llx %8zuK %3s (%s)\n",
                       (uintptr_t) info.BaseAddress,
                       (uintptr_t) info.BaseAddress + info.RegionSize,
                       (size_t) info.RegionSize,
                       protection, type);
        }
        addr = (uint8_t *) info.BaseAddress + info.RegionSize;
    }
}

#elif defined(darwin_HOST_OS)

void reportMemoryMap() {
    // Inspired by MacFUSE /proc implementation
    debugBelch("\nMemory map:\n");
    while (true) {
        mach_vm_size_t vmsize;
        mach_vm_address_t address;
        vm_region_basic_info_data_t info;
        vm_region_flavor_t flavor = VM_REGION_BASIC_INFO;
        memory_object_name_t object;
        mach_msg_type_number_t info_count = VM_REGION_BASIC_INFO_COUNT;
        kern_return_t kr =
            mach_vm_region(mach_task_self(), &address, &vmsize, flavor,
                           (vm_region_info_t)&info, &info_count, &object);
        if (kr == KERN_SUCCESS) {
            debugBelch("%p-%p %8zuK %c%c%c/%c%c%c\n",
                       (void *) address,
                       (void *) (address + vmsize),
                       ((size_t) vmsize) >> 10,
                       (info.protection & VM_PROT_READ)        ? 'r' : '-',
                       (info.protection & VM_PROT_WRITE)       ? 'w' : '-',
                       (info.protection & VM_PROT_EXECUTE)     ? 'x' : '-',
                       (info.max_protection & VM_PROT_READ)    ? 'r' : '-',
                       (info.max_protection & VM_PROT_WRITE)   ? 'w' : '-',
                       (info.max_protection & VM_PROT_EXECUTE) ? 'x' : '-');
            address += vmsize;
        } else if (kr == KERN_INVALID_ADDRESS) {
            // We presumably reached the end of address space
            break;
        } else {
            debugBelch("  Error: %s\n", mach_error_string(kr));
            break;
        }
    }
}

#else

// Linux et al.
void reportMemoryMap() {
    debugBelch("\nMemory map:\n");
    FILE *f = fopen("/proc/self/maps", "r");
    if (f == NULL) {
        debugBelch("  Could not open /proc/self/maps\n");
        return;
    }

    while (true) {
        char buf[256];
        size_t n = fread(buf, 1, sizeof(buf)-1, f);
        if (n <= 0) {
            debugBelch("  Error: %s\n", strerror(errno));
            break;
        }
        buf[n] = '\0';
        debugBelch("%s", buf);
        if (n < sizeof(buf)-1) {
            break;
        }
    }
    debugBelch("\n");
    fclose(f);
}

#endif
