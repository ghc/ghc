#include <string.h>
#include <stddef.h>

#include <Rts.h>
#include "PathUtils.h"

#include "sm/Storage.h"
#include "sm/OSMem.h"
#include "RtsUtils.h"
#include "LinkerInternals.h"
#include "linker/M32Alloc.h"

/* Platform specific headers */
#if defined(OBJFORMAT_PEi386)
#  include "linker/PEi386.h"
#elif defined(darwin_HOST_OS) || defined(ios_HOST_OS)
#  include "linker/MachO.h"
#  include <regex.h>
#  include <mach/machine.h>
#  include <mach-o/fat.h>
#endif

#include <ctype.h>

#define FAIL(...) do {\
   errorBelch("loadArchive: "__VA_ARGS__); \
   goto fail;\
} while (0)

#define DEBUG_LOG(...) IF_DEBUG(linker, debugBelch("loadArchive: " __VA_ARGS__))

#if defined(darwin_HOST_OS) || defined(ios_HOST_OS)
/* Read 4 bytes and convert to host byte order */
static uint32_t read4Bytes(const char buf[static 4])
{
    return ntohl(*(uint32_t*)buf);
}

static StgBool loadFatArchive(char tmp[static 20], FILE* f, pathchar* path)
{
    uint32_t nfat_arch, nfat_offset, cputype, cpusubtype;
#if defined(i386_HOST_ARCH)
    const uint32_t mycputype = CPU_TYPE_X86;
    const uint32_t mycpusubtype = CPU_SUBTYPE_X86_ALL;
#elif defined(x86_64_HOST_ARCH)
    const uint32_t mycputype = CPU_TYPE_X86_64;
    const uint32_t mycpusubtype = CPU_SUBTYPE_X86_64_ALL;
#elif defined(powerpc_HOST_ARCH)
    const uint32_t mycputype = CPU_TYPE_POWERPC;
    const uint32_t mycpusubtype = CPU_SUBTYPE_POWERPC_ALL;
#elif defined(powerpc64_HOST_ARCH)
    const uint32_t mycputype = CPU_TYPE_POWERPC64;
    const uint32_t mycpusubtype = CPU_SUBTYPE_POWERPC_ALL;
#elif defined(aarch64_HOST_ARCH)
    const uint32_t mycputype = CPU_TYPE_ARM64;
    const uint32_t mycpusubtype = CPU_SUBTYPE_ARM64_ALL;
#else
#error Unknown Darwin architecture
#endif

    nfat_arch = read4Bytes(tmp + 4);
    DEBUG_LOG("found a fat archive containing %d architectures\n", nfat_arch);
    nfat_offset = 0;
    for (uint32_t i = 0; i < nfat_arch; i++) {
        /* search for the right arch */
        int n = fread(tmp, 1, 12, f);
        if (n != 12) {
            errorBelch("Failed reading arch from `%" PATH_FMT "'", path);
            return false;
        }
        cputype = read4Bytes(tmp);
        cpusubtype = read4Bytes(tmp + 4);
        if (cputype == mycputype && cpusubtype == mycpusubtype) {
            DEBUG_LOG("found my archive in a fat archive\n");
            nfat_offset = read4Bytes(tmp + 8);
            break;
        }
    }
    if (nfat_offset == 0) {
        errorBelch("Fat archive contains %d architectures, "
                   "but none of them are compatible with the host",
                   (int)nfat_arch);
        return false;
    } else {
        /* Seek to the correct architecture */
        int n = fseek(f, nfat_offset, SEEK_SET);
        if (n != 0) {
            errorBelch("Failed to seek to arch in `%" PATH_FMT "'", path);
            return false;
        }

        /* Read the header */
        n = fread(tmp, 1, 8, f);
        if (n != 8) {
            errorBelch("Failed reading header from `%" PATH_FMT "'", path);
            return false;
        }

        /* Check the magic number */
        if (strncmp(tmp, "!<arch>\n", 8) != 0) {
            errorBelch("couldn't find archive in `%" PATH_FMT "'"
                       "at offset %d", path, nfat_offset);
            return false;
        }
    }
    return true;
}
#endif

static StgBool readThinArchiveMember(int n, int memberSize, pathchar* path,
        char* fileName, char* image)
{
    StgBool has_succeeded = false;
    FILE* member = NULL;
    pathchar *pathCopy, *dirName, *memberPath, *objFileName;
    memberPath = NULL;
    /* Allocate and setup the dirname of the archive.  We'll need
     this to locate the thin member */
    pathCopy = pathdup(path); // Convert the char* to a pathchar*
    dirName = pathdir(pathCopy);
    /* Append the relative member name to the dirname.  This should be
     be the full path to the actual thin member. */
    int memberLen = pathlen(dirName) + 1 + strlen(fileName) + 1;
    memberPath = stgMallocBytes(pathsize * memberLen, "loadArchive(file)");
    objFileName = mkPath(fileName);
    pathprintf(memberPath, memberLen, WSTR("%" PATH_FMT "%" PATH_FMT), dirName,
            objFileName);
    stgFree(objFileName);
    stgFree(dirName);
    member = pathopen(memberPath, WSTR("rb"));
    if (!member) {
        errorBelch("loadObj: can't read thin archive `%" PATH_FMT "'",
                   memberPath);
        goto inner_fail;
    }
    n = fread(image, 1, memberSize, member);
    if (n != memberSize) {
        errorBelch("loadArchive: error whilst reading `%s'",
                   fileName);
        goto inner_fail;
    }
    has_succeeded = true;

inner_fail:
    fclose(member);
    stgFree(memberPath);
    stgFree(pathCopy);
    return has_succeeded;
}

static StgBool checkFatArchive(char magic[static 20], FILE* f, pathchar* path)
{
    StgBool success;
    success = false;
#if defined(darwin_HOST_OS) || defined(ios_HOST_OS)
    /* Not a standard archive, look for a fat archive magic number: */
    if (read4Bytes(magic) == FAT_MAGIC)
        success = loadFatArchive(magic, f, path);
    else
        errorBelch("loadArchive: Neither an archive, nor a fat archive: "
                   "`%" PATH_FMT "'", path);
#else
    (void)magic;
    (void)f;
    errorBelch("loadArchive: Not an archive: `%" PATH_FMT "'", path);
#endif
    return success;
}

/**
 * Look up the filename in the GNU-variant index file pointed to by
 * gnuFileIndex.
 * @param fileName_ a pointer to a pointer to the file name to be looked up.
 * The file name must have been allocated with `StgMallocBytes`, and will
 * be reallocated on return; the old value is now _invalid_.
 * @param gnuFileIndexSize The size of the index.
 */
static StgBool
lookupGNUArchiveIndex(int gnuFileIndexSize, char **fileName_,
    char* gnuFileIndex, pathchar* path, size_t* thisFileNameSize,
    size_t* fileNameSize)
{
    int n;
    char *fileName = *fileName_;
    if (isdigit(fileName[1])) {
        int i;
        for (n = 2; isdigit(fileName[n]); n++)
            ;

        fileName[n] = '\0';
        n = atoi(fileName + 1);
        if (gnuFileIndex == NULL) {
            errorBelch("loadArchive: GNU-variant filename "
                    "without an index while reading from `%" PATH_FMT "'",
                    path);
            return false;
        }
        if (n < 0 || n > gnuFileIndexSize) {
            errorBelch("loadArchive: GNU-variant filename "
                    "offset %d out of range [0..%d] "
                    "while reading filename from `%" PATH_FMT "'",
                    n, gnuFileIndexSize, path);
            return false;
        }
        if (n != 0 && gnuFileIndex[n - 1] != '\n') {
            errorBelch("loadArchive: GNU-variant filename offset "
                    "%d invalid (range [0..%d]) while reading "
                    "filename from `%" PATH_FMT "'",
                    n, gnuFileIndexSize, path);
            return false;
        }
        for (i = n; gnuFileIndex[i] != '\n'; i++)
            ;

        size_t FileNameSize = i - n - 1;
        if (FileNameSize >= *fileNameSize) {
            /* Double it to avoid potentially continually
             increasing it by 1 */
            *fileNameSize = FileNameSize * 2;
            *fileName_ = fileName = stgReallocBytes(fileName, *fileNameSize,
                    "loadArchive(fileName)");
        }
        memcpy(fileName, gnuFileIndex + n, FileNameSize);
        fileName[FileNameSize] = '\0';
        *thisFileNameSize = FileNameSize;
    }
    /* Skip 32-bit symbol table ("/" + 15 blank characters)
       and  64-bit symbol table ("/SYM64/" + 9 blank characters) */
    else if (0 == strncmp(fileName + 1, "               ", 15) ||
             0 == strncmp(fileName + 1, "SYM64/         ", 15)) {
        fileName[0] = '\0';
        *thisFileNameSize = 0;
    }
    else {
        errorBelch("loadArchive: invalid GNU-variant filename `%.16s' "
                   "while reading filename from `%" PATH_FMT "'",
                   fileName, path);
        return false;
    }

    return true;
}

static HsInt loadArchive_ (pathchar *path)
{
    ObjectCode* oc = NULL;
    char *image = NULL;
    HsInt retcode = 0;
    int memberSize;
    FILE *f = NULL;
    int n;
    size_t thisFileNameSize = (size_t)-1; /* shut up bogus GCC warning */
    char *fileName;
    size_t fileNameSize;
    int isObject, isGnuIndex, isThin, isImportLib;
    char tmp[20];
    char *gnuFileIndex;
    int gnuFileIndexSize;
    int misalignment = 0;

    DEBUG_LOG("start\n");
    DEBUG_LOG("Loading archive `%" PATH_FMT" '\n", path);

    /* Check that we haven't already loaded this archive.
       Ignore requests to load multiple times */
    if (isAlreadyLoaded(path)) {
        IF_DEBUG(linker,
                 debugBelch("ignoring repeated load of %" PATH_FMT "\n", path));
        return 1; /* success */
    }

    gnuFileIndex = NULL;
    gnuFileIndexSize = 0;

    fileNameSize = 32;
    fileName = stgMallocBytes(fileNameSize, "loadArchive(fileName)");

    isThin = 0;
    isImportLib = 0;

    f = pathopen(path, WSTR("rb"));
    if (!f)
        FAIL("loadObj: can't read `%" PATH_FMT "'", path);

    /* Check if this is an archive by looking for the magic "!<arch>\n"
     * string.  Usually, if this fails, we belch an error and return.  On
     * Darwin however, we may have a fat archive, which contains archives for
     * more than one architecture.  Fat archives start with the magic number
     * 0xcafebabe, always stored big endian.  If we find a fat_header, we scan
     * through the fat_arch structs, searching through for one for our host
     * architecture.  If a matching struct is found, we read the offset
     * of our archive data (nfat_offset) and seek forward nfat_offset bytes
     * from the start of the file.
     *
     * A subtlety is that all of the members of the fat_header and fat_arch
     * structs are stored big endian, so we need to call byte order
     * conversion functions.
     *
     * If we find the appropriate architecture in a fat archive, we gobble
     * its magic "!<arch>\n" string and continue processing just as if
     * we had a single architecture archive.
     */

    n = fread ( tmp, 1, 8, f );
    if (n != 8) {
        FAIL("Failed reading header from `%" PATH_FMT "'", path);
    }
    if (strncmp(tmp, "!<arch>\n", 8) == 0) {}
    /* Check if this is a thin archive by looking for the magic string "!<thin>\n"
     *
     * ar thin libraries have the exact same format as normal archives except they
     * have a different magic string and they don't copy the object files into the
     * archive.
     *
     * Instead each header entry points to the location of the object file on disk.
     * This is useful when a library is only created to satisfy a compile time dependency
     * instead of to be distributed. This saves the time required for copying.
     *
     * Thin archives are always flattened. They always only contain simple headers
     * pointing to the object file and so we need not allocate more memory than needed
     * to find the object file.
     *
     */
    else if (strncmp(tmp, "!<thin>\n", 8) == 0) {
        isThin = 1;
    }
    else {
        StgBool success = checkFatArchive(tmp, f, path);
        if (!success)
            goto fail;
    }
    DEBUG_LOG("loading archive contents\n");

    while (1) {
        DEBUG_LOG("reading at %ld\n", ftell(f));
        n = fread ( fileName, 1, 16, f );
        if (n != 16) {
            if (feof(f)) {
                DEBUG_LOG("EOF while reading from '%" PATH_FMT "'\n", path);
                break;
            }
            else {
                FAIL("Failed reading file name from `%" PATH_FMT "'", path);
            }
        }

#if defined(darwin_HOST_OS) || defined(ios_HOST_OS)
        if (strncmp(fileName, "!<arch>\n", 8) == 0) {
            DEBUG_LOG("found the start of another archive, breaking\n");
            break;
        }
#endif

        n = fread ( tmp, 1, 12, f );
        if (n != 12)
            FAIL("Failed reading mod time from `%" PATH_FMT "'", path);
        n = fread ( tmp, 1, 6, f );
        if (n != 6)
            FAIL("Failed reading owner from `%" PATH_FMT "'", path);
        n = fread ( tmp, 1, 6, f );
        if (n != 6)
            FAIL("Failed reading group from `%" PATH_FMT "'", path);
        n = fread ( tmp, 1, 8, f );
        if (n != 8)
            FAIL("Failed reading mode from `%" PATH_FMT "'", path);
        n = fread ( tmp, 1, 10, f );
        if (n != 10)
            FAIL("Failed reading size from `%" PATH_FMT "'", path);
        tmp[10] = '\0';
        for (n = 0; isdigit(tmp[n]); n++);
        tmp[n] = '\0';
        memberSize = atoi(tmp);

        DEBUG_LOG("size of this archive member is %d\n", memberSize);
        n = fread ( tmp, 1, 2, f );
        if (n != 2)
            FAIL("Failed reading magic from `%" PATH_FMT "'", path);
        if (strncmp(tmp, "\x60\x0A", 2) != 0)
            FAIL("Failed reading magic from `%" PATH_FMT "' at %ld. Got %c%c",
                 path, ftell(f), tmp[0], tmp[1]);

        isGnuIndex = 0;
        /* Check for BSD-variant large filenames */
        if (0 == strncmp(fileName, "#1/", 3)) {
            size_t n = 0;
            fileName[16] = '\0';
            if (isdigit(fileName[3])) {
                for (n = 4; isdigit(fileName[n]); n++)
                    ;

                fileName[n] = '\0';
                thisFileNameSize = atoi(fileName + 3);
                memberSize -= thisFileNameSize;
                if (thisFileNameSize >= fileNameSize) {
                    /* Double it to avoid potentially continually
                       increasing it by 1 */
                    fileNameSize = thisFileNameSize * 2;
                    fileName = stgReallocBytes(fileName, fileNameSize,
                                               "loadArchive(fileName)");
                }
                n = fread(fileName, 1, thisFileNameSize, f);
                if (n != thisFileNameSize) {
                    errorBelch("Failed reading filename from `%" PATH_FMT "'",
                               path);
                    goto fail;
                }
                fileName[thisFileNameSize] = 0;
                /* On OS X at least, thisFileNameSize is the size of the
                   fileName field, not the length of the fileName
                   itself. */
                thisFileNameSize = strlen(fileName);
            } else {
                errorBelch("BSD-variant filename size not found "
                           "while reading filename from `%" PATH_FMT "'", path);
                goto fail;
            }
        }
        /* Check for GNU file index file */
        else if (0 == strncmp(fileName, "//", 2)) {
            fileName[0] = '\0';
            thisFileNameSize = 0;
            isGnuIndex = 1;
        }
        /* Check for a file in the GNU file index */
        else if (fileName[0] == '/') {
            if (!lookupGNUArchiveIndex(gnuFileIndexSize, &fileName,
                     gnuFileIndex, path, &thisFileNameSize, &fileNameSize)) {
                goto fail;
            }
        }
        /* Finally, the case where the filename field actually contains
           the filename */
        else {
            /* GNU ar terminates filenames with a '/', this allowing
               spaces in filenames. So first look to see if there is a
               terminating '/'. */
            for (thisFileNameSize = 0;
                 thisFileNameSize < 16;
                 thisFileNameSize++) {
                if (fileName[thisFileNameSize] == '/') {
                    fileName[thisFileNameSize] = '\0';
                    break;
                }
            }
            /* If we didn't find a '/', then a space teminates the
               filename. Note that if we don't find one, then
               thisFileNameSize ends up as 16, and we already have the
               '\0' at the end. */
            if (thisFileNameSize == 16) {
                for (thisFileNameSize = 0;
                     thisFileNameSize < 16;
                     thisFileNameSize++) {
                    if (fileName[thisFileNameSize] == ' ') {
                        fileName[thisFileNameSize] = '\0';
                        break;
                    }
                }
            }
        }

        DEBUG_LOG("Found member file `%s'\n", fileName);

        /* TODO: Stop relying on file extensions to determine input formats.
                 Instead try to match file headers. See Trac #13103.  */
        isObject = (thisFileNameSize >= 2 && strncmp(fileName + thisFileNameSize - 2, ".o"  , 2) == 0)
                || (thisFileNameSize >= 4 && strncmp(fileName + thisFileNameSize - 4, ".p_o", 4) == 0)
                || (thisFileNameSize >= 4 && strncmp(fileName + thisFileNameSize - 4, ".obj", 4) == 0);

#if defined(OBJFORMAT_PEi386)
        /*
        * Note [MSVC import files (ext .lib)]
        * MSVC compilers store the object files in
        * the import libraries with extension .dll
        * so on Windows we should look for those too.
        * The PE COFF format doesn't specify any specific file name
        * for sections. So on windows, just try to load it all.
        *
        * Linker members (e.g. filename / are skipped since they are not needed)
        */
        isImportLib = thisFileNameSize >= 4 && strncmp(fileName + thisFileNameSize - 4, ".dll", 4) == 0;

        /*
         * Note [GCC import files (ext .dll.a)]
         * GCC stores import information in the same binary format
         * as the object file normally has. The only difference is that
         * all the information are put in .idata sections. The only real
         * way to tell if we're dealing with an import lib is by looking
         * at the file extension.
         */
        isImportLib = isImportLib || endsWithPath(path, WSTR(".dll.a"));
#endif // windows

        DEBUG_LOG("\tthisFileNameSize = %d\n", (int)thisFileNameSize);
        DEBUG_LOG("\tisObject = %d\n", isObject);

        if (isObject) {
            char *archiveMemberName;

            DEBUG_LOG("Member is an object file...loading...\n");

#if defined(mingw32_HOST_OS)
            // TODO: We would like to use allocateExec here, but allocateExec
            //       cannot currently allocate blocks large enough.
            image = allocateImageAndTrampolines(path, fileName, f, memberSize,
                                                isThin);
#elif defined(darwin_HOST_OS) || defined(ios_HOST_OS)
            if (RTS_LINKER_USE_MMAP)
                image = mmapForLinker(memberSize, MAP_ANONYMOUS, -1, 0);
            else {
                /* See loadObj() */
                misalignment = machoGetMisalignment(f);
                image = stgMallocBytes(memberSize + misalignment,
                                        "loadArchive(image)");
                image += misalignment;
            }

#else // not windows or darwin
            image = stgMallocBytes(memberSize, "loadArchive(image)");
#endif
            if (isThin) {
                if (!readThinArchiveMember(n, memberSize, path,
                        fileName, image)) {
                    goto fail;
                }
            }
            else
            {
                n = fread ( image, 1, memberSize, f );
                if (n != memberSize) {
                    FAIL("error whilst reading `%" PATH_FMT "'", path);
                }
            }

            archiveMemberName = stgMallocBytes(pathlen(path) + thisFileNameSize + 3,
                                               "loadArchive(file)");
            sprintf(archiveMemberName, "%" PATH_FMT "(%.*s)",
                    path, (int)thisFileNameSize, fileName);

            oc = mkOc(path, image, memberSize, false, archiveMemberName
                     , misalignment);
#ifdef OBJFORMAT_MACHO
            ocInit_MachO( oc );
#endif

            stgFree(archiveMemberName);

            if (0 == loadOc(oc)) {
                stgFree(fileName);
                fclose(f);
                return 0;
            } else {
#if defined(OBJFORMAT_PEi386)
                if (isImportLib)
                {
                    findAndLoadImportLibrary(oc);
                    stgFree(oc);
                    oc = NULL;
                    break;
                } else {
#endif
                    oc->next = objects;
                    objects = oc;
#if defined(OBJFORMAT_PEi386)
                }
#endif
            }
        }
        else if (isGnuIndex) {
            if (gnuFileIndex != NULL) {
                FAIL("GNU-variant index found, but already have an index, \
while reading filename from `%" PATH_FMT "'", path);
            }
            DEBUG_LOG("Found GNU-variant file index\n");
#if RTS_LINKER_USE_MMAP
            gnuFileIndex = mmapForLinker(memberSize + 1, MAP_ANONYMOUS, -1, 0);
#else
            gnuFileIndex = stgMallocBytes(memberSize + 1, "loadArchive(image)");
#endif
            n = fread ( gnuFileIndex, 1, memberSize, f );
            if (n != memberSize) {
                FAIL("error whilst reading `%" PATH_FMT "'", path);
            }
            gnuFileIndex[memberSize] = '/';
            gnuFileIndexSize = memberSize;
        }
        else if (isImportLib) {
#if defined(OBJFORMAT_PEi386)
            if (checkAndLoadImportLibrary(path, fileName, f)) {
                DEBUG_LOG("Member is an import file section... "
                          "Corresponding DLL has been loaded...\n");
            }
            else {
                DEBUG_LOG("Member is not a valid import file section... "
                          "Skipping...\n");
                n = fseek(f, memberSize, SEEK_CUR);
                if (n != 0)
                    FAIL("error whilst seeking by %d in `%" PATH_FMT "'",
                    memberSize, path);
            }
#endif
        }
        else {
            DEBUG_LOG("`%s' does not appear to be an object file\n",
                      fileName);
            if (!isThin || thisFileNameSize == 0) {
                n = fseek(f, memberSize, SEEK_CUR);
                if (n != 0)
                    FAIL("error whilst seeking by %d in `%" PATH_FMT "'",
                         memberSize, path);
            }
        }

        /* .ar files are 2-byte aligned */
        if (!(isThin && thisFileNameSize > 0) && memberSize % 2) {
            DEBUG_LOG("trying to read one pad byte\n");
            n = fread ( tmp, 1, 1, f );
            if (n != 1) {
                if (feof(f)) {
                    DEBUG_LOG("found EOF while reading one pad byte\n");
                    break;
                }
                else {
                    FAIL("Failed reading padding from `%" PATH_FMT "'", path);
                }
            }
            DEBUG_LOG("successfully read one pad byte\n");
        }
        DEBUG_LOG("reached end of archive loading while loop\n");
    }
    retcode = 1;
fail:
    if (f != NULL)
        fclose(f);

    if (fileName != NULL)
        stgFree(fileName);
    if (gnuFileIndex != NULL) {
#if RTS_LINKER_USE_MMAP
        munmap(gnuFileIndex, gnuFileIndexSize + 1);
#else
        stgFree(gnuFileIndex);
#endif
    }

    if (RTS_LINKER_USE_MMAP)
        m32_allocator_flush();

    DEBUG_LOG("done\n");
    return retcode;
}

HsInt loadArchive (pathchar *path)
{
   ACQUIRE_LOCK(&linker_mutex);
   HsInt r = loadArchive_(path);
   RELEASE_LOCK(&linker_mutex);
   return r;
}
