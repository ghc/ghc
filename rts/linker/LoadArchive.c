#include <string.h>
#include <stddef.h>

#include <Rts.h>

#include "sm/Storage.h"
#include "sm/OSMem.h"
#include "RtsUtils.h"
#include "PathUtils.h"
#include "LinkerInternals.h"
#include "linker/M32Alloc.h"

/* Platform specific headers */
#if defined(OBJFORMAT_PEi386)
#  include "linker/PEi386.h"
#elif defined(darwin_HOST_OS)
#  include "linker/MachO.h"
#  include <regex.h>
#  include <mach/machine.h>
#  include <mach-o/fat.h>
#endif

#include <ctype.h>

static HsInt loadArchive_ (pathchar *path)
{
    ObjectCode* oc;
    char *image;
    int memberSize;
    FILE *f;
    int n;
    size_t thisFileNameSize;
    char *fileName;
    size_t fileNameSize;
    int isObject, isGnuIndex, isThin, isImportLib;
    char tmp[20];
    char *gnuFileIndex;
    int gnuFileIndexSize;
#if defined(darwin_HOST_OS)
    int i;
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
#else
#error Unknown Darwin architecture
#endif
#endif
    int misalignment = 0;

    /* TODO: don't call barf() on error, instead return an error code, freeing
     * all resources correctly.  This function is pretty complex, so it needs
     * to be refactored to make this practical. */

    IF_DEBUG(linker, debugBelch("loadArchive: start\n"));
    IF_DEBUG(linker, debugBelch("loadArchive: Loading archive `%" PATH_FMT" '\n", path));

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
        barf("loadObj: can't read `%" PATH_FMT "'", path);

    /* Check if this is an archive by looking for the magic "!<arch>\n"
     * string.  Usually, if this fails, we barf and quit.  On Darwin however,
     * we may have a fat archive, which contains archives for more than
     * one architecture.  Fat archives start with the magic number 0xcafebabe,
     * always stored big endian.  If we find a fat_header, we scan through
     * the fat_arch structs, searching through for one for our host
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
    if (n != 8)
        barf("loadArchive: Failed reading header from `%" PATH_FMT "'", path);
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
#if defined(darwin_HOST_OS)
    /* Not a standard archive, look for a fat archive magic number: */
    else if (ntohl(*(uint32_t *)tmp) == FAT_MAGIC) {
        nfat_arch = ntohl(*(uint32_t *)(tmp + 4));
        IF_DEBUG(linker, debugBelch("loadArchive: found a fat archive containing %d architectures\n", nfat_arch));
        nfat_offset = 0;

        for (i = 0; i < (int)nfat_arch; i++) {
            /* search for the right arch */
            n = fread( tmp, 1, 20, f );
            if (n != 8)
                barf("loadArchive: Failed reading arch from `%s'", path);
            cputype = ntohl(*(uint32_t *)tmp);
            cpusubtype = ntohl(*(uint32_t *)(tmp + 4));

            if (cputype == mycputype && cpusubtype == mycpusubtype) {
                IF_DEBUG(linker, debugBelch("loadArchive: found my archive in a fat archive\n"));
                nfat_offset = ntohl(*(uint32_t *)(tmp + 8));
                break;
            }
        }

        if (nfat_offset == 0) {
           barf ("loadArchive: searched %d architectures, but no host arch found", (int)nfat_arch);
        }
        else {
            n = fseek( f, nfat_offset, SEEK_SET );
            if (n != 0)
                barf("loadArchive: Failed to seek to arch in `%s'", path);
            n = fread ( tmp, 1, 8, f );
            if (n != 8)
                barf("loadArchive: Failed reading header from `%s'", path);
            if (strncmp(tmp, "!<arch>\n", 8) != 0) {
                barf("loadArchive: couldn't find archive in `%s' at offset %d", path, nfat_offset);
            }
        }
    }
    else {
        barf("loadArchive: Neither an archive, nor a fat archive: `%s'", path);
    }
#else
    else {
        barf("loadArchive: Not an archive: `%" PATH_FMT "'", path);
    }
#endif

    IF_DEBUG(linker, debugBelch("loadArchive: loading archive contents\n"));

    while (1) {
        IF_DEBUG(linker, debugBelch("loadArchive: reading at %ld\n", ftell(f)));
        n = fread ( fileName, 1, 16, f );
        if (n != 16) {
            if (feof(f)) {
                IF_DEBUG(linker, debugBelch("loadArchive: EOF while reading from '%" PATH_FMT "'\n", path));
                break;
            }
            else {
                barf("loadArchive: Failed reading file name from `%" PATH_FMT "'", path);
            }
        }

#if defined(darwin_HOST_OS)
        if (strncmp(fileName, "!<arch>\n", 8) == 0) {
            IF_DEBUG(linker, debugBelch("loadArchive: found the start of another archive, breaking\n"));
            break;
        }
#endif

        n = fread ( tmp, 1, 12, f );
        if (n != 12)
            barf("loadArchive: Failed reading mod time from `%" PATH_FMT "'", path);
        n = fread ( tmp, 1, 6, f );
        if (n != 6)
            barf("loadArchive: Failed reading owner from `%" PATH_FMT "'", path);
        n = fread ( tmp, 1, 6, f );
        if (n != 6)
            barf("loadArchive: Failed reading group from `%" PATH_FMT "'", path);
        n = fread ( tmp, 1, 8, f );
        if (n != 8)
            barf("loadArchive: Failed reading mode from `%" PATH_FMT "'", path);
        n = fread ( tmp, 1, 10, f );
        if (n != 10)
            barf("loadArchive: Failed reading size from `%" PATH_FMT "'", path);
        tmp[10] = '\0';
        for (n = 0; isdigit(tmp[n]); n++);
        tmp[n] = '\0';
        memberSize = atoi(tmp);

        IF_DEBUG(linker, debugBelch("loadArchive: size of this archive member is %d\n", memberSize));
        n = fread ( tmp, 1, 2, f );
        if (n != 2)
            barf("loadArchive: Failed reading magic from `%" PATH_FMT "'", path);
        if (strncmp(tmp, "\x60\x0A", 2) != 0)
            barf("loadArchive: Failed reading magic from `%" PATH_FMT "' at %ld. Got %c%c",
                 path, ftell(f), tmp[0], tmp[1]);

        isGnuIndex = 0;
        /* Check for BSD-variant large filenames */
        if (0 == strncmp(fileName, "#1/", 3)) {
            fileName[16] = '\0';
            if (isdigit(fileName[3])) {
                for (n = 4; isdigit(fileName[n]); n++);
                fileName[n] = '\0';
                thisFileNameSize = atoi(fileName + 3);
                memberSize -= thisFileNameSize;
                if (thisFileNameSize >= fileNameSize) {
                    /* Double it to avoid potentially continually
                       increasing it by 1 */
                    fileNameSize = thisFileNameSize * 2;
                    fileName = stgReallocBytes(fileName, fileNameSize, "loadArchive(fileName)");
                }
                n = fread ( fileName, 1, thisFileNameSize, f );
                if (n != (int)thisFileNameSize) {
                    barf("loadArchive: Failed reading filename from `%" PATH_FMT "'",
                         path);
                }
                fileName[thisFileNameSize] = 0;

                /* On OS X at least, thisFileNameSize is the size of the
                   fileName field, not the length of the fileName
                   itself. */
                thisFileNameSize = strlen(fileName);
            }
            else {
                barf("loadArchive: BSD-variant filename size not found while reading filename from `%s'", path);
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
            if (isdigit(fileName[1])) {
                int i;

                for (n = 2; isdigit(fileName[n]); n++);
                fileName[n] = '\0';
                n = atoi(fileName + 1);

                if (gnuFileIndex == NULL) {
                    barf("loadArchive: GNU-variant filename without an index while reading from `%s'", path);
                }
                if (n < 0 || n > gnuFileIndexSize) {
                    barf("loadArchive: GNU-variant filename offset %d out of range [0..%d] while reading filename from `%s'", n, gnuFileIndexSize, path);
                }
                if (n != 0 && gnuFileIndex[n - 1] != '\n') {
                    barf("loadArchive: GNU-variant filename offset %d invalid (range [0..%d]) while reading filename from `%s'", n, gnuFileIndexSize, path);
                }
                for (i = n; gnuFileIndex[i] != '\n'; i++);
                thisFileNameSize = i - n - 1;
                if (thisFileNameSize >= fileNameSize) {
                    /* Double it to avoid potentially continually
                       increasing it by 1 */
                    fileNameSize = thisFileNameSize * 2;
                    fileName = stgReallocBytes(fileName, fileNameSize, "loadArchive(fileName)");
                }
                memcpy(fileName, gnuFileIndex + n, thisFileNameSize);
                fileName[thisFileNameSize] = '\0';
            }
            else if (fileName[1] == ' ') {
                fileName[0] = '\0';
                thisFileNameSize = 0;
            }
            else {
                barf("loadArchive: GNU-variant filename offset not found while reading filename from `%s'", path);
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

        IF_DEBUG(linker,
                 debugBelch("loadArchive: Found member file `%s'\n", fileName));

        isObject = (thisFileNameSize >= 2 && strncmp(fileName + thisFileNameSize - 2, ".o"  , 2) == 0)
                || (thisFileNameSize >= 4 && strncmp(fileName + thisFileNameSize - 4, ".p_o", 4) == 0);

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

        IF_DEBUG(linker, debugBelch("loadArchive: \tthisFileNameSize = %d\n", (int)thisFileNameSize));
        IF_DEBUG(linker, debugBelch("loadArchive: \tisObject = %d\n", isObject));

        if (isObject) {
            char *archiveMemberName;

            IF_DEBUG(linker, debugBelch("loadArchive: Member is an object file...loading...\n"));

#if defined(mingw32_HOST_OS)
            // TODO: We would like to use allocateExec here, but allocateExec
            //       cannot currently allocate blocks large enough.
            image = allocateImageAndTrampolines(path, fileName, f, memberSize,
                                                isThin);
#elif defined(darwin_HOST_OS)
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
                FILE *member;
                pathchar *pathCopy, *dirName, *memberPath, *objFileName;

                /* Allocate and setup the dirname of the archive.  We'll need
                    this to locate the thin member */
                pathCopy = pathdup(path); // Convert the char* to a pathchar*
                dirName  = pathdir(pathCopy);

                /* Append the relative member name to the dirname.  This should be
                   be the full path to the actual thin member. */
                int memberLen = pathlen(dirName) + 1 + strlen(fileName) + 1;
                memberPath    = stgMallocBytes(pathsize * memberLen, "loadArchive(file)");
                objFileName   = mkPath(fileName);
                pathprintf(memberPath, memberLen, WSTR("%" PATH_FMT "%" PATH_FMT), dirName, objFileName);
                stgFree(objFileName);
                stgFree(dirName);

                member = pathopen(memberPath, WSTR("rb"));
                if (!member)
                    barf("loadObj: can't read thin archive `%" PATH_FMT "'", memberPath);

                n = fread ( image, 1, memberSize, member );
                if (n != memberSize) {
                    barf("loadArchive: error whilst reading `%s'", fileName);
                }

                fclose(member);
                stgFree(memberPath);
                stgFree(pathCopy);
            }
            else
            {
                n = fread ( image, 1, memberSize, f );
                if (n != memberSize) {
                    barf("loadArchive: error whilst reading `%" PATH_FMT "'", path);
                }
            }

            archiveMemberName = stgMallocBytes(pathlen(path) + thisFileNameSize + 3,
                                               "loadArchive(file)");
            sprintf(archiveMemberName, "%" PATH_FMT "(%.*s)",
                    path, (int)thisFileNameSize, fileName);

            oc = mkOc(path, image, memberSize, rtsFalse, archiveMemberName
                     , misalignment);

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
                barf("loadArchive: GNU-variant index found, but already have an index, while reading filename from `%s'", path);
            }
            IF_DEBUG(linker, debugBelch("loadArchive: Found GNU-variant file index\n"));
#if RTS_LINKER_USE_MMAP
            gnuFileIndex = mmapForLinker(memberSize + 1, MAP_ANONYMOUS, -1, 0);
#else
            gnuFileIndex = stgMallocBytes(memberSize + 1, "loadArchive(image)");
#endif
            n = fread ( gnuFileIndex, 1, memberSize, f );
            if (n != memberSize) {
                barf("loadArchive: error whilst reading `%" PATH_FMT "'", path);
            }
            gnuFileIndex[memberSize] = '/';
            gnuFileIndexSize = memberSize;
        }
        else if (isImportLib) {
#if defined(OBJFORMAT_PEi386)
            if (checkAndLoadImportLibrary(path, fileName, f)) {
                IF_DEBUG(linker, debugBelch("loadArchive: Member is an import file section... Corresponding DLL has been loaded...\n"));
            }
            else {
                IF_DEBUG(linker, debugBelch("loadArchive: Member is not a valid import file section... Skipping...\n"));
                n = fseek(f, memberSize, SEEK_CUR);
                if (n != 0)
                    barf("loadArchive: error whilst seeking by %d in `%" PATH_FMT "'",
                    memberSize, path);
            }
#endif
        }
        else {
            IF_DEBUG(linker, debugBelch("loadArchive: '%s' does not appear to be an object file\n", fileName));
            if (!isThin || thisFileNameSize == 0) {
                n = fseek(f, memberSize, SEEK_CUR);
                if (n != 0)
                    barf("loadArchive: error whilst seeking by %d in `%" PATH_FMT "'",
                         memberSize, path);
            }
        }

        /* .ar files are 2-byte aligned */
        if (!(isThin && thisFileNameSize > 0) && memberSize % 2) {
            IF_DEBUG(linker, debugBelch("loadArchive: trying to read one pad byte\n"));
            n = fread ( tmp, 1, 1, f );
            if (n != 1) {
                if (feof(f)) {
                    IF_DEBUG(linker, debugBelch("loadArchive: found EOF while reading one pad byte\n"));
                    break;
                }
                else {
                    barf("loadArchive: Failed reading padding from `%" PATH_FMT "'", path);
                }
            }
            IF_DEBUG(linker, debugBelch("loadArchive: successfully read one pad byte\n"));
        }
        IF_DEBUG(linker, debugBelch("loadArchive: reached end of archive loading while loop\n"));
    }

    fclose(f);

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

    IF_DEBUG(linker, debugBelch("loadArchive: done\n"));
    return 1;
}

HsInt loadArchive (pathchar *path)
{
   ACQUIRE_LOCK(&linker_mutex);
   HsInt r = loadArchive_(path);
   RELEASE_LOCK(&linker_mutex);
   return r;
}
