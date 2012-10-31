/*
 * Simple 'touch' program for Windows
 *
 */
#if !defined(_MSC_VER) && !defined(__MINGW32__) && !defined(_WIN32)
#error "Win32-only, the platform you're using is supposed to have 'touch' already."
#else
#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <errno.h>
#include <utime.h>
#include <windows.h>

/*
With Windows 7 in a virtual box VM on OS X, some very odd things happen
with dates and time stamps when SSHing into cygwin. e.g. here the
"Change" time is in the past:

$ date; touch foo; stat foo
Fri Dec  2 16:58:07 GMTST 2011
  File: `foo'
  Size: 0               Blocks: 0          IO Block: 65536  regular
empty file
Device: 540aba0bh/1409989131d   Inode: 562949953592977  Links: 1
Access: (0644/-rw-r--r--)  Uid: ( 1000/     ian)   Gid: (  513/    None)
Access: 2011-12-02 16:58:07.414457900 +0000
Modify: 2011-12-02 16:58:07.414457900 +0000
Change: 2011-12-02 16:58:03.495141800 +0000
 Birth: 2011-12-02 16:57:57.731469900 +0000

And if we copy such a file, then the copy is older (as determined by the
"Modify" time) than the original:

$ date; touch foo; stat foo; cp foo bar; stat bar
Fri Dec  2 16:59:10 GMTST 2011
  File: `foo'
  Size: 0               Blocks: 0          IO Block: 65536  regular
empty file
Device: 540aba0bh/1409989131d   Inode: 1407374883725128  Links: 1
Access: (0644/-rw-r--r--)  Uid: ( 1000/     ian)   Gid: (  513/    None)
Access: 2011-12-02 16:59:10.118457900 +0000
Modify: 2011-12-02 16:59:10.118457900 +0000
Change: 2011-12-02 16:59:06.189477700 +0000
 Birth: 2011-12-02 16:57:57.731469900 +0000
  File: `bar'
  Size: 0               Blocks: 0          IO Block: 65536  regular
empty file
Device: 540aba0bh/1409989131d   Inode: 281474976882512  Links: 1
Access: (0644/-rw-r--r--)  Uid: ( 1000/     ian)   Gid: (  513/    None)
Access: 2011-12-02 16:59:06.394555800 +0000
Modify: 2011-12-02 16:59:06.394555800 +0000
Change: 2011-12-02 16:59:06.395532400 +0000
 Birth: 2011-12-02 16:58:40.921899600 +0000

This means that make thinks that things are out of date when it
shouldn't, so reinvokes itself repeatedly until the MAKE_RESTARTS
infinite-recursion test triggers.

The touchy program, like most other programs, creates files with both
Modify and Change in the past, which is still a little odd, but is
consistent, so doesn't break make.

We used to use _utime(argv[i],NULL)) to set the file modification times,
but after a BST -> GMT change this started giving files a modification
time an hour in the future:

$ date; utils/touchy/dist/build/tmp/touchy testfile; stat testfile
Tue, Oct 30, 2012 11:33:06 PM
  File: `testfile'
  Size: 0               Blocks: 0          IO Block: 65536  regular empty file
Device: 540aba0bh/1409989131d   Inode: 9851624184986293  Links: 1
Access: (0755/-rwxr-xr-x)  Uid: ( 1000/     ian)   Gid: (  513/    None)
Access: 2012-10-31 00:33:06.000000000 +0000
Modify: 2012-10-31 00:33:06.000000000 +0000
Change: 2012-10-30 23:33:06.769118900 +0000
 Birth: 2012-10-30 23:33:06.769118900 +0000

so now we use the Win32 functions GetSystemTimeAsFileTime and SetFileTime.
*/

int
main(int argc, char** argv)
{
    int i;
    FILETIME ft;
    BOOL b;
    HANDLE hFile;

    if (argc == 1) {
        fprintf(stderr, "Usage: %s <files>\n", argv[0]);
        return 1;
    }

    for (i = 1; i < argc; i++) {
        hFile = CreateFile(argv[i], GENERIC_WRITE, 0, NULL, OPEN_ALWAYS,
                           FILE_ATTRIBUTE_NORMAL, NULL);
        if (hFile == INVALID_HANDLE_VALUE) {
            fprintf(stderr, "Unable to open %s\n", argv[i]);
            exit(1);
        }
        GetSystemTimeAsFileTime(&ft);
        b = SetFileTime(hFile, (LPFILETIME) NULL, (LPFILETIME) NULL, &ft);
        if (b == 0) {
            fprintf(stderr, "Unable to change mod. time for %s\n", argv[i]);
            exit(1);
        }
        b = CloseHandle(hFile);
        if (b == 0) {
            fprintf(stderr, "Closing failed for %s\n", argv[i]);
            exit(1);
        }
    }

    return 0;
}
#endif
