/*
 * Simple _utime() wrapper for setting the mod. time on files
 * to the current system time.
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

int
main(int argc, char** argv)
{
  int rc;
  int i=0;
  int fd;
  int wBitSet = 0;
  struct _stat sb;

  if (argc == 1) {
    fprintf(stderr, "Usage: %s <files>\n", argv[0]);
    return 1;
  }
  
  
  while (i++ < (argc-1)) {
    if ( (_access(argv[i], 00) < 0) && (errno == ENOENT || errno == EACCES) ) {
       /* File doesn't exist, try creating it. */
      if ( (fd = _open(argv[i], _O_CREAT | _O_EXCL | _O_TRUNC, _S_IREAD | _S_IWRITE)) < 0 ) {
      	fprintf(stderr, "Unable to create %s, skipping.\n", argv[i]);
      } else {
      	_close(fd);
      }
    }
    if ( (_access(argv[i], 02)) < 0 ) {
    	/* No write permission, try setting it first. */
	if (_stat(argv[i], &sb) < 0) {
	   fprintf(stderr, "Unable to change mod. time for %s  (%d)\n", argv[i], errno);
	   continue;
	}
	if (_chmod(argv[i], (sb.st_mode & _S_IREAD) | _S_IWRITE) < 0) {
	   fprintf(stderr, "Unable to change mod. time for %s  (%d)\n", argv[i], errno);
	   continue;
	}
	wBitSet = 1;
    }
    if ( (rc = _utime(argv[i],NULL)) < 0) {
      fprintf(stderr, "Unable to change mod. time for %s  (%d)\n", argv[i], errno);
    }
    if (wBitSet) {
	/* Turn the file back into a read-only file */
   	_chmod(argv[i], (sb.st_mode & _S_IREAD));
	wBitSet = 0;
    }
  }
  
  return 0;
}
#endif
