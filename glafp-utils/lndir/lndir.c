/* $XConsortium: lndir.c /main/16 1996/09/28 16:16:40 rws $ */
/* Create shadow link tree (after X11R4 script of the same name)
   Mark Reinhold (mbr@lcs.mit.edu)/3 January 1990 */

/* 
Copyright (c) 1990,  X Consortium

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the X Consortium shall not be
used in advertising or otherwise to promote the sale, use or other dealings
in this Software without prior written authorization from the X Consortium.

*/

/* From the original /bin/sh script:

  Used to create a copy of the a directory tree that has links for all
  non-directories (except those named RCS, SCCS or CVS.adm).  If you are
  building the distribution on more than one machine, you should use
  this technique.

  If your master sources are located in /usr/local/src/X and you would like
  your link tree to be in /usr/local/src/new-X, do the following:

   	%  mkdir /usr/local/src/new-X
	%  cd /usr/local/src/new-X
   	%  lndir ../X
*/

#include "lndir-Xos.h"
#include <stdlib.h>
#include <stdio.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <errno.h>

#ifndef X_NOT_POSIX
#include <dirent.h>
#else
#ifdef SYSV
#include <dirent.h>
#else
#ifdef USG
#include <dirent.h>
#else
#include <sys/dir.h>
#ifndef dirent
#define dirent direct
#endif
#endif
#endif
#endif
#ifndef MAXPATHLEN
#define MAXPATHLEN 2048
#endif

#ifdef __CYGWIN32__
#include <sys/cygwin.h>
#endif

#if NeedVarargsPrototypes
#include <stdarg.h>
#endif

#ifdef X_NOT_STDC_ENV
extern int errno;
#endif
int silent = 0;			/* -silent */
int ignore_links = 0;		/* -ignorelinks */

char *rcurdir;
char *curdir;

int force=0;

void
quit (
#if NeedVarargsPrototypes
    int code, char * fmt, ...)
#else
    code, fmt, a1, a2, a3)
    char *fmt;
#endif
{
#if NeedVarargsPrototypes
    va_list args;
    va_start(args, fmt);
    vfprintf (stderr, fmt, args);
    va_end(args);
#else
    fprintf (stderr, fmt, a1, a2, a3);
#endif
    putc ('\n', stderr);
    exit (code);
}

void
quiterr (code, s)
    char *s;
{
    perror (s);
    exit (code);
}

void
msg (
#if NeedVarargsPrototypes
    char * fmt, ...)
#else
    fmt, a1, a2, a3)
    char *fmt;
#endif
{
#if NeedVarargsPrototypes
    va_list args;
#endif
    if (curdir) {
	fprintf (stderr, "%s:\n", curdir);
	curdir = 0;
    }
#if NeedVarargsPrototypes
    va_start(args, fmt);
    vfprintf (stderr, fmt, args);
    va_end(args);
#else
    fprintf (stderr, fmt, a1, a2, a3);
#endif
    putc ('\n', stderr);
}

void
mperror (s)
    char *s;
{
    if (curdir) {
	fprintf (stderr, "%s:\n", curdir);
	curdir = 0;
    }
    perror (s);
}


int equivalent(lname, rname)
    char *lname;
    char *rname;
{
    char *s;

    if (!strcmp(lname, rname))
	return 1;
    for (s = lname; *s && (s = strchr(s, '/')); s++) {
	while (s[1] == '/')
	    strcpy(s+1, s+2);
    }
    return !strcmp(lname, rname);
}


/* Recursively create symbolic links from the current directory to the "from"
   directory.  Assumes that files described by fs and ts are directories. */

dodir (fn, fs, ts, rel)
char *fn;			/* name of "from" directory, either absolute or
				   relative to cwd */
struct stat *fs, *ts;		/* stats for the "from" directory and cwd */
int rel;			/* if true, prepend "../" to fn before using */
{
    DIR *df;
    struct dirent *dp;
    char buf[MAXPATHLEN + 1], *p;
    char symbuf[MAXPATHLEN + 1];
    char basesym[MAXPATHLEN + 1];
    struct stat sb, sc;
    int n_dirs;
    int symlen;
    int basesymlen = -1;
    char *ocurdir;

    if ((fs->st_dev == ts->st_dev) && (fs->st_ino == ts->st_ino)) {
	msg ("%s: From and to directories are identical!", fn);
	return 1;
    }

    if (rel)
	strcpy (buf, "../");
    else
	buf[0] = '\0';
    strcat (buf, fn);
    
    if (!(df = opendir (buf))) {
	msg ("%s: Cannot opendir", buf);
	return 1;
    }

    p = buf + strlen (buf);
    *p++ = '/';
    n_dirs = fs->st_nlink;
    while (dp = readdir (df)) {
	if (dp->d_name[strlen(dp->d_name) - 1] == '~')
	    continue;
	if (dp->d_name[0] == '.' && dp->d_name[1] == '#') /* 'non-conflict files' left behind by CVS */
	    continue;
	strcpy (p, dp->d_name);

	if (n_dirs > 0) {
	    if (stat (buf, &sb) < 0) {
		mperror (buf);
		continue;
	    }

#ifdef S_ISDIR
	    if(S_ISDIR(sb.st_mode))
#else
	    if (sb.st_mode & S_IFDIR) 
#endif
	    {
		/* directory */
#ifndef __CYGWIN32__   /* don't trust cygwin's n_dirs count */
		n_dirs--;
#endif
		if (dp->d_name[0] == '.' &&
		    (dp->d_name[1] == '\0' || (dp->d_name[1] == '.' &&
					       dp->d_name[2] == '\0')))
		    continue;
		if (!strcmp (dp->d_name, "RCS"))
		    continue;
		if (!strcmp (dp->d_name, "SCCS"))
		    continue;
		if (!strcmp (dp->d_name, "CVS"))
		    continue;
		if (!strcmp (dp->d_name, ".svn"))
		    continue;
		if (!strcmp (dp->d_name, "_darcs"))
		    continue;
		if (!strcmp (dp->d_name, "CVS.adm"))
		    continue;
		ocurdir = rcurdir;
		rcurdir = buf;
		curdir = silent ? buf : (char *)0;
		if (!silent)
		    printf ("%s:\n", buf);
		if ((stat (dp->d_name, &sc) < 0) && (errno == ENOENT)) {
		    if (mkdir (dp->d_name, 0777) < 0 ||
			stat (dp->d_name, &sc) < 0) {
			mperror (dp->d_name);
			curdir = rcurdir = ocurdir;
			continue;
		    }
		}
		if (readlink (dp->d_name, symbuf, sizeof(symbuf) - 1) >= 0) {
		    msg ("%s: is a link instead of a directory", dp->d_name);
		    curdir = rcurdir = ocurdir;
		    continue;
		}
		if (chdir (dp->d_name) < 0) {
		    mperror (dp->d_name);
		    curdir = rcurdir = ocurdir;
		    continue;
		}
		dodir (buf, &sb, &sc, (buf[0] != '/'));
		if (chdir ("..") < 0)
		    quiterr (1, "..");
		curdir = rcurdir = ocurdir;
		continue;
	    }
	}

	/* non-directory */
	symlen = readlink (dp->d_name, symbuf, sizeof(symbuf) - 1);
	if (symlen >= 0)
	    symbuf[symlen] = '\0';

	/* The option to ignore links exists mostly because
	   checking for them slows us down by 10-20%.
	   But it is off by default because this really is a useful check. */
	if (!ignore_links) {
	    /* see if the file in the base tree was a symlink */
	    basesymlen = readlink(buf, basesym, sizeof(basesym) - 1);
	    if (basesymlen >= 0)
		basesym[basesymlen] = '\0';
	}

	if (symlen >= 0) {
	  if (!equivalent (basesymlen>=0 ? basesym : buf, symbuf)) {
	    if (force) {
	      unlink(dp->d_name);
	      if (symlink (basesymlen>=0 ? basesym : buf, dp->d_name) < 0)
		mperror (dp->d_name);
	    } else {
	      /* Link exists in new tree.  Print message if it doesn't match. */
	      msg ("%s: %s", dp->d_name, symbuf);
	    }
	  }
	} else {
	  if (symlink (basesymlen>=0 ? basesym : buf, dp->d_name) < 0)
	    mperror (dp->d_name);
	}
    }
    
    closedir (df);
    return 0;
}


main (ac, av)
int ac;
char **av;
{
    char *prog_name = av[0];
    char* tn;
    struct stat fs, ts;
#ifdef __CYGWIN32__
    /*   
    The lndir code assumes unix-style paths to work. cygwin
    lets you get away with using dos'ish paths (e.g., "f:/oo")
    in most contexts. Using them with 'lndir' will seriously
    confuse the user though, so under-the-hood, we convert the
    path into something POSIX-like.
    */
    static char fn[MAXPATHLEN+1];
#else
    char *fn;
#endif

    while (++av, --ac) {
      if (strcmp(*av, "-silent") == 0)
	  silent = 1;
      else if (strcmp(*av, "-f") == 0)
	  force = 1;
      else if (strcmp(*av, "-ignorelinks") == 0)
	  ignore_links = 1;
      else if (strcmp(*av, "--") == 0) {
	  ++av, --ac;
	  break;
      } else
	  break;
    }

    if (ac < 1 || ac > 2)
	quit (1, "usage: %s [-f] [-silent] [-ignorelinks] fromdir [todir]",
	      prog_name);

#ifdef __CYGWIN32__
    cygwin_conv_to_full_posix_path(av[0], fn);
#else
    fn = av[0];
#endif

    if (ac == 2)
	tn = av[1];
    else
	tn = ".";

    /* to directory */
    if (stat (tn, &ts) < 0) {
      if (force && (tn[0] != '.' || tn[1] != '\0') ) {
         mkdir(tn, S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH );
      } 
      else {
	quiterr (1, tn);
#ifdef S_ISDIR
        if (!(S_ISDIR(ts.st_mode)))
#else
        if (!(ts.st_mode & S_IFDIR))
#endif
	   quit (2, "%s: Not a directory", tn);
      }
    }
    if (chdir (tn) < 0)
	quiterr (1, tn);

    /* from directory */
    if (stat (fn, &fs) < 0)
	quiterr (1, fn);
#ifdef S_ISDIR
    if (!(S_ISDIR(fs.st_mode)))
#else
    if (!(fs.st_mode & S_IFDIR))
#endif
	quit (2, "%s: Not a directory", fn);

    exit (dodir (fn, &fs, &ts, 0));
}
