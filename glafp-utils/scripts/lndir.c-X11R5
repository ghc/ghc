/* $XConsortium: lndir.c,v 1.5 91/07/21 15:52:11 rws Exp $ */
/* Create shadow link tree (after X11R4 script of the same name)
   Mark Reinhold (mbr@lcs.mit.edu)/3 January 1990 */

/* Copyright 1990, Massachusetts Institute of Technology

   Permission to use, copy, modify, and distribute this program for any purpose
   and without fee is hereby granted, provided that this copyright and
   permission notice appear on all copies and supporting documentation, that
   the name of MIT not be used in advertising or publicity pertaining to
   distribution of this program without specific prior permission, and that
   notice be given in supporting documentation that copying and distribution is
   by permission of MIT.  MIT makes no representations about the suitability of
   this software for any purpose.  It is provided "as is" without expressed or
   implied warranty.
*/

/* From the original /bin/sh script:

   Used to create a copy of the a directory tree that has links for all
   non-directories (except those named RCS or SCCS).  If you are
   building the distribution on more than one machine, you should use
   this script.

   If your master sources are located in /usr/local/src/X and you would like
   your link tree to be in /usr/local/src/new-X, do the following:

   	%  mkdir /usr/local/src/new-X
	%  cd /usr/local/src/new-X
   	%  lndir ../X
*/

#include <X11/Xos.h>
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

extern int errno;
int silent;

void
quit (code, fmt, a1, a2, a3)
char *fmt;
{
    fprintf (stderr, fmt, a1, a2, a3);
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
msg (fmt, a1, a2, a3)
char *fmt;
{
    fprintf (stderr, fmt, a1, a2, a3);
    putc ('\n', stderr);
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
    struct stat sb, sc;
    int n_dirs;

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
	strcpy (p, dp->d_name);

	if (n_dirs > 0) {
	    if (stat (buf, &sb) < 0) {
		perror (buf);
		continue;
	    }

	    if (sb.st_mode & S_IFDIR) {
		/* directory */
		n_dirs--;
		if (dp->d_name[0] == '.' &&
		    (dp->d_name[1] == '\0' || (dp->d_name[1] == '.' &&
					       dp->d_name[2] == '\0')))
		    continue;
		if (!strcmp (dp->d_name, "RCS"))
		    continue;
		if (!strcmp (dp->d_name, "SCCS"))
		    continue;
		if (!silent)
		    printf ("%s:\n", buf);
		if ((stat (dp->d_name, &sc) < 0) && (errno == ENOENT)) {
		    if (mkdir (dp->d_name, 0777) < 0 ||
			stat (dp->d_name, &sc) < 0) {
			perror (dp->d_name);
			continue;
		    }
		}
		if (readlink (dp->d_name, symbuf, sizeof(symbuf) - 1) >= 0) {
		    msg ("%s: is a link instead of a directory\n", dp->d_name);
		    continue;
		}
		if (chdir (dp->d_name) < 0) {
		    perror (dp->d_name);
		    continue;
		}
		dodir (buf, &sb, &sc, (buf[0] != '/'));
		if (chdir ("..") < 0)
		    quiterr (1, "..");
		continue;
	    }
	}

	/* non-directory */
	if (symlink (buf, dp->d_name) < 0) {
	    int saverrno = errno;
	    int symlen;
	    symlen = readlink(dp->d_name, symbuf, sizeof(symbuf) - 1);
	    errno = saverrno;
	    if (symlen > 0)
		symbuf[symlen] = '\0';
	    if (symlen < 0 || strcmp(symbuf, buf))
		perror (dp->d_name);
	}
    }

    closedir (df);
    return 0;
}


main (ac, av)
int ac;
char **av;
{
    char *fn, *tn;
    struct stat fs, ts;

    silent = 0;
    if (ac > 1 && !strcmp(av[1], "-silent")) {
	silent = 1;
    }
    if (ac < silent + 2 || ac > silent + 3)
	quit (1, "usage: %s [-silent] fromdir [todir]", av[0]);

    fn = av[silent + 1];
    if (ac == silent + 3)
	tn = av[silent + 2];
    else
	tn = ".";

    /* to directory */
    if (stat (tn, &ts) < 0)
	quiterr (1, tn);
    if (!(ts.st_mode & S_IFDIR))
	quit (2, "%s: Not a directory", tn);
    if (chdir (tn) < 0)
	quiterr (1, tn);

    /* from directory */
    if (stat (fn, &fs) < 0)
	quiterr (1, fn);
    if (!(fs.st_mode & S_IFDIR))
	quit (2, "%s: Not a directory", fn);

    exit (dodir (fn, &fs, &ts, 0));
}
