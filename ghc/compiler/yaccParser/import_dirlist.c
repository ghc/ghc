/**********************************************************************
*                                                                     *
*                                                                     *
*      Import Directory List Handling                                 *
*                                                                     *
*                                                                     *
**********************************************************************/

#include <stdio.h>

#include "hspincl.h"
#include "constants.h"
#include "utils.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#else
#ifdef HAVE_TYPES_H
#include <types.h>
#endif
#endif

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#ifdef HAVE_SYS_FILE_H
#include <sys/file.h>
#endif

#ifndef HAVE_ACCESS
#define R_OK "r"
#define F_OK "r"
short
access(const char *fileName, const char *mode)
{
    FILE *fp = fopen(fileName, mode);
    if (fp != NULL) {
	(void) fclose(fp);
	return 0;
    }
    return 1;
}
#endif /* HAVE_ACCESS */


list	imports_dirlist, sys_imports_dirlist; /* The imports lists */
extern  char HiSuffix[];
extern  char PreludeHiSuffix[];
/* OLD 95/08: extern BOOLEAN ExplicitHiSuffixGiven; */

#define MAX_MATCH 16

/*
  This finds a module along the imports directory list.
*/

void
find_module_on_imports_dirlist(char *module_name, BOOLEAN is_sys_import, char *returned_filename)
{
    char try[FILENAME_SIZE];

    list imports_dirs;

#ifdef HAVE_STAT
    struct stat sbuf[MAX_MATCH];
#endif

    int no_of_matches = 0;
    BOOLEAN tried_source_dir = FALSE;

    char *try_end;
    char *suffix_to_use    = (is_sys_import) ? PreludeHiSuffix : HiSuffix;
    char *suffix_to_report = suffix_to_use; /* save this for reporting, because we
						might change suffix_to_use later */
    int modname_len = strlen(module_name);

    /* 
       Check every directory in (sys_)imports_dirlist for the imports file.
       The first directory in the list is the source directory.
    */
    for (imports_dirs = (is_sys_import) ? sys_imports_dirlist : imports_dirlist;
	 tlist(imports_dirs) == lcons; 
	 imports_dirs = ltl(imports_dirs))
      {
	char *dir = (char *) lhd(imports_dirs);
	strcpy(try, dir);

	try_end = try + strlen(try);

#ifdef macintosh /* ToDo: use DIR_SEP_CHAR */
	if (*(try_end - 1) != ':')
	    strcpy (try_end++, ":");
#else
	if (*(try_end - 1) != '/')
	  strcpy (try_end++, "/");
#endif /* ! macintosh */

	strcpy(try_end, module_name);

	strcpy(try_end+modname_len, suffix_to_use);

	/* See whether the file exists and is readable. */
	if (access (try,R_OK) == 0)
	  {
	    if ( no_of_matches == 0 ) 
		strcpy(returned_filename, try);

	    /* Return as soon as a match is found in the source directory. */
	    if (!tried_source_dir)
	      return;

#ifdef HAVE_STAT
    	    if ( no_of_matches < MAX_MATCH && stat(try, sbuf + no_of_matches) == 0 )
    	      {
    	    	int i;
    	    	for (i = 0; i < no_of_matches; i++)
    	    	  {
    	    	    if ( sbuf[no_of_matches].st_dev == sbuf[i].st_dev &&
    	    	    	 sbuf[no_of_matches].st_ino == sbuf[i].st_ino)
    	    	      goto next;    /* Skip dups */
    	    	  }
              }
#endif /* HAVE_STAT */
    	    no_of_matches++;
	  }
	else if (access (try,F_OK) == 0)
	  fprintf(stderr,"Warning: %s exists, but is not readable\n",try);

      next:	
	tried_source_dir = TRUE;
      }

    if ( no_of_matches == 0 && ! is_sys_import ) { /* Nothing so far */

	/* If we are explicitly meddling about with .hi suffixes,
	   then some system-supplied modules may need to be looked
	   for with PreludeHiSuffix; unsavoury but true...
	*/
	suffix_to_use = PreludeHiSuffix;

	for (imports_dirs = sys_imports_dirlist;
	     tlist(imports_dirs) == lcons; 
	     imports_dirs = ltl(imports_dirs))
	  {
	    char *dir = (char *) lhd(imports_dirs);
	    strcpy(try, dir);

	    try_end = try + strlen(try);

#ifdef macintosh /* ToDo: use DIR_SEP_STRING */
	    if (*(try_end - 1) != ':')
		strcpy (try_end++, ":");
#else
	    if (*(try_end - 1) != '/')
	      strcpy (try_end++, "/");
#endif /* ! macintosh */

	    strcpy(try_end, module_name);

	    strcpy(try_end+modname_len, suffix_to_use);

	    /* See whether the file exists and is readable. */
	    if (access (try,R_OK) == 0)
	      {
		if ( no_of_matches == 0 ) 
		    strcpy(returned_filename, try);

#ifdef HAVE_STAT
    	        if ( no_of_matches < MAX_MATCH && stat(try, sbuf + no_of_matches) == 0 )
    	          {
    	    	    int i;
    	    	    for (i = 0; i < no_of_matches; i++)
    	    	      {
    	    	        if ( sbuf[no_of_matches].st_dev == sbuf[i].st_dev &&
    	    	    	     sbuf[no_of_matches].st_ino == sbuf[i].st_ino)
    	    	          goto next_again;    /* Skip dups */
    	    	      }
                  }
#endif /* HAVE_STAT */
    	        no_of_matches++;
	      }
	    else if (access (try,F_OK) == 0)
	      fprintf(stderr,"Warning: %s exists, but is not readable\n",try);
          next_again:
	   /*NOTHING*/;
	  }
    }

    /* Error checking */

    switch ( no_of_matches ) {
    default:
	  fprintf(stderr,"Warning: found %d %s files for module \"%s\"\n",
			no_of_matches, suffix_to_report, module_name);
    	  break;
    case 0:
    	  {
	    char disaster_msg[MODNAME_SIZE+1000];
	    sprintf(disaster_msg,"can't find interface (%s) file for module \"%s\"%s",
			suffix_to_report, module_name,
			(strncmp(module_name, "PreludeGlaIO", 12) == 0)
			? "\n(The PreludeGlaIO interface no longer exists);"
			:(
			(strncmp(module_name, "PreludePrimIO", 13) == 0)
			? "\n(The PreludePrimIO interface no longer exists -- just use PreludeGlaST);"
			:(
			(strncmp(module_name, "Prelude", 7) == 0)
			? "\n(Perhaps you forgot a `-fglasgow-exts' flag?);"
			: ""
	    )));
	    hsperror(disaster_msg);
    	    break;
    	  }
    case 1:
    	/* Everything is fine */
    	break;
    }
}
