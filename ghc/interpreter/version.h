/* --------------------------------------------------------------------------
 * Version number
 * ------------------------------------------------------------------------*/

/* Define this as a 14 character string uniquely identifying the current 
 * version.
 * Major releases from Nottingham/Yale are of the form "<month><year>"
 * Minor releases from Nottingham/Yale are of the form "[Beta YYMMDD]"
 * Anyone else should use a different format to avoid confusion.    
 */
#define MAJOR_RELEASE 0

#if MAJOR_RELEASE
#define HUGS_VERSION "October 1999  "
#else
#define HUGS_VERSION "991015 (STG)  "
#endif

