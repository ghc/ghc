/*
  Include File for the Lexical Analyser and Parser.

  19/11/91	kh	Created.
*/


#ifndef __CONSTANTS_H
#define __CONSTANTS_H

/*
  Important Literal Constants.
*/

#define MODNAME_SIZE		512		/* Size of Module Name buffers 	*/
#define FILENAME_SIZE		4096		/* Size of File buffers 	*/
#define ERR_BUF_SIZE	  	512		/* Size of error buffers 	*/

#ifdef YYLMAX					/* Get rid of YYLMAX 		*/
#undef YYLMAX					/* Ugly -- but necessary 	*/
#endif

#define	YYLMAX			8192		/* Size of yytext -- limits strings, identifiers etc. */


#define HASH_TABLE_SIZE		993		/* Default number of entries in the hash table. */

#define MAX_CONTEXTS 		100   		/* Maximum nesting of wheres, cases etc */

#define MAX_INFIX 		500		/* Maximum number of infix operators */


#ifdef TRUE
#undef TRUE
#endif

#ifdef FALSE
#undef FALSE
#endif

#define TRUE	1
#define FALSE	0
typedef int BOOLEAN;

#endif /* __CONSTANTS_H */
