/* 
 * (c) The University of Glasgow 2002
 *
 * Time Runtime Support
 */
#include "HsBase.h"

#if defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32) /* to the end */

HsAddr __hscore_timezone( void )
{ return (HsAddr)&_timezone; }

HsAddr __hscore_tzname( void )
{ return (HsAddr)_tzname; }
#endif
