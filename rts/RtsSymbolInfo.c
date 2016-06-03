/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000-2015
 *
 * RTS Symbols
 *
 * ---------------------------------------------------------------------------*/

#include "ghcplatform.h"
#include "RtsSymbolInfo.h"

#include "Rts.h"
#include "HsFFI.h"

#include "Hash.h"
#include "RtsUtils.h"

typedef struct _SymbolInfo {
    /* Determines if the
       symbol is weak */
    HsBool isWeak;

} SymbolInfo;

/* -----------------------------------------------------------------------------
* Performs a check to see if the symbol at the given address
* is a weak symbol or not.
*
* Returns: HS_BOOL_TRUE on symbol being weak, else HS_BOOL_FALSE
*/
HsBool isSymbolWeak(ObjectCode *owner, void *label)
{
    SymbolInfo *info;
    if (owner
        && label
        && owner->extraInfos
        && (info = lookupStrHashTable(owner->extraInfos, label)) != NULL)
    {
        return info->isWeak;
    }

    return HS_BOOL_FALSE;
}

/* -----------------------------------------------------------------------------
* Marks the symbol at the given address as weak or not.
* If the extra symbol infos table has not been initialized
* yet this will create and allocate a new Hashtable
*/
void setWeakSymbol(ObjectCode *owner, void *label)
{
    SymbolInfo *info;
    if (owner && label)
    {
        info = NULL;
        if (!owner->extraInfos)
        {
            owner->extraInfos = allocStrHashTable();
        }
        else {
            info = lookupStrHashTable(owner->extraInfos, label);
        }

        if (!info){
            info = stgMallocBytes(sizeof(SymbolInfo), "setWeakSymbol");
        }

        info->isWeak = HS_BOOL_TRUE;

        insertStrHashTable(owner->extraInfos, label, info);
    }
}
