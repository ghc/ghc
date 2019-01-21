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

#include <stdbool.h>

/* Generic function to update any extra info fields.  */
void setSymbolInfo(ObjectCode *owner, const void *label, symbolUpdater updater)
{
    SymbolInfo *info;
    if (owner && label)
    {
        info = NULL;
        if (!owner->extraInfos)
            owner->extraInfos = allocStrHashTable();
        else
            info = lookupStrHashTable(owner->extraInfos, label);

        if (!info)
        {
            info = stgMallocBytes(sizeof(SymbolInfo), "setSymbolInfo");
            info->kind = 0;
        }

        updater(info);
        insertStrHashTable(owner->extraInfos, label, info);
    }
}

/* -----------------------------------------------------------------------------
* Performs a check to see if the symbol at the given address
* is a weak symbol or not.
*
* Returns: true on symbol being weak, else false
*/
bool isSymbolWeak(ObjectCode *owner, const void *label)
{
    SymbolInfo *info;
    return owner
        && label
        && owner->extraInfos
        && (info = lookupStrHashTable(owner->extraInfos, label)) != NULL
        && (info->kind & KIND_WEAK) == KIND_WEAK;
}

/* -----------------------------------------------------------------------------
* Performs a check to see if the symbol at the given address
* is an import symbol or not.
*
* Returns: true on symbol being weak, else false
*/
bool isSymbolImport(ObjectCode *owner, const void *label)
{
    SymbolInfo *info;
    return owner
        && label
        && owner->extraInfos
        && (info = lookupStrHashTable(owner->extraInfos, label)) != NULL
        && (info->kind & KIND_IMPORT) == KIND_IMPORT;
}

static void markWeak(SymbolInfo* info)
{
    if(info)
      info->kind |= KIND_WEAK;
}

static void markImport(SymbolInfo* info)
{
    if(info)
      info->kind |= KIND_IMPORT;
}

static void unmarkImport(SymbolInfo* info)
{
    if(info)
      info->kind &= ~KIND_IMPORT;
}

/* -----------------------------------------------------------------------------
* Marks the symbol at the given address as weak or not.
* If the extra symbol infos table has not been initialized
* yet this will create and allocate a new StrHashtable
*/
void setWeakSymbol(ObjectCode *owner, const void *label)
{
    setSymbolInfo (owner, label, &markWeak);
}

/* -----------------------------------------------------------------------------
* Marks the symbol at the given address as import or not.
* If the extra symbol infos table has not been initialized
* yet this will create and allocate a new StrHashtable
*/
void setImportSymbol(ObjectCode *owner, const void *label)
{
    setSymbolInfo (owner, label, &markImport);
}

/* -----------------------------------------------------------------------------
* Clear the import symbol flag.
* If the extra symbol infos table has not been initialized
* yet this will create and allocate a new StrHashtable
*/
void clearImportSymbol(ObjectCode *owner, const void *label)
{
    setSymbolInfo (owner, label, &unmarkImport);
}
