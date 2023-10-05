/* unique has the following structure:
 * HsInt unique =
 *    (unique_tag << (sizeof (HsInt) - UNIQUE_TAG_BITS)) | unique_number
 */
#define UNIQUE_TAG_BITS 8
