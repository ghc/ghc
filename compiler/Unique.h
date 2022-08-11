/* unique has the following structure:
 * HsInt64 unique =
 *    (unique_tag << (64 - UNIQUE_TAG_BITS)) | unique_number
 */
#define UNIQUE_TAG_BITS 8
