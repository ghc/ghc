/* defines a few MIN_VERSION_...() macros used by some of the bootstrap packages */

#if __GLASGOW_HASKELL__ >= 800
/* macros are generated accurately by GHC on the fly */
#elif __GLASGOW_HASKELL__ >= 711
/* package base-4.9.0.0 */
# define MIN_VERSION_base(major1,major2,minor) (\
  (major1) <  4 || \
  (major1) == 4 && (major2) <  9 || \
  (major1) == 4 && (major2) == 9 && (minor) <= 0)
/* package bytestring-0.10.8 */
# define MIN_VERSION_bytestring(major1,major2,minor) (\
  (major1) <  0 || \
  (major1) == 0 && (major2) <  10 || \
  (major1) == 0 && (major2) == 10 && (minor) <= 8)

#elif __GLASGOW_HASKELL__ >= 709
/* package base-4.8.0.0 */
# define MIN_VERSION_base(major1,major2,minor) (\
  (major1) <  4 || \
  (major1) == 4 && (major2) <  8 || \
  (major1) == 4 && (major2) == 8 && (minor) <= 0)
/* package bytestring-0.10.6 */
# define MIN_VERSION_bytestring(major1,major2,minor) (\
  (major1) <  0 || \
  (major1) == 0 && (major2) <  10 || \
  (major1) == 0 && (major2) == 10 && (minor) <= 6)

#elif __GLASGOW_HASKELL__ >= 707
/* package base-4.7.0 */
# define MIN_VERSION_base(major1,major2,minor) (\
  (major1) <  4 || \
  (major1) == 4 && (major2) <  7 || \
  (major1) == 4 && (major2) == 7 && (minor) <= 0)
/* package bytestring-0.10.4 */
# define MIN_VERSION_bytestring(major1,major2,minor) (\
  (major1) <  0 || \
  (major1) == 0 && (major2) <  10 || \
  (major1) == 0 && (major2) == 10 && (minor) <= 4)
#endif
