/* defines a few MIN_VERSION_...() macros used by some of the bootstrap packages */

#if __GLASGOW_HASKELL__ >= 709
/* package base-4.8.0.0 */
# define MIN_VERSION_base(major1,major2,minor) (\
  (major1) <  4 || \
  (major1) == 4 && (major2) <  8 || \
  (major1) == 4 && (major2) == 8 && (minor) <= 0)
#elif __GLASGOW_HASKELL__ >= 707
/* package base-4.7.0 */
# define MIN_VERSION_base(major1,major2,minor) (\
  (major1) <  4 || \
  (major1) == 4 && (major2) <  7 || \
  (major1) == 4 && (major2) == 7 && (minor) <= 0)
#elif __GLASGOW_HASKELL__ >= 705
/* package base-4.6.0 */
# define MIN_VERSION_base(major1,major2,minor) (\
  (major1) <  4 || \
  (major1) == 4 && (major2) <  6 || \
  (major1) == 4 && (major2) == 6 && (minor) <= 0)
#endif
