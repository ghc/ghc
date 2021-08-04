See also http://pvp.haskell.org/faq

# 1.2.0.1

* Package update: support for `bytestring >=0.11`
 
# 1.2.0.0

* Security fix: reject non-canonical base64 encoded values - ([#38](https://github.com/haskell/base64-bytestring/pull/38)) fixing issue [#24](https://github.com/haskell/base64-bytestring/issues/24).

* Security fix: reject bytestrings with improper padding that can be "completed" by the unpadded-Base64url workflow, and homogenize error messages ([#33](https://github.com/haskell/base64-bytestring/pull/33))

* Test coverage expanded to 98% of the library. All critical paths covered.


# 1.1.0.0

* `joinWith` has been removed ([#32](https://github.com/haskell/base64-bytestring/pull/32))
* Bugfix: `decode` formerly allowed for padding chars to be interspersed in a valid base64-encoded string. This is now not the case, and it is fully spec-compliant as of [#31](https://github.com/haskell/base64-bytestring/pull/31)
* The default behavior for Base64url `decode` is now to support arbitrary padding. If you need strict padded or unpadded decode semantics, use `decodePadded` or `decodeUnpadded`.
* Added strict unpadded and padded decode functions for Base64url ([#30](https://github.com/haskell/base64-bytestring/pull/30))
* Added unpadded encode for Base64url
  ([#26](https://github.com/haskell/base64-bytestring/pull/26)).

----

### 1.0.0.3

* Made performance more robust
  ([#27](https://github.com/haskell/base64-bytestring/pull/27)).
* Improved documentation
  ([#23](https://github.com/haskell/base64-bytestring/pull/23)).
* Improved the performance of decodeLenient a bit
  ([#21](https://github.com/haskell/base64-bytestring/pull/21)).

### 1.0.0.2

* Fixed a write past allocated memory in joinWith (potential security
  issue).

## 0.1.1.0 - 1.0.0.1

* Changelog not recorded for these versions.

### 0.1.0.3

*  Fixed: wrong encoding table on big-endian systems.
*  Fixed: too big indices in encoding table construction.

### 0.1.0.2

*  Changelog not recorded up to this version.
