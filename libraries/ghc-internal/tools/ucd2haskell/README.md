# Generating GHC’s Unicode modules

`GHC.Internal.Unicode.*` modules are generated with the internal tool `ucd2haskell`.

```bash
cd ucd2haskell
./ucd.sh download && ./ucd.sh generate
```

# Updating Unicode modules

1. Update the Unicode version in `unicode_version`.
2. _Comment_ the line in `ucd.sh` with `VERIFY_CHECKSUM=y`.
3. Run `./ucd.sh download`.
4. Update the checksums in `ucd.sh` and _uncomment_ `VERIFY_CHECKSUM=y`.
5. Run `./ucd.sh generate`. This will generate the `GHC.Internal.Unicode.*`
   modules.
6. Check and update the output of the tests `base/tests/unicodeXXX.hs`.
7. Compare with Python (see hereinafter) and fix any error.
   __Note:__ you need a Python version with the same Unicode version.
8. Mention the update in `base` changelog and in the User’s Guide.

# Comparing with Python

In order to check Unicode implementation in GHC, we compare the results obtained
with Python.

__Warning:__ A Python version with the _exact same Unicode version_ is required.

## GHC

Check the properties of all the characters.

```bash
ghc -O2 tests/export_all_chars.hs
./tests/export_all_chars > tests/all_chars.csv
python3 tests/check_all_chars.py tests/all_chars.csv
```

## GHC tests data

Check the Unicode test data (`unicodeNNN.stdout`).

```bash
python3 tests/check_test_data.py
```
