# Revision history for ghc-platform

## 0.1.0.0 -- 2023-06-20

* First version. Split off the `GHC.Platform.ArchOS` module from the `ghc-boot`
    package into this reinstallable standalone package which abides by the PVP,
    in part motivated by the ongoing work on `ghc-toolchain` towards runtime retargetability.
