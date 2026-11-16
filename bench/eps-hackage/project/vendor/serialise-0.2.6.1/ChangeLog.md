# Revision history for serialise

## 0.2.6.1  -- 2023-11-13

* Support GHC 9.8

## 0.2.6.0  -- 2022-09-24

* Support GHC 9.4

* Drop GHC 8.0 and 8.2 support

## 0.2.4.0  -- UNRELEASED

* Add instances for Data.Void, strict and these.

## 0.2.3.0  -- 2020-05-10

* Bounds bumps and GHC 8.10 compatibility

## 0.2.2.0  -- 2019-12-29

* Export `encodeContainerSkel`, `encodeMapSkel` and `decodeMapSkel` from
  `Codec.Serialise.Class`

* Fix `Serialise` instances for `TypeRep` and `SomeTypeRep` (#216)

* Bounds bumps and GHC 8.8 compatibility

## 0.2.1.0  -- 2018-10-11

* Bounds bumps and GHC 8.6 compatibility

## 0.2.0.0  -- 2017-11-30

* Improved robustness in presence of invalid UTF-8 strings

* Add encoders and decoders for `ByteArray`

* Export `GSerialiseProd(..) and GSerialiseSum(..)`


## 0.1.0.0  -- 2017-06-28

* First version. Released on an unsuspecting world.
