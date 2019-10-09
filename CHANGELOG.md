# Revision history for small-bytearray-builder

## 0.2.2.0 -- 2019-??-??

* Introduce `consLensBE32` for efficient serialization of wire protocols
  that require prefixing a payload with its length.
* Add `int64BE` as a convenience.
* Add little-endian encoding functions for `Word16`, `Word32`, and `Word64`.

## 0.2.1.0 -- 2019-09-05

* Stop exporting data constructor in `Data.ByteArray.Builder`.
  This is technically a breaking change, but it was only
  exported by accident. So, with this release, we will technically
  violate PVP, and the previous release will be deprecated on hackage.
* Add more functions for encoding unsigned words: `word16PaddedLowerHex`,
  `word16LowerHex`, `word16UpperHex`, `word8LowerHex`.
* Unroll loop for `word8Dec`.

## 0.2.0.0 -- 2019-09-04

* Use `natural-arithmetic` to make manipulation of bounds possible.
* Add more functions for encoding numbers. This includes
  `word8/16/32/64` and `int8/16/32/64`.
* Rename the modules.
* Correct a serious error in the implementation of `bytes`.
* Make `pasteGrowST` accept an initial offset.
* Add a `pasteGrowST` for length-indexed builders.
* Add function for rendering floating-point numbers in a slightly
  inaccurate way.
* Add functions for encoding `ShortText` as UTF-8 and as a JSON string.

## 0.1.1.0 -- 2019-07-30

* Add several additional functions for encoding numbers.

## 0.1.0.0 -- 2019-06-25

* First version
