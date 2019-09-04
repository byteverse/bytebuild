# Revision history for small-bytearray-builder

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
