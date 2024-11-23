## 1.5

* Support GHC >= 9.6
* Use `buildVariantExtractor` in `Codec.Winery.Query.con`
* Require mtl >= 2.3
* Added an instance for `Solo`
* Added `deserialiseOnly`
* `extractConstructor`, `extractConstructorBy`, and `extractVoid` are now deprecated

## 1.4

* Added an instance for `NonEmpty`
* Removed the instance for `Option`
* Added `withSchema`
* Added `alterSchemaGen` and `alterExtractor`
* Removed long-deprecated `unwrapExtractor`, `bundleRecord`, `bundleRecordDefault`, `bundleVariant`

## 1.3.2

* Inlined methods of various `Serialise` instances
* Added `buildRecordExtractor` and `bextractors`
* Added a `Serialise` instance for `Barbie`

## 1.3.1

* Added `bundleVia`, deprecating `bundleRecord` and `bundleVariant`

## 1.3

* Fixed an incorrect behaviour of `extractConstructor`
* `WineryException` now contains a hierarchy of `TypeRep`s
* Added `gvariantExtractors` and `buildVariantExtractor`
* Added `SingleField`

## 1.2

* Removed `Plan` and `mkPlan`
* Added `mkExtractor`
* `unwrapExtractor` is deprecated
* Removed `Data.Winery`

## 1.1.3

* Added `schemaToBuilder`
* Added `TestGen` and `Tested` instances for `Maybe a`

## 1.1.2

* Added `encodeTerm`
* Split the `Codec.Winery` module

## 1.1.1

* Changed the internal representation of `Decoder`

## 1.1

* Renamed `Data.Winery` to `Codec.Winery`

## 1.0.2

* `bootstrapSchema` returns `UnsupportedSchemaVersion` rather than an error

## 1.0.1

* Added `bundleSerialise`

## 1

* Changed the encoding more compact
* Decoders are now stateful
* Significantly improved the performance
* `decodeCurrent` is now a method of `Serialise`
* Added `STag`

## 0.3

* Supported `UTCTime`
* Added an instance for lazy `ByteString`
* Added `toEncodingWithSchema`

## 0.2

* Renamed `extract*With` to `extract*By` for consistency
* Added `hPut`
* Improved the performance
* Added `-J` option to `winery` which exports a JSON
* Decoder now throws `DecodeException` rather than error calls

## 0.1.1

* Add `Data.Winery.Query`
* The command line tool supports a simple query language

## 0.1

Overhauled the encoding; Sorry, incompatible with 0

## 0

Initial release
