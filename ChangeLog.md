# 1

* Changed the encoding more compact
* Decoders are now stateful
* Significantly improved the performance
* `decodeCurrent` is now a method of `Serialise`
* Added `STag`

# 0.3

* Supported `UTCTime`
* Added an instance for lazy `ByteString`
* Added `toEncodingWithSchema`

# 0.2

* Renamed `extract*With` to `extract*By` for consistency
* Added `hPut`
* Improved the performance
* Added `-J` option to `winery` which exports a JSON
* Decoder now throws `DecodeException` rather than error calls

# 0.1.1

* Add `Data.Winery.Query`
* The command line tool supports a simple query language

# 0.1

Overhauled the encoding; Sorry, incompatible with 0

# 0

Initial release
