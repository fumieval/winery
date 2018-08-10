# winery

winery is a serialisation library for Haskell.

* __Fast encoding__: can create a bytestring or write to a handle efficiently
* __Compact representation__: uses VLQ by default. Separates schemata and contents
* __Stateless decoding__: you can decode a value without reading all the leading bytes
* __Inspectable__: data can be read without the original instance

## Interface

The interface is simple; `serialise` encodes a value with its schema, and
`deserialise` decodes a ByteString using the schema in it.

```haskell
class Serialise a

serialise :: Serialise a => a -> B.ByteString
deserialise :: Serialise a => B.ByteString -> Either String a
```

It's also possible to serialise schemata and data separately.

```haskell
-- Note that 'Schema' is an instance of 'Serialise'
schema :: Serialise a => proxy a -> Schema
serialiseOnly :: Serialise a => a -> B.ByteString
```

`getDecoder` gives you a deserialiser.

```haskell
getDecoder :: Serialise a => Schema -> Either StrategyError (ByteString -> a)
```

For user-defined datatypes, you can either define instances

```haskell
instance Serialise Foo where
  schemaVia = gschemaViaRecord
  toEncoding = gtoEncodingRecord
  deserialiser = gdeserialiserRecord Nothing
```

for single-constructor records, or just

```haskell
instance Serialise Foo
```

for any ADT. The former explicitly describes field names in the schema, and the
latter does constructor names.

## Streaming output

You can write data to a handle without allocating a ByteString. You can see the
length before serialisation.

```haskell
toEncoding :: Serialise a => a -> Encoding
hPutEncoding :: Handle -> Encoding -> IO ()
getSize :: Encoding -> Int
```

## The schema

The definition of `Schema` is as follows:

```haskell
data Schema = SSchema !Word8
  | SUnit
  | SBool
  | SChar
  | SWord8
  | SWord16
  | SWord32
  | SWord64
  | SInt8
  | SInt16
  | SInt32
  | SInt64
  | SInteger
  | SFloat
  | SDouble
  | SBytes
  | SText
  | SList !Schema
  | SArray !(VarInt Int) !Schema -- fixed size
  | SProduct [Schema]
  | SProductFixed [(VarInt Int, Schema)] -- fixed size
  | SRecord [(T.Text, Schema)]
  | SVariant [(T.Text, [Schema])]
  | SFix Schema -- ^ binds a fixpoint
  | SSelf !Word8 -- ^ @SSelf n@ refers to the n-th innermost fixpoint
  deriving (Show, Read, Eq, Generic)
```

The `Serialise` instance is derived by generics.

There are some special schemata:

* `SSchema n` is a schema of schema. The winery library stores the concrete schema of `Schema` for each version, so it can deserialise data even if the schema changes.
* `SFix` binds a fixpoint.
* `SSelf n` refers to the n-th innermost fixpoint bound by `SFix`. This allows it to provide schemata for inductive datatypes.

## Backward compatibility

If having default values for missing fields is sufficient, you can pass a
default value to `gdeserialiserRecord`:

```haskell
  deserialiser = gdeserialiserRecord $ Just $ Foo "" 42 0
```

You can also build a custom deserialiser using the Alternative instance and combinators such as `extractField`, `extractConstructor`, etc.

## Pretty-printing

`Term` can be deserialised from any winery data. It can be pretty-printed using the `Pretty` instance:

```
{ bar: "hello"
, baz: 3.141592653589793
, foo: Just 42
}
```

You can use the `winery` command-line tool to inspect values.

```
$ winery '.[:10] | .first_name .last_name' benchmarks/data.winery
Shane Plett
Mata Snead
Levon Sammes
Irina Gourlay
Brooks Titlow
Antons Culleton
Regine Emerton
Starlin Laying
Orv Kempshall
Elizabeth Joseff
Cathee Eberz
```

At the moment, the following queries are supported:

* `.` return itself
* `.[]` enumerate all the elements in a list
* `.[i]` get the i-th element
* `.[i:j]` enumerate i-th to j-th items
* `.foo` Get a field named `foo`
* `F | G` compose queries (left to right)

## Benchmark

```haskell
data TestRec = TestRec
  { id_ :: !Int
  , first_name :: !Text
  , last_name :: !Text
  , email :: !Text
  , gender :: !Gender
  , num :: !Int
  , latitude :: !Double
  , longitude :: !Double
  } deriving (Show, Generic)
```

(De)serialisation of the datatype above using generic instances:

```
serialise/list/winery                    mean 847.4 μs  ( +- 122.7 μs  )
serialise/list/binary                    mean 1.221 ms  ( +- 169.0 μs  )
serialise/list/serialise                 mean 290.4 μs  ( +- 34.98 μs  )
serialise/item/winery                    mean 243.1 ns  ( +- 27.50 ns  )
serialise/item/binary                    mean 1.080 μs  ( +- 75.82 ns  )
serialise/item/serialise                 mean 322.4 ns  ( +- 21.09 ns  )
serialise/file/winery                    mean 681.9 μs  ( +- 247.0 μs  )
serialise/file/binary                    mean 1.731 ms  ( +- 611.6 μs  )
serialise/file/serialise                 mean 652.9 μs  ( +- 185.8 μs  )
deserialise/winery                       mean 733.2 μs  ( +- 11.70 μs  )
deserialise/binary                       mean 1.582 ms  ( +- 122.3 μs  )
deserialise/serialise                    mean 823.3 μs  ( +- 38.08 μs  )

```

Not bad, considering that binary and serialise don't encode field names.
