# winery

![logo](https://github.com/fumieval/winery/blob/master/art/logo256px.png?raw=true)
[![Build Status](https://travis-ci.org/fumieval/winery.svg?branch=master)](https://travis-ci.org/fumieval/winery)
[![Hackage](https://img.shields.io/hackage/v/winery.svg)](https://hackage.haskell.org/package/winery)
[![Discord](https://img.shields.io/discord/664807830116892674?color=%237095ec&label=Discord&style=plastic)](https://discord.gg/DG93Tgs)

winery is a serialisation library focusing on __performance__, __compactness__
and __compatibility__. The primary feature is that metadata (types, field names,
etc) are packed into one schema.

A number of formats, like JSON and CBOR, attach metadata for each value:

`[{"id": 0, "name": "Alice"}, {"id": 1, "name": "Bob"}]`

In contrast, winery stores them separately, eliminating redundancy while
guaranteeing well-typedness:

```
0402 0402 0269 6410 046e 616d 6514  [{ id :: Integer, name :: Text }]
0200 0541 6c69 6365 0103 426f 62    [(0, "Alice"), (1, "Bob")]
```

Unlike other libraries that don't preserve metadata (e.g.` binary`, `cereal`, `store`) at all, winery also
allows readers to decode values regardless of the current implementation.

## Interface

The interface is simple; `serialise` encodes a value with its schema, and
`deserialise` decodes a ByteString using the schema in it.

```haskell
class Serialise a where
  schema :: Serialise a => proxy a -> Schema

serialise :: Serialise a => a -> B.ByteString
deserialise :: Serialise a => B.ByteString -> Either WineryException a
```

It's also possible to serialise schemata and data separately. `serialiseSchema`
encodes a schema and its version number into a ByteString, and
`serialiseOnly` serialises a value without a schema.

```haskell
serialiseSchema :: Schema -> B.ByteString
serialiseOnly :: Serialise a => a -> B.ByteString
```

In order to decode data generated this way, pass the result of `deserialiseSchema`
to `getDecoder`. Finally run `evalDecoder` to deserialise them.

```haskell
deserialiseSchema :: B.ByteString -> Either WineryException Schema
getDecoder :: Serialise a => Schema -> Either WineryException (Decoder a)
evalDecoder :: Decoder a -> B.ByteString -> a
```

## Deriving an instance

The recommended way to create an instance of `Serialise` is to use `DerivingVia`.

```haskell
  deriving Generic
  deriving Serialise via WineryRecord Foo
```

for single-constructor records, or just

```haskell
  deriving Generic
  deriving Serialise via WineryVariant Foo
```

for any ADT. The former explicitly describes field names in the schema, and the
latter does constructor names.

## Backward compatibility

If the representation is not the same as the current version (i.e. the schema
 is different), the data cannot be decoded directly. This is where extractors
come in.

`Extractor` parses a schema and returns a function which gives a value back from
a `Term`.

If having default values for missing fields is sufficient, you can pass a
default value to `gextractorRecord`:

```haskell
  extractor = gextractorRecord $ Just $ Foo "" 42 0
```

You can also build an extractor using combinators such as `extractField`, `extractConstructor`, etc.

```haskell
buildExtractor
  $ ("None", \() -> Nothing)
  `extractConstructor` ("Some", Just)
  `extractConstructor` extractVoid
  :: Extractor (Maybe a)
```

`Extractor` is Alternative, meaning that multiple extractors (such as a default
generic implementation and fallback plans) can be combined into one.

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

## Performance

A useful library should also be fast. Benchmarking encoding/decoding of the
following datatype.

```haskell
data Gender = Male | Female

data TestRec = TestRec
  { id_ :: !Int
  , first_name :: !Text
  , last_name :: !Text
  , email :: !Text
  , gender :: !Gender
  , num :: !Int
  , latitude :: !Double
  , longitude :: !Double
  }
```

Here's the result:

|           | encode 1 | encode 1000 | decode  | length  |
|-----------|----------|-------------|---------| ------- |
| winery    | __0.28 μs__  | __0.26 ms__ | __0.81 ms__ | __58662__ |
| cereal    | 0.82 μs  | 0.78 ms     | 0.90 ms | 91709  |
| binary    | 1.7 μs   | 1.7 ms      | 2.0 ms  | 125709 |
| serialise | 0.61 μs  | 0.50 ms     | 1.4 ms  | 65437  |
| store     | 54 ns    | 56 μs       | 0.13 ms | 126410 |
| aeson     | 9.9 μs   | 9.7 ms      | 17 ms   | 160558 |
