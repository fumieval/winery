# winery

winery is a serialisation library for Haskell.

* __Fast encoding__: can create a bytestring or write to a handle efficiently
* __Compact representation__: uses VLQ by default. Separates schemata and contents
* __Inspectable__: data can be read without the original instance

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

The recommended way to create an instance of `Serialise` is `DerivingVia`.
For user-defined datatypes, you can derive

```haskell
  deriving Serialise via WineryRecord Foo
```

for single-constructor records, or just

```haskell
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

You can also build a custom deserialiser using combinators such as `extractField`, `extractConstructor`, etc.

```haskell
buildExtractor $ ("None", \() -> Nothing) `extractConstructor` ("Some", Just) `extractConstructor` extractVoid
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
