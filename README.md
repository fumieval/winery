# winery

winery is a serialisation library for Haskell.

* __Fast encoding__: can create a bytestring or write to a handle efficiently
* __Compact representation__: uses VLQ by default. Separates schemata and contents
* __Inspectable__: data can be read without the original instance

## Interface

The interface is simple; `serialise` encodes a value with its schema, and
`deserialise` decodes a ByteString using the schema in it.

```haskell
class Serialise a

serialise :: Serialise a => a -> B.ByteString
deserialise :: Serialise a => B.ByteString -> Either WineryException a
```

It's also possible to serialise schemata and data separately.

```haskell
-- Note that 'Schema' is an instance of 'Serialise'
schema :: Serialise a => proxy a -> Schema
serialiseOnly :: Serialise a => a -> B.ByteString
```

`getDecoder` gives you a deserialiser.

```haskell
getDecoder :: Serialise a => Schema -> Either WineryException (ByteString -> a)
```

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

## The schema

The definition of `Schema` is as follows:

```haskell
type Schema = SchemaP Int

data SchemaP a = SFix !(SchemaP a)
  | SVar !a
  | SVector !(SchemaP a)
  | SProduct !(V.Vector (SchemaP a))
  | SRecord !(V.Vector (T.Text, SchemaP a))
  | SVariant !(V.Vector (T.Text, SchemaP a))
  | SSchema !Word8
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
  | SUTCTime -- ^ nanoseconds from POSIX epoch
  | STag !Tag !(SchemaP a)
  | SLet !(SchemaP a) !(SchemaP a)
```

The `Serialise` instance is derived by generics.

There are some special schemata:

* `SSchema n` is a schema of schema. The winery library stores the concrete schema of `Schema` for each version, so it can deserialise data even if the schema changes.
* `SFix` binds a fixpoint.
* `SSelf n` refers to the n-th innermost fixpoint bound by `SFix`. This allows it to provide schemata for inductive datatypes.
* `STag` attaches a tag to a schema which can aid backward compatibility and/or inspectability.

```haskell
data Tag = TagInt !Int
  | TagStr !T.Text
  | TagList ![Tag]
```

## Backward compatibility

If having default values for missing fields is sufficient, you can pass a
default value to `gextractorRecord`:

```haskell
  extractor = gextractorRecord $ Just $ Foo "" 42 0
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
