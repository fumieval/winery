# winery

winery is a serialisation library for Haskell. It tries to achieve two
goals: compact representation and perpetual inspectability.


The `binary` library provides a compact representation, but there is no way to
inspect the serialised value without the original instance.

There's `serialise`, which is an alternative library based on CBOR. Every value has to be accompanied with tags, so it tends to be redundant for arrays of small values. Encoding records with field names is also redudant.

## Interface

The interface is simple; `serialise` encodes a value with its schema, and
`deserialise` decodes a ByteString using the schema in it.

```haskell
class Serialise a

serialise :: Serialise a => a -> B.ByteString
deserialise :: Serialise a => B.ByteString -> Either String a
```

It's also possible to (de)serialise schemata and data separately.

```haskell
schema :: Serialise a => proxy a -> Schema
serialiseOnly :: Serialise a => a -> B.ByteString
deserialiseWithSchema :: Serialise a => Schema -> B.ByteString -> Either String a
```

For user-defined datatypes, you can either define instances

```haskell
instance Serialise Foo where
  schemaVia = gschemaViaRecord
  toEncoding = gtoEncodingRecord
  deserialiser = gdeserialiserRecord Nothing
```

for single-constructor records, or

```haskell
instance Serialise Foo where
  schemaVia = gschemaViaVariant
  toEncoding = gtoEncodingVariant
  deserialiser = gdeserialiserVariant
```

for any ADT. The former explicitly describes field names in the schema, and the
latter does constructor names.

## The schema

The definition of `Schema` is as follows:

```haskell
data Schema = SSchema !Word8
  | SUnit
  | SBool
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
