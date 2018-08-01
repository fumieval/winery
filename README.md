# winery

winery is a serialisation library for Haskell. It tries to achieve two
goals: compact representation and perpetual inspectability.

The standard `binary` library has no way to inspect the serialised value without the original instance.

There's `serialise`, which is an alternative library based on CBOR. Every value has to be accompanied with tags, so it tends to be redundant for arrays of small values. Encoding records with field names is also redudant.

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
serialise/list/winery                    mean 830.4 μs  ( +- 126.1 μs  )
serialise/list/binary                    mean 1.268 ms  ( +- 126.3 μs  )
serialise/list/serialise                 mean 309.5 μs  ( +- 22.33 μs  )
serialise/item/winery                    mean 248.9 ns  ( +- 22.84 ns  )
serialise/item/binary                    mean 1.222 μs  ( +- 77.28 ns  )
serialise/item/serialise                 mean 384.6 ns  ( +- 15.63 ns  )
deserialise/winery                       mean 972.5 μs  ( +- 150.6 μs  )
deserialise/binary                       mean 1.721 ms  ( +- 99.67 μs  )
deserialise/serialise                    mean 957.3 μs  ( +- 80.95 μs  )
```

Not bad, considering that binary and serialise don't encode field names.
