# binary-tagged

[![Build Status](https://travis-ci.org/phadej/binary-tagged.svg?branch=master)](https://travis-ci.org/phadej/binary-tagged)
[![Hackage](https://img.shields.io/hackage/v/binary-tagged.svg)](http://hackage.haskell.org/package/binary-tagged)

Structurally tag binary serialisation stream.

Say you have:

```hs
data Record = Record
  { _recordFields :: HM.HashMap Text (Integer, ByteString)
  , _recordEnabled :: Bool
  }
  deriving (Eq, Show, Generic)

instance Binary Record
instance HasStructuralInfo Record
instance HasSemanticVersion Record
```

then you can serialise and deserialise `Record` values with a structure tag by simply

```hs
encodeTaggedFile "cachefile" record
decodeTaggedFile "cachefile" :: IO Record
```

If structure of `Record` changes in between, deserialisation will fail early.

The overhead is next to non-observable, see [a simple benchmark](https://github.com/phadej/binary-tagged/blob/master/bench/Bench.hs) and the [results](https://rawgit.com/phadej/binary-tagged/master/bench.html).
