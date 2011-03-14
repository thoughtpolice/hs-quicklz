# hs-quicklz: haskell bindings to quicklz

This library implements haskell bindings to [QuickLZ](http://quicklz.com), a fast
compression library. The interface is very simple:

    compress    :: ByteString -> ByteString
    decompress  :: ByteString -> ByteString
    decompress' :: ByteString -> ByteString -- overlapping decompression

As the name states, QuickLZ is fast - very fast at both compression and decompression.
Per its own benchmarks, at compression level 1, QuickLZ 1.5.0 has a compression speed
of 308Mbyte/s, and a decompression speed of 358Mbyte/s (their benchmarks, Core i7 920.)
This package has some benchmarks and tests associated - I would like to expand the benchmarks
to include bigger data sets in the future.

There is currently only an interface for strict bytestrings.

I would like to implement the streaming mode functionality sometime in the future, perhaps
tying it to John Millikin's [enumerator](http://hackage.haskell.org/package/enumerator) package.
