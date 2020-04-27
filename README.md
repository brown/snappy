# Snappy

A Common Lisp implementation of Google's Snappy data compression library.  The
original name for Google's Snappy compression library was Zippy.

## The Snappy API

```
compress                     Compress a vector.
maximum-compressed-length    The maximum size of data after compression.
uncompress                   Uncompress a vector.
uncompressed-length          Size of compressed data after decompression.
```

For more information, see the documentation strings in
[snappy.lisp](https://github.com/brown/snappy/blob/master/snappy.lisp), the
example code in
[snappy-test.lisp](https://github.com/brown/snappy/blob/master/snappy-test.lisp),
and [Google's GitHub Snappy page](https://github.com/google/snappy).
