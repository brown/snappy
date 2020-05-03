# Snappy

A Common Lisp implementation of Google's Snappy data compression library.  The
original name for Google's Snappy compression library was Zippy.

## The Snappy API

#### compress buffer index limit

```
Compresses the contents of BUFFER, a vector of (UNSIGNED-BYTE 8), from position
INDEX to position LIMIT.  Returns two values, a vector of type (UNSIGNED-BYTE 8)
holding the compressed data and an integer indicating the number of
compressed octets in the vector.
```

#### maximum-compressed-length uncompressed-length

```
Returns the maximum size a vector of length UNCOMPRESSED-LENGTH may take up
after it is compressed.
```

#### uncompress buffer index limit

```
Uncompresses BUFFER, a vector of (UNSIGNED-BYTE 8), from position INDEX to
LIMIT.  Returns the uncompressed data as a vector of (UNSIGNED-BYTE 8).
```

#### uncompressed-length buffer index limit

```
Returns the uncompressed length of the compressed data stored in BUFFER from
position INDEX to LIMIT.
```

For more information, see the documentation strings in
[snappy.lisp](https://github.com/brown/snappy/blob/master/snappy.lisp), the
example code in
[snappy-test.lisp](https://github.com/brown/snappy/blob/master/snappy-test.lisp),
and [Google's GitHub Snappy page](https://github.com/google/snappy).
