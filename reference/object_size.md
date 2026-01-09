# Calculate object size in memory and on disk

Returns size of R objects in memory and estimated/actual size on disk.
For objects: estimates .rda size by default, .json if object is JSON
string. For files: returns actual file size.

## Usage

``` r
object_size(x, units = "auto", compression = "gzip")
```

## Arguments

- x:

  An R object, JSON string, or path to a file

- units:

  Character, one of "auto", "B", "KB", "MB", "GB". Default "auto"

- compression:

  Compression for .rda files. Default "gzip"

## Value

A list with class "object_size" containing memory, disk, ratio, format
