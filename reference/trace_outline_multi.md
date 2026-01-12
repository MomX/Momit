# Extract multiple outlines from binary mask

Traces boundaries of all objects in a binary mask using base R's
contourLines(). Each connected object gets its own outline.

## Usage

``` r
trace_outline_multi(mask, min_size = 10)
```

## Arguments

- mask:

  Logical matrix. TRUE = object, FALSE = background

- min_size:

  Integer. Minimum number of points for an outline. Default 10

## Value

List of outline matrices, each (n × 2) with class c("out", "coo",
"matrix", "array")

## Examples

``` r
if (FALSE) { # \dontrun{
# From mask matrix
img <- img_load("petri_dish.png")
mask <- img_mask(img)
outlines <- trace_outline_multi(mask)
} # }
```
