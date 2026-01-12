# Extract outline from binary mask

Traces the boundary of an object in a binary mask using Moore-Neighbor
chain code algorithm. Outline starts at rightmost point and traces
counter-clockwise (positive/mathematical direction).

## Usage

``` r
trace_outline(mask)
```

## Arguments

- mask:

  Logical matrix. TRUE = object, FALSE = background

## Value

Matrix (n × 2) with x,y coordinates. Class c("out", "coo", "matrix",
"array")

## Examples

``` r
if (FALSE) { # \dontrun{
# From mask matrix
mask <- img_mask("leaf.png")
outline <- trace_outline(mask)
} # }
```
