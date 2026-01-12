# Extract multiple boundaries from binary mask

Low-level function that extracts raw boundary coordinates for all
objects in a mask using contourLines(). Returns list of coordinate
matrices without additional processing (no reordering, no direction
checking).

## Usage

``` r
trace_boundary_multi(mask)
```

## Arguments

- mask:

  Logical matrix. TRUE = object, FALSE = background

## Value

List of matrices, each (n × 2) with raw x,y coordinates

## Examples

``` r
if (FALSE) { # \dontrun{
mask <- img_mask(img_load("petri_dish.png"))
boundaries <- trace_boundary_multi(mask)
} # }
```
