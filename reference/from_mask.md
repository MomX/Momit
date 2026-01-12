# Extract outline from mask image (convenience wrapper)

Loads image, converts to binary mask using flood fill, and extracts
outline. Combines img_load(), img_mask() and trace_outline().

## Usage

``` r
from_mask(path, fuzz = 20)
```

## Arguments

- path:

  Character. Path to image file

- fuzz:

  Integer. Fuzz/tolerance for flood fill (0-255). Default 20

## Value

Matrix (n × 2) with x,y coordinates. Class c("out", "coo", "matrix",
"array")

## Examples

``` r
if (FALSE) { # \dontrun{
# Simple use
outline <- from_mask("leaf.png")

# Adjust fuzz for noisy backgrounds
outline <- from_mask("leaf.png", fuzz = 30)
} # }
```
