# Extract multiple outlines from mask image

Extracts outlines from a mask containing multiple objects using base R's
contourLines(). Each connected object gets its own outline. No external
dependencies required.

## Usage

``` r
from_mask_multi(path, fuzz = 20, min_size = 10)
```

## Arguments

- path:

  Character. Path to image file, or mask matrix

- fuzz:

  Integer. Fuzz/tolerance for flood fill (0-255). Default 20 Only used
  if path is character

- min_size:

  Integer. Minimum number of points for an outline. Default 10 Filters
  out tiny spurious contours

## Value

List of outline matrices, each (n × 2) with class c("out", "coo",
"matrix", "array")

## Examples

``` r
if (FALSE) { # \dontrun{
# Extract all objects
outlines <- from_mask_multi("petri_dish.png")
length(outlines)  # Number of objects found

# Plot them
plot(outlines[[1]], type = "l", asp = 1)
for (i in 2:length(outlines)) {
  lines(outlines[[i]], col = i)
}
} # }
```
