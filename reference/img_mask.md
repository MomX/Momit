# Convert image to binary mask using flood fill

Creates a binary mask by flood-filling the background from top-left
corner, then setting all non-background pixels to black (TRUE). This
handles complex backgrounds and multiple objects uniformly.

## Usage

``` r
img_mask(img, fuzz = 20, invert = TRUE)
```

## Arguments

- img:

  Integer matrix (0-255) from img_load()

- fuzz:

  Integer. Fuzz/tolerance for flood fill (0-255). Default 20 Controls
  how similar pixels must be to be considered background.

  - Low (5-10): Very strict, only very similar pixels

  - Medium (20-30): Reasonable for most cases

  - High (50+): Aggressive, may include object edges

- invert:

  Logical. Invert the mask? Default TRUE

  - TRUE: object = FALSE (white), background = TRUE (black)

  - FALSE: object = TRUE (black), background = FALSE (white)

## Value

Logical matrix (TRUE/FALSE depending on invert setting)

## Examples

``` r
if (FALSE) { # \dontrun{
# Standard (inverted): white object on black background
img <- img_load("leaf.png")
mask <- img_mask(img)

# Non-inverted: black object on white background
mask_std <- img_mask(img, invert = FALSE)

# Adjust fuzz for noisy backgrounds
mask <- img_mask(img, fuzz = 30)
} # }
```
