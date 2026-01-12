# Add padding (border) to image

Adds a border of pixels around an image. Useful for ensuring objects
don't touch image edges before flood fill.

## Usage

``` r
img_pad(img, n = 10, value = NULL)
```

## Arguments

- img:

  Matrix or array. Image from img_load() or binary mask

- n:

  Integer. Number of pixels to add on each side. Default 10

- value:

  Numeric or logical. Value for padding pixels.

  - For grayscale (0-255): Default 255 (white)

  - For masks: Default FALSE (background)

  - For color: Specify as c(R, G, B) or single value (default 255)

## Value

Padded image with dimensions increased by 2\*n in each direction

## Examples

``` r
if (FALSE) { # \dontrun{
img <- img_load("leaf.png")
img_padded <- img_pad(img, n = 20)
dim(img)        # e.g., 100 x 100
dim(img_padded) # 140 x 140

# Pad a mask
mask <- img_mask(img)
mask_padded <- img_pad(mask, n = 10, value = FALSE)
} # }
```
