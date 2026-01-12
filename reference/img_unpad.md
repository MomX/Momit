# Remove padding (border) from image

Removes a border of pixels from around an image. Inverse of img_pad().

## Usage

``` r
img_unpad(img, n = 10)
```

## Arguments

- img:

  Matrix or array. Padded image

- n:

  Integer. Number of pixels to remove from each side. Default 10

## Value

Unpadded image with dimensions decreased by 2\*n in each direction

## Examples

``` r
if (FALSE) { # \dontrun{
img <- img_load("leaf.png")
img_padded <- img_pad(img, n = 20)
img_back <- img_unpad(img_padded, n = 20)
identical(img, img_back)  # TRUE
} # }
```
