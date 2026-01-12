# Plot image matrix

Plots grayscale or color images with proper axes. Uses base graphics
with useRaster=TRUE for speed. Images are displayed in their natural
orientation (as they appear in image viewers).

## Usage

``` r
img_plot(img, col = gray.colors(256, start = 0, end = 1), main = NULL)
```

## Arguments

- img:

  Matrix or array. Either:

  - Integer matrix (0-255) for grayscale from img_load()

  - Logical matrix for binary masks

  - Array (nrow, ncol, 3) for RGB color images

- col:

  Color palette for grayscale. Default gray scale. Use c("white",
  "black") for masks

- main:

  Character. Plot title. Default NULL

## Value

Invisibly returns the image

## Examples

``` r
if (FALSE) { # \dontrun{
# Plot grayscale image
img <- img_load("leaf.png")
img_plot(img)

# Plot binary mask
mask <- img_mask(img)
img_plot(mask, col = c("white", "black"), main = "Binary mask")
} # }
```
