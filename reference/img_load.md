# Load image as grayscale matrix

Reads any image format and returns a grayscale matrix of integers
(0-255). Useful for custom image processing before outline extraction.

## Usage

``` r
img_load(path, color = FALSE, flip = FALSE)
```

## Arguments

- path:

  Character. Path to image file

- color:

  Logical. Keep as RGB color? Default FALSE (convert to grayscale)

- flip:

  Logical. Flip vertically? (Images are top-down, coords bottom-up).
  Default TRUE

## Value

If color=FALSE: Integer matrix (nrow = height, ncol = width) with values
0-255 If color=TRUE: Integer array (nrow = height, ncol = width, 3) with
RGB values 0-255

## Examples

``` r
if (FALSE) { # \dontrun{
# Load as grayscale
img <- img_load("leaf.jpg")
dim(img)  # height x width

# Load as color
img_rgb <- img_load("leaf.jpg", color = TRUE)
dim(img_rgb)  # height x width x 3

# For most uses, just call from_mask() directly
outline <- from_mask("leaf.jpg")
} # }
```
