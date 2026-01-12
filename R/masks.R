#' @importFrom grDevices as.raster gray.colors contourLines
#' @importFrom graphics axis box image lines par plot rasterImage rect segments text
#' @importFrom magick image_read image_convert image_data image_fill
NULL

# Image loading and preprocessing ----------------------------------------

#' Load image as grayscale matrix
#'
#' Reads any image format and returns a grayscale matrix of integers (0-255).
#' Useful for custom image processing before outline extraction.
#'
#' @param path Character. Path to image file
#' @param color Logical. Keep as RGB color? Default FALSE (convert to grayscale)
#' @param flip Logical. Flip vertically? (Images are top-down, coords bottom-up). Default TRUE
#'
#' @return If color=FALSE: Integer matrix (nrow = height, ncol = width) with values 0-255
#'         If color=TRUE: Integer array (nrow = height, ncol = width, 3) with RGB values 0-255
#' @export
#'
#' @examples
#' \dontrun{
#' # Load as grayscale
#' img <- img_load("leaf.jpg")
#' dim(img)  # height x width
#'
#' # Load as color
#' img_rgb <- img_load("leaf.jpg", color = TRUE)
#' dim(img_rgb)  # height x width x 3
#'
#' # For most uses, just call from_mask() directly
#' outline <- from_mask("leaf.jpg")
#' }
img_load <- function(path, color = FALSE, flip = FALSE) {
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Package 'magick' required. Install with: install.packages('magick')")
  }

  if (!file.exists(path)) {
    stop("File not found: ", path)
  }

  # Load image
  img <- magick::image_read(path)

  if (color) {
    # Keep as RGB
    arr <- magick::image_data(img, channels = "rgb")

    # arr is (3, width, height) in raw format - need to rearrange to (height, width, 3)
    # Convert each channel separately
    img_array <- array(0L, dim = c(dim(arr)[3], dim(arr)[2], 3))

    # Extract each channel, convert to integer, transpose
    r_channel <- arr[1, , ]
    storage.mode(r_channel) <- "integer"
    img_array[, , 1] <- t(r_channel)

    g_channel <- arr[2, , ]
    storage.mode(g_channel) <- "integer"
    img_array[, , 2] <- t(g_channel)

    b_channel <- arr[3, , ]
    storage.mode(b_channel) <- "integer"
    img_array[, , 3] <- t(b_channel)

    # Flip vertically if requested
    if (flip) {
      img_array <- img_array[nrow(img_array):1, , ]
    }

    return(img_array)

  } else {
    # Convert to grayscale
    img <- magick::image_convert(img, type = "Grayscale")

    # Extract as array and drop channel dimension
    arr <- magick::image_data(img, channels = "gray")[1, , ]

    # Transpose to get (height, width)
    grey <- t(arr)
    storage.mode(grey) <- "integer"

    # Flip vertically if requested
    if (flip) {
      grey <- grey[nrow(grey):1, ]
    }

    return(grey)
  }
}

#' Convert image to binary mask using flood fill
#'
#' Creates a binary mask by flood-filling the background from top-left corner,
#' then setting all non-background pixels to black (TRUE). This handles complex
#' backgrounds and multiple objects uniformly.
#'
#' @param img Integer matrix (0-255) from img_load()
#' @param fuzz Integer. Fuzz/tolerance for flood fill (0-255). Default 20
#'   Controls how similar pixels must be to be considered background.
#'   - Low (5-10): Very strict, only very similar pixels
#'   - Medium (20-30): Reasonable for most cases
#'   - High (50+): Aggressive, may include object edges
#' @param invert Logical. Invert the mask? Default TRUE
#'   - TRUE: object = FALSE (white), background = TRUE (black)
#'   - FALSE: object = TRUE (black), background = FALSE (white)
#'
#' @return Logical matrix (TRUE/FALSE depending on invert setting)
#' @export
#'
#' @examples
#' \dontrun{
#' # Standard (inverted): white object on black background
#' img <- img_load("leaf.png")
#' mask <- img_mask(img)
#'
#' # Non-inverted: black object on white background
#' mask_std <- img_mask(img, invert = FALSE)
#'
#' # Adjust fuzz for noisy backgrounds
#' mask <- img_mask(img, fuzz = 30)
#' }
img_mask <- function(img, fuzz = 20, invert = TRUE) {

  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Package 'magick' required. Install with: install.packages('magick')")
  }

  nr <- nrow(img)
  nc <- ncol(img)

  # Convert matrix back to magick image (need to un-flip first)
  img_to_fill <- img[nr:1, ]

  # Convert to magick raster format (0-1 range)
  img_raster <- as.raster(img_to_fill / 255)
  img_magick <- magick::image_read(img_raster)

  # Flood fill from top-left corner
  # Fill background with a marker color (red in this case)
  img_filled <- img_magick %>%
    magick::image_fill(color = "red", point = "+1+1", fuzz = fuzz * (100/255))

  # Convert back to grayscale to see what was filled
  arr <- magick::image_data(img_filled, channels = "rgb")

  # Check which pixels are red (background) vs not (object)
  # Red channel will be high (255) for filled areas
  red_channel <- t(arr[1, , ])
  storage.mode(red_channel) <- "integer"

  # Flip back to match original orientation
  red_channel <- red_channel[nrow(red_channel):1, ]

  # Create mask: red (background) = FALSE, non-red (object) = TRUE
  mask <- red_channel < 200  # Anything not red becomes TRUE (object)

  # Invert if requested
  if (invert) {
    mask <- !mask
  }

  mask
}

#' Plot image matrix
#'
#' Plots grayscale or color images with proper axes.
#' Uses base graphics with useRaster=TRUE for speed. Images are displayed in
#' their natural orientation (as they appear in image viewers).
#'
#' @param img Matrix or array. Either:
#'   - Integer matrix (0-255) for grayscale from img_load()
#'   - Logical matrix for binary masks
#'   - Array (nrow, ncol, 3) for RGB color images
#' @param col Color palette for grayscale. Default gray scale. Use c("white", "black") for masks
#' @param main Character. Plot title. Default NULL
#'
#' @return Invisibly returns the image
#' @export
#'
#' @examples
#' \dontrun{
#' # Plot grayscale image
#' img <- img_load("leaf.png")
#' img_plot(img)
#'
#' # Plot binary mask
#' mask <- img_mask(img)
#' img_plot(mask, col = c("white", "black"), main = "Binary mask")
#' }
img_plot <- function(img,
                     col = gray.colors(256, start = 0, end = 1),
                     main = NULL) {

  # Check if color image (3D array)
  is_color <- length(dim(img)) == 3 && dim(img)[3] == 3

  if (is_color) {
    # Color image
    nr <- dim(img)[1]
    nc <- dim(img)[2]

    # Flip vertically to display naturally (row 1 at top)
    img_display <- img[nr:1, , ]

    # Set up plotting area
    par(mar = c(3, 3, 2, 1))

    # Define axis tick positions
    x_ticks <- c(1, round(nc/2), nc)
    y_ticks <- c(1, round(nr/2), nr)

    # Convert to raster (0-1 range)
    img_raster <- as.raster(img_display / 255)

    # Plot
    plot(1, type = "n", xlim = c(1, nc), ylim = c(1, nr),
         xlab = "", ylab = "", main = main, axes = FALSE, asp = 1)
    rasterImage(img_raster, 1, 1, nc, nr)

    # Add axes
    axis(1, at = x_ticks, labels = x_ticks)
    axis(2, at = y_ticks, labels = y_ticks, las = 1)
    box()

  } else {
    # Grayscale or binary image
    nr <- nrow(img)
    nc <- ncol(img)

    # Convert logical to numeric if needed
    if (is.logical(img)) {
      img_numeric <- img * 1
    } else {
      img_numeric <- img
    }

    # Flip vertically to display naturally (row 1 at top)
    img_display <- img_numeric[nr:1, ]

    # Set up plotting area
    par(mar = c(3, 3, 2, 1))

    # Define axis tick positions
    x_ticks <- c(1, round(nc/2), nc)
    y_ticks <- c(1, round(nr/2), nr)

    # Plot image
    image(1:nc, 1:nr, t(img_display),
          col = col,
          useRaster = TRUE,
          axes = FALSE,
          xlab = "",
          ylab = "",
          main = main,
          asp = 1)

    # Add axes
    axis(1, at = x_ticks, labels = x_ticks)
    axis(2, at = y_ticks, labels = y_ticks, las = 1)
    box()
  }

  invisible(img)
}


#' Add padding (border) to image
#'
#' Adds a border of pixels around an image. Useful for ensuring objects
#' don't touch image edges before flood fill.
#'
#' @param img Matrix or array. Image from img_load() or binary mask
#' @param n Integer. Number of pixels to add on each side. Default 10
#' @param value Numeric or logical. Value for padding pixels.
#'   - For grayscale (0-255): Default 255 (white)
#'   - For masks: Default FALSE (background)
#'   - For color: Specify as c(R, G, B) or single value (default 255)
#'
#' @return Padded image with dimensions increased by 2*n in each direction
#' @export
#'
#' @examples
#' \dontrun{
#' img <- img_load("leaf.png")
#' img_padded <- img_pad(img, n = 20)
#' dim(img)        # e.g., 100 x 100
#' dim(img_padded) # 140 x 140
#'
#' # Pad a mask
#' mask <- img_mask(img)
#' mask_padded <- img_pad(mask, n = 10, value = FALSE)
#' }
img_pad <- function(img, n = 10, value = NULL) {

  # Determine default value if not provided
  if (is.null(value)) {
    if (is.logical(img)) {
      value <- FALSE
    } else {
      value <- 255  # White for grayscale
    }
  }

  # Handle different image types
  if (length(dim(img)) == 3) {
    # Color image (height x width x 3)
    nr <- dim(img)[1]
    nc <- dim(img)[2]

    # Expand value to RGB if needed
    if (length(value) == 1) {
      value <- rep(value, 3)
    }

    # Create padded array
    padded <- array(0L, dim = c(nr + 2*n, nc + 2*n, 3))

    # Fill with padding value
    for (ch in 1:3) {
      padded[, , ch] <- value[ch]
    }

    # Copy original image to center
    padded[(n+1):(nr+n), (n+1):(nc+n), ] <- img

    # Preserve storage mode
    if (is.integer(img)) {
      storage.mode(padded) <- "integer"
    }

  } else {
    # Grayscale or binary (height x width)
    nr <- nrow(img)
    nc <- ncol(img)

    # Create padded matrix
    if (is.logical(img)) {
      padded <- matrix(value, nrow = nr + 2*n, ncol = nc + 2*n)
    } else {
      padded <- matrix(value, nrow = nr + 2*n, ncol = nc + 2*n)
      if (is.integer(img)) {
        storage.mode(padded) <- "integer"
      }
    }

    # Copy original image to center
    padded[(n+1):(nr+n), (n+1):(nc+n)] <- img
  }

  padded
}

#' Remove padding (border) from image
#'
#' Removes a border of pixels from around an image. Inverse of img_pad().
#'
#' @param img Matrix or array. Padded image
#' @param n Integer. Number of pixels to remove from each side. Default 10
#'
#' @return Unpadded image with dimensions decreased by 2*n in each direction
#' @export
#'
#' @examples
#' \dontrun{
#' img <- img_load("leaf.png")
#' img_padded <- img_pad(img, n = 20)
#' img_back <- img_unpad(img_padded, n = 20)
#' identical(img, img_back)  # TRUE
#' }
img_unpad <- function(img, n = 10) {

  if (length(dim(img)) == 3) {
    # Color image (height x width x 3)
    nr <- dim(img)[1]
    nc <- dim(img)[2]

    if (nr <= 2*n || nc <= 2*n) {
      stop("Padding size (", n, ") too large for image dimensions (",
           nr, " x ", nc, ")")
    }

    # Extract center portion
    unpadded <- img[(n+1):(nr-n), (n+1):(nc-n), ]

  } else {
    # Grayscale or binary (height x width)
    nr <- nrow(img)
    nc <- ncol(img)

    if (nr <= 2*n || nc <= 2*n) {
      stop("Padding size (", n, ") too large for image dimensions (",
           nr, " x ", nc, ")")
    }

    # Extract center portion
    unpadded <- img[(n+1):(nr-n), (n+1):(nc-n)]
  }

  unpadded
}

# Outline extraction ------------------------------------------------------

#' Extract outline from binary mask
#'
#' Traces the boundary of an object in a binary mask using Moore-Neighbor
#' chain code algorithm. Outline starts at rightmost point and traces
#' counter-clockwise (positive/mathematical direction).
#'
#' @param mask Logical matrix. TRUE = object, FALSE = background
#'
#' @return Matrix (n × 2) with x,y coordinates. Class c("out", "coo", "matrix", "array")
#' @export
#'
#' @examples
#' \dontrun{
#' # From mask matrix
#' mask <- img_mask("leaf.png")
#' outline <- trace_outline(mask)
#' }
trace_outline <- function(mask) {

  # Trace boundary using chain code algorithm
  coords <- trace_boundary(mask)

  if (nrow(coords) == 0) {
    stop("No object found in mask")
  }

  # Reorder to start at rightmost point
  coords <- reorder_start_right(coords)

  # Ensure counter-clockwise (positive) direction
  if (!is_clockwise(coords)) {
    coords <- coords[nrow(coords):1, ]
  }

  # Set classes
  class(coords) <- c("out", "coo", "matrix", "array")
  coords
}

#' Extract multiple outlines from binary mask
#'
#' Traces boundaries of all objects in a binary mask using base R's contourLines().
#' Each connected object gets its own outline.
#'
#' @param mask Logical matrix. TRUE = object, FALSE = background
#' @param min_size Integer. Minimum number of points for an outline. Default 10
#'
#' @return List of outline matrices, each (n × 2) with class c("out", "coo", "matrix", "array")
#' @export
#'
#' @examples
#' \dontrun{
#' # From mask matrix
#' img <- img_load("petri_dish.png")
#' mask <- img_mask(img)
#' outlines <- trace_outline_multi(mask)
#' }
trace_outline_multi <- function(mask, min_size = 10) {

  # Ensure mask has correct orientation (TRUE = object)
  if (mask[1, 1]) {
    mask <- !mask
  }

  # Use contourLines to extract all boundaries
  boundaries <- trace_boundary_multi(mask)

  if (length(boundaries) == 0) {
    stop("No boundaries found in mask")
  }

  # Process each boundary into a proper outline
  outlines <- lapply(boundaries, function(coords) {

    # Filter by minimum size
    if (nrow(coords) < min_size) {
      return(NULL)
    }

    # Reorder to start at rightmost point
    coords <- reorder_start_right(coords)

    # Ensure counter-clockwise (positive) direction
    if (!is_clockwise(coords)) {
      coords <- coords[nrow(coords):1, ]
    }

    # Set classes
    class(coords) <- c("out", "coo", "matrix", "array")
    coords
  })

  # Remove NULL entries (filtered by min_size)
  outlines <- outlines[!sapply(outlines, is.null)]

  if (length(outlines) == 0) {
    stop("No outlines remaining after filtering (min_size = ", min_size, ")")
  }

  message("Extracted ", length(outlines), " outlines")

  outlines
}


# Trace boundary using Moore-Neighbor chain code algorithm
trace_boundary <- function(mask) {
  nr <- nrow(mask)
  nc <- ncol(mask)

  # Background is defined as the top-left pixel
  # If top-left is TRUE, invert the mask so background becomes FALSE
  if (mask[1, 1]) {
    mask <- !mask
  }

  # Find starting point: scan from top-left for first TRUE pixel
  start_found <- FALSE
  for (i in 1:nr) {
    for (j in 1:nc) {
      if (mask[i, j]) {
        start_r <- i
        start_c <- j
        start_found <- TRUE
        break
      }
    }
    if (start_found) break
  }

  if (!start_found) {
    stop("No object pixels found in mask")
  }

  # Make sure we start at a boundary pixel (has at least one FALSE neighbor)
  # Scan from starting point to find actual boundary
  found_boundary <- FALSE
  for (i in start_r:nr) {
    for (j in 1:nc) {
      if (mask[i, j]) {
        # Check if this is a boundary pixel
        is_boundary <- FALSE
        for (di in -1:1) {
          for (dj in -1:1) {
            if (di == 0 && dj == 0) next
            ni <- i + di
            nj <- j + dj
            if (ni < 1 || ni > nr || nj < 1 || nj > nc) {
              is_boundary <- TRUE
              break
            }
            if (!mask[ni, nj]) {
              is_boundary <- TRUE
              break
            }
          }
          if (is_boundary) break
        }
        if (is_boundary) {
          start_r <- i
          start_c <- j
          found_boundary <- TRUE
          break
        }
      }
    }
    if (found_boundary) break
  }

  # Moore neighborhood: 8 directions clockwise from East
  # Row offset, Col offset
  moore <- matrix(c(
    0,  1,   # 0: E
    1,  1,   # 1: SE
    1,  0,   # 2: S
    1, -1,   # 3: SW
    0, -1,   # 4: W
    -1, -1,   # 5: NW
    -1,  0,   # 6: N
    -1,  1    # 7: NE
  ), ncol = 2, byrow = TRUE)

  # Boundary coordinates
  boundary_r <- c()
  boundary_c <- c()

  # Current position
  curr_r <- start_r
  curr_c <- start_c

  # Start by checking all directions to find first boundary pixel
  start_dir <- NULL
  for (dir in 0:7) {
    nr_check <- curr_r + moore[dir + 1, 1]
    nc_check <- curr_c + moore[dir + 1, 2]
    if (nr_check >= 1 && nr_check <= nr && nc_check >= 1 && nc_check <= nc) {
      if (!mask[nr_check, nc_check]) {
        # Found a background pixel, so previous direction is where we should start
        start_dir <- (dir + 7) %% 8  # Go back one direction
        break
      }
    }
  }

  if (is.null(start_dir)) {
    # Isolated pixel or solid region
    start_dir <- 4  # Default to West
  }

  prev_dir <- start_dir

  # Trace boundary
  max_iterations <- nr * nc * 2  # Safety limit
  iteration <- 0

  repeat {
    iteration <- iteration + 1
    if (iteration > max_iterations) {
      warning("Maximum iterations reached in boundary tracing")
      break
    }

    # Record current position
    boundary_r <- c(boundary_r, curr_r)
    boundary_c <- c(boundary_c, curr_c)

    # Search for next boundary pixel
    # Start searching from (prev_dir - 2) mod 8 (the backtrack direction)
    search_start <- (prev_dir + 6) %% 8

    found <- FALSE
    for (i in 0:7) {
      check_dir <- (search_start + i) %% 8
      new_r <- curr_r + moore[check_dir + 1, 1]
      new_c <- curr_c + moore[check_dir + 1, 2]

      # Check bounds
      if (new_r < 1 || new_r > nr || new_c < 1 || new_c > nc) next

      # Check if this is an object pixel
      if (mask[new_r, new_c]) {
        # Found next boundary pixel
        curr_r <- new_r
        curr_c <- new_c
        prev_dir <- check_dir
        found <- TRUE
        break
      }
    }

    if (!found) {
      # No neighbors found - shouldn't happen for valid boundary
      break
    }

    # Check if we've returned to start (need at least 3 pixels before checking)
    if (length(boundary_r) > 3) {
      if (curr_r == start_r && curr_c == start_c) {
        break
      }
    }
  }

  # Convert to x, y coordinates (col, row)
  # Flip y-coordinates so they match the displayed image orientation
  # When img_plot displays with [nr:1, ], row 1 appears at top
  # So y = nr - row + 1 makes y increase upward
  coords <- cbind(
    x = boundary_c,
    y = nr - boundary_r + 1
  )

  coords
}


#' Extract multiple boundaries from binary mask
#'
#' Low-level function that extracts raw boundary coordinates for all objects
#' in a mask using contourLines(). Returns list of coordinate matrices without
#' additional processing (no reordering, no direction checking).
#'
#' @param mask Logical matrix. TRUE = object, FALSE = background
#'
#' @return List of matrices, each (n × 2) with raw x,y coordinates
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' mask <- img_mask(img_load("petri_dish.png"))
#' boundaries <- trace_boundary_multi(mask)
#' }
trace_boundary_multi <- function(mask) {

  nr <- nrow(mask)
  nc <- ncol(mask)

  # Convert mask to numeric (contourLines needs numeric matrix)
  mask_numeric <- as.numeric(mask)
  dim(mask_numeric) <- c(nr, nc)

  # Extract contours at level 0.5 (between 0 and 1)
  # This gives us the boundary between FALSE (0) and TRUE (1)
  contours <- contourLines(
    x = 1:nc,
    y = 1:nr,
    z = t(mask_numeric),  # contourLines expects (x, y, z) with z as t(matrix)
    levels = 0.5
  )

  if (length(contours) == 0) {
    return(list())
  }

  # Convert each contour to coordinate matrix
  boundaries <- lapply(contours, function(cnt) {
    # Extract coordinates and flip y to match image orientation
    cbind(x = cnt$x, y = nr - cnt$y + 1)
  })

  boundaries
}

#' Extract outline from mask image (convenience wrapper)
#'
#' Loads image, converts to binary mask using flood fill, and extracts outline.
#' Combines img_load(), img_mask() and trace_outline().
#'
#' @param path Character. Path to image file
#' @param fuzz Integer. Fuzz/tolerance for flood fill (0-255). Default 20
#'
#' @return Matrix (n × 2) with x,y coordinates. Class c("out", "coo", "matrix", "array")
#' @export
#'
#' @examples
#' \dontrun{
#' # Simple use
#' outline <- from_mask("leaf.png")
#'
#' # Adjust fuzz for noisy backgrounds
#' outline <- from_mask("leaf.png", fuzz = 30)
#' }
from_mask <- function(path, fuzz = 20) {
  img <- img_load(path)
  mask <- img_mask(img, fuzz = fuzz)
  trace_outline(mask)
}

#' Extract multiple outlines from mask image
#'
#' Extracts outlines from a mask containing multiple objects using base R's
#' contourLines(). Each connected object gets its own outline. No external
#' dependencies required.
#'
#' @param path Character. Path to image file, or mask matrix
#' @param fuzz Integer. Fuzz/tolerance for flood fill (0-255). Default 20
#'   Only used if path is character
#' @param min_size Integer. Minimum number of points for an outline. Default 10
#'   Filters out tiny spurious contours
#'
#' @return List of outline matrices, each (n × 2) with class c("out", "coo", "matrix", "array")
#' @export
#'
#' @examples
#' \dontrun{
#' # Extract all objects
#' outlines <- from_mask_multi("petri_dish.png")
#' length(outlines)  # Number of objects found
#'
#' # Plot them
#' plot(outlines[[1]], type = "l", asp = 1)
#' for (i in 2:length(outlines)) {
#'   lines(outlines[[i]], col = i)
#' }
#' }
from_mask_multi <- function(path, fuzz = 20, min_size = 10) {

  # Get mask
  if (is.character(path)) {
    img <- img_load(path)
    mask <- img_mask(img, fuzz = fuzz)
  } else {
    mask <- path
  }

  # Ensure mask has correct orientation (TRUE = object)
  if (mask[1, 1]) {
    mask <- !mask
  }

  nr <- nrow(mask)
  nc <- ncol(mask)

  # Convert mask to numeric (contourLines needs numeric matrix)
  mask_numeric <- as.numeric(mask)
  dim(mask_numeric) <- c(nr, nc)

  # Extract contours at level 0.5 (between 0 and 1)
  # This gives us the boundary between FALSE (0) and TRUE (1)
  contours <- contourLines(
    x = 1:nc,
    y = 1:nr,
    z = t(mask_numeric),  # contourLines expects (x, y, z) with z as t(matrix)
    levels = 0.5
  )

  if (length(contours) == 0) {
    stop("No contours found in mask")
  }

  # Convert each contour to outline matrix
  outlines <- lapply(contours, function(cnt) {
    # Extract coordinates
    coords <- cbind(x = cnt$x, y = nr - cnt$y + 1)

    # Filter by minimum size
    if (nrow(coords) < min_size) {
      return(NULL)
    }

    # Reorder to start at rightmost point
    coords <- reorder_start_right(coords)

    # Ensure counter-clockwise (positive) direction
    if (!is_clockwise(coords)) {
      coords <- coords[nrow(coords):1, ]
    }

    # Set classes
    class(coords) <- c("out", "coo", "matrix", "array")
    coords
  })

  # Remove NULL entries (filtered by min_size)
  outlines <- outlines[!sapply(outlines, is.null)]

  if (length(outlines) == 0) {
    stop("No outlines remaining after filtering (min_size = ", min_size, ")")
  }

  message("Extracted ", length(outlines), " outlines")

  outlines
}

# Internal helpers --------------------------------------------------------

# Reorder outline to start at rightmost point
reorder_start_right <- function(coords) {
  n <- nrow(coords)

  # Find rightmost point (max x)
  start_idx <- which.max(coords[, 1])

  # Handle ties with topmost (min y)
  if (length(start_idx) > 1) {
    start_idx <- start_idx[which.min(coords[start_idx, 2])]
  }

  # Reorder
  if (start_idx == 1) {
    coords
  } else {
    rbind(coords[start_idx:n, ], coords[1:(start_idx-1), ])
  }
}

# Check if outline is clockwise using shoelace formula
is_clockwise <- function(coords) {
  # Shoelace formula for signed area
  n <- nrow(coords)
  area <- 0
  for (i in 1:n) {
    j <- ifelse(i == n, 1, i + 1)
    area <- area + (coords[j, 1] - coords[i, 1]) * (coords[j, 2] + coords[i, 2])
  }
  area > 0
}
