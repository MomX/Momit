#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols
NULL

# Import from Momocs ------------------------------------------------------

#' Import Momocs objects to tibble format
#'
#' Converts legacy Momocs objects (Out, Opn, Ldk classes) to tidy tibble format
#' compatible with Momocs2. Combines the $coo list and $fac data frame into a
#' single tibble with list columns.
#'
#' @param x A Momocs object (Out, Opn, or Ldk)
#'
#' @return A tibble with:
#'   - List column "coo" with classes:
#'     - c("out", "coo") for Out objects (closed outlines)
#'     - c("cur", "coo") for Opn objects (open curves)
#'     - c("ldk", "coo") for Ldk objects (landmarks)
#'   - Individual matrices in coo have class "xy"
#'   - Additional list column "ldk_id" if landmarks were present
#'   - All columns from $fac (can be empty)
#' @export
#'
#' @examples
#' \dontrun{
#' # Load Momocs object
#' momit_load("Momocs/bot.rda")
#'
#' # Convert to tibble
#' bot_tbl <- from_Momocs(bot)
#'
#' # Check structure
#' class(bot_tbl$coo)     # "out" "coo" "list"
#' class(bot_tbl$coo[[1]]) # "xy" "matrix" "array"
#' }
from_Momocs <- function(x) {

  # Determine coordinate type based on Momocs class
  partition_class <- if (inherits(x, "Out")) {
    "out"
  } else if (inherits(x, "Opn")) {
    "cur"
  } else if (inherits(x, "Ldk")) {
    "ldk"
  } else {
    stop("Unsupported Momocs class. Must be Out, Opn, or Ldk")
  }

  # Apply "xy" class to each coordinate matrix
  coo_list <- lapply(x$coo, function(coords) {
    class(coords) <- c("xy", "matrix", "array")
    coords
  })

  # Set class on the list column itself
  class(coo_list) <- c(partition_class, "coo", "list")

  # Create base tibble with coordinates
  result <- tibble::tibble(coo = coo_list)

  # Add landmarks as separate column if present and non-empty
  if (!is.null(x$ldk) && length(x$ldk) > 0) {
    # Keep individual vectors as plain numeric
    ldk_list <- x$ldk

    # Set class on the list column itself
    class(ldk_list) <- c("ldk_id", "list")

    result$ldk_id <- ldk_list
  }

  # Combine with $fac if present and non-empty
  if (!is.null(x$fac) && nrow(x$fac) > 0) {
    result <- dplyr::bind_cols(result, x$fac)
  }

  result
}
