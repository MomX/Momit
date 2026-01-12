#' @importFrom tibble tibble
NULL

# Export to Momocs ------------------------------------------------------

#' Export tibble to Momocs format
#'
#' Converts a Momocs2 tibble back to legacy Momocs format (Out, Opn, or Ldk).
#' This is a regressive function but may be useful for compatibility with
#' legacy Momocs code or other tools.
#'
#' @param x A tibble with a "coo" list column
#' @param type Character. Type of Momocs object to create: "Out", "Opn", or "Ldk".
#'   If NULL (default), inferred from the class of the coo column.
#'
#' @return A Momocs object (Out, Opn, or Ldk) with:
#'   - $coo: list of coordinate matrices (without classes)
#'   - $fac: tibble with all non-coo, non-ldk_id columns
#'   - $ldk: list of landmark positions (if ldk_id column present)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Convert from Momocs
#' bot_tbl <- from_Momocs(bot)
#'
#' # Do some processing...
#'
#' # Convert back to Momocs
#' bot_back <- to_Momocs(bot_tbl)
#'
#' # Check class
#' class(bot_back)  # "Out" "Coo"
#' }
to_Momocs <- function(x, type = NULL) {

  if (!"coo" %in% names(x)) {
    stop("Tibble must have a 'coo' column")
  }

  # Infer type from coo column class if not specified
  if (is.null(type)) {
    coo_class <- class(x$coo)
    if ("out" %in% coo_class) {
      type <- "Out"
    } else if ("cur" %in% coo_class) {
      type <- "Opn"
    } else if ("ldk" %in% coo_class) {
      type <- "Ldk"
    } else {
      stop("Cannot infer Momocs type. Specify 'type' argument.")
    }
  }

  type <- match.arg(type, c("Out", "Opn", "Ldk"))

  # Extract coo list (remove classes from matrices)
  coo_list <- lapply(x$coo, function(m) {
    class(m) <- c("matrix", "array")
    m
  })

  # Set names from row names or create default names
  if (!is.null(rownames(x))) {
    names(coo_list) <- rownames(x)
  } else if (nrow(x) > 0) {
    names(coo_list) <- paste0("shp", seq_len(nrow(x)))
  }

  # Extract fac (all columns except coo and ldk_id)
  exclude_cols <- c("coo", "ldk_id")
  fac_cols <- setdiff(names(x), exclude_cols)

  if (length(fac_cols) > 0) {
    fac <- x[, fac_cols, drop = FALSE]
  } else {
    # Empty tibble with correct number of rows
    fac <- tibble::tibble(.rows = nrow(x))
  }

  # Create base object
  result <- list(
    coo = coo_list,
    fac = fac
  )

  # Add ldk if present
  if ("ldk_id" %in% names(x)) {
    result$ldk <- unclass(x$ldk_id)
    names(result$ldk) <- names(coo_list)
  } else {
    result$ldk <- list()
  }

  # Set Momocs class
  class(result) <- c(type, "Coo")

  result
}
