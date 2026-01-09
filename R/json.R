#' Export tibble to JSON with smart matrix handling
#'
#' Converts tibbles to JSON, intelligently handling matrix columns as arrays
#' of coordinate pairs. Optionally preserves column classes in metadata block.
#'
#' @param x A tibble, typically from Momocs2
#' @param path Optional path to write JSON file. If NULL, returns JSON string
#' @param pretty Logical, pretty-print JSON? Default TRUE
#' @param preserve_classes Logical, preserve column classes in metadata? Default TRUE
#' @param ... Additional arguments passed to jsonlite::toJSON
#'
#' @return If path: invisible(x). Otherwise: JSON string
#' @export
#'
#' @examples
#' \dontrun{
#' # Return JSON string
#' json_str <- to_json(bot)
#'
#' # Write to file with metadata
#' to_json(bot, "bot.json")
#'
#' # Write without metadata (smaller files)
#' to_json(bot, "bot_bare.json", preserve_classes = FALSE)
#' }
to_json <- function(x, path = NULL, pretty = TRUE,
                    preserve_classes = TRUE, ...) {

  # Prepare data: convert matrices to lists of coordinate pairs
  x_prepared <- prepare_for_json(x)

  # Build output structure
  if (preserve_classes) {
    col_metadata <- extract_column_metadata(x)
    output <- list(
      metadata = list(
        version = "0.1.0",
        created = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
        n_rows = nrow(x),
        columns = col_metadata
      ),
      data = x_prepared
    )
  } else {
    output <- x_prepared
  }

  # Convert to JSON
  json_str <- jsonlite::toJSON(
    output,
    pretty = pretty,
    dataframe = "rows",
    auto_unbox = TRUE,
    null = "null",
    ...
  )

  # Write to file or return string
  if (!is.null(path)) {
    writeLines(json_str, path)
    invisible(x)
  } else {
    json_str
  }
}

#' Import JSON to tibble with smart matrix reconstruction
#'
#' Reads JSON and reconstructs matrix columns from coordinate arrays.
#' Automatically detects and applies column metadata if present.
#'
#' @param path Path to JSON file (optional if text provided)
#' @param text JSON string (optional if path provided)
#' @param ... Additional arguments passed to jsonlite::fromJSON
#'
#' @return A tibble with matrix columns reconstructed and classes applied
#' @export
#'
#' @examples
#' \dontrun{
#' # From file
#' bot2 <- from_json("bot.json")
#'
#' # From string
#' json_str <- to_json(bot)
#' bot3 <- from_json(text = json_str)
#' }
from_json <- function(path = NULL, text = NULL, ...) {

  # Read JSON from file or string
  if (!is.null(path)) {
    json <- jsonlite::fromJSON(
      path,
      simplifyVector = TRUE,
      simplifyDataFrame = TRUE,
      ...
    )
  } else if (!is.null(text)) {
    json <- jsonlite::fromJSON(
      text,
      simplifyVector = TRUE,
      simplifyDataFrame = TRUE,
      ...
    )
  } else {
    stop("Must provide either 'path' or 'text'")
  }

  # Detect structure: with or without metadata
  if ("data" %in% names(json) && "metadata" %in% names(json)) {
    # Structured format with metadata
    x <- tibble::as_tibble(json$data)
    col_metadata <- json$metadata$columns
  } else {
    # Simple format without metadata
    x <- tibble::as_tibble(json)
    col_metadata <- NULL
  }

  # Reconstruct matrix columns
  x <- reconstruct_matrices(x)

  # Apply column classes from metadata
  if (!is.null(col_metadata)) {
    x <- apply_column_metadata(x, col_metadata)
  }

  x
}

# Internal helpers --------------------------------------------------------

# Prepare tibble for JSON export by converting matrix columns to nested lists
prepare_for_json <- function(x) {
  matrix_cols <- detect_matrix_columns(x)

  if (length(matrix_cols) == 0) {
    return(x)
  }

  # Convert each matrix column to list of coordinate vectors
  x_prepared <- x
  for (col in matrix_cols) {
    x_prepared[[col]] <- purrr::map(x[[col]], function(m) {
      if (is.null(m)) {
        return(NULL)
      }
      # Convert matrix to list of rows (coordinate pairs/triplets)
      lapply(seq_len(nrow(m)), function(i) as.numeric(m[i, ]))
    })
  }

  x_prepared
}

# Extract column classes and attributes for metadata
extract_column_metadata <- function(x) {
  metadata <- list()

  for (col in names(x)) {
    col_data <- x[[col]]

    # For list columns (like matrices)
    if (is.list(col_data)) {
      # Get column-level class
      col_class <- class(col_data)

      # Get first non-NULL element for element-level class
      first_valid <- purrr::detect(col_data, ~!is.null(.x))
      if (!is.null(first_valid)) {
        metadata[[col]] <- list(
          col_class = col_class,  # class of the column itself (e.g., "out", "list")
          elem_class = class(first_valid),  # class of elements (e.g., "out", "coo", "matrix")
          type = if (is.matrix(first_valid)) "matrix" else "list",
          dims = if (is.matrix(first_valid)) dim(first_valid) else NULL
        )
      }
    } else {
      # For regular columns (character, factor, numeric, etc.)
      metadata[[col]] <- list(
        col_class = class(col_data),
        type = typeof(col_data)
      )

      # Store factor levels if applicable
      if (is.factor(col_data)) {
        metadata[[col]]$levels <- levels(col_data)
      }
    }
  }

  metadata
}

# Apply column metadata (classes) to reconstructed tibble
apply_column_metadata <- function(x, col_metadata) {
  for (col in names(col_metadata)) {
    if (!(col %in% names(x))) next

    meta <- col_metadata[[col]]

    # Handle list columns (matrices and other list elements)
    if (is.list(x[[col]])) {
      # First, apply class to each non-NULL element in list column
      if (!is.null(meta$elem_class)) {
        x[[col]] <- purrr::map(x[[col]], function(elem) {
          if (!is.null(elem)) {
            class(elem) <- meta$elem_class
          }
          elem
        })
      }

      # Then, apply class to the column itself
      if (!is.null(meta$col_class)) {
        class(x[[col]]) <- meta$col_class
      }
    }
    # Handle factors
    else if (!is.null(meta$col_class) && "factor" %in% meta$col_class) {
      if (!is.null(meta$levels)) {
        x[[col]] <- factor(x[[col]], levels = meta$levels)
      } else {
        x[[col]] <- as.factor(x[[col]])
      }
    }
  }

  x
}
# Reconstruct matrices from JSON-imported coordinate arrays
reconstruct_matrices <- function(x) {
  coord_cols <- detect_coordinate_columns(x)

  if (length(coord_cols) == 0) {
    return(x)
  }

  # Reconstruct each coordinate column as matrix
  x_reconstructed <- x
  for (col in coord_cols) {
    x_reconstructed[[col]] <- purrr::map(x[[col]], function(coords) {
      if (is.null(coords) || length(coords) == 0) {
        return(NULL)
      }
      # Convert list of coordinate vectors to matrix
      do.call(rbind, lapply(coords, as.numeric))
    })
  }

  x_reconstructed
}

# Detect matrix columns in tibble (list columns containing matrices)
detect_matrix_columns <- function(x) {
  is_matrix_col <- purrr::map_lgl(x, function(col) {
    if (!is.list(col)) return(FALSE)
    # Check first non-NULL element
    first_valid <- purrr::detect(col, ~!is.null(.x))
    if (is.null(first_valid)) return(FALSE)
    is.matrix(first_valid)
  })

  names(x)[is_matrix_col]
}

# Detect coordinate array columns (lists of numeric 2D/3D points)
detect_coordinate_columns <- function(x) {
  is_coord_col <- purrr::map_lgl(x, function(col) {
    if (!is.list(col)) return(FALSE)

    # Check first non-NULL element
    first_valid <- purrr::detect(col, ~!is.null(.x) && length(.x) > 0)
    if (is.null(first_valid)) return(FALSE)

    # Should be a list of numeric vectors
    if (!is.list(first_valid)) return(FALSE)

    # Check first coordinate
    first_coord <- first_valid[[1]]
    if (!is.numeric(first_coord)) return(FALSE)

    # Should be length 2 or 3 (xy or xyz)
    coord_length <- length(first_coord)
    if (!(coord_length %in% 2:3)) return(FALSE)

    # Check that all coordinates have same length
    all_same_length <- all(purrr::map_int(first_valid, length) == coord_length)
    all_same_length
  })

  names(x)[is_coord_col]
}
