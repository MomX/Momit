
#' Calculate object size in memory and on disk
#'
#' Returns size of R objects in memory and estimated/actual size on disk.
#' For objects: estimates .rda size by default, .json if object is JSON string.
#' For files: returns actual file size.
#'
#' @param x An R object, JSON string, or path to a file
#' @param units Character, one of "auto", "B", "KB", "MB", "GB". Default "auto"
#' @param compression Compression for .rda files. Default "gzip"
#'
#' @return A list with class "object_size" containing memory, disk, ratio, format
#'
#' @importFrom utils object.size
#' @export
object_size <- function(x, units = "auto", compression = "gzip") {

  # Check if x is a file path
  if (is.character(x) && length(x) == 1 && file.exists(x)) {
    return(file_size(x, units = units))
  }

  # Check if x is a JSON string (character vector from to_json)
  if (is.character(x) && length(x) == 1 && !file.exists(x)) {
    # Likely a JSON string
    return(json_string_size(x, units = units))
  }

  # Otherwise treat as R object
  memory_size <- as.numeric(object.size(x))

  # Estimate disk size by saving to temp file
  tmp <- tempfile(fileext = ".rda")
  on.exit(unlink(tmp), add = TRUE)

  save(x, file = tmp, compress = compression)
  disk_size <- file.info(tmp)$size

  # Calculate ratio
  ratio <- disk_size / memory_size

  # Format output
  result <- list(
    memory = format_bytes(memory_size, units),
    disk = format_bytes(disk_size, units),
    ratio = round(ratio, 3),
    format = ".rda"
  )

  class(result) <- c("object_size", "list")
  result
}

# Get size of JSON string (internal helper)
json_string_size <- function(json_str, units = "auto") {
  memory_size <- as.numeric(object.size(json_str))

  # Disk size would be the number of bytes when written
  disk_size <- nchar(json_str, type = "bytes")

  # Calculate ratio
  ratio <- disk_size / memory_size

  result <- list(
    memory = format_bytes(memory_size, units),
    disk = format_bytes(disk_size, units),
    ratio = round(ratio, 3),
    format = ".json"
  )

  class(result) <- c("object_size", "list")
  result
}

# Get file size (internal helper)
file_size <- function(path, units = "auto") {
  if (!file.exists(path)) {
    stop("File not found: ", path)
  }

  size <- file.info(path)$size

  # Detect file format from extension
  ext <- tolower(tools::file_ext(path))
  format <- if (ext == "") "file" else paste0(".", ext)

  result <- list(
    memory = NA,  # not applicable for files
    disk = format_bytes(size, units),
    ratio = NA,
    format = format
  )

  class(result) <- c("object_size", "list")
  result
}

# Format bytes to human-readable units
format_bytes <- function(bytes, units = "auto") {
  if (units == "auto") {
    # Auto-select appropriate unit
    if (bytes < 1024) {
      units <- "B"
    } else if (bytes < 1024^2) {
      units <- "KB"
    } else if (bytes < 1024^3) {
      units <- "MB"
    } else {
      units <- "GB"
    }
  }

  # Convert to requested unit
  value <- switch(units,
                  B = bytes,
                  KB = bytes / 1024,
                  MB = bytes / 1024^2,
                  GB = bytes / 1024^3,
                  stop("Invalid units: ", units)
  )

  # Format with appropriate precision
  if (value < 10) {
    formatted <- sprintf("%.2f %s", value, units)
  } else if (value < 100) {
    formatted <- sprintf("%.1f %s", value, units)
  } else {
    formatted <- sprintf("%.0f %s", value, units)
  }

  formatted
}

#' @export
print.object_size <- function(x, ...) {
  cat("Object size:\n")
  if (!is.na(x$memory)) {
    cat("  Memory (R):   ", x$memory, "\n", sep = "")
    cat("    Disk (", x$format, "):   ", x$disk, " (",
        round(x$ratio * 100, 1), "%)\n", sep = "")
  } else {
    cat("    Disk (", x$format, "):   ", x$disk, "\n", sep = "")
  }
  invisible(x)
}
