# Export tibble to JSON with smart matrix handling

Converts tibbles to JSON, intelligently handling matrix columns as
arrays of coordinate pairs. Optionally preserves column classes in
metadata block.

## Usage

``` r
to_json(x, path = NULL, pretty = TRUE, preserve_classes = TRUE, ...)
```

## Arguments

- x:

  A tibble, typically from Momocs2

- path:

  Optional path to write JSON file. If NULL, returns JSON string

- pretty:

  Logical, pretty-print JSON? Default TRUE

- preserve_classes:

  Logical, preserve column classes in metadata? Default TRUE

- ...:

  Additional arguments passed to jsonlite::toJSON

## Value

If path: invisible(x). Otherwise: JSON string

## Examples

``` r
if (FALSE) { # \dontrun{
# Return JSON string
json_str <- to_json(bot)

# Write to file with metadata
to_json(bot, "bot.json")

# Write without metadata (smaller files)
to_json(bot, "bot_bare.json", preserve_classes = FALSE)
} # }
```
