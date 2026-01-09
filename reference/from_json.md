# Import JSON to tibble with smart matrix reconstruction

Reads JSON and reconstructs matrix columns from coordinate arrays.
Automatically detects and applies column metadata if present.

## Usage

``` r
from_json(path = NULL, text = NULL, ...)
```

## Arguments

- path:

  Path to JSON file (optional if text provided)

- text:

  JSON string (optional if path provided)

- ...:

  Additional arguments passed to jsonlite::fromJSON

## Value

A tibble with matrix columns reconstructed and classes applied

## Examples

``` r
if (FALSE) { # \dontrun{
# From file
bot2 <- from_json("bot.json")

# From string
json_str <- to_json(bot)
bot3 <- from_json(text = json_str)
} # }
```
