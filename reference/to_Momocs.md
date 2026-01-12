# Export tibble to Momocs format

Converts a Momocs2 tibble back to legacy Momocs format (Out, Opn, or
Ldk). This is a regressive function but may be useful for compatibility
with legacy Momocs code or other tools.

## Usage

``` r
to_Momocs(x, type = NULL)
```

## Arguments

- x:

  A tibble with a "coo" list column

- type:

  Character. Type of Momocs object to create: "Out", "Opn", or "Ldk". If
  NULL (default), inferred from the class of the coo column.

## Value

A Momocs object (Out, Opn, or Ldk) with:

- \$coo: list of coordinate matrices (without classes)

- \$fac: tibble with all non-coo, non-ldk_id columns

- \$ldk: list of landmark positions (if ldk_id column present)

## Examples

``` r
if (FALSE) { # \dontrun{
# Convert from Momocs
bot_tbl <- from_Momocs(bot)

# Do some processing...

# Convert back to Momocs
bot_back <- to_Momocs(bot_tbl)

# Check class
class(bot_back)  # "Out" "Coo"
} # }
```
