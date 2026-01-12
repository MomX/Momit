# Import Momocs objects to tibble format

Converts legacy Momocs objects (Out, Opn, Ldk classes) to tidy tibble
format compatible with Momocs2. Combines the \$coo list and \$fac data
frame into a single tibble with list columns.

## Usage

``` r
from_Momocs(x)
```

## Arguments

- x:

  A Momocs object (Out, Opn, or Ldk)

## Value

A tibble with:

- List column "coo" with classes:

  - c("out", "coo") for Out objects (closed outlines)

  - c("cur", "coo") for Opn objects (open curves)

  - c("ldk", "coo") for Ldk objects (landmarks)

- Individual matrices in coo have class "xy"

- Additional list column "ldk_id" if landmarks were present

- All columns from \$fac (can be empty)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load Momocs object
momit_load("Momocs/bot.rda")

# Convert to tibble
bot_tbl <- from_Momocs(bot)

# Check structure
class(bot_tbl$coo)     # "out" "coo" "list"
class(bot_tbl$coo[[1]]) # "xy" "matrix" "array"
} # }
```
