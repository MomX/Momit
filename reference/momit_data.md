# Example data files in inst/extdata

The Momit package includes example data files for testing and
demonstrations. Access them with
[`momit_example()`](https://momx.github.io/Momit/reference/momit_example.md).

## Format

- **malus/**:

  Apple leaf images (2 JPG files)

- **vitis/**:

  Grapevine leaf images organized by specimen: - `0641_cPinNoN_B/` -
  Pinot Noir specimen (6 JPG files, dorsal/ventral views) -
  `0786_cCabSaN_B/` - Cabernet Sauvignon specimen (6 JPG files,
  dorsal/ventral views) - `meta.csv` - Metadata for vitis specimens

- **Momocs/**:

  Legacy Momocs datasets (.rda files): - `bot.rda` - Bottle outlines
  dataset - `chaff.rda` - Cereal chaff outlines - `hearts.rda` - Heart
  shapes dataset - `olea.rda` - Olive stones dataset

- **tps/**:

  TPS landmark files (various sources): - `AE/` - Two data files -
  `JC/` - Single dataset

- **tpsdig/**:

  TPS files from tpsdig software (2 files)

- **nts/**:

  NTS format files: - `RohlfArchieWingOutlines.nts` - Wing outlines -
  `RohlfSlice1990Mosq.nts` - Mosquito data

- **txt/**:

  Plain text coordinate files (2 face datasets)

## Examples

``` r
if (FALSE) { # \dontrun{
# List all available files
momit_example()

# Get path to specific file
momit_example("malus/malus_1.jpg")

# Load and use an image
img <- img_load(momit_example("malus/malus_1.jpg"))
img_plot(img)

# Load Momocs dataset
momit_example("Momocs/bot.rda") %>% load(envir = .GlobalEnv)
} # }
```
