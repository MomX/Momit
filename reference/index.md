# Package index

## Data import

Import morphometric data

- [`from_Momocs()`](https://momx.github.io/Momit/reference/from_Momocs.md)
  : Import Momocs objects to tibble format
- [`from_json()`](https://momx.github.io/Momit/reference/from_json.md) :
  Import JSON to tibble with smart matrix reconstruction
- [`from_mask()`](https://momx.github.io/Momit/reference/from_mask.md) :
  Extract outline from mask image (convenience wrapper)
- [`from_mask_multi()`](https://momx.github.io/Momit/reference/from_mask_multi.md)
  : Extract multiple outlines from mask image

## Data export

Export morphometric data

- [`to_Momocs()`](https://momx.github.io/Momit/reference/to_Momocs.md) :
  Export tibble to Momocs format
- [`to_json()`](https://momx.github.io/Momit/reference/to_json.md) :
  Export tibble to JSON with smart matrix handling

## Image loading and preprocessing

Functions for loading and preparing images for outline extraction

- [`img_load()`](https://momx.github.io/Momit/reference/img_load.md) :
  Load image as grayscale matrix
- [`img_mask()`](https://momx.github.io/Momit/reference/img_mask.md) :
  Convert image to binary mask using flood fill
- [`img_pad()`](https://momx.github.io/Momit/reference/img_pad.md) : Add
  padding (border) to image
- [`img_unpad()`](https://momx.github.io/Momit/reference/img_unpad.md) :
  Remove padding (border) from image
- [`img_plot()`](https://momx.github.io/Momit/reference/img_plot.md) :
  Plot image matrix

## Outline extraction

Extract outlines from mask images

- [`from_mask()`](https://momx.github.io/Momit/reference/from_mask.md) :
  Extract outline from mask image (convenience wrapper)
- [`from_mask_multi()`](https://momx.github.io/Momit/reference/from_mask_multi.md)
  : Extract multiple outlines from mask image
- [`trace_outline()`](https://momx.github.io/Momit/reference/trace_outline.md)
  : Extract outline from binary mask
- [`trace_outline_multi()`](https://momx.github.io/Momit/reference/trace_outline_multi.md)
  : Extract multiple outlines from binary mask

## Raw data files

Helper functions for examples and data inspection

- [`momit_example()`](https://momx.github.io/Momit/reference/momit_example.md)
  : Get path to example files
- [`momit_data`](https://momx.github.io/Momit/reference/momit_data.md) :
  Example data files in inst/extdata
- [`obj_size()`](https://momx.github.io/Momit/reference/obj_size.md) :
  Calculate object size in memory and on disk
