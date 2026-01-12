# Get path to example files

Get path to example files

## Usage

``` r
momit_example(file = NULL)
```

## Arguments

- file:

  Character. File name or path within extdata

## Examples

``` r
momit_example() # all examples
#>  [1] "Momocs/bot.rda"                                         
#>  [2] "Momocs/chaff.rda"                                       
#>  [3] "Momocs/hearts.rda"                                      
#>  [4] "Momocs/olea.rda"                                        
#>  [5] "malus/malus_1.jpg"                                      
#>  [6] "malus/malus_2.jpg"                                      
#>  [7] "nts/RohlfArchieWingOutlines.nts"                        
#>  [8] "nts/RohlfSlice1990Mosq.nts"                             
#>  [9] "tps/AE/data1.tps"                                       
#> [10] "tps/AE/data2.tps"                                       
#> [11] "tps/JC/data.TPS"                                        
#> [12] "tpsdig/allo1.tps"                                       
#> [13] "tpsdig/allo2.tps"                                       
#> [14] "txt/face1.txt"                                          
#> [15] "txt/face2.txt"                                          
#> [16] "vitis/jpg/0641_cPinNoN_B/0641_cPinNoN_B-B02_P01_VD.jpg" 
#> [17] "vitis/jpg/0641_cPinNoN_B/0641_cPinNoN_B_B01_P01_VD.jpg" 
#> [18] "vitis/jpg/0641_cPinNoN_B/0641_cPinNoN_B_B01_P01_VL.jpg" 
#> [19] "vitis/jpg/0641_cPinNoN_B/0641_cPinNoN_B_B02_P01_VL.jpg" 
#> [20] "vitis/jpg/0641_cPinNoN_B/0641_cPinNoN_B_B03_P02_VL.jpg" 
#> [21] "vitis/jpg/0641_cPinNoN_B/0641b_cPinNoN_B_B03_P02_VD.jpg"
#> [22] "vitis/jpg/0786_cCabSaN_B/0786-cCabSaN_B_B02_P01_VL.jpg" 
#> [23] "vitis/jpg/0786_cCabSaN_B/0786_cCabSaN_B_B01_P01_VD.jpg" 
#> [24] "vitis/jpg/0786_cCabSaN_B/0786_cCabSaN_B_B01_P01_VL.jpg" 
#> [25] "vitis/jpg/0786_cCabSaN_B/0786_cCabSaN_B_B02_P01_VD.jpg" 
#> [26] "vitis/jpg/0786_cCabSaN_B/0786_cCabSaN_B_B03_P01_VD.jpg" 
#> [27] "vitis/jpg/0786_cCabSaN_B/0786_cCabSaN_B_B03_P01_VL.jpg" 
#> [28] "vitis/meta.csv"                                         
# a text like file would return path and be good for from_tps etc.
momit_example("tps/AE/data1.tps")
#> [1] "/home/runner/work/_temp/Library/Momit/extdata/tps/AE/data1.tps"

# note that .rda example must be either embraced with load()
# or be piped with %>% load(envir = .GlobalEnv)
# this will work:
load(momit_example("Momocs/bot.rda"))
# this will also work:
momit_example("Momocs/bot.rda") %>% load(envir = .GlobalEnv)
# but this will NOT work:
momit_example("Momocs/bot.rda") %>% load()
```
