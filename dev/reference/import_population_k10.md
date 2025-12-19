# Import population K10

import_population_k10() is an Import function that reads a data object
in its native format and converts it to an R object. Specifically, this
function implements an algorithm to import population k10. The function
returns Population K10 (a tibble).

## Usage

``` r
import_population_k10(
  dir_1L_chr,
  fl_nm_1L_chr = "HILDA k10.xlsx",
  areas_chr = c("Intervention", "Matched"),
  divider_1L_chr = "\\"
)
```

## Arguments

- dir_1L_chr:

  Directory (a character vector of length one)

- fl_nm_1L_chr:

  File name (a character vector of length one), Default: 'HILDA
  k10.xlsx'

- areas_chr:

  Areas (a character vector), Default: c("Intervention", "Matched")

- divider_1L_chr:

  Divider (a character vector of length one), Default: '\\

## Value

Population K10 (a tibble)
