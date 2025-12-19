# Import results batches

import_results_batches() is an Import function that reads a data object
in its native format and converts it to an R object. Specifically, this
function implements an algorithm to import results batches. The function
returns Results (a list).

## Usage

``` r
import_results_batches(batches_1L_int = integer(0), dir_1L_chr)
```

## Arguments

- batches_1L_int:

  Batches (an integer vector of length one), Default: integer(0)

- dir_1L_chr:

  Directory (a character vector of length one)

## Value

Results (a list)
