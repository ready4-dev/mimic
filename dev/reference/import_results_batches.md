# Import results batches

import_results_batches() is an Import function that reads a data object
in its native format and converts it to an R object. Specifically, this
function implements an algorithm to import results batches. The function
returns Results (a list).

## Usage

``` r
import_results_batches(
  batches_1L_int = integer(0),
  dir_1L_chr = character(0),
  drop_params_1L_lgl = FALSE,
  ratify_1L_lgl = FALSE,
  suffix_1L_chr = "",
  use_chr = character(0),
  Y_MimicRepos = MimicRepos()
)
```

## Arguments

- batches_1L_int:

  Batches (an integer vector of length one), Default: integer(0)

- dir_1L_chr:

  Directory (a character vector of length one)

- drop_params_1L_lgl:

  Drop parameters (a logical vector of length one), Default: FALSE

- ratify_1L_lgl:

  Ratify (a logical vector of length one), Default: FALSE

- suffix_1L_chr:

  Suffix (a character vector of length one), Default: ""

- use_chr:

  Use (a character vector), Default: character(0)

- Y_MimicRepos:

  (an instance of the MimicRepos class), Default: MimicRepos()

## Value

Results (a list)
