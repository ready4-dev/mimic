# Make results matrix

make_results_matrix() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make results
matrix. The function returns Results (a matrix).

## Usage

``` r
make_results_matrix(
  data_tb,
  names_chr,
  arms_1L_chr = "Data",
  var_1L_chr = "Cost"
)
```

## Arguments

- data_tb:

  Data (a tibble)

- names_chr:

  Names (a character vector)

- arms_1L_chr:

  Arms (a character vector of length one), Default: 'Data'

- var_1L_chr:

  Variable (a character vector of length one), Default: 'Cost'

## Value

Results (a matrix)
