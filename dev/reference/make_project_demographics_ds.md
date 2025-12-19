# Make project demographics dataset

make_project_demographics_ds() is a Make function that creates a new R
object. Specifically, this function implements an algorithm to make
project demographics dataset. The function returns Demographics (a
tibble).

## Usage

``` r
make_project_demographics_ds(raw_data_ls, uid_1L_chr = "UID")
```

## Arguments

- raw_data_ls:

  Raw data (a list)

- uid_1L_chr:

  Unique identifier (a character vector of length one), Default: 'UID'

## Value

Demographics (a tibble)
