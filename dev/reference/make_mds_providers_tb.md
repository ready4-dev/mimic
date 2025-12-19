# Make Minimum Dataset providers tibble

make_mds_providers_tb() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make minimum
dataset providers tibble. The function returns Providers (a tibble).

## Usage

``` r
make_mds_providers_tb(raw_mds_data_ls, additional_chr = character(0))
```

## Arguments

- raw_mds_data_ls:

  Raw Minimum Dataset data (a list)

- additional_chr:

  Additional (a character vector), Default: character(0)

## Value

Providers (a tibble)
