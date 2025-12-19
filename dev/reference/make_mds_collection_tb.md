# Make Minimum Dataset collection tibble

make_mds_collection_tb() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make minimum
dataset collection tibble. The function returns Collection (a tibble).

## Usage

``` r
make_mds_collection_tb(
  raw_mds_data_ls,
  drop_chr = c("collection_occasion_tags", "X.covid19")
)
```

## Arguments

- raw_mds_data_ls:

  Raw Minimum Dataset data (a list)

- drop_chr:

  Drop (a character vector), Default: c("collection_occasion_tags",
  "X.covid19")

## Value

Collection (a tibble)
