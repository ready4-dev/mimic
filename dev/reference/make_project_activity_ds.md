# Make project activity dataset

make_project_activity_ds() is a Make function that creates a new R
object. Specifically, this function implements an algorithm to make
project activity dataset. The function returns Activity (a tibble).

## Usage

``` r
make_project_activity_ds(raw_data_ls, type_1L_chr = c("initial", "wip"))
```

## Arguments

- raw_data_ls:

  Raw data (a list)

- type_1L_chr:

  Type (a character vector of length one), Default: c("initial", "wip")

## Value

Activity (a tibble)
