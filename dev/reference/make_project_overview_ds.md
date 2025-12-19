# Make project overview dataset

make_project_overview_ds() is a Make function that creates a new R
object. Specifically, this function implements an algorithm to make
project overview dataset. The function returns Overview (a tibble).

## Usage

``` r
make_project_overview_ds(raw_data_ls, demographics_tb)
```

## Arguments

- raw_data_ls:

  Raw data (a list)

- demographics_tb:

  Demographics (a tibble)

## Value

Overview (a tibble)
