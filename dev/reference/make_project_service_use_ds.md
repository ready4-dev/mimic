# Make project service use dataset

make_project_service_use_ds() is a Make function that creates a new R
object. Specifically, this function implements an algorithm to make
project service use dataset. The function is called for its side effects
and does not return a value.

## Usage

``` r
make_project_service_use_ds(
  processed_ls,
  data_extract_dtm = as.POSIXct("2024-10-25")
)
```

## Arguments

- processed_ls:

  Processed (a list)

- data_extract_dtm:

  Data extract (a date vector), Default: as.POSIXct("2024-10-25")

## Value

Y (A dataset and data dictionary pair.)
