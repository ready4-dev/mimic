# Make Minimum Dataset services tibble

make_mds_services_tb() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make minimum
dataset services tibble. The function returns Services (a tibble).

## Usage

``` r
make_mds_services_tb(
  processed_ls,
  end_dtm = lubridate::NA_Date_,
  episode_keys_chr = character(0),
  start_dtm = lubridate::NA_Date_,
  summarise_1L_lgl = FALSE
)
```

## Arguments

- processed_ls:

  Processed (a list)

- end_dtm:

  End (a date vector), Default: lubridate::NA_Date\_

- episode_keys_chr:

  Episode keys (a character vector), Default: character(0)

- start_dtm:

  Start (a date vector), Default: lubridate::NA_Date\_

- summarise_1L_lgl:

  Summarise (a logical vector of length one), Default: FALSE

## Value

Services (a tibble)
