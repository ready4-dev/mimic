# Make project minutes dataset

make_project_minutes_ds() is a Make function that creates a new R
object. Specifically, this function implements an algorithm to make
project minutes dataset. The function is called for its side effects and
does not return a value.

## Usage

``` r
make_project_minutes_ds(
  processed_ls,
  cut_weeks_int = c(14, 53),
  drop_1L_lgl = TRUE,
  model_data_ls = NULL,
  period_dtm = lubridate::years(1) - lubridate::days(1),
  type_1L_chr = c("imputed", "unimputed"),
  what_1L_chr = c("wide", "long")
)
```

## Arguments

- processed_ls:

  Processed (a list)

- cut_weeks_int:

  Cut weeks (an integer vector), Default: c(14, 53)

- drop_1L_lgl:

  Drop (a logical vector of length one), Default: TRUE

- model_data_ls:

  Model data (a list), Default: NULL

- period_dtm:

  Period (a date vector), Default: lubridate::years(1) -
  lubridate::days(1)

- type_1L_chr:

  Type (a character vector of length one), Default: c("imputed",
  "unimputed")

- what_1L_chr:

  What (a character vector of length one), Default: c("wide", "long")

## Value

X (A dataset and data dictionary pair.)
