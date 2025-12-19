# Make actives tibble

make_actives_tb() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make actives
tibble. The function returns Actives (an output object of multiple
potential types).

## Usage

``` r
make_actives_tb(
  model_data_ls,
  as_tsibble_1L_lgl = FALSE,
  date_end_dtm = NULL,
  date_start_dtm = NULL,
  date_var_1L_chr = make_temporal_vars(),
  periods_1L_int = integer(0)
)
```

## Arguments

- model_data_ls:

  Model data (a list)

- as_tsibble_1L_lgl:

  As tsibble (a logical vector of length one), Default: FALSE

- date_end_dtm:

  Date end (a date vector), Default: NULL

- date_start_dtm:

  Date start (a date vector), Default: NULL

- date_var_1L_chr:

  Date variable (a character vector of length one), Default:
  make_temporal_vars()

- periods_1L_int:

  Periods (an integer vector of length one), Default: integer(0)

## Value

Actives (an output object of multiple potential types)
