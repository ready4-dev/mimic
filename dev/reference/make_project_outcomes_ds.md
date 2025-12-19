# Make project outcomes dataset

make_project_outcomes_ds() is a Make function that creates a new R
object. Specifically, this function implements an algorithm to make
project outcomes dataset. The function returns Outcomes (an output
object of multiple potential types).

## Usage

``` r
make_project_outcomes_ds(
  data_ls,
  as_dyad_1L_lgl = T,
  cleanse_1L_chr = c("all", "none", "dims", "items", "itemdims"),
  demographics_tb = tibble::tibble(),
  complete_1L_lgl = TRUE,
  drop_1L_lgl = TRUE,
  drop_chr = character(0),
  mdls_lup = NULL,
  mdl_meta_data_ls = NULL,
  outcomes_chr = c("gad2", "phq2", "gad7", "phq9", "k10", "chu9d_utl"),
  start_at_1L_int = -2L,
  uid_1L_chr = "UID",
  weeks_dbl = c(0, 12),
  Y_Ready4useDyad = ready4use::Ready4useDyad(),
  type_1L_chr = c("final", "initial")
)
```

## Arguments

- data_ls:

  Data (a list)

- as_dyad_1L_lgl:

  As dyad (a logical vector of length one), Default: T

- cleanse_1L_chr:

  Cleanse (a character vector of length one), Default: c("all", "none",
  "dims", "items", "itemdims")

- demographics_tb:

  Demographics (a tibble), Default: tibble::tibble()

- complete_1L_lgl:

  Complete (a logical vector of length one), Default: TRUE

- drop_1L_lgl:

  Drop (a logical vector of length one), Default: TRUE

- drop_chr:

  Drop (a character vector), Default: character(0)

- mdls_lup:

  Models (a lookup table), Default: NULL

- mdl_meta_data_ls:

  Model meta data (a list), Default: NULL

- outcomes_chr:

  Outcomes (a character vector), Default: c("gad2", "phq2", "gad7",
  "phq9", "k10", "chu9d_utl")

- start_at_1L_int:

  Start at (an integer vector of length one), Default: -2L

- uid_1L_chr:

  Unique identifier (a character vector of length one), Default: 'UID'

- weeks_dbl:

  Weeks (a double vector), Default: c(0, 12)

- Y_Ready4useDyad:

  PARAM_DESCRIPTION, Default: ready4use::Ready4useDyad()

- type_1L_chr:

  Type (a character vector of length one), Default: c("final",
  "initial")

## Value

Outcomes (an output object of multiple potential types)
